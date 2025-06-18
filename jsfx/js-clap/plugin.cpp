#include "clap/events.h"
#include "clap/ext/params.h"
#include "clap/factory/plugin-factory.h"
#include "clap/id.h"
#include "dirscan.h"
#include "windows.h"
#include <handleapi.h>
#include <windowsx.h>
#undef min
#undef max
#include "clap/clap.h"
#include "clap/helpers/plugin.hh"
#include "clap/helpers/plugin.hxx"
#include "clap/helpers/host-proxy.hh"
#include "clap/helpers/host-proxy.hxx"
#include "clap/plugin-features.h"
#include "gui/choc_MessageLoop.h"
#include "containers/choc_SingleReaderSingleWriterFIFO.h"
#include "audio/choc_SampleBuffers.h"
#include <cstdint>
#include <unordered_map>
#include "../sfxui.h"
#include "../reajs-vst2/resource.h"

HINSTANCE g_hInst;
bool g_allow_edit = false;
char g_default_effect[256]; // if [0] == '!' then disable switching
void Sliders_Init(HINSTANCE hInst, bool reg, int hslider_res_id = 0); // slider-control.cpp
void Meters_Init(HINSTANCE hInst, bool reg);                          // meter-control.cpp

struct UiMessage
{
    UiMessage() {}
    UiMessage(int type_, clap_id parid_, float val) : type{type_}, parid{parid_}, values{val} {}
    int type = 0;
    clap_id parid = CLAP_INVALID_ID;
    float values[6] = {0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f};
};

using CommFIFO = choc::fifo::SingleReaderSingleWriterFIFO<UiMessage>;
WDL_FastString g_root_path;

struct JSFXClap : public clap::helpers::Plugin<clap::helpers::MisbehaviourHandler::Terminate,
                                               clap::helpers::CheckingLevel::Maximal>
{
    SX_Instance *m_sxinst = nullptr;
    std::string m_sxname;
    WDL_Mutex m_mutex;
    HWND m_hwndcfg, m_sxhwnd = 0;
    double sampleRate = 0.0f;
    std::vector<double> m_default_par_values;
    std::vector<double> sxProcessingBuffer;
    static constexpr size_t subChunkSize = 32;
    choc::messageloop::Timer m_test_timer;
    JSFXClap(const clap_host *host, const clap_plugin_descriptor *desc)
        : clap::helpers::Plugin<clap::helpers::MisbehaviourHandler::Terminate,
                                clap::helpers::CheckingLevel::Maximal>(desc, host)

    {
        memset(&m_auto_states, 0, sizeof(m_auto_states));
        g_root_path.Set(R"(E:\PortableApps\reaper6)");
        m_default_par_values.reserve(1024);
        sxProcessingBuffer.resize(subChunkSize * 4);
        return;
        m_test_timer = choc::messageloop::Timer{1000, [this]() {
                                                    loadNextJSFX();
                                                    return true;
                                                }};
    }
    std::vector<std::string> m_test_fx{"delay/delay", "utility/volume_pan",
                                       "sstillwell/eventhorizon2"};
    int m_test_fx_counter = 0;
    void loadNextJSFX()
    {
        std::cout << "opening " << m_test_fx[m_test_fx_counter] << std::endl;
        Open(m_test_fx[m_test_fx_counter].c_str());
        ++m_test_fx_counter;
        if (m_test_fx_counter == m_test_fx.size())
            m_test_fx_counter = 0;
    }
    bool activate(double sampleRate_, uint32_t minFrameCount,
                  uint32_t maxFrameCount) noexcept override
    {
        sampleRate = sampleRate_;
        Open(R"(delay/delay)");
        return true;
    }
    void deactivate() noexcept override { Close(); }
    double midi_sendrecvFunc(int action, double *ts, double *msg1, double *msg23)
    {
        return 0.0;
        // TODO: implement for Clap
#ifdef JSCLAPMIDI
        if (action < 0)
        {
            if (m_pelist)
                while (m_pelist_rdpos < m_pelist->numEvents)
                {
                    VstEvent *evt = m_pelist->events[m_pelist_rdpos++];
                    if (evt)
                    {
                        if (evt->type == kVstMidiType && evt->byteSize >= 24)
                        {
                            VstMidiEvent *e = (VstMidiEvent *)evt;

                            *ts = (double)e->deltaFrames;
                            unsigned char *md = (unsigned char *)e->midiData;
                            *msg1 = (double)md[0];
                            *msg23 = (double)((int)md[1] + ((int)md[2] << 8));
                            return 1.0;
                        }
                        PassThruEvent(evt);
                    }
                }
            if (!m_pelist_rdpos)
                m_pelist_rdpos++;
        }
        else if (action == 1) // 3-byte send
        {
            int fo = (int)*ts;
            if (fo < 0)
                fo = 0;

            VstMidiEvent *evt = (VstMidiEvent *)malloc(sizeof(VstMidiEvent));
            memset(evt, 0, sizeof(VstMidiEvent));
            evt->type = kVstMidiType;
            evt->byteSize = 24;
            evt->deltaFrames = fo;

            int m = (int)*msg1;
            if (m < 0x80)
                m = 0x80;
            else if (m > 0xff)
                m = 0xff;
            evt->midiData[0] = (unsigned char)m;
            m = (int)(*msg23) & 0xff;
            evt->midiData[1] = (unsigned char)m;
            m = (((int)(*msg23)) >> 8) & 0xff;
            evt->midiData[2] = (unsigned char)m;

            m_midioutqueue.Add((VstEvent *)evt);
            return 1.0;
        }
        else if (action == 2) // sysex
        {
            int fo = (int)*ts;
            if (fo < 0)
                fo = 0;

            int len = (int)*msg23;
            if (len < 0)
                len = 0;

            int offs = 0;
            if (len >= 2 && (((int)msg1[0]) & 0xFF) == 0xF0 &&
                (((int)msg1[len - 1]) & 0xFF) == 0xF7) // we'll add this
            {
                ++offs;
                len -= 2;
            }

            VstMidiSysexEvent *evt =
                (VstMidiSysexEvent *)malloc(sizeof(VstMidiSysexEvent) + len + 2);
            memset(evt, 0, sizeof(VstMidiSysexEvent) + len + 2);
            unsigned char *syx = (unsigned char *)evt + sizeof(VstMidiSysexEvent);

            evt->type = kVstSysExType;
            evt->deltaFrames = fo;
            evt->byteSize = sizeof(VstMidiSysexEvent);
            evt->sysexDump = (char *)syx;
            evt->dumpBytes = len + 2;

            syx[0] = 0xF0;
            int i;
            for (i = 0; i < len; ++i)
                syx[i + 1] = (((int)msg1[i + offs]) & 0xFF);
            syx[len + 1] = 0xF7;

            m_midioutqueue.Add((VstEvent *)evt);
            return 1.0;
        }

        return 0.0;
#endif
    }
    static double midi_sendrecv(void *ctx, int action, double *ts, double *msg1, double *msg23,
                                double *midibus)
    {
        JSFXClap *_this = (JSFXClap *)ctx;
        return _this->midi_sendrecvFunc(action, ts, msg1, msg23);
    }
    void Open(const char *name)
    {
        if (name && std::string(name) != m_sxname)
            m_sxname = name;

        SX_Instance *newinst = sx_createInstance(g_root_path.Get(), m_sxname.c_str(), NULL);
        // assert(newinst);
        {
            WDL_MutexLock m(&m_mutex);
            Close();
            m_sxinst = newinst;

            if (m_sxinst)
            {
                int npars = sx_getNumParms(m_sxinst);
                m_default_par_values.resize(npars);
                for (int i = 0; i < npars; ++i)
                {
                    m_default_par_values[i] = sx_getParmVal(m_sxinst, i, nullptr, nullptr, nullptr);
                }
                sx_updateHostNch(m_sxinst, -1);
                sx_set_midi_ctx(m_sxinst, midi_sendrecv, this);
                sx_set_host_ctx(m_sxinst, this, NULL);
            }
        }
        if (m_sxinst)
        {
            // this doesn't refresh the generic plugin parameter editors in Bitwig and Reaper
            // also, in Reaper may be causing crashes/hangs
            _host.paramsRescan(CLAP_PARAM_RESCAN_ALL);
        }

        if (m_hwndcfg)
        {
            SendMessage(m_hwndcfg, WM_USER + 1000, 0, 0);
        }
    }

    void Close()
    {
        if (m_sxinst)
        {
            sx_deleteUI(m_sxinst);
            sx_destroyInstance(m_sxinst);
            m_sxinst = nullptr;
            m_sxhwnd = 0;
        }
    }
    bool implementsParams() const noexcept override { return true; }
    bool isValidParamId(clap_id paramId) const noexcept override
    {
        if (!m_sxinst)
            return false;
        if (paramId >= 0 && paramId < sx_getNumParms(m_sxinst))
            return true;
        return false;
    }
    uint32_t paramsCount() const noexcept override
    {
        if (m_sxinst)
            return sx_getNumParms(m_sxinst);
        return 0;
    }
    bool paramsInfo(uint32_t paramIndex, clap_param_info *info) const noexcept override
    {
        if (!m_sxinst)
            return false;
        if (paramIndex >= sx_getNumParms(m_sxinst))
            return false;
        info->cookie = nullptr;
        info->flags = CLAP_PARAM_IS_AUTOMATABLE;
        info->default_value = m_default_par_values[paramIndex];
        double minval = 0.0;
        double maxval = 1.0;
        sx_getParmVal(m_sxinst, paramIndex, &minval, &maxval, nullptr);
        info->min_value = minval;
        info->max_value = maxval;
        info->id = paramIndex;
        strcpy_s(info->module, "");
        sx_getParmName(m_sxinst, paramIndex, info->name, CLAP_NAME_SIZE);
        return true;
    }
    bool paramsValue(clap_id paramId, double *value) noexcept override
    {
        if (!value)
            return false;
        if (!m_sxinst)
            return false;
        if (paramId >= sx_getNumParms(m_sxinst))
            return false;
        *value = sx_getParmVal(m_sxinst, paramId, nullptr, nullptr, nullptr);
        return true;
    }
    bool paramsValueToText(clap_id paramId, double value, char *display,
                           uint32_t size) noexcept override
    {
        if (!m_sxinst)
            return false;
        sx_getParmDisplay(m_sxinst, paramId, display, size, &value);
        return true;
    }
    bool implementsNotePorts() const noexcept override { return false; }
    uint32_t notePortsCount(bool isInput) const noexcept override { return 0; }
    bool notePortsInfo(uint32_t index, bool isInput,
                       clap_note_port_info *info) const noexcept override
    {
        if (isInput)
        {
            info->id = 5012;
            strcpy_s(info->name, "Note input");
            info->preferred_dialect = CLAP_NOTE_DIALECT_CLAP;
            info->supported_dialects = CLAP_NOTE_DIALECT_CLAP | CLAP_NOTE_DIALECT_MIDI;
        }
        return false;
    }
    bool implementsAudioPorts() const noexcept override { return true; }
    uint32_t audioPortsCount(bool isInput) const noexcept override
    {
        if (isInput)
            return 1;
        return 1;
    }
    bool audioPortsInfo(uint32_t index, bool isInput,
                        clap_audio_port_info *info) const noexcept override
    {
        if (index == 0)
        {
            info->id = isInput ? 5000 : 17000;
            if (isInput)
                strcpy_s(info->name, "main input");
            else
                strcpy_s(info->name, "main output");
            info->flags = CLAP_AUDIO_PORT_IS_MAIN;
            info->in_place_pair = CLAP_INVALID_ID;
            info->channel_count = 2;
            info->port_type = CLAP_PORT_STEREO;
            return true;
        }
        return false;
    }
    void handleNextEvent(const clap_event_header_t *nextEvent, bool is_from_ui)
    {
        if (nextEvent->space_id != CLAP_CORE_EVENT_SPACE_ID)
            return;

        switch (nextEvent->type)
        {
        case CLAP_EVENT_NOTE_OFF:
        case CLAP_EVENT_NOTE_CHOKE:
        {
            auto nevt = reinterpret_cast<const clap_event_note *>(nextEvent);
            break;
        }
        case CLAP_EVENT_NOTE_ON:
        {
            auto nevt = reinterpret_cast<const clap_event_note *>(nextEvent);
            break;
        }
        case CLAP_EVENT_NOTE_EXPRESSION:
        {
            auto nexp = reinterpret_cast<const clap_event_note_expression *>(nextEvent);
            break;
        }
        case CLAP_EVENT_PARAM_VALUE:
        {
            auto pevt = reinterpret_cast<const clap_event_param_value *>(nextEvent);
            if (m_sxinst && pevt->param_id >= 0 && pevt->param_id < sx_getNumParms(m_sxinst))
            {
                // not completely clear what the sample offset refers to, but maybe a
                // good guess would be offset from the current processing buffer start...?
                // in which case we could use pevt->header.time as expected
                sx_setParmVal(m_sxinst, pevt->param_id, pevt->value, 0);
            }
            break;
        }
        case CLAP_EVENT_PARAM_MOD:
        {
            auto pevt = reinterpret_cast<const clap_event_param_mod *>(nextEvent);

            break;
        }
        default:
            break;
        }
    }
    void paramsFlush(const clap_input_events *in, const clap_output_events *out) noexcept override
    {
        return;
    }

    clap_process_status process(const clap_process *process) noexcept override
    {
        WDL_MutexLock m(&m_mutex);
        auto frameCount = process->frames_count;
        float *ip[2];
        ip[0] = &process->audio_inputs->data32[0][0];
        ip[1] = &process->audio_inputs->data32[1][0];
        float *op[2];
        op[0] = &process->audio_outputs->data32[0][0];
        op[1] = &process->audio_outputs->data32[1][0];

        auto inEvents = process->in_events;
        auto inEventsSize = inEvents->size(inEvents);

        // just do a simple subchunking here without a ringbuffer etc
        const clap_event_header_t *nextEvent{nullptr};
        uint32_t nextEventIndex{0};
        if (inEventsSize != 0)
        {
            nextEvent = inEvents->get(inEvents, nextEventIndex);
        }
        uint32_t chunkSize = subChunkSize;
        uint32_t pos = 0;

        while (pos < frameCount)
        {
            uint32_t adjChunkSize = std::min(chunkSize, frameCount - pos);
            while (nextEvent && nextEvent->time < pos + adjChunkSize)
            {
                auto iev = inEvents->get(inEvents, nextEventIndex);
                handleNextEvent(iev, false);
                nextEventIndex++;
                if (nextEventIndex >= inEventsSize)
                    nextEvent = nullptr;
                else
                    nextEvent = inEvents->get(inEvents, nextEventIndex);
            }
            for (int i = 0; i < adjChunkSize; ++i)
            {
                sxProcessingBuffer[i * 2 + 0] = ip[0][i + pos];
                sxProcessingBuffer[i * 2 + 1] = ip[1][i + pos];
            }
            sx_processSamples(m_sxinst, sxProcessingBuffer.data(), adjChunkSize, 2, sampleRate,
                              120.0, 4, 4, 1.0, 0.0, 0.0, 1.0, 1.0, 0);
            for (int i = 0; i < adjChunkSize; ++i)
            {
                op[0][i + pos] = sxProcessingBuffer[i * 2 + 0];
                op[1][i + pos] = sxProcessingBuffer[i * 2 + 1];
            }
            pos += adjChunkSize;
        }
        return CLAP_PROCESS_CONTINUE;
    }
    bool implementsState() const noexcept override { return false; }
    bool stateSave(const clap_ostream *stream) noexcept override
    {
        uint32_t version = 0;
        stream->write(stream, &version, sizeof(uint32_t));
        return true;
    }
    bool stateLoad(const clap_istream *stream) noexcept override
    {
        if (!stream)
            return false;
        char buf[1024];
        auto r = stream->read(stream, (void *)buf, 1024);
        if (r == 0)
            return false;
        return true;
    }
    void CreateUI(HWND hwndDlg)
    {
        if (m_sxinst)
        {
            m_sxhwnd = sx_createUI(m_sxinst, g_hInst, hwndDlg, this);
            if (m_sxhwnd)
            {
                RECT r;
                GetWindowRect(GetDlgItem(hwndDlg, IDC_RECT), &r);
                ScreenToClient(hwndDlg, (LPPOINT)&r);
                ScreenToClient(hwndDlg, ((LPPOINT)&r) + 1);
                bool forcesize = false;
                if (!g_allow_edit)
                {
                    WDL_WndSizer__rec *r1 = m_sxinst->resizer.get_item(IDC_EDITFX);
                    WDL_WndSizer__rec *r2 = m_sxinst->resizer.get_item(IDC_BUTTON1);
                    WDL_WndSizer__rec *r3 = m_sxinst->resizer.get_item(IDC_EFFECTNAME);
                    if (r1 && r2 && r3)
                    {
                        const int adj = r1->real_orig.right - r2->real_orig.right;
                        r2->orig.left = (r2->real_orig.left += adj);
                        r2->orig.right = (r2->real_orig.right += adj);
                        r3->orig.right = (r3->real_orig.right += adj);
                    }
                    ShowWindow(GetDlgItem(m_sxhwnd, IDC_EDITFX), SW_HIDE);
                }
                if (!m_sxinst->m_config_items.GetSize())
                {
                    WDL_WndSizer__rec *r2 = m_sxinst->resizer.get_item(IDC_BUTTON1);
                    WDL_WndSizer__rec *r3 = m_sxinst->resizer.get_item(IDC_EFFECTNAME);
                    if (r2 && r3)
                    {
                        const int adj = r2->real_orig.right - r3->real_orig.right;
                        r3->orig.right = (r3->real_orig.right += adj);
                    }
                    ShowWindow(GetDlgItem(m_sxhwnd, IDC_BUTTON1), SW_HIDE);
                }

                SetWindowPos(m_sxhwnd, NULL, r.left, r.top, r.right - r.left, r.bottom - r.top,
                             SWP_NOZORDER | SWP_NOACTIVATE);
                if (forcesize)
                    SendMessage(m_sxhwnd, WM_SIZE, 0, 0);

                ShowWindow(m_sxhwnd, SW_SHOWNA);
            }
        }
    }
    bool implementsGui() const noexcept override { return true; }
    bool guiIsApiSupported(const char *api, bool isFloating) noexcept override
    {
        if (isFloating)
            return false;
        if (!strcmp(api, "win32"))
            return true;
        return false;
    }
    // virtual bool guiGetPreferredApi(const char **api, bool *is_floating) noexcept { return
    // false;
    // }
    bool guiCreate(const char *api, bool isFloating) noexcept override
    {
        if (isFloating)
            return false;
        if (!strcmp(api, "win32"))
        {
            return true;
        }

        return false;
    }
    void guiDestroy() noexcept override { DestroyWindow(m_hwndcfg); }
    // virtual bool guiSetScale(double scale) noexcept { return false; }
    bool guiShow() noexcept override { return true; }
    bool guiHide() noexcept override { return true; }
    bool guiGetSize(uint32_t *width, uint32_t *height) noexcept override
    {
        *width = m_last_w;
        *height = m_last_h;
        return true;
    }
    // virtual bool guiCanResize() const noexcept { return false; }
    // virtual bool guiGetResizeHints(clap_gui_resize_hints_t *hints) noexcept { return false; }
    bool guiAdjustSize(uint32_t *width, uint32_t *height) noexcept override
    {
        return guiGetSize(width, height);
    }
    // virtual bool guiSetSize(uint32_t width, uint32_t height) noexcept { return false; }
    // virtual void guiSuggestTitle(const char *title) noexcept {}
    int m_last_w = 700;
    int m_last_h = 500;
    WDL_WndSizer m_resizer;
    struct AudioMasterCallback
    {
    };
    AudioMasterCallback *m_cb = nullptr;
    bool m_auto_states[NUM_SLIDERS];
    short cfgRect[4] = {0, 0, 0, 0};
    const char *escapeAmpersandsForMenu(
        const char *in, char *buf,
        int bufsz) // converts & to && on all platforms, returns either in or buf
    {
        const char *oin = in;
        if (!in || !buf || bufsz < 1)
            return oin;

        int nc = 0;
        int opos = 0;
        while (*in && opos < bufsz - 1)
        {
            if (*in == '&')
            {
                if (opos >= bufsz - 2)
                    break;
                buf[opos++] = '&';
                nc++;
            }
            buf[opos++] = *in++;
        }
        if (!nc)
            return oin;

        buf[opos] = 0;
        return buf;
    }
    void InsertMenuItemFilter(HMENU hMenu, int pos, BOOL byPos, MENUITEMINFO *mi)
    {
        if (mi && (mi->fMask & MIIM_TYPE) && mi->fType == MFT_STRING && mi->dwTypeData &&
            strstr(mi->dwTypeData, "&"))
        {
            MENUITEMINFO m = *mi;
            char tmp[2048];
            m.dwTypeData = (char *)escapeAmpersandsForMenu(mi->dwTypeData, tmp, sizeof(tmp));
            InsertMenuItem(hMenu, pos, byPos, &m);
        }
        else
        {
            InsertMenuItem(hMenu, pos, byPos, mi);
        }
    }
    HMENU CreateMenuForPath(const char *path, WDL_PtrList<char> *menufns)
    {
        HMENU hMenu = NULL;

        int subpos = 0;
        int itempos = 0;

        WDL_DirScan ds;
        if (!ds.First(path))
        {
            do
            {
                const char *fn = ds.GetCurrentFN();
                if (fn[0] != '.')
                {
                    if (ds.GetCurrentIsDirectory())
                    {
                        WDL_FastString tmp(path);
                        tmp.Append("/");
                        tmp.Append(fn);
                        HMENU sub = CreateMenuForPath(tmp.Get(), menufns);
                        if (sub)
                        {
                            if (!hMenu)
                                hMenu = CreatePopupMenu();
                            MENUITEMINFO mii = {sizeof(mii), MIIM_SUBMENU | MIIM_TYPE | MIIM_STATE,
                                                MFT_STRING,  MFS_ENABLED,
                                                0,           sub,
                                                0,           0,
                                                0,           (char *)fn};
                            InsertMenuItemFilter(hMenu, subpos++, TRUE, &mii);
                        }
                    }
                    else
                    {
                        const char *ext = fn;
                        while (*ext)
                            ext++;
                        while (ext > fn && *ext != '.')
                            ext--;
                        if (ext == fn ||
                            (stricmp(ext, ".png") && stricmp(ext, ".dll") &&
                             stricmp(ext, ".jsfx-inc") && stricmp(ext, ".jpg") &&
                             stricmp(ext, ".zip") && stricmp(ext, ".exe") && stricmp(ext, ".dat") &&
                             stricmp(ext, ".bmp") && stricmp(ext, ".rpl") && stricmp(ext, ".db") &&
                             stricmp(ext, ".wav") && stricmp(ext, ".ogg")))
                        {
                            if (!hMenu)
                                hMenu = CreatePopupMenu();

                            WDL_FastString tmp(path);
                            tmp.Append("/");
                            tmp.Append(fn);
                            menufns->Add(strdup(tmp.Get()));

                            MENUITEMINFO mii = {sizeof(mii),
                                                MIIM_TYPE | MIIM_ID | MIIM_STATE,
                                                MFT_STRING,
                                                MFS_ENABLED,
                                                (UINT)menufns->GetSize(),
                                                NULL,
                                                0,
                                                0,
                                                0,
                                                (char *)fn};
                            InsertMenuItemFilter(hMenu, subpos + itempos++, TRUE, &mii);
                        }
                    }
                }
            } while (!ds.Next());
        }
        return hMenu;
    }
    bool DoEffectMenu(HWND hwndParent, int xpos, int ypos)
    {
        WDL_FastString curpath(g_root_path.Get());
        curpath.Append("/effects");
        WDL_PtrList<char> fns;
        HMENU hMenu = CreateMenuForPath(curpath.Get(), &fns);

        if (!hMenu)
        {
            hMenu = CreatePopupMenu();
            MENUITEMINFO mii = {
                sizeof(mii), MIIM_TYPE | MIIM_STATE,        MFT_STRING, MFS_GRAYED, 0, NULL, 0, 0,
                0,           (char *)"No JS effects found!"};
            InsertMenuItem(hMenu, 0, TRUE, &mii);
        }
        int ret =
            TrackPopupMenu(hMenu, TPM_NONOTIFY | TPM_RETURNCMD, xpos, ypos, 0, hwndParent, NULL);

        if (fns.Get(ret - 1))
        {
            char *p = fns.Get(ret - 1);
            if (strlen(p) > strlen(curpath.Get()) &&
                !strnicmp(p, curpath.Get(), strlen(curpath.Get())))
                p += strlen(curpath.Get()) + 1;
            Open(p);
        }
        else
            ret = 0;
        fns.Empty(true);
        DestroyMenu(hMenu);
        return !!ret;
    }
    WDL_DLGRET CfgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
    {
        static int m_ignore_editmsg;
        static HWND resizeCap;
        static int resizeCap_xo, resizeCap_yo;
        switch (uMsg)
        {
        case WM_INITDIALOG:
            m_hwndcfg = hwndDlg;
            m_resizer.init(hwndDlg);
            m_resizer.init_item(IDC_FRAME, 0, 0, 1, 1);
            m_resizer.init_item(IDC_RECT, 0, 0, 1, 1);
            if (g_default_effect[0] != '!')
            {
                m_resizer.init_item(IDC_CUREFFECT, 0, 0, 1, 0);
                m_resizer.init_item(IDC_BUTTON1, 1, 0, 1, 0);
            }

            SetDlgItemText(hwndDlg, IDC_FRAME, "ReaJS by Cockos Incorporated - www.reaper.fm");

            if (g_default_effect[0] != '!')
                SetDlgItemText(hwndDlg, IDC_CUREFFECT, m_sxname.c_str());

            if (m_last_w > 0 && m_last_h > 0)
            {
                SetWindowPos(hwndDlg, NULL, 0, 0, m_last_w, m_last_h,
                             SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOMOVE);
            }

            CreateUI(hwndDlg);

            ShowWindow(hwndDlg, SW_SHOWNA);
            return 0;
        case WM_USER + 1040:
        {
            bool isEnd = false;
            if (wParam & 0x80000000)
            {
                wParam &= ~0x80000000;
                isEnd = true;
            }

            // if (m_cb && wParam < (WPARAM)m_effect.numParams && m_sxinst)
            if (m_cb && wParam < (WPARAM)256 && m_sxinst)
            {

                double minv = 0.0, maxv = 1.0,
                       v = sx_getParmVal(m_sxinst, wParam, &minv, &maxv, NULL);
                if (v <= minv)
                    v = 0.0;
                else if (v >= maxv)
                    v = 1.0;
                else
                    v = ((v - minv) / (maxv - minv));

                if (!m_auto_states[wParam])
                {
                    m_auto_states[wParam] = true;
                    // m_cb(&m_effect, audioMasterBeginEdit, wParam, 0, NULL, 0.0f);
                }

                // m_cb(&m_effect, audioMasterAutomate, wParam, 0, NULL, (float)v);
                if (isEnd)
                {
                    m_auto_states[wParam] = false;
                    // m_cb(&m_effect, audioMasterEndEdit, wParam, 0, NULL, 0.0f);
                }
            }
        }

            return 0;
        case WM_SETCURSOR:
        {
            RECT r;
            POINT p;
            GetCursorPos(&p);
            GetClientRect(hwndDlg, &r);
            ScreenToClient(hwndDlg, &p);
            int dx = r.right - p.x;
            int dy = r.bottom - p.y;
            if (dx >= 0 && dy >= 0 && dx + dy < 16)
            {
                SetCursor(LoadCursor(NULL, IDC_SIZENWSE));
            }
            else
                SetCursor(LoadCursor(NULL, IDC_ARROW));
        }
            return 1;
        case WM_LBUTTONDOWN:
        {
            RECT r;
            GetClientRect(hwndDlg, &r);
            resizeCap_xo = GET_X_LPARAM(lParam);
            resizeCap_yo = GET_Y_LPARAM(lParam);
            int dx = r.right - resizeCap_xo;
            int dy = r.bottom - resizeCap_yo;
            if (dx >= 0 && dy >= 0 && dx + dy < 16)
            {
                SetCapture(resizeCap = hwndDlg);
            }
        }
            return 0;
        case WM_MOUSEMOVE:
            if (GetCapture() == hwndDlg && resizeCap == hwndDlg)
            {
                RECT r;
                GetClientRect(hwndDlg, &r);
                int newx = GET_X_LPARAM(lParam) - resizeCap_xo;
                int newy = GET_Y_LPARAM(lParam) - resizeCap_yo;
                if (newx || newy)
                {
                    resizeCap_xo += newx;
                    resizeCap_yo += newy;
                    newx += r.right;
                    newy += r.bottom;
                    if (newx < 100)
                        newx = 100;
                    if (newy < 100)
                        newy = 100;
                    m_last_w = newx;
                    m_last_h = newy;

                    if (newx != r.right || newy != r.bottom)
                        SetWindowPos(hwndDlg, NULL, 0, 0, newx, newy,
                                     SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);

                    cfgRect[0] = r.top;
                    cfgRect[1] = r.left;
                    cfgRect[2] = r.top + newy;
                    cfgRect[3] = r.left + newx;

                    int dx = newx - r.right, dy = newy - r.bottom;

                    HWND parent = GetParent(hwndDlg);
                    if (parent)
                    {
                        HWND gparent = (GetWindowLong(parent, GWL_STYLE) & WS_CHILD)
                                           ? GetParent(parent)
                                           : NULL;
                        RECT r, r2;
                        GetWindowRect(parent, &r);
                        if (gparent)
                            GetWindowRect(gparent, &r2);

                        SetWindowPos(parent, NULL, 0, 0, r.right - r.left + dx,
                                     abs(r.bottom - r.top) + dy, SWP_NOMOVE | SWP_NOZORDER);
                        if (gparent)
                            SetWindowPos(gparent, NULL, 0, 0, r2.right - r2.left + dx,
                                         abs(r2.bottom - r2.top) + dy, SWP_NOMOVE | SWP_NOZORDER);
                    }
                }
            }
            return 0;
        case WM_LBUTTONUP:
            ReleaseCapture();
            resizeCap = 0;
            return 0;
        case WM_SIZE:
            if (wParam != SIZE_MINIMIZED)
            {
                m_resizer.onResize();
                if (m_sxhwnd)
                {
                    RECT r;
                    GetWindowRect(GetDlgItem(hwndDlg, IDC_RECT), &r);
                    ScreenToClient(hwndDlg, (LPPOINT)&r);
                    ScreenToClient(hwndDlg, ((LPPOINT)&r) + 1);
                    SetWindowPos(m_sxhwnd, NULL, r.left, r.top, r.right - r.left, r.bottom - r.top,
                                 SWP_NOZORDER | SWP_NOACTIVATE);
                }
            }
            return 0;
        case WM_COMMAND:
            switch (LOWORD(wParam))
            {
            case IDC_BUTTON1:
                if (g_default_effect[0] != '!')
                {
                    RECT r;
                    GetWindowRect(GetDlgItem(hwndDlg, LOWORD(wParam)), &r);
                    if (DoEffectMenu(hwndDlg, r.left, r.bottom))
                    {
                        SetDlgItemText(hwndDlg, IDC_CUREFFECT, m_sxname.c_str());
                        // todo: notify other stuff?
                    }
                }
                break;
            }
            return 0;
        case WM_USER + 1000:
            CreateUI(hwndDlg);
            if (g_default_effect[0] != '!')
                SetDlgItemText(hwndDlg, IDC_CUREFFECT, m_sxname.c_str());
            return 0;
        case WM_DESTROY:
            m_resizer.init(NULL);

            if (m_sxinst)
            {
                sx_deleteUI(m_sxinst);
                m_sxhwnd = 0;
            }
            m_hwndcfg = 0;
            return 0;
        }
        return 0;
    }
    static WDL_DLGRET dlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
    {
        if (uMsg == WM_INITDIALOG)
            SetWindowLongPtr(hwndDlg, GWLP_USERDATA, lParam);
        JSFXClap *_this = (JSFXClap *)GetWindowLongPtr(hwndDlg, GWLP_USERDATA);
        if (_this)
            return _this->CfgProc(hwndDlg, uMsg, wParam, lParam);

#ifdef _WIN32
        if (uMsg >= 0x127 && uMsg <= 0x129)
        {
            SetWindowLongPtr(hwndDlg, DWLP_MSGRESULT, 0);
            return 1;
        }
#endif
        return 0;
    }
    bool guiSetParent(const clap_window *window) noexcept override
    {
        if (m_hwndcfg)
            DestroyWindow(m_hwndcfg);

        m_hwndcfg = CreateDialogParam(g_hInst,
                                      g_default_effect[0] == '!' ? MAKEINTRESOURCE(IDD_VST_CFG2)
                                                                 : MAKEINTRESOURCE(IDD_VST_CFG),
                                      (HWND)window->win32, dlgProc, (LPARAM)this);

        assert(m_hwndcfg != 0);
        SetParent(m_hwndcfg, (HWND)window->win32);
        return true;
    }
    // virtual bool guiSetTransient(const clap_window *window) noexcept { return false; }
};

const char *features[] = {CLAP_PLUGIN_FEATURE_AUDIO_EFFECT, nullptr};
clap_plugin_descriptor desc = {
    CLAP_VERSION, "com.xenakios.jsfx",   "mycompany jsfx", "mycompany", "", "", "",
    "0.0.0",      "mycompany donothing", features};

static const clap_plugin *clap_create_plugin(const clap_plugin_factory *f, const clap_host *host,
                                             const char *plugin_id)
{
    if (std::string(plugin_id) != std::string(desc.id))
        return nullptr;
    // I know it looks like a leak right? but the clap-plugin-helpers basically
    // take ownership and destroy the wrapper when the host destroys the
    // underlying plugin (look at Plugin<h, l>::clapDestroy if you don't believe me!)
    auto wr = new JSFXClap(host, &desc);
    return wr->clapPlugin();
}

uint32_t get_plugin_count(const struct clap_plugin_factory *factory) { return 1; }

const clap_plugin_descriptor *get_plugin_descriptor(const clap_plugin_factory *f, uint32_t w)
{
    return &desc;
}

const CLAP_EXPORT struct clap_plugin_factory the_factory = {
    get_plugin_count,
    get_plugin_descriptor,
    clap_create_plugin,
};

static const void *get_factory(const char *factory_id)
{
    if (std::string(factory_id) == CLAP_PLUGIN_FACTORY_ID)
        return &the_factory;
    return nullptr;
}

// clap_init and clap_deinit are required to be fast, but we have nothing we need to do here
bool clap_init(const char *p)
{
    get_eel_funcdesc = default_get_eel_funcdesc;
    return true;
}

void clap_deinit() {}

extern "C"
{
#ifdef _WIN32

    BOOL WINAPI DllMain(HINSTANCE hDllInst, DWORD fdwReason, LPVOID res)
    {
        if (fdwReason == DLL_PROCESS_ATTACH)
        {
            g_hInst = hDllInst;

            Sliders_Init(hDllInst, true, IDB_HSLIDER);
            Meters_Init(hDllInst, true);
        }
        if (fdwReason == DLL_PROCESS_DETACH)
        {
            Sliders_Init(hDllInst, false);
            Meters_Init(hDllInst, false);
        }
        return TRUE;
    }

#endif
    // clang-format off
const CLAP_EXPORT struct clap_plugin_entry clap_entry = {
        CLAP_VERSION,
        clap_init,
        clap_deinit,
        get_factory
};
    // clang-format on
}
