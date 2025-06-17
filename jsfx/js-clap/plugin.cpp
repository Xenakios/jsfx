#include "clap/events.h"
#include "clap/ext/params.h"
#include "clap/factory/plugin-factory.h"
#include "clap/id.h"
#include "windows.h"
#include <handleapi.h>
#undef min
#undef max
#include "clap/clap.h"
#include "clap/helpers/plugin.hh"
#include "clap/helpers/plugin.hxx"
#include "clap/helpers/host-proxy.hh"
#include "clap/helpers/host-proxy.hxx"
#include "clap/plugin-features.h"
// #include "sst/basic-blocks/params/ParamMetadata.h"
#include "containers/choc_SingleReaderSingleWriterFIFO.h"
#include "audio/choc_SampleBuffers.h"
#include <cstdint>
#include <unordered_map>
#include "../sfxui.h"
#include "../reajs-vst2/resource.h"

HINSTANCE g_hInst;

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
    JSFXClap(const clap_host *host, const clap_plugin_descriptor *desc)
        : clap::helpers::Plugin<clap::helpers::MisbehaviourHandler::Terminate,
                                clap::helpers::CheckingLevel::Maximal>(desc, host)

    {
        g_root_path.Set(R"(E:\PortableApps\reaper6)");
        m_default_par_values.reserve(1024);
        Open(R"(utility/volume_pan)");
    }
    bool activate(double sampleRate_, uint32_t minFrameCount,
                  uint32_t maxFrameCount) noexcept override
    {
        sampleRate = sampleRate_;

        return true;
    }
    void deactivate() noexcept override {}
    void Open(const char *name)
    {
        if (name && std::string(name) != m_sxname)
            m_sxname = name;

        SX_Instance *newinst = sx_createInstance(g_root_path.Get(), m_sxname.c_str(), NULL);
        assert(newinst);
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
                // sx_set_midi_ctx(m_sxinst, midi_sendrecv, this);
                sx_set_host_ctx(m_sxinst, this, NULL);
            }
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
        sprintf_s(display, size, "%f", value);
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
        auto frameCount = process->frames_count;
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
        uint32_t chunkSize = 32;
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
            choc::buffer::SeparateChannelLayout<float> layout(process->audio_outputs->data32, pos);
            choc::buffer::ChannelArrayView<float> bufview(layout, {2, adjChunkSize});
            bufview.clear();
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
    bool implementsGui() const noexcept override { return false; }
    bool guiIsApiSupported(const char *api, bool isFloating) noexcept override { return false; }
    // virtual bool guiGetPreferredApi(const char **api, bool *is_floating) noexcept { return false;
    // }
    bool guiCreate(const char *api, bool isFloating) noexcept override { return false; }
    void guiDestroy() noexcept override {}
    // virtual bool guiSetScale(double scale) noexcept { return false; }
    bool guiShow() noexcept override { return false; }
    bool guiHide() noexcept override { return false; }
    bool guiGetSize(uint32_t *width, uint32_t *height) noexcept override { return false; }
    // virtual bool guiCanResize() const noexcept { return false; }
    // virtual bool guiGetResizeHints(clap_gui_resize_hints_t *hints) noexcept { return false; }
    bool guiAdjustSize(uint32_t *width, uint32_t *height) noexcept override
    {
        return guiGetSize(width, height);
    }
    // virtual bool guiSetSize(uint32_t width, uint32_t height) noexcept { return false; }
    // virtual void guiSuggestTitle(const char *title) noexcept {}
    bool guiSetParent(const clap_window *window) noexcept override { return false; }
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
