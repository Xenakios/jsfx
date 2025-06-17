cmake_minimum_required(VERSION 3.25)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

include_directories(WDL jmde)

add_library(jsfx MODULE
    jsfx/jsfx_api.cpp
    jsfx/effectload.cpp
    jsfx/effectproc.cpp
    jsfx/miscfunc.cpp
    jsfx/sfx_edit.cpp
    jsfx/sfxui.cpp
    jsfx/res.rc
    WDL/win32_curses/eel_edit.cpp
    WDL/win32_curses/curses_editor.cpp
    WDL/win32_curses/curses_win32.cpp
    WDL/wingui/wndsize.cpp
    WDL/fft.c
    WDL/eel2/nseel-caltab.c
    WDL/eel2/nseel-cfunc.c
    WDL/eel2/nseel-lextab.c
    WDL/eel2/nseel-yylex.c
    WDL/eel2/nseel-compiler.c
    WDL/eel2/nseel-ram.c
    WDL/eel2/nseel-eval.c
    WDL/lice/lice.cpp
    WDL/lice/lice_arc.cpp
    WDL/lice/lice_line.cpp
    WDL/lice/lice_text.cpp
    WDL/lice/lice_image.cpp
    WDL/lice/lice_textnew.cpp
    WDL/eel2/asm-nseel-x64.obj
    WDL/localize/localize.cpp
    # jsfx/standalone-helpers/meter-control.cpp
    # jsfx/standalone-helpers/slider-control.cpp
)
target_compile_definitions(jsfx PRIVATE EEL_F_SIZE=4)