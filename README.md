# portmidi-mlton
FFI interface to portmidi for MLton : <cr>
portmidi.sml + portmidi_mlton_FFI_lib.sml + cstring.sml

Simple tests : main.sml + test.mlb + Makefile

application example can be see at :<br>
https://github.com/jh-midi/AtomSQ-accordion

the better is to compile portmidi yourself because the Fedora, Debian, Ubuntu distribution don't <br>
provide the best version with virtualdevice creation.<br>
work well with the v2.0.3 release : <br>

https://github.com/PortMidi/portmidi/releases/tag/v2.0.3<br>

the most recent git version cause segmentation fault when starting Ardour.
<br>
the Makefile here is for Fedora 42<br>
you have to change the library path for another distribution<br>
$ make test </br>
$ make test-static<br>









