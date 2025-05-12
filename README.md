# portmidi-mlton
FFI interface to portmidi for MLton : <cr>
portmidi.sml + portmidi_mlton_FFI_lib.sml + cstring.sml

Simple tests : main.sml + test.mlb + Makefile

application example can be see at :<br>
https://github.com/jh-midi/AtomSQ-accordion

the better is to compile portmidi yourself because the Fedora, Debian, Ubuntu distribution don't <br>
provide the last version with virtualdevice creation.<br>
compile with the dynamic lib :<br>
*https://github.com/PortMidi/portmidi/releases/tag/v2.0.3<br>
the recent version cause segmentation fault with Ardour.
<br>
<br>It work with Fedora 42.<br>






