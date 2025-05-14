# portmidi-mlton
FFI interface to portmidi for MLton : <cr>
portmidi_mlton.sml (all in one)

Simple tests : main.sml + test.mlb + Makefile

the better is to compile portmidi yourself because the Fedora, Debian, Ubuntu distribution don't <br>
provide the best version with virtualdevice creation, and because the most recent git version <br>
cause segmentation fault when starting Ardour...<br>
I suggest the v2.0.3 release which run well : <br>

https://github.com/PortMidi/portmidi/releases/tag/v2.0.3<br>


<br>
the Makefile here is for Fedora 42<br>
you have to change the library path for another distribution<br>
$ make test </br>
$ ./test<br>
for the static compilation you should have libgmp.a and libportmidi.a in your library path<br>
and the application can be distribued all in one<br>
$ make test-static<br>

another pplication example can be see at :<br>
https://github.com/jh-midi/AtomSQ-accordion







