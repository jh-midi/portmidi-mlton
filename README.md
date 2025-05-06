# portmidi-mlton
FFI interface to portmidi for MLton : <cr>
portmidi.sml + portmidi_mlton_FFI_lib.sml

the rest of files is a program example which set Presonus ATOM SQ in programmable mode <br>
and implement diatonic B/C and G/C accordions on it.<br>
Plus the knobs can be set to relative mode. <br>
It use the A,B ... G for showing the name of played notes.<br>
The pads button 1 and 2 action the push pull<br>

you have to compile portmidi yourself, and adapt the Makefile<br>
to your system. Here it work with Fedora 42.<br>
It work also on Windows with apropriate dll and Makefile and create a virtual port "diato" on loopmidi<br>
make

