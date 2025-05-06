atomSQ: init.sml display.sml main.sml atomsq_ok.sml portmidi.sml portmidi_mlton_FFI_lib.sml atomSQ.mlb
	mlton -default-ann 'allowFFI true'  -link-opt '-Wl,-rpath=/usr/local/lib64 -lportmidi -lasound' atomSQ.mlb
# link only with dynamic lib portmidi.so because static link don't work with Ardour
# link with static and this play with surgeXT
# need install of alsa lib and portmidi dynamic library

#atomStatic:  atomsq_ok.sml portmidi.sml portmidi_mlton_FFI_lib.sml atomSQ.mlb
#	mlton  -default-ann 'allowFFI true'  -link-opt '-L./archive -lportmidi -lasound' atomSQ.mlb
