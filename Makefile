test: main.sml  cstring.sml portmidi.sml portmidi_mlton_FFI_lib.sml test.mlb
	mlton -default-ann 'allowFFI true'  -link-opt '-Wl,-rpath=/usr/local/lib64 -lportmidi -lasound' test.mlb
