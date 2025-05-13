test: main.sml  cstring.sml portmidi.sml portmidi_mlton_FFI_lib.sml test.mlb
	mlton -default-ann 'allowFFI true'  -link-opt '-Wl,-rpath=/usr/local/lib64 -lportmidi -lasound' test.mlb
test-static: main.sml  cstring.sml portmidi.sml portmidi_mlton_FFI_lib.sml test-static.mlb
	mlton -output 'test-static'  -default-ann 'allowFFI true' -link-opt '-L/usr/local/lib64 -l:libportmidi.a -lasound' test.mlb
