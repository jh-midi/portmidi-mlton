test: main.sml  portmidi_mlton.sml test.mlb
	mlton -default-ann 'allowFFI true'  -link-opt '-Wl,-rpath=/usr/local/lib64 -lportmidi -lasound' test.mlb
test-static: main.sml  portmidi_mlton.sml test.mlb
	mlton -output 'test-static' -default-ann 'allowFFI true' -link-opt '-L/usr/local/lib64 -l:libportmidi.a  -l:libgmp.a -lasound' test.mlb
