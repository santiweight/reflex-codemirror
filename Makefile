codemirror-example:
	cd example && nix-build -A ghcjs.example -o result-reflex-codemirror
	google-chrome ./example/result-reflex-codemirror/bin/reflex-codemirror-exe.jsexe/index.html
