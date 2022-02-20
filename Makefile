
dev:
	cargo build
	cd target/debug && ln -sf libonehistory_emacs.dylib onehistory.so
