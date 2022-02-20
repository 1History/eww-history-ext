
dev:
	cargo build
	cd target/debug && ln -sf libonehistory.dylib onehistory-dyn.so

release:
	cargo build --release
	cd target/release && ln -sf libonehistory.dylib onehistory-dyn.so
