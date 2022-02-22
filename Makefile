
SRC = libemacs_onehistory.dylib
DST = onehistory-dyn.so

dev:
	cargo build
	ln -sf target/debug/$(SRC) $(DST)

release:
	cargo build --release
	mv target/release/$(SRC) $(DST)
