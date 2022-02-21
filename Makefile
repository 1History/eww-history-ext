
SRC = libonehistory_emacs.dylib
DST = onehistory-dyn.so

dev:
	cargo build
	ln -sf target/debug/$(SRC) $(DST)

release:
	cargo build --release
	cp target/release/$(SRC) $(DST)
