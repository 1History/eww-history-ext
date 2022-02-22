
SRC = libeww_history_ext.dylib
DST = eww-history-ext-dyn.so

dev:
	cargo build
	ln -sf target/debug/$(SRC) $(DST)

release:
	cargo build --release
	mv target/release/$(SRC) $(DST)
