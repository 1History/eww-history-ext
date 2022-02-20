
SRC = libonehistory_emacs.dylib
DST = onehistory-dyn.so

dev:
	cargo build
	cd target/debug && ln -sf $(SRC) $(DST)

release:
	# cargo build --release
	cp target/release/$(SRC) $(DST)
