
SRC = libeww_history_ext.dylib
DST = eww-history-ext-dyn.so

dev:
	cargo build
	ln -sf target/debug/$(SRC) $(DST)

release:
	cargo build --release
	mv target/release/$(SRC) $(DST)

rust-lint:
	cargo fmt -- --check && cargo clippy -- -Dwarnings

rust-test:
	cargo test

lisp-test:
	pwd && ls -alh
	emacs -Q -batch -l ert -l eww-history-ext-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.so
