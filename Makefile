
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

fix:
	cargo fix --allow-dirty --allow-staged

rust-test:
	cargo test

lisp-test:
	pwd && ls -alh
	emacs -Q -batch -l eww-history-ext-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.so


.PHONY: dev release rust-lint fix rust-test lisp-test clean
