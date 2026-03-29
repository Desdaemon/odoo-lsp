set quiet

test *args="--workspace --no-fail-fast": (_test args)

bench: (ensure_cargo "gungraun-runner")
	gungraun-runner -V
	cargo bench -p odoo-lsp-tests

install profile="dev":
	cargo install --path . --profile={{profile}}

coverage clean="0": (ensure_cargo "cargo-llvm-cov")
	test {{clean}} = "0" || cargo llvm-cov clean --workspace -v
	cargo llvm-cov nextest -p odoo-lsp -p odoo-lsp-tests --html --open

[private]
ensure_cargo command:
	command -v {{command}} >/dev/null || \
		(command -v cargo-binstall >/dev/null && cargo binstall {{command}} --force -y) || \
		(command -v cargo >/dev/null && cargo install cargo-binstall && cargo binstall {{command}} --force -y) || \
		(echo 'cargo is not installed, exiting' && exit 1)

[private]
report-md:
	glow -p -w 0 || cat

[private]
_test *args: (ensure_cargo "cargo-nextest")
	NEXTEST_EXPERIMENTAL_LIBTEST_JSON=1 NO_COLOR=1 cargo nextest run --message-format=libtest-json-plus {{args}} \
		| ./scripts/libtest_to_md.mjs | just report-md
