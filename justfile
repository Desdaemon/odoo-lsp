set quiet

test *args="--no-fail-fast": (ensure_cargo "cargo-nextest")
    cargo nextest run -p odoo-lsp -p odoo-lsp-tests {{args}}

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
	command -v {{command}} || \
		(command -v cargo-binstall && cargo binstall {{command}} --force -y) || \
		(command -v cargo && cargo install cargo-binstall && cargo binstall {{command}} --force -y) || \
		(echo 'cargo is not installed, exiting' && exit 1)
