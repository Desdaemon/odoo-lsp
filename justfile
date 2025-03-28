test *args: (ensure_cargo "cargo-nextest")
    cargo nextest run -p odoo-lsp -p odoo-lsp-tests {{args}}

bench: (ensure_cargo "iai-callgrind-runner")
    cargo bench -p odoo-lsp-tests

[private]
ensure_cargo command:
    command -v {{command}} || \
        (command -v cargo-binstall && cargo binstall {{command}} --force -y) || \
        (command -v cargo && cargo install cargo-binstall && cargo binstall {{command}} --force -y) || \
        (echo 'cargo is not installed, exiting' && exit 1)
