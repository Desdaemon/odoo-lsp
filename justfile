test: ensure_nextest
    cargo nextest run

[private]
ensure_nextest:
    #!/usr/bin/env sh
    command -v cargo-nextest && exit
    command -v cargo-binstall && cargo binstall cargo-nextest --force -y && exit
    command -v cargo && cargo install cargo-binstall && cargo binstall cargo-nextest && exit
    echo 'cargo is not installed, exiting' && exit 1
