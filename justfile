test:
    #!/usr/bin/env bash
    cd testing
    command -v uv || (echo 'uv package manager not found; exiting.' && exit 1)
    test -d .venv || uv venv
    . .venv/bin/activate
    uv pip compile requirements.in -o requirements.txt
    uv pip sync requirements.txt
    cargo build
    pytest -vv --color=yes

