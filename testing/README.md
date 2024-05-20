End-to-end testing for odoo-lsp.

```shell
pip install -U setuptools uv
uv venv
source .venv/bin/activate
uv pip compile requirements.in > requirements.txt
uv pip sync requirements.txt
```
