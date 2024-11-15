End-to-end testing for odoo-lsp.

## Running tests

Make sure [uv] and [just] are installed, then run `just test`.
These same tests are also run in CI, so it's recommended to add them after a bug fix to prevent regressions.

[uv]: https://docs.astral.sh/uv/
[just]: https://github.com/casey/just

## Creating tests

See [fixtures/basic/foo/models.py](fixtures/basic/foo/models.py) for an example how to write tests.
You can use the below command to quickly scaffold a test case. (TODO)

```shell
python scaffold.py fixtures/testcase [foo bar ...]
```
