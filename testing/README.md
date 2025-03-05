End-to-end testing for odoo-lsp.

## Running tests

Make sure [uv] and [just] are installed, then run `just test`.
These same tests are also run in CI, so it's recommended to add them after a bug fix to prevent regressions.

[uv]: https://docs.astral.sh/uv/
[just]: https://github.com/casey/just

## Creating tests

See [fixtures/basic/foo/models.py](fixtures/basic/foo/models.py) for an example how to write tests.
You can use the below command to quickly scaffold a test case.

```shell
python scaffold.py fixtures/testcase [foo bar ...]
```

### Fixture commands

When writing fixtures, you can use these magic strings in comments to mark a point of interest.
- `^complete foo bar ...`: expect a space-delimited list of items at this cursor
- `^diag Message`: expect a diagnostic from the server at this cursor
- `^type Model("foo.bar")`: request and expect a Rust debug repr of the type of the item at this cursor
