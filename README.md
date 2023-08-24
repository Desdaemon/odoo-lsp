# odoo-lsp

[![nightly](https://github.com/Desdaemon/odoo-lsp/actions/workflows/nightly.yml/badge.svg)](https://github.com/Desdaemon/odoo-lsp/actions/workflows/nightly.yml)

## Features

### Completion/goto-references for XML IDs

Works for `record`s, `template`s and `env.ref()`!

[![template inherit_id demo](https://raw.githubusercontent.com/Desdaemon/odoo-lsp/main/static/inherit_id.2.gif)](https://asciinema.org/a/603783)

For more features check out the [wiki].

## Install

```shell
cargo install --git https://github.com/Desdaemon/odoo-lsp
```

Nightly binaries are also available for x64 platforms, please check [Releases] for the latest downloads.

## Usage

For more information please check the [wiki] (in construction).

### VSCode

> **Note**
> odoo-lsp is under heavy development and requires some additional setup.
> If you'd like to help, please file an issue or create a PR to help expedite the installation process!

1. Ensure that you have `odoo-lsp` on your path (or define the SERVER_PATH environment variable pointing to one)
2. Download the VSCode extension package from [Releases] (a file ending in .vsix)
3. Open the **Command Palette** <kbd>Ctrl+Shift+P</kbd>, select **Extensions: Install from VSIX** and pick the file
   you downloaded in step 2

### Helix

See [.helix/languages.toml](./examples/.helix/languages.toml)

1. Ensure that you have `odoo-lsp` on your path (or define the SERVER_PATH environment variable pointing to one)
2. Determine your Helix runtime folder, e.g. `~/.config/helix/` on Linux
3. Modify `languages.toml` in your Helix runtime folder (create one if none exists) to include these lines:

```toml
[language-server]
# Update the path to match your setup
odoo-lsp.command = "odoo-lsp"

[[language]]
name = "xml"
language-servers = ["odoo-lsp"]

[[language]]
name = "python"
language-servers = [
   "odoo-lsp",
   # add the default language servers here
   # and any you need
]
```

4. Alternatively, modify `$ROOT/.helix/languages.toml` where `$ROOT` is your Odoo modules root to include the above lines.

## Development

1. `pnpm i`
2. `cargo build`
3. In VSCode, press <kbd>F5</kbd> or change to the Debug panel and click `Launch Client`

> **Note**
> If encountered errors like `Cannot find module '/xxx/xxx/dist/extension.js'`
> please try run command `tsc -b` manually, you could refer https://github.com/IWANABETHATGUY/tower-lsp-boilerplate/issues/6 for more details

[wiki]: https://github.com/Desdaemon/odoo-lsp/wiki
[Releases]: https://github.com/Desdaemon/odoo-lsp/releases
