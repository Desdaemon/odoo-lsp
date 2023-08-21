# odoo-lsp

[![nightly](https://github.com/Desdaemon/odoo-lsp/actions/workflows/rust.yml/badge.svg)](https://github.com/Desdaemon/odoo-lsp/actions/workflows/rust.yml)

## Install

```shell
cargo install --git https://github.com/Desdaemon/odoo-lsp
```

Nightly binaries are also available for x64 platforms, please check [Releases](https://github.com/Desdaemon/odoo-lsp/releases) for the latest downloads.

## Usage

For more information please check the [wiki](https://github.com/Desdaemon/odoo-lsp/wiki) (in construction).

### VSCode (todo)

### Helix

See [.helix/languages.toml](./examples/.helix/languages.toml)

## Development

1. `pnpm i`
2. `cargo build`
3. In VSCode, press <kbd>F5</kbd> or change to the Debug panel and click `Launch Client`
   > **Note**
   >
   > If encountered errors like `Cannot find module '/xxx/xxx/dist/extension.js'`
   > please try run command `tsc -b` manually, you could refer https://github.com/IWANABETHATGUY/tower-lsp-boilerplate/issues/6 for more details
