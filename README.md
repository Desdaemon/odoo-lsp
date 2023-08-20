# odoo-lsp

## Install

```shell
cargo install --git https://github.com/Desdaemon/odoo-lsp
```

## Usage

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
