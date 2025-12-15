# odoo-lsp for Zed

An extension for the [Zed] editor.

![demo](./static/demo.png)

## Install

The extension is available from the [Zed extensions registry](https://zed.dev/extensions/odoo-lsp).

To install from source, ensure that `cargo` and `rustup` are [installed](https://www.rust-lang.org/tools/install),
then select `Install Dev Extension` in Zed and point to this directory.

> [!NOTE]
> In the latest version as of writing (Zed 0.216.1) it might be required to add odoo-lsp explicitly as a language server in your `settings.json` as follows:
> ```json
> {
>   // ...
>   "languages": {
>     "Python": {
>       "language_servers": ["odoo-lsp", "..."]
>     },
>     "xml": {
>       "language_servers": ["odoo-lsp", "..."]
>     }
>   }
> }
> ```

[Zed]: https://zed.dev
