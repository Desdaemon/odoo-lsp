# odoo-lsp

[![nightly](https://github.com/Desdaemon/odoo-lsp/actions/workflows/nightly.yml/badge.svg)](https://github.com/Desdaemon/odoo-lsp/actions/workflows/nightly.yml)

## Features

### Completion, definition and references for models, XML IDs and model fields

Works for `record`s, `template`s, `env.ref()` and other structures.

[![model demo](https://raw.githubusercontent.com/Desdaemon/odoo-lsp/main/static/model.gif)](https://asciinema.org/a/604545)

### Browse models and XML records as workspace symbols

[![symbols demo](https://raw.githubusercontent.com/Desdaemon/odoo-lsp/main/static/symbols.gif)](https://asciinema.org/a/604560)

For more features check out the [wiki].

## Install

The VSCode extension handles downloading the latest releases automatically; other editors need `odoo-lsp` on the path.
Nightly binaries are also available for major platforms, please check [Releases] for the latest downloads.

```bash
# One-line
VERSION="v0.1.2"
VERSION="nightly-$(date +'%Y%m%d')" # today's nightly build
wget https://github.com/Desdaemon/odoo-lsp/releases/download/$VERSION/odoo-lsp-x86_64-unknown-linux-musl.tgz -O - | tar -xzvf -

# With cargo-binstall
cargo binstall odoo-lsp

# Install from source
cargo install odoo-lsp
```

## Setup

For usage instructions please check the [wiki] (work in progress).

### VSCode

odoo-lsp is available from the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=Desdaemon.odoo-lsp) and the
[Open VSX Registry](https://open-vsx.org/extension/Desdaemon/odoo-lsp). Alternatively, you can grab the latest nightly builds from [Releases].

### Helix

See [.helix/languages.toml](./examples/.helix/languages.toml)

1. Ensure that you have `odoo-lsp` on your path
2. Determine your Helix runtime folder, e.g. `~/.config/helix/` on Linux
3. Modify `languages.toml` in your Helix runtime folder (create one if none exists) to include these lines:

```toml
[language-server]
# Update the path to match your setup
odoo-lsp.command = "odoo-lsp"

[[language]]
name = "xml"
language-servers = ["odoo-lsp"]
roots = [".odoo_lsp", ".odoo_lsp.json"]

[[language]]
name = "python"
roots = [
   ".odoo_lsp", ".odoo_lsp.json",
   # add the default roots here
]
# Order is important here
language-servers = [
   "odoo-lsp",
   # add the default language servers here
   "pylsp",
   # and any you need
]
```

4. Alternatively, modify `$ROOT/.helix/languages.toml` where `$ROOT` is your Odoo modules root to include the above lines.

### Neovim via [lsp-zero.nvim]

1. Ensure that you have `odoo-lsp` on your path
2. Configure your Neovim (Lua) configuration file e.g. at `~/.config/nvim/init.lua` to use [lsp-zero.nvim],
   adding odoo-lsp as a new server using `lsp.new_server` before calling `lsp.setup()`:

```lua
-- lsp-zero stanza
local lsp = require('lsp-zero').preset({})
lsp.on_attach(function(client, bufnr)
  lsp.default_keymaps({buffer = bufnr})
end)

-- define our custom language server here
lsp.new_server({
  name = 'odoo-lsp',
  cmd = {'odoo-lsp'},
  filetypes = {'javascript', 'xml', 'python'},
  root_dir = function()
    return lsp.dir.find_first({'.odoo_lsp', '.odoo_lsp.json', '.git'})
  end,
})

-- LSP setup done
lsp.setup()
```

A complete example can be found in [examples/init.lua](examples/init.lua).

## Development

1. `pnpm i`
2. `cargo build`
3. In VSCode, press <kbd>F5</kbd> or change to the Debug panel and click `Launch Client`

> **Note**
> If encountered errors like `Cannot find module '/xxx/xxx/dist/extension.js'`
> please try run command `tsc -b` manually, you could refer https://github.com/IWANABETHATGUY/tower-lsp-boilerplate/issues/6 for more details

[wiki]: https://github.com/Desdaemon/odoo-lsp/wiki
[Releases]: https://github.com/Desdaemon/odoo-lsp/releases
[lsp-zero.nvim]: https://github.com/VonHeikemen/lsp-zero.nvim