-- setup lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- declare and install plugins
require("lazy").setup({
	{
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    dependencies = {
      -- LSP Support
      {'neovim/nvim-lspconfig'},             -- Required
      {'williamboman/mason.nvim'},           -- Optional
      {'williamboman/mason-lspconfig.nvim'}, -- Optional

      -- Autocompletion
      {'hrsh7th/nvim-cmp'},     -- Required
      {'hrsh7th/cmp-nvim-lsp'}, -- Required
      {'L3MON4D3/LuaSnip'},     -- Required
    }
  }
})

-- lsp-zero stanza
local lsp = require('lsp-zero').preset({})
lsp.on_attach(function(client, bufnr)
	lsp.default_keymaps({buffer = bufnr})
end)

local lspconfigs = require 'lspconfig.configs'

-- define our custom language server here
lspconfigs.odoo_lsp = {
  default_config = {
    name = 'odoo-lsp',
    cmd = {'odoo-lsp'},
    filetypes = {'javascript', 'xml', 'python'},
    root_dir = require('lspconfig.util').root_pattern('.odoo_lsp', '.odoo_lsp.json', '.git')
  }
}

local configured_lsps = {
  odoo_lsp = {},
  -- optional but recommended, requires pyright-langserver on path
  -- see https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#pyright
  -- pyright = {},
  -- optional, this is the same LSP used by VSCode for XML
  -- see https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#lemminx
  -- lemminx = {},
  -- optional, use `odoo-lsp tsconfig` to generate a tsconfig.json
  -- see https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#tsserver
  -- tsserver = {},
}

local lspconfig = require 'lspconfig'
for name, config in pairs(configured_lsps) do
  lspconfig[name].setup(config)
end

-- setup tab-completion
local cmp_action = require('lsp-zero').cmp_action()
require('cmp').setup {
  mapping = {
    ['<Tab>'] = cmp_action.luasnip_supertab(),
    ['<S-Tab>'] = cmp_action.luasnip_shift_supertab(),
  }
}

-- vim:ts=2:sw=2
