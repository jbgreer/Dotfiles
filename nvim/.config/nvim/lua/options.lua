vim.opt.guicursor = ""
vim.opt.termguicolors = true

-- leader
vim.g.mapleader = ' '

-- sensible indention defaults
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.smartindent = true

-- no line-wrapping
vim.opt.wrap = false

-- no swapfile or backup
vim.opt.swapfile = false
vim.opt.backup = false

-- no highlighting on search, but incremental search
vim.opt.hlsearch = false
vim.opt.incsearch = true

-- line numbers
vim.opt.number = true
vim.opt.numberwidth = 4
vim.opt.relativenumber = true
vim.opt.signcolumn = 'number'

-- turn off language integrations
vim.g.loaded_python3_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_node_provider = 0

-- no mouse
vim.opt.mouse = ''

-- clipboard
vim.opt.clipboard = 'unnamedplus'

-- update time
vim.opt.updatetime = 100
