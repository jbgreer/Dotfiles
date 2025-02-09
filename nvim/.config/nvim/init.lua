-- Nixvim's internal module table
-- Can be used to share code throughout init.lua
local _M = {}

-- Set up globals {{{
do
    local nixvim_globals = {
        loaded_perl_provider = 0,
        loaded_python3_provider = 0,
        loaded_python_provider = 0,
        loaded_ruby_provider = 0,
        mapleader = " ",
        maplocalleader = " ",
    }

    for k, v in pairs(nixvim_globals) do
        vim.g[k] = v
    end
end
-- }}}

-- Set up options {{{
do
    local nixvim_options = {
        clipboard = "unnamedplus",
        colorcolumn = "100",
        completeopt = { "menu", "menuone", "noselect" },
        cursorcolumn = false,
        cursorline = false,
        expandtab = true,
        fileencoding = "utf-8",
        foldexpr = "nvim_treesitter#foldexpr()",
        foldlevel = 99,
        foldmethod = "expr",
        hidden = true,
        ignorecase = true,
        inccommand = "split",
        incsearch = true,
        laststatus = 3,
        modeline = true,
        modelines = 100,
        mouse = "a",
        mousemodel = "extend",
        number = true,
        numberwidth = 4,
        relativenumber = true,
        scrolloff = 8,
        shiftwidth = 2,
        signcolumn = "number",
        smartcase = true,
        smartindent = true,
        softtabstop = 0,
        spell = false,
        splitbelow = true,
        splitright = true,
        swapfile = false,
        tabstop = 2,
        termguicolors = true,
        textwidth = 0,
        undofile = true,
        updatetime = 100,
        wrap = false,
    }

    for k, v in pairs(nixvim_options) do
        vim.opt[k] = v
    end
end
-- }}}

require("catppuccin").setup({ flavour = "mocha", integrations = { cmp = true, gitsigns = true, treesitter = true } })

vim.loader.enable()
-- Highlight groups {{
do
    local highlights = { ExtraWhitespace = { bg = "red" } }

    for k, v in pairs(highlights) do
        vim.api.nvim_set_hl(0, k, v)
    end
end
-- }}

-- Match groups {{
do
    local match = { ExtraWhitespace = "\\s\\+$" }

    for k, v in pairs(match) do
        vim.fn.matchadd(k, v)
    end
end
-- }}

--- nixvim/default.nix extraConfigLuaPre begin
--- nixvim/default.nix extraConfigLuaPre end

vim.cmd([[let $BAT_THEME = 'catppuccin'

colorscheme catppuccin
]])
require("nvim-web-devicons").setup({})

-- LSP {{{
do
    local __lspServers = {
        {
            extraOptions = {
                on_attach = function(client, bufnr)
                    require("lsp-format").on_attach(client)
                end,
            },
            name = "pyright",
        },
        {
            extraOptions = {
                on_attach = function(client, bufnr)
                    require("lsp-format").on_attach(client)
                end,
            },
            name = "nil_ls",
        },
        {
            extraOptions = {
                on_attach = function(client, bufnr)
                    require("lsp-format").on_attach(client)
                end,
            },
            name = "lua_ls",
        },
    }
    -- Adding lspOnAttach function to nixvim module lua table so other plugins can hook into it.
    _M.lspOnAttach = function(client, bufnr) end
    local __lspCapabilities = function()
        capabilities = vim.lsp.protocol.make_client_capabilities()

        capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

        return capabilities
    end

    local __setup = {
        on_attach = _M.lspOnAttach,
        capabilities = __lspCapabilities(),
    }

    for i, server in ipairs(__lspServers) do
        if type(server) == "string" then
            require("lspconfig")[server].setup(__setup)
        else
            local options = server.extraOptions

            if options == nil then
                options = __setup
            else
                options = vim.tbl_extend("keep", options, __setup)
            end

            require("lspconfig")[server.name].setup(options)
        end
    end
end
-- }}}

local cmp = require("cmp")
cmp.setup({
    mapping = {
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<C-d>"] = cmp.mapping.scroll_docs(-4),
        ["<C-e>"] = cmp.mapping.abort(),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<CR>"] = cmp.mapping.confirm({ select = true }),
        ["<S-Tab>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "s" }),
        ["<Tab>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "s" }),
    },
    snippet = {
        expand = function(args)
            require("luasnip").lsp_expand(args.body)
        end,
    },
    sources = { { name = "nvim_lsp" }, { name = "path" }, { name = "buffer" }, { name = "luasnip" } },
})

require("which-key").setup({})

vim.opt.runtimepath:prepend(vim.fs.joinpath(vim.fn.stdpath("data"), "site"))
require("nvim-treesitter.configs").setup({
    indent = { enable = true },
    parser_install_dir = vim.fs.joinpath(vim.fn.stdpath("data"), "site"),
    refactor = {
        highlight_current_scope = { enable = false },
        highlight_definitions = { clear_on_cursor_move = true, enable = true },
        navigation = {
            enable = false,
            keymaps = {
                goto_definition = "gnd",
                goto_next_usage = "<a-*>",
                goto_previous_usage = "<a-#>",
                list_definitions = "gnD",
                list_definitions_toc = "gO",
            },
        },
        smart_rename = { enable = false, keymaps = { smart_rename = "grr" } },
    },
})

require("telescope").setup({
    defaults = {
        file_ignore_patterns = { "^.git/", "^.mypy_cache/", "^__pycache__/", "^output/", "^data/", "%.ipynb" },
        set_env = { COLORTERM = "truecolor" },
    },
})

local __telescopeExtensions = { "ui-select", "fzf" }
for i, extension in ipairs(__telescopeExtensions) do
    require("telescope").load_extension(extension)
end

require("luasnip").config.setup({})

require("luasnip.loaders.from_vscode").lazy_load({})

require("luasnip.loaders.from_vscode").lazy_load({})

require("lualine").setup({
    globalstatus = true,
    sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch" },
        lualine_c = { "filename", "diff" },
        lualine_x = {
            "diagnostics",
            {
                function()
                    local msg = ""
                    local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
                    local clients = vim.lsp.get_active_clients()
                    if next(clients) == nil then
                        return msg
                    end
                    for _, client in ipairs(clients) do
                        local filetypes = client.config.filetypes
                        if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
                            return client.name
                        end
                    end
                    return msg
                end,
                color = { fg = "#ffffff" },
                icon = "",
            },
            "encoding",
            "fileformat",
            "filetype",
        },
    },
})

require("lsp-format").setup({})

require("gitsigns").setup({})

require("neo-tree").setup({
    close_if_last_window = true,
    document_symbols = { custom_kinds = {} },
    window = { auto_expand_width = true, width = 30 },
})

-- Set up keybinds {{{
do
    local __nixvim_binds = {
        { action = "<cmd>Telescope buffers<cr>", key = "<leader>fb", mode = "n" },
        { action = "<cmd>Telescope diagnostics<cr>", key = "<leader>fd", mode = "n" },
        { action = "<cmd>Telescope find_files<cr>", key = "<leader>ff", mode = "n" },
        { action = "<cmd>Telescope live_grep<cr>", key = "<leader>fg", mode = "n" },
        { action = "<cmd>Telescope help_tags<cr>", key = "<leader>fh", mode = "n" },
        { action = "<NOP>", key = "<Space>", mode = "n", options = { desc = "No-op", unique = true } },
        { action = ":noh<CR>", key = "<Esc>", mode = "n", options = { desc = "Clear search results", unique = true } },
        { action = "y$", key = "Y", mode = "n", options = { desc = "Fix y behavior" } },
        {
            action = ":b#<CR>",
            key = "<C-c>",
            mode = "n",
            options = { desc = "bounce between two most recent files", unique = true },
        },
        { action = ":close<CR>", key = "<C-x>", mode = "n", options = { desc = "Close", unique = true } },
        { action = ":w<CR>", key = "<leader>s", mode = "n", options = { desc = "Save", unique = true } },
        { action = ":w<CR>", key = "<C-s>", mode = "n", options = { desc = "Save", unique = true } },
        {
            action = "<C-w>h",
            key = "<leader>h",
            mode = "n",
            options = { desc = "Navigate to left window", unique = true },
        },
        {
            action = "<C-w>l",
            key = "<leader>l",
            mode = "n",
            options = { desc = "Navigate to right window", unique = true },
        },
        {
            action = ":resize -2<CR>",
            key = "<C-Up>",
            mode = "n",
            options = { desc = "Resize up 2 lines", unique = true },
        },
        {
            action = ":resize +2<CR>",
            key = "<C-Down>",
            mode = "n",
            options = { desc = "Resize down 2 lines", unique = true },
        },
        {
            action = ":vertical resize -2<CR>",
            key = "<C-Left>",
            mode = "n",
            options = { desc = "Resize left 2 lines", unique = true },
        },
        {
            action = ":vertical resize +2<CR>",
            key = "<C-Right>",
            mode = "n",
            options = { desc = "Resize right 2 lines", unique = true },
        },
        {
            action = ":wincmd k<CR>",
            key = "<C-k>",
            mode = "n",
            options = { desc = "Navigate to lower window", unique = true },
        },
        {
            action = ":wincmd j<CR>",
            key = "<C-j>",
            mode = "n",
            options = { desc = "Navigate to upper window", unique = true },
        },
        {
            action = ":wincmd h<CR>",
            key = "<C-h>",
            mode = "n",
            options = { desc = "Navigate to left window", unique = true },
        },
        { action = ":wincmd l<CR>", key = "<C-l>", mode = "n", options = { desc = "Navigate to right window" } },
        {
            action = ":move -2<CR>",
            key = "<M-j>",
            mode = "n",
            options = { desc = "Move current line up 2", unique = true },
        },
        {
            action = ":move +2<CR>",
            key = "<M-k>",
            mode = "n",
            options = { desc = "Move current line down 2", unique = true },
        },
        { action = ">gv", key = ">", mode = "v", options = { desc = "Indent right", unique = true } },
        { action = ">gv", key = "<TAB>", mode = "v", options = { desc = "Indent right", unique = true } },
        { action = "<gv", key = "<", mode = "v", options = { desc = "Indent left", unique = true } },
        { action = "<gv", key = "<S-TAB>", mode = "v", options = { desc = "Indent left", unique = true } },
        {
            action = ":m '<-2<CR>gv=gv",
            key = "K",
            mode = "v",
            options = { desc = "Move selected block up 2 lines", unique = true },
        },
        {
            action = ":m '<+1<CR>gv=gv",
            key = "J",
            mode = "v",
            options = { desc = "Move selected block down 1 line", unique = true },
        },
        {
            action = ":Neotree filesystem reveal left<CR>",
            key = "<leader>nt",
            mode = "n",
            options = { desc = "Neo-tree filesystem browser", silent = true, unique = true },
        },
        {
            action = ":Neotree buffers reveal float<CR>",
            key = "<leader>nb",
            mode = "n",
            options = { desc = "Neo-tree buffer browser", silent = true, unique = true },
        },
    }
    for i, map in ipairs(__nixvim_binds) do
        vim.keymap.set(map.mode, map.key, map.action, map.options)
    end
end
-- }}}

--- nixvim/default.nix extraConfigLua begin
--- nixvim/default.nix extraConfigLua end

-- Set up autogroups {{
do
    local __nixvim_autogroups = { nixvim_binds_LspAttach = { clear = true } }

    for group_name, options in pairs(__nixvim_autogroups) do
        vim.api.nvim_create_augroup(group_name, options)
    end
end
-- }}
-- Set up autocommands {{
do
    local __nixvim_autocommands = {
        {
            callback = function(args)
                do
                    local __nixvim_binds = {
                        {
                            action = vim.diagnostic.goto_next,
                            key = "<leader>j",
                            mode = "n",
                            options = { desc = "Lsp diagnostic goto_next", silent = true },
                        },
                        {
                            action = vim.diagnostic.goto_prev,
                            key = "<leader>k",
                            mode = "n",
                            options = { desc = "Lsp diagnostic goto_prev", silent = true },
                        },
                        {
                            action = vim.lsp.buf.hover,
                            key = "K",
                            mode = "n",
                            options = { desc = "Lsp buf hover", silent = true },
                        },
                        {
                            action = vim.lsp.buf.references,
                            key = "gD",
                            mode = "n",
                            options = { desc = "Lsp buf references", silent = true },
                        },
                        {
                            action = vim.lsp.buf.definition,
                            key = "gd",
                            mode = "n",
                            options = { desc = "Lsp buf definition", silent = true },
                        },
                        {
                            action = vim.lsp.buf.implementation,
                            key = "gi",
                            mode = "n",
                            options = { desc = "Lsp buf implementation", silent = true },
                        },
                        {
                            action = vim.lsp.buf.type_definition,
                            key = "gt",
                            mode = "n",
                            options = { desc = "Lsp buf type_definition", silent = true },
                        },
                    }

                    for i, map in ipairs(__nixvim_binds) do
                        local options = vim.tbl_extend("keep", map.options or {}, { buffer = args.buf })
                        vim.keymap.set(map.mode, map.key, map.action, options)
                    end
                end
            end,
            desc = "Load keymaps for LspAttach",
            event = "LspAttach",
            group = "nixvim_binds_LspAttach",
        },
        { command = "norm zz", desc = "Vertically center document when entering insert mode", event = "InsertEnter" },
        { command = "%s/\\s\\+$//e", desc = "Remove trailing whitespace on save", event = "BufWrite" },
        { command = "wincmd L", desc = "open help in vertical split", event = "FileType", pattern = "help" },
        {
            command = "setlocal tabstop=2 shiftwidth=2",
            desc = "set indent to 2 spaces for nix files",
            event = "FileType",
            pattern = "nix",
        },
        {
            command = "setlocal spell spelllang=en",
            desc = "enable spellcheck for tex / latex / markdown",
            event = "FileType",
            pattern = { "tex", "latex", "markdown" },
        },
    }

    for _, autocmd in ipairs(__nixvim_autocommands) do
        vim.api.nvim_create_autocmd(autocmd.event, {
            group = autocmd.group,
            pattern = autocmd.pattern,
            buffer = autocmd.buffer,
            desc = autocmd.desc,
            callback = autocmd.callback,
            command = autocmd.command,
            once = autocmd.once,
            nested = autocmd.nested,
        })
    end
end
-- }}

--- nixvim/default.nix extraConfigLuaPost begin
--- nixvim/default.nix extraConfigLuaPost end
