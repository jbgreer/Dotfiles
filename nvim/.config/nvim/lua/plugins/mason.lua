return {
	{
		"williamboman/mason.nvim",
		lazy = false,
		config = function()
			require("mason").setup()
		end
	},
	{
		"williamboman/mason-lspconfig.nvim",
		lazy = false,
		config = function()
			require("mason-lspconfig").setup({
				ensure_installed = {
					"elixirls",
					"lua_ls",
					"pyright",
					"rust_analyzer",
				}
			})
		end
	},
	{
		"neovim/nvim-lspconfig",
		lazy = false,
		config = function()
			local capabilities =require('cmp_nvim_lsp').default_capabilities()

			local lspconfig = require("lspconfig")

			lspconfig.lua_ls.setup({
				capabilities = capabilities,
			})

			lspconfig.elixirls.setup({
				capabilities = capabilities,
				cmd = { vim.fn.stdpath('data') .. "/mason/bin/" .. "elixir-ls" },
			})

			-- keybindings for lsp
			vim.keymap.set("n", "K", vim.lsp.buf.hover, {})
			vim.keymap.set("n", "<leader>gd", vim.lsp.buf.definition, {})
			vim.keymap.set("n", "<leader>gr", vim.lsp.buf.references, {})
			vim.keymap.set("n", "<leader>gr", vim.lsp.buf.references, {})
			vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, {})
			vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, {})
		end
	}
}
