return {
  {
    "mfussenegger/nvim-dap"
  },
  {
    "nvim-neotest/nvim-nio"
  },
  {
    "theHamsta/nvim-dap-virtual-text"
  },
	{
    "rcarriga/nvim-dap-ui",
    config = function()
      require("dapui").setup()
      require("nvim-dap-virtual-text").setup()

      local dap = require("dap")

      dap.adapters.elixir = {
        type = 'executable',
        command = vim.fn.stdpath('data') .. "/mason/bin/" .. "elixir-ls-debugger",
        args = {}
      }
      dap.configurations.elixir = {
        {
          type = "mix_task",
          name = "elixir",
          request = "launch",
          task = "test",
          taskArgs = { "--trace" },
          startApps = false,
          projectDir = "${workspaceFolder}",
          requireFiles = {}
        }
      }

      dap.adapters.mix_task = {
        type = 'executable',
        command = vim.fn.stdpath('data') .. "/mason/bin/" .. "elixir-ls-debugger",
        args = {}
      }
      dap.configurations.mix_task = {
        type = "mix_task",
        name = "mix test",
        request = "launch",
        task = "test",
        taskArgs = { "--trace" },
        startApps = false,
        projectDir = "${workspaceFolder}",
        requireFiles = {
          "test/**/test_helper.exs",
          "test/**/*_test.exs"
        }
      }

      vim.fn.sign_define('DapBreakpoint', { text='🔴', texthl='DapBreakpoint', linehl='DapBreakpoint', numhl='DapBreakpoint' })
      vim.keymap.set("n", "<leader>dt", ":DapUiToggle<CR>", {noremap=true})
      vim.keymap.set("n", "<leader>db", ":DapToggleBreakpoint<CR>", {noremap=true})
      vim.keymap.set("n", "<leader>dc", ":DapContinue<CR>", {noremap=true})
      vim.keymap.set("n", "<leader>dr", ":lua require('dapui').open({reset = true})<CR>", {noremap=true})
    end
	}
}
