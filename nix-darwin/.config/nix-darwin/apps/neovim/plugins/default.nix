{
  imports = [
    ./barbar.nix
    ./comment.nix
    ./dap.nix
    ./elixirls.nix
    ./floaterm.nix
    ./fugitive.nix
    ./lsp.nix
    ./lua-ls.nix
    ./lualine.nix
    ./markdown-preview.nix
    #./neorg.nix
    ./neo-tree.nix
    ./rust-analyzer.nix
    ./startify.nix
    ./tagbar.nix
    ./telescope.nix
    ./treesitter.nix
    ./vimtex.nix
    ./which-key.nix
  ];

  programs.nixvim = {
    colorschemes.catppuccin = {
			enable = true;
			settings.flavour = "mocha";
		};


    plugins = {
      gitsigns = {
        enable = true;
        settings.signs = {
          add.text = "+";
          change.text = "~";
        };
      };

      nvim-autopairs.enable = true;

      nvim-colorizer = {
        enable = true;
        userDefaultOptions.names = false;
      };

      oil.enable = true;
    };
  };
}
