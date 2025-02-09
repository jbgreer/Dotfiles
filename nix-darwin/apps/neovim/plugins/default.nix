# neovim/plugins/default.nix
{
  imports = [
    ./friendly-snippets.nix
    ./gitsigns.nix
    ./lazygit.nix
    ./lualine.nix
    ./luasnip.nix
    ./lsp.nix
    ./neo-tree.nix
    ./nvim-cmp.nix
    ./nvim-treesitter.nix
    ./telescope.nix
    ./which-key.nix
  ];
}

