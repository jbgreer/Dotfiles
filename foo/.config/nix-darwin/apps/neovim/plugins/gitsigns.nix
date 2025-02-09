# neovim/plugins/gitsigns.nix
# https://github.com/lewis6991/gitsigns.nvim/
{
  programs.nixvim.plugins.gitsigns = {
    enable = true;
  };
}

