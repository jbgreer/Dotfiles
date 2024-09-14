# neovim/plugings/nvim-treesitter.nix
# https://github.com/nvim-treesitter/nvim-treesitter/
{
  programs.nixvim.plugins = {
    treesitter = {
      enable = true;

      nixvimInjections = true;

      folding = true;
      settings = {
        indent.enable = true;
      };
    };

    treesitter-refactor = {
      enable = true;
      highlightDefinitions.enable = true;
    };
  };
}
