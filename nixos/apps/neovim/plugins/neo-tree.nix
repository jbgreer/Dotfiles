# neovim/plugins/neo-tree.nix
# https://github.com/nvim-neo-tree/neo-tree.nvim
{
  programs.nixvim = {
    keymaps = [
      {
        action = ":Neotree filesystem reveal left<CR>";
        key = "<leader>nt";
        mode = "n";
        options = {
          desc = "Neo-tree filesystem browser";
          silent = true;
          unique = true;
        };
      }
      {
        action = ":Neotree buffers reveal float<CR>";
        key = "<leader>nb";
        mode = "n";
        options = {
          desc = "Neo-tree buffer browser";
          silent = true;
          unique = true;
        };
      }
    ];

    plugins.neo-tree = {
      enable = true;

      closeIfLastWindow = true;
      window = {
        width = 30;
        autoExpandWidth = true;
      };
    };
  };
}
