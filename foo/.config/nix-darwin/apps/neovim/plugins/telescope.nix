# neovim/plugins/telescope.nix
# https://github.com/nvim-telescope/telescope.nvim/
{

  programs.nixvim = {
    plugins.web-devicons.enable = true;

    plugins.telescope = {
      enable = true;

      extensions = {
        fzf-native.enable = true;
        ui-select.enable = true;
      };

      keymaps = {
        "<leader>ff" = "find_files";
        "<leader>fg" = "live_grep";
        "<leader>fb" = "buffers";
        "<leader>fh" = "help_tags";
        "<leader>fd" = "diagnostics";

        #"<C-p>" = "git_files";
        #"<leader>p" = "oldfiles";
        #"<C-/>" = "live_grep";
      };

      #keymapsSilent = true;

      settings.defaults = {
        file_ignore_patterns = [
          "^.git/"
          "^.mypy_cache/"
          "^__pycache__/"
          "^output/"
          "^data/"
          "%.ipynb"
        ];
        set_env.COLORTERM = "truecolor";
      };
    };
  };
}
