# neovim/plugins/lsp.nix
#
{
  programs.nixvim.plugins = {
    lsp = {
      enable = true;

      keymaps = {
        silent = true;
        diagnostic = {
          "<leader>k" = "goto_prev";
          "<leader>j" = "goto_next";
        };

        lspBuf = {
          gd = "definition";
          gD = "references";
          gt = "type_definition";
          gi = "implementation";
          K = "hover";
          #"<F2>" = "rename";
        };
      };

      servers = {
        lua-ls.enable = true;
        nil-ls.enable = true;
        pyright.enable = true;
      };
    };

    lsp-format = {
      enable = true;
      lspServersToEnable = [
        "lua-ls"
        "nil-ls"
        "pyright"
      ];
    };
  };
}
