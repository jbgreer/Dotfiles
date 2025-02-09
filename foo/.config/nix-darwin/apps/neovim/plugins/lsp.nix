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
        lua_ls.enable = true;
        nil_ls.enable = true;
        pyright.enable = true;
      };
    };

    lsp-format = {
      enable = true;
      lspServersToEnable = [
        "lua_ls"
        "nil_ls"
        "pyright"
      ];
    };
  };
}
