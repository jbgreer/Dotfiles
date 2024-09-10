{
  programs.nixvim = {
    plugins = {
      lsp = {
        enable = true;

        keymaps = {
          silent = true;
          diagnostic = {
            # Navigate in diagnostics
            "<leader>k" = "goto_prev";
            "<leader>j" = "goto_next";
          };

          lspBuf = {
            gd = "definition";
            gD = "references";
            gt = "type_definition";
            gi = "implementation";
            K = "hover";
            "<F2>" = "rename";
          };
        };

        servers = {
          lua-ls.enable = true;
          elixirls.enable = true;
          rust-analyzer.enable = true;
        };
      };

      lsp-format = {
        enable = true;
        lspServersToEnable = [
          "elixirls"
          "lua-ls"
          "rust-analyzer"
        ];
      };
    };
  };
}
