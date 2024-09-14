# neovim/plugins/nvim-cmp.nix
# https://github.com/hrsh7th/nvim-cmp/
{
  programs.nixvim = {

    plugins = {

      cmp = {
				enable = true;
        autoEnableSources = true;

        settings = {

          snippet.expand = "function(args) require('luasnip').lsp_expand(args.body) end";

          mapping = {
            "<C-d>"     = "cmp.mapping.scroll_docs(-4)";
            "<C-f>"     = "cmp.mapping.scroll_docs(4)";
            "<C-Space>" = "cmp.mapping.complete()";
            "<C-e>"     = "cmp.mapping.abort()";
            "<CR>"      = "cmp.mapping.confirm({ select = true })";
            "<Tab>"     = "cmp.mapping(cmp.mapping.select_next_item(), {'i', 's'})";
            "<S-Tab>"   = "cmp.mapping(cmp.mapping.select_prev_item(), {'i', 's'})";
          };

          sources = [
            { name = "nvim_lsp"; }
            { name = "path"; }
            { name = "buffer"; }
            { name = "luasnip"; }
          ];
        };
      };

      # https://github.com/hrsh7th/cmp-buffer/
      cmp-buffer.enable = true;

      # https://github.com/hrsh7th/cmp-nvim-lsp/
      cmp-nvim-lsp.enable = true;

      # https://github.com/hrsh7th/cmp-path/
      cmp-path.enable = true;

      # https://github.com/saadparwaiz1/cmp_luasnip/
      cmp_luasnip.enable = true;
    };
  };
}

