# neovim/plugins/luasnip.nix
# https://github.com/L3MON4D3/LuaSnip
{
  programs.nixvim.plugins.luasnip = {
    enable = true;
    fromVscode = [
      {}
    ];
  };
}

