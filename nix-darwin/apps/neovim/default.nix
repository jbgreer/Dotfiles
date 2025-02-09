# neovim/defaujlt.nix
{
  config,
  pkgs,
  nixvim,
  ...
}:

{
  programs.nixvim = {
    enable = true;

    colorschemes.catppuccin = {
      enable = true;
      settings = {
        flavour = "mocha";
        # list of plugins to enable colorscheme support
        # see https://github.com/catppuccin/nvim#integrations
        integrations =  {
          cmp = true;
          gitsigns = true;
          # lualine = true;
          treesitter = true;
        };
      };
    };

    defaultEditor = true;

    extraConfigLua = ''
      --- nixvim/default.nix extraConfigLua begin
      --- nixvim/default.nix extraConfigLua end
   '';

    extraConfigLuaPre = ''
      --- nixvim/default.nix extraConfigLuaPre begin
      --- nixvim/default.nix extraConfigLuaPre end
    '';

    extraConfigLuaPost = ''
      --- nixvim/default.nix extraConfigLuaPost begin
      --- nixvim/default.nix extraConfigLuaPost end
    '';

    luaLoader.enable = true;

    # Highlight white space after ene of text
    highlight.ExtraWhitespace.bg = "red";
    match.ExtraWhitespace = "\\s\\+$";

    withNodeJs = false;
    withRuby = false;

    viAlias = true;
    vimAlias = true;
  };

  imports = [
    ./options.nix
    ./autocommands.nix
    ./clipboard.nix
    ./keymaps.nix
    ./plugins
  ];
}

