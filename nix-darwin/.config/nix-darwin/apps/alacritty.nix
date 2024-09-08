# ./apps/alacritty.nix

{ config, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    catppuccin = {
      enable = true;
      flavor = "mocha";
    };
    settings = {
      window = {
        dimensions = {
          columns = 90;
          lines = 53;
        };
        padding = {
          x = 10;
          y = 10;
        };
        decorations = "Full";
        opacity = 1.0;
      };
      scrolling.history = 1000;
      font = {
        normal = {
          family = "FiraCode Nerd Font";
          style = "Regular";
        };
        bold = {
          family = "FiraCode Nerd Font";
          style = "Bold";
        };
        italic = {
          family = "FiraCode Nerd Font";
          style = "Italic";
        };
        size = 16;
      };
    };
  };
}
