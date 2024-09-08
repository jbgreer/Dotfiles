# ./apps/bat.nix

{ config, pkgs, ... }:

{
  programs.bat = {
    enable = true;
    config = {
      theme = "Catppuccin Mocha";
    };
    themes = {
          "Catppuccin Mocha" = {
            src = pkgs.fetchFromGitHub {
              owner = "catppuccin";
              repo = "bat";
              rev = "d3feec47b16a8e99eabb34cdfbaa115541d374fc";
              hash = "sha256-s0CHTihXlBMCKmbBBb8dUhfgOOQu9PBCQ+uviy7o47w=";
            };
            file = "Catppuccin Mocha.tmTheme";
          };
    };
  };
}


