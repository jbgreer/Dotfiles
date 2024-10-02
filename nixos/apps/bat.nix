# ./apps/bat.nix

{ config, pkgs, ... }:

{
  programs.bat = {
    enable = true;
    catppuccin = {
      enable = true;
      flavor = "mocha";
    };
  };
}


