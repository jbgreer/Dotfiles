# ./apps/lazygit.nix

{ config, pkgs, ... }:

{
  programs.lazygit = {
    enable = true;
    catppuccin = {
      enable = true;
      flavor = "mocha";
    };
  };
}
