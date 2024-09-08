# ./apps/zellij.nix

{ config, pkgs, ... }:

{
  programs.zellij = {
    enable = true;
    catppuccin = {
      enable = true;
      flavor = "mocha";
    };
  };
}
