# ./apps/home-manager.nix

{ config, pkgs, ... }:

{
  programs.home-manager = {
    enable = true;
  };
}
