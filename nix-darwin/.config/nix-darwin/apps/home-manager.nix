# ./apps/home-manager.nix

{ config, lib, pkgs, ... }:

{
  programs.home-manager = {
    enable = true;
  };
}
