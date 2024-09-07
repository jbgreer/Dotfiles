# ./apps/htop.nix

{ config, pkgs, ... }:

{
  programs.htop = {
    enable = true;
  };
}
