# ./apps/htop.nix

{ config, lib, pkgs, ... }:

{
  programs.htop = {
    enable = true;
  };
}
