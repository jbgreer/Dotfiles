# ./apps/bat.nix

{ config, pkgs, ... }:

{
  programs.bat = {
    enable = true;
  };
}
