# ./apps/bat.nix

{ config, lib, pkgs, ... }:

{
  programs.bat = {
    enable = true;
  };
}
