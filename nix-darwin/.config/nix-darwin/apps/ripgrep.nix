# ./apps/ripgrep.nix

{ config, lib, pkgs, ... }:

{
  programs.ripgrep = {
    enable = true;
  };
}
