# ./apps/ripgrep.nix

{ config, pkgs, ... }:

{
  programs.ripgrep = {
    enable = true;
  };
}
