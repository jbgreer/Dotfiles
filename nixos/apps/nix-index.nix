# ./apps/nix-index.nix

{ config, pkgs, ... }:

{
  programs.nix-index = {
    enable = true;
  };
}
