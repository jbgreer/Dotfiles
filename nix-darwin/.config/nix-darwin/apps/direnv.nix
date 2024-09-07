# ./apps/direnv.nix

{ config, pkgs, ... }:

{
  programs.direnv = {
    enable = true;
  };
}
