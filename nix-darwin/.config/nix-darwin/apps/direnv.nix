# ./apps/direnv.nix

{ config, lib, pkgs, ... }:

{
  programs.direnv = {
    enable = true;
  };
}
