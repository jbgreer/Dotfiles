# ./apps/fd.nix

{ config, pkgs, ... }:

{
  programs.fd = {
    enable = true;
  };
}
