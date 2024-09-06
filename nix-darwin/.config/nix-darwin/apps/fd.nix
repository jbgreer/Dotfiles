# ./apps/fd.nix

{ config, lib, pkgs, ... }:

{
  programs.fd = {
    enable = true;
  };
}
