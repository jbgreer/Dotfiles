# ./apps/lazygit.nix

{ config, lib, pkgs, ... }:

{
  programs.lazygit = {
    enable = true;
  };
}
