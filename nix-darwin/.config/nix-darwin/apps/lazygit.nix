# ./apps/lazygit.nix

{ config, pkgs, ... }:

{
  programs.lazygit = {
    enable = true;
  };
}
