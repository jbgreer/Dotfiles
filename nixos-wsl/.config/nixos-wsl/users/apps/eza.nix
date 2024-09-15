# ./apps/eza.nix

{ config, pkgs, ... }:

{
  programs.eza = {
    enable = true;
    git = true;
    enableZshIntegration = true;
  };
}
