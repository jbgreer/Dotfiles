# ./apps/direnv.nix

{ config, pkgs, ... }:

{
  programs = {
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
    zsh.enable = true;
  };
}

