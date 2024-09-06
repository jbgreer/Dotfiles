# ./apps/oh-my-posh.nix

{ config, lib, pkgs, ... }:

{
  programs.oh-my-posh = {
    enable = true;
    useTheme = "atomic";
    enableZshIntegration = true;
  };
}

