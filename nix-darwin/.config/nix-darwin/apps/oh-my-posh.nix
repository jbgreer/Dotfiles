# ./apps/oh-my-posh.nix

{ config, pkgs, ... }:

{
  programs.oh-my-posh = {
    enable = true;
    useTheme = "atomic";
    enableZshIntegration = true;
  };
}

