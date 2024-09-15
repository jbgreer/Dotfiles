# ./apps/oh-my-posh.nix

{ config, pkgs, ... }:

{
  programs.oh-my-posh = {
    enable = true;
    useTheme = "catppuccin_mocha";
    enableZshIntegration = true;
  };
}

