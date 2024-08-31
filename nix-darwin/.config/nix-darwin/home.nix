# home.nix

{ config, pkgs, ... }:

{
  home.username = "jbgreer"; home.homeDirectory = "/Users/jbgreer";
  home.stateVersion = "24.05";  # WARNING DO NOT CHANGE


  # user account packages
  home.packages = [
    #pkgs.age     # modern encryption tool with small explicity keys
    #pkgs.direnv  # shell extension that manages the environment
    #pkgs.glow    # render Markdown on the CLI
    pkgs.neovim   # vim text editor fork focused on extensibility & agility
    #pkgs.sshs    # terminal user interface for SSH
    pkgs.starship # a minimal, blazing flast, customizable shell prompt
  ];

  home.file = {
    ".zshenv".source      = ../../../zsh/.zshenv;
    ".config/zsh" = {
       source             = ../../../zsh/.config/zsh;
       recursive = true;
    };
  };

  home.sessionPath = [
    "/run/current-system/sw/bin"
    "$HOME/.nix-profile/bin"
  ];

  programs.home-manager.enable = true;
  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    dotDir = ".config/zsh";
    autosuggestion.enable = true;
    enableCompletion = true;
    shellAliases = {
      l = "ls -al";
    };
  };

  xdg.enable = true; 

}
