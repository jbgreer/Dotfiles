# ./apps/zsh.nix

{ config, pkgs, ... }:

{
  programs.zsh = {
    autosuggestion.enable = true;
    defaultKeymap = "viins";
    dotDir = ".config/zsh";
    enable = true;
    enableCompletion = true;
    envExtra = ''
    # programs.zsh.envExtra
    # programs.zsh.envExtra
    '';
    initExtraBeforeCompInit = ''
    # programs.zsh.initExtraBeforeCompInit
    # programs.zsh.initExtraBeforeCompInit
    '';
    initExtra = ''
    # programs.zsh.initExtra
    # programs.zsh.initExtra
    '';
    initExtraFirst = ''
    # programs.zsh.initExtraFirst
    # programs.zsh.initExtraFirst
    '';
    loginExtra = ''
      # start hyprland only on 1st console
      [ -z  "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ] && [ $(tty) = "/dev/tty1" ] && exec Hyprland
    '';
    profileExtra = ''
    # programs.zsh.profileExtra
    # programs.zsh.profileExtra
    '';
    shellAliases = {
      l = "ls -al";
    };
    sessionVariables = {
      EDITOR = "nvim";
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";
      XDG_RUNTIME_DIR = "$HOME/.xdg";
    };
    syntaxHighlighting = {
      catppuccin.enable = true;
      catppuccin.flavor = "mocha";
    };
  };
}


