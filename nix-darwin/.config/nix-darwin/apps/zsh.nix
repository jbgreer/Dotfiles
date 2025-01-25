# ./apps/zsh.nix

{ ... }:

{
  catppuccin.zsh-syntax-highlighting.enable = true;
  catppuccin.zsh-syntax-highlighting.flavor = "mocha";

  programs.zsh = {
    autosuggestion.enable = true;
    defaultKeymap = "viins";
    dotDir = ".config/zsh";
    enable = true;
    enableCompletion = true;
    envExtra = ''
    # programs.zsh.envExtra

    # Racket
    [ -d '/Applications/Racket v8.14/bin' ] && path+=('/Applications/Racket v8.14/bin')

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
    profileExtra = ''
    # programs.zsh.profileExtra
    # programs.zsh.profileExtra
    '';
    shellAliases = {
      l = "ls -al";
      vim = "nvim";
    };
    sessionVariables = {
      EDITOR = "nvim";
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";
      XDG_RUNTIME_DIR = "$HOME/.xdg";
    };
  };
}

