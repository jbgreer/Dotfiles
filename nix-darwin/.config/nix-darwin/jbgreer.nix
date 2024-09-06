# jbgreer.niprofileExtra

{ config, lib, pkgs, ... }:

{
  home = {
    activation = {
      copyApplications = let
        apps = pkgs.buildEnv {
          name = "home-manager-applications";
          paths = config.home.packages;
          pathsToLink = "/Applications";
        };
      in lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        baseDir="$HOME/Applications/Home Manager Apps"
        if [ -d "$baseDir" ]; then
          rm -rf "$baseDir"
          rm -rf "$baseDir.backup"
        fi
        mkdir -p "$baseDir"
        for appFile in ${apps}/Applications/*; do
          target="$baseDir/$(basename "$appFile")"
          $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
          $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
        done
      '';
    };

    username = "jbgreer";
    homeDirectory = "/Users/jbgreer";
    stateVersion = "24.05";  # WARNING DO NOT CHANGE

    # user account packages
    packages = with pkgs; [
      age                 # modern encryption tool with small explicity keys
      alacritty           # GPU-accelerated terminal emulator
      bat                 # cat clone with highlighting and git integration
      direnv              # shell extension that manages the environment
      fd                  # simple, fast, friendly alternative to find
      fzf                 # command-line fuzzy-finder
      glow                # render Markdown on the CLI
      gnupg               # GNU Privacy Guard
      google-chrome       # web browser developed by Google
      htop                # display machine stas
      lazygit             # Simple terminal UI for git commands
      nix-zsh-completions # zsh compleitions for nix
      neovim              # vim fork focused on extensibility & agility
      oh-my-posh          # prompt theme engine for any shell
      readline            # library for interactive line editing
      ripgrep             # love child of silver searcher and grep
      silver-searcher     # code-searching tool like ack
      ssh-copy-id         # tool to copy SSH public keys to a remote machine
      tldr                # better man pages
      tmux                # terminal multiplexer
      tree                # produce indented directory listing
      tree-sitter         # Parser generator and incremental parsing library
      zellij              # terminal workspace with batteries included
    ];

    file = {
      ".gitconfig".source             = ../../../git/.gitconfig;
      ".config/nvim" = {
        source                       = ../../../nvim/.config/nvim;
        recursive = true;
      };
      ".tmux.conf".source             = ../../../tmux/.tmux.conf;
    };

    sessionPath = [
      "/run/current-system/sw/bin"
      "$HOME/.nix-profile/bin"
    ];
  };

  programs.alacritty = {
    enable = true;
    settings = {
      window = {
        dimensions = {
          columns = 90;
          lines = 53;
        };
        padding = {
          x = 10;
          y = 10;
        };
        decorations = "Full";
        opacity = 1.0;
      };
      scrolling.history = 1000;
      font = {
        normal = {
          family = "FiraCode Nerd Font";
          style = "Regular";
        };
        bold = {
          family = "FiraCode Nerd Font";
          style = "Bold";
        };
        italic = {
          family = "FiraCode Nerd Font";
          style = "Italic";
        };
        size = 16;
      };
    };
  };
  programs.bat.enable = true;
  programs.direnv.enable = true;
  programs.fd.enable = true;
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.home-manager.enable = true;
  programs.htop.enable = true;
  programs.lazygit.enable = true;
  programs.oh-my-posh = {
    enable = true;
    useTheme = "atomic";
    enableZshIntegration = true;
  };
  programs.readline.enable = true;
  programs.ripgrep.enable = true;
  programs.tmux = {
    enable = true;
    #enableFzf = true;
  };
  programs.zellij.enable = true;
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
  };

  xdg.enable = true; 
}
