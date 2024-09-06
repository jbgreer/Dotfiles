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
      #".gitconfig".source             = ../../../git/.gitconfig;
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

  imports = [
    ./apps/alacritty.nix
    ./apps/bat.nix
    ./apps/direnv.nix
    ./apps/fd.nix
    ./apps/fzf.nix
    ./apps/git.nix
    ./apps/htop.nix
    ./apps/lazygit.nix
    ./apps/home-manager.nix
    ./apps/oh-my-posh.nix
    ./apps/readline.nix
    ./apps/ripgrep.nix
    ./apps/tmux.nix
    ./apps/zsh.nix
  ];

  xdg.enable = true; 
}
