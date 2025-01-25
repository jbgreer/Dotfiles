# jbgreer.niprofileExtra

{ config, lib, pkgs, ... }:

{
  catppuccin = {
    enable = true;
    flavor = "mocha";
  };

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
      #jk '';
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
      eza                 # a modern replacement for ls
      fd                  # simple, fast, friendly alternative to find
      font-awesome        # Font Awesome - OTF font
      fzf                 # command-line fuzzy-finder
      glow                # render Markdown on the CLI
      gnupg               # GNU Privacy Guard
      google-chrome       # web browser developed by Google
      htop                # display machine stas
      inkscape            # feature-rich vector graphics editor
      lazygit             # Simple terminal UI for git commands
      nix-zsh-completions # zsh compleitions for nix
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

    sessionPath = [
      "/run/current-system/sw/bin"
      "$HOME/.nix-profile/bin"
    ];
  };

  imports = [
    ./apps/alacritty.nix
    ./apps/bat.nix
    ./apps/direnv.nix
    ./apps/emacs.nix
    ./apps/eza.nix
    ./apps/fd.nix
    ./apps/fzf.nix
    ./apps/git.nix
    ./apps/htop.nix
    ./apps/lazygit.nix
    ./apps/home-manager.nix
    ./apps/neovim
    ./apps/oh-my-posh.nix
    ./apps/readline.nix
    ./apps/ripgrep.nix
    ./apps/tmux.nix
    ./apps/zsh.nix
  ];

  # emacs config files for now
  home.file.".config/emacs" = {
    source = ./config/emacs;
    recursive = true;
  };

  xdg.enable = true;
}
