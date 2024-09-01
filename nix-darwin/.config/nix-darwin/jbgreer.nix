# jbgreer.nix

{ config, pkgs, ... }:

{
  home.username = "jbgreer";
  home.homeDirectory = "/Users/jbgreer";
  home.stateVersion = "24.05";  # WARNING DO NOT CHANGE

  # user account packages
  home.packages = [
    #pkgs.alacritty           # GPU-accelerated terminal emulator
    pkgs.age                 # modern encryption tool with small explicity keys
    pkgs.bat                 # cat clone with highlighting and git integration
    pkgs.direnv              # shell extension that manages the environment
    pkgs.fd                  # simple, fast, friendly alternative to find
    pkgs.file                # shows the types of files
    pkgs.fzf                 # command-line fuzzy-finder
    #pkgs.glow                # render Markdown on the CLI
    #pkgs.kitty               # Modern, featureful, OpenGL based terminal emulator
    #pkgs.lazygit             # Simple terminal UI for git commands
    #pkgs.nix-zsh-completions # zsh compleitions for nix
    #pkgs.nixpkgs-fmt         # format nix files
    pkgs.neovim              # vim fork focused on extensibility & agility
    pkgs.ripgrep             # love child of silver searcher and grep
    #pkgs.sshs                # terminal user interface for SSH
    #pkgs.tldr                # better man pages
    pkgs.tmux                # terminal multiplexer
    #pkgs.tree-sitter         # Parser generator and incremental parsing library
    #pkgs.zellij              # terminal workspace with batteries included
  ];

  home.file = {
    ".gitconfig".source             = ../../../git/.gitconfig;
    ".config/nvim" = {
       source                       = ../../../nvim/.config/nvim;
       recursive = true;
    };
    #".config/starship.toml".source = ../../../starship/.config/starship.toml;
    ".tmux.conf".source             = ../../../tmux/.tmux.conf;
    ".zshenv".source                = ../../../zsh/.zshenv;
    ".config/zsh" = {
       source                       = ../../../zsh/.config/zsh;
       recursive = true;
    };
  };

  home.sessionPath = [
    "/run/current-system/sw/bin"
    "$HOME/.nix-profile/bin"
  ];

  programs.home-manager.enable = true;
  programs.tmux.enable = true;
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
