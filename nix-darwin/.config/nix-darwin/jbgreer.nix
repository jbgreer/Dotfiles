# jbgreer.nix

{ config, pkgs, ... }:

{
  home = {
    username = "jbgreer";
    homeDirectory = "/Users/jbgreer";
    stateVersion = "24.05";  # WARNING DO NOT CHANGE

    # user account packages
    packages = [
      pkgs.age                 # modern encryption tool with small explicity keys
      pkgs.alacritty           # GPU-accelerated terminal emulator
      pkgs.bat                 # cat clone with highlighting and git integration
      pkgs.direnv              # shell extension that manages the environment
      pkgs.fd                  # simple, fast, friendly alternative to find
      pkgs.fzf                 # command-line fuzzy-finder
      pkgs.glow                # render Markdown on the CLI
      pkgs.google-chrome       # freeware browser developed by Google
      pkgs.gnupg               # GNU Privacy Guard
      pkgs.htop                # display machine stas
      #pkgs.kitty               # Modern, featureful, OpenGL based terminal emulator
      pkgs.lazygit             # Simple terminal UI for git commands
      pkgs.nix-zsh-completions # zsh compleitions for nix
      #pkgs.nixpkgs-fmt         # format nix files
      pkgs.neovim              # vim fork focused on extensibility & agility
      pkgs.readline            # library for interactive line editing
      pkgs.ripgrep             # love child of silver searcher and grep
      pkgs.silver-searcher     # code-searching tool like ack
      pkgs.ssh-copy-id         # tool to copy SSH public keys to a remote machine
      #pkgs.sshs                # terminal user interface for SSH
      pkgs.tldr                # better man pages
      pkgs.tmux                # terminal multiplexer
      pkgs.tree                # produce indented directory listing
      pkgs.tree-sitter         # Parser generator and incremental parsing library
      pkgs.zellij              # terminal workspace with batteries included
    ];

    file = {
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

    sessionPath = [
      "/run/current-system/sw/bin"
      "$HOME/.nix-profile/bin"
    ];
  };

  programs.alacritty.enable = true;
  programs.bat.enable = true;
  programs.direnv.enable = true;
  programs.fd.enable = true;
  programs.fzf.enable = true;
  programs.home-manager.enable = true;
  programs.htop.enable = true;
  programs.lazygit.enable = true;
  programs.readline.enable = true;
  programs.ripgrep.enable = true;
  programs.tmux = {
    enable = true;
    #enableFzf = true;
  };
  programs.zellij.enable = true;
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
