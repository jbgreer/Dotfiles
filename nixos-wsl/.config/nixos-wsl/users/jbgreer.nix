# jbgreer.niprofileExtra
{ config, lib, pkgs, self, nixvim, catppuccin, ... }:

{
  catppuccin = {
    enable = true;
    flavor = "mocha";
  };

  home = {

    username = "jbgreer";
    homeDirectory = "/home/jbgreer";
    stateVersion = "24.05";  # WARNING DO NOT CHANGE

    # user account packages
    packages = with pkgs; [
      age                 # modern encryption tool with small explicity keys
      alacritty           # GPU-accelerated terminal emulator
      bat                 # cat clone with highlighting and git integration
      direnv              # shell extension that manages the environment
      eza                 # a modern replacement for ls
      fd                  # simple, fast, friendly alternative to find
      fzf                 # command-line fuzzy-finder
      glow                # render Markdown on the CLI
      gnupg               # GNU Privacy Guard
      htop                # display machine stas
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

  xdg.enable = true;
}
