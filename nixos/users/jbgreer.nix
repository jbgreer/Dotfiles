{
  config,
  lib,
  pkgs,
  nixvim,
  catppuccin,
  ...
}:

{
  home.username = "jbgreer";
  home.homeDirectory = "/home/jbgreer";

  home.packages = with pkgs; [
    alacritty # terminal
    asciidoctor # text formatting / type setting
    bat # cat substitute
    brightnessctl # read and control device brightness
    cmake # builder
    fd # file display
    file # learn file type
    firefox # web browser
    fira-code # font
    fira-code-symbols # additional font
    font-awesome # additional font
    fzf # fast finder
    gcc # compiler
    gnupg # encrypt/decrypt/sign/validate
    kitty # terminal
    lazygit # git ui
    libnotify # for hyprland - notification daemon support
    lua-language-server # for neovime
    material-icons # icons
    makeWrapper # for Nixos?
    ninja # builder
    nix-zsh-completions # zsh completions for nix
    nixpkgs-fmt # format nix files
    pavucontrol # read and control sound volume
    pfetch # shell-based system info tool
    ripgrep # grep substitute
    #rofi-wayland # for hyprland dmenu support
    swaynotificationcenter # notifications for hyprland
    swww # for hyprland
    tldr # better man pages
    tmux # terminal multiplexer
    tofi # for hyprland dmenu support
    tree-sitter # for neovim
    unzip # uncompress files
    wl-clipboard # wayland cut and paste clipboard
    zellij # terminal multiplexer
    zip # compress files
    (import ../scripts/task-waybar.nix { inherit pkgs; })
  ];

  imports = [
    ../apps/alacritty.nix
    ../apps/bat.nix
    ../apps/direnv.nix
    ../apps/eza.nix
    ../apps/fd.nix
    ../apps/fzf.nix
    ../apps/git.nix
    ../apps/helix.nix
    ../apps/home-manager.nix
    ../apps/htop.nix
    ../apps/kitty.nix
    ../apps/lazygit.nix
    ../apps/neovim
    ../apps/nix-index.nix
    ../apps/oh-my-posh.nix
    ../apps/readline.nix
    ../apps/ripgrep.nix
    ../apps/starship.nix
    ../apps/swaylock.nix
    ../apps/tmux.nix
    ../apps/tofi.nix
    ../apps/waybar.nix
    ../apps/zellij.nix
    ../apps/zsh.nix
  ];

  home.file.".config/pipewire/pipewire.conf".source = ../apps/pipewire/pipewire.conf;

  home.file.".config/hypr" = {
    source = ../apps/hyprland;
    recursive = true;
  };

  home.file.".config/tofi" = {
    source = ../apps/tofi;
    recursive = true;
  };

  home.file.".config/swaync" = {
    source = ../apps/swaync;
    recursive = true;
  };

  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
    };
  };

  catppuccin = {
    enable = true;
    flavor = "mocha";
  };

  # integrate nix-index into shell
  programs.nix-index.enable = true;

  # self-manage home-manager
  programs.home-manager.enable = true;

  # DO NOT CHANGE WITHOUT READING MANUAL
  home.stateVersion = "24.05";
}
