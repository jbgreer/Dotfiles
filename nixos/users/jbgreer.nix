{
  pkgs,
  ...
}:

{
  home.username = "jbgreer";
  home.homeDirectory = "/home/jbgreer";

  home.packages = with pkgs; [
    alacritty
    asciidoctor
    bat
    brightnessctl
    eza
    fd
    file
    fira-code
    fira-code-symbols
    font-awesome
    fzf
    gnupg
    htop
    kitty
    lazygit
    libnotify
    material-icons
    nix-zsh-completions # zsh completions for nix
    nixpkgs-fmt # format nix files
    pavucontrol
    pfetch # shell-based system info tool
    ripgrep # grep substitute
    rofi-wayland
    swaynotificationcenter
    tldr # better man pages
    tofi
    tmux
    tree-sitter # for neovim
    unzip # uncompress files
    wl-clipboard
    xdg-desktop-portal-hyprland
    zellij
    zip # compress files
  ];

  imports = [
    ../apps/alacritty.nix
    ../apps/bat.nix
    ../apps/direnv.nix
    ../apps/eza.nix
    ../apps/fd.nix
    ../apps/fzf.nix
    ../apps/git.nix
    ../apps/home-manager.nix
    ../apps/htop.nix
    ../apps/hyprlock.nix
    ../apps/kitty.nix
    ../apps/lazygit.nix
    ../apps/neovim
    ../apps/oh-my-posh.nix
    ../apps/tmux.nix
    ../apps/readline.nix
    ../apps/ripgrep.nix
    #    ../apps/swaylock.nix
    ../apps/waybar.nix
    ../apps/zellij.nix
    ../apps/zsh.nix
    #(import ../scripts/task-waybar.nix { inherit pkgs; })
  ];

  home.file.".config/pipewire/pipewire.conf".source = ../apps/pipewire/pipewire.conf;

  home.file.".config/hypr" = {
    source = ../apps/hyprland;
    recursive = true;
  };

  home.file.".config/rofi" = {
    source = ../apps/rofi;
    recursive = true;
  };

  home.file.".config/swaync" = {
    source = ../apps/swaync;
    recursive = true;
  };

  # integrate nix-index into shell
  programs.nix-index.enable = true;

  # self-manage home-manager
  programs.home-manager.enable = true;

  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    systemd.variables = [ "--all" ];
  };

  xdg = {
    portal = {
      enable = true;
      extraPortals = [
        pkgs.xdg-desktop-portal
        pkgs.xdg-desktop-portal-gtk
        pkgs.xdg-desktop-portal-hyprland
      ];
      configPackages = [
        pkgs.xdg-desktop-portal
        pkgs.xdg-desktop-portal-gtk
        pkgs.xdg-desktop-portal-hyprland
      ];
    };
    userDirs = {
      enable = true;
      createDirectories = true;
    };
  };

  catppuccin = {
    enable = true;
    flavor = "mocha";
  };

  # DO NOT CHANGE WITHOUT READING MANUAL
  home.stateVersion = "24.05";
}
