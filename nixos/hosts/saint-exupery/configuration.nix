# configuration_bootstrap.nix
{
  inputs,
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Bootloader settings
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;
  boot.supportedFilesystems = [
    "btrfs"
    "ntfs"
    "fat32"
  ];
  hardware.enableAllFirmware = true;

  # set hostname
  networking.hostName = "saint-exupery";
  networking.networkmanager.enable = true;

  # set locale and timezone
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };
  time.timeZone = "America/Chicago";

  # set username
  users.users.jbgreer = {
    isNormalUser = true;
    extraGroups = [
      "networkmanager"
      "wheel"
      "video"
      "audio"
    ];
    shell = pkgs.zsh;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # system-wide packages
  environment = {
    shells = with pkgs; [
      bash
      zsh
    ];
    systemPackages = with pkgs; [
      curl
      cryptsetup
      exfatprogs
      git
      home-manager
      ntfs3g
      sbctl
      wget
      vim
      xdg-desktop-portal-hyprland
      zsh
    ];
  };

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      fira-code
      fira-code-symbols
      (nerdfonts.override { fonts = [ "FiraCode" ]; })
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        sansSerif = [ "FiraCode" ];
        serif = [ "FiraCode" ];
        monospace = [ "FiraCode" ];
      };
    };
  };

  security.pam.services.hyprlock = {};
  security.rtkit.enable = true;

  services.gvfs.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  services.udisks2.enable = true;
  services.upower.enable = true;

  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    portalPackage =  inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
  };

  environment.pathsToLink = [ "/share/xdg-desktop-portal" "/share/applications" ];

  # must enable here because it is available system wide
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # WARNING! Be careful when changing.
  system.stateVersion = "24.05";

  # Enable use of flakes
  nix.settings.experimental-features = "nix-command flakes";
}
