{
  inputs,
  pkgs,
  config,
  lib,
  ...
}: {

  time.timeZone = "America/Chicago";
  networking.hostName = "vonnegut";

  programs.zsh.enable = true;
  environment = {
    shells = with pkgs; [
      bash
      zsh
    ];
    systemPackages = with pkgs; [
      curl
      git
      wget
      vim
      zsh
    ];

    enableAllTerminfo = true;
  };

  security.sudo.wheelNeedsPassword = false;

  users.users.jbgreer = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [
      "wheel"
    ];
  };

  users.defaultUserShell = pkgs.zsh;

  system.stateVersion = "24.05";

  wsl = {
    enable = true;
    wslConf.automount.root = "/mnt";
    wslConf.interop.appendWindowsPath = false;
    wslConf.network.generateHosts = false;
    defaultUser = "jbgreer";
    startMenuLaunchers = true;
  };
}
