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

  fonts = {
    packages = with pkgs; [
      fira-code
      fira-code-symbols
      font-awesome
      (nerdfonts.override { fonts = [ "FiraCode" ]; })
    ];
  };

  #security.sudo.wheelNeedsPassword = false;

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
