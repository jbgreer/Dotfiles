# ./apps/git.nix

{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    extraConfig.init.defaultBranch = "main";
    signing.key = "607574694C77731C";
    userEmail = "jbgreer@gmail.com";
    userName = "Jim Greer";
  };
}

