# ./apps/git.nix

{ pkgs, ... }:

{
  programs.git = {
    aliases = {
      co = "checkout";
      undo = "reset HEAD~1 --mixed";
      amend = "commit -a --amend";
    };
    enable = true;
    extraConfig.init.defaultBranch = "main";
    package = pkgs.gitAndTools.gitFull;
    signing.key = "607574694C77731C";
    userEmail = "jbgreer@gmail.com";
    userName = "Jim Greer";
  };
}

