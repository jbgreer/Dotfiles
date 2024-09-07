# ./apps/readline.nix

{ config, pkgs, ... }:

{
  programs.readline = {
    enable = true;
  };
}
