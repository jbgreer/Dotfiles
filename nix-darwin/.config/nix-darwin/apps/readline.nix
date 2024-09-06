# ./apps/readline.nix

{ config, lib, pkgs, ... }:

{
  programs.readline = {
    enable = true;
  };
}
