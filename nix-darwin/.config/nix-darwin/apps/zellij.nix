# ./apps/zellij.nix

{ config, lib, pkgs, ... }:

{
  programs.zellij = {
    enable = true;
  };
}
