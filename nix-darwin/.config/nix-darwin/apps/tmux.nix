# ./apps/tmux.nix

{ config, lib, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    #enableFzf = true;
  };
}

