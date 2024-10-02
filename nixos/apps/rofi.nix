{ config, pkgs, ... }:

{
  programs.rofi-wayland = {
    enable = true;
    #theme = "solarized";
    font = "Fira Code 12";
  };
}
