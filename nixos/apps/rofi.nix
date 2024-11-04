# apps/rofi.nix

{
  ...
}:

{
  programs.rofi-wayland = {
    enable = true;
    font = "Fira Code 12";
  };
}
