# apps/hyprlock.nix

{
  ...
}:

{
  programs.hyprlock = {
    enable = true;
    catppuccin = {
      enable = true;
      flavor = "mocha";
    };
  };
}
