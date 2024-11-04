# apps/starship.nix

{
  ...
}:

{
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
  };
}
