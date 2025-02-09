# ./apps/eza.nix

{ ... }:

{
  programs.eza = {
    enable = true;
    git = true;
    enableZshIntegration = true;
  };
}
