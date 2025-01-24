# ./apps/lazygit.nix

{ ... }:

{
  catppuccin.lazygit.enable = true;
  catppuccin.lazygit.flavor = "mocha";

  programs.lazygit = {
    enable = true;
  };
}
