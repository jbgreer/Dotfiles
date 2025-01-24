# ./apps/bat.nix

{ ... }:

{
  catppuccin.bat.enable = true;
  catppuccin.bat.flavor = "mocha";

  programs.bat = {
    enable = true;
  };
}


