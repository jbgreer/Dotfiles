# ./apps/bat.nix

{
  ...
}:

{
  programs.bat = {
    enable = true;
    catppuccin = {
      enable = true;
      flavor = "mocha";
    };
  };
}


