# ./apps/zellij.nix

{
  ...
}:

{
  programs.zellij = {
    enable = true;
    catppuccin = {
      enable = true;
      flavor = "mocha";
    };
  };
}
