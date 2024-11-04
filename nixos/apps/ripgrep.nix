# ./apps/ripgrep.nix

{
  config,
  ...
}:

{
  programs.ripgrep = {
    enable = true;
  };
}
