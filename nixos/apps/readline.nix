# ./apps/readline.nix

{
  config,
  ...
}:

{
  programs.readline = {
    enable = true;
  };
}
