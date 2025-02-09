# ./apps/fzf.nix

{ ... }:


{
  catppuccin.fzf.enable = true;
  catppuccin.fzf.flavor = "mocha";

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
}
