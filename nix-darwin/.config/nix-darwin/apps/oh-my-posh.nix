# ./apps/oh-my-posh.nix

{ ... }:

{
  programs.oh-my-posh = {
    enable = true;
    useTheme = "catppuccin_mocha";
    enableZshIntegration = true;
  };
}

