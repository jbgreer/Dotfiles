# apps/emacs.nix

{ pkgs, ... }:

{
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
    # additionalPath = "";
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
  ];

  # emacs config files for now
  home.file.".config/emacs" = {
    source = ../config/emacs;
    recursive = true;
  };
}
