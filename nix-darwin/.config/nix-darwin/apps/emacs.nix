# apps/emacs.nix

{ pkgs, ... }:

let
  emacs-overlay = import (fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    sha256 = "1jppksrfvbk5ypiqdz4cddxdl8z6zyzdb2srq8fcffr327ld5jj2";
  });
  my-emacs = pkgs.emacs29.override {
    withNativeCompilation = true;
    withSQLite3 = true;
    withTreeSitter = true;
    withWebP = true;
  };
  my-emacs-with-packages = (pkgs.emacsPackagesFor my-emacs).emacsWithPackages (epkgs: with epkgs; [
    #company
    #counsel
    #evil
    #evil-collection
    #evil-surround
    #evil-textobj-anyblock
    #geiser-racket
    #magit
    #multi-vterm
    #paredit
    #pdf-tools
    #projectile
    #quack
    #racket-mode
    #rainbow-delimiters
    #scribble-mode
    #smartparens
    #transient
    #treesit-grammars.with-all-grammars
    vterm
  ]);
in
{
  programs.emacs = {
    enable = true;
    package = my-emacs-with-packages;
  };

  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
    (aspellWithDicts (d: [d.en]))
    ghostscript
    tetex
    poppler
  ];
}
