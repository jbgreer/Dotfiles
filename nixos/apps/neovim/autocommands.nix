{
  programs.nixvim.autoCmd = [
    {
      desc = "Vertically center document when entering insert mode";
      event = "InsertEnter";
      command = "norm zz";
    }

    {
      desc = "Remove trailing whitespace on save";
      event = "BufWrite";
      command = "%s/\\s\\+$//e";
    }

    {
      desc = "open help in vertical split";
      event = "FileType";
      pattern = "help";
      command = "wincmd L";
    }

    {
      desc = "set indent to 2 spaces for nix files";
      event = "FileType";
      pattern = "nix";
      command = "setlocal tabstop=2 shiftwidth=2";
    }

    {
      desc = "enable spellcheck for tex / latex / markdown";
      event = "FileType";
      pattern = [
        "tex"
        "latex"
        "markdown"
      ];
      command = "setlocal spell spelllang=en";
    }
  ];
}
