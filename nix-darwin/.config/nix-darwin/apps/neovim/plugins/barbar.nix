{
  programs.nixvim.plugins.barbar = {
    enable = true;
    keymaps = {
      #silent = true;

      next.key = "<TAB>";
      previous.key = "<S-TAB>";
      close.key = "<C-w>";
    };
  };
}
