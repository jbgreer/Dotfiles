{
  programs.nixvim = {
    plugins.markdown-preview = {
      enable = true;

      settings = {
        autoClose = false;
        theme = "dark";
      };
    };

    keymaps = [
      {
        mode = "n";
        key = "<leader>m";
        action = ":MarkdownPreview<cr>";
        options.silent = true;
      }
    ];
  };
}
