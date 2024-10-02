{
  programs.nixvim = {

    keymaps = [
      {
        action = "<NOP>";
        key = "<Space>";
        mode = "n";
        options = {
          desc = "No-op";
          unique = true;
        };
      }
      {
        action = ":noh<CR>";
        key = "<Esc>";
        mode = "n";
        options = {
          desc = "Clear search results";
          unique = true;
        };
      }
      {
        action = "y$";
        key = "Y";
        mode = "n";
        options = {
          desc = "Fix y behavior";
          #unique = true;
        };
      }
      {
        action = ":b#<CR>";
        key = "<C-c>";
        mode = "n";
        options = {
          desc = "bounce between two most recent files";
          unique = true;
        };
      }
      {
        action = ":close<CR>";
        key = "<C-x>";
        mode = "n";
        options = {
          desc = "Close";
          unique = true;
        };
      }
      {
        action = ":w<CR>";
        key = "<leader>s";
        mode = "n";
        options = {
          desc = "Save";
          unique = true;
        };
      }
      {
        action = ":w<CR>";
        key = "<C-s>";
        mode = "n";
        options = {
          desc = "Save";
          unique = true;
        };
      }
      {
        action = "<C-w>h";
        key = "<leader>h";
        mode = "n";
        options = {
          desc = "Navigate to left window";
          unique = true;
        };
      }
      {
        action = "<C-w>l";
        key = "<leader>l";
        mode = "n";
        options = {
          desc = "Navigate to right window";
          unique = true;
        };
      }
      {
        action = ":resize -2<CR>";
        key = "<C-Up>";
        mode = "n";
        options = {
          desc = "Resize up 2 lines";
          unique = true;
        };
      }
      {
        action = ":resize +2<CR>";
        key = "<C-Down>";
        mode = "n";
        options = {
          desc = "Resize down 2 lines";
          unique = true;
        };
      }
      {
        action = ":vertical resize -2<CR>";
        key = "<C-Left>";
        mode = "n";
        options = {
          desc = "Resize left 2 lines";
          unique = true;
        };
      }
      {
        action = ":vertical resize +2<CR>";
        key = "<C-Right>";
        mode = "n";
        options = {
          desc = "Resize right 2 lines";
          unique = true;
        };
      }
      {
        action = ":wincmd k<CR>";
        key = "<C-k>";
        mode = "n";
        options = {
          desc = "Navigate to lower window";
          unique = true;
        };
      }
      {
        action = ":wincmd j<CR>";
        key = "<C-j>";
        mode = "n";
        options = {
          desc = "Navigate to upper window";
          unique = true;
        };
      }
      {
        action = ":wincmd h<CR>";
        key = "<C-h>";
        mode = "n";
        options = {
          desc = "Navigate to left window";
          unique = true;
        };
      }
      {
        action = ":wincmd l<CR>";
        key = "<C-l>";
        mode = "n";
        options = {
          desc = "Navigate to right window";
          #unique = true;
        };
      }
      {
        action = ":move -2<CR>";
        key = "<M-j>";
        mode = "n";
        options = {
          desc = "Move current line up 2";
          unique = true;
        };
      }
      {
        action = ":move +2<CR>";
        key = "<M-k>";
        mode = "n";
        options = {
          desc = "Move current line down 2";
          unique = true;
        };
      }
      {
        action = ">gv";
        key = ">";
        mode = "v";
        options = {
          desc = "Indent right";
          unique = true;
        };
      }
      {
        action = ">gv";
        key = "<TAB>";
        mode = "v";
        options = {
          desc = "Indent right";
          unique = true;
        };
      }
      {
        action = "<gv";
        key = "<";
        mode = "v";
        options = {
          desc = "Indent left";
          unique = true;
        };
      }
      {
        action = "<gv";
        key = "<S-TAB>";
        mode = "v";
        options = {
          desc = "Indent left";
          unique = true;
        };
      }
      {
        action = ":m '<-2<CR>gv=gv";
        key = "K";
        mode = "v";
        options = {
          desc = "Move selected block up 2 lines";
          unique = true;
        };
      }
      {
        action = ":m '<+1<CR>gv=gv";
        key = "J";
        mode = "v";
        options = {
          desc = "Move selected block down 1 line";
          unique = true;
        };
      }

    ];
  };
}

