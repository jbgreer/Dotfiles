# ./apps/tmux.nix

{ config, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    catppuccin = {
      enable = true;
      flavor = "mocha";
    };
    clock24 = true;
    extraConfig = "
      # tmux.nix extraConfig

      set -g status-position top  # 

      # change vertical split key to |
      unbind %
      bind | split-window -h 

      # change hortizontal split key to -
      unbind '\"'
      bind - split-window -v

      # resize of panel
      bind -r j resize-pane -D 5
      bind -r k resize-pane -U 5
      bind -r l resize-pane -R 5
      bind -r h resize-pane -L 5
      
      bind -r m resize-pane -Z
      
      # start selecting text with v
      bind-key -T copy-mode-vi 'v' send -X begin-selection 
      # copy text with y
      bind-key -T copy-mode-vi 'y' send -X copy-selection 
      # don't exit copy mode when dragging with mouse
      unbind -T copy-mode-vi MouseDragEnd1Pane 

      #set -g @catppuccin_flavour 'mocha'
      set -g @catppuccin_window_left_separator \"\"
      set -g @catppuccin_window_right_separator \" \"
      set -g @catppuccin_window_middle_separator \" █\"
      set -g @catppuccin_window_number_position \"right\"
      set -g @catppuccin_window_default_fill \"number\"
      set -g @catppuccin_window_default_text \"#W\"
      set -g @catppuccin_window_current_fill \"number\"
      set -g @catppuccin_window_current_text \"#W\"
      set -g @catppuccin_status_modules_right \"directory user host session\"
      set -g @catppuccin_status_left_separator  \" \"
      set -g @catppuccin_status_right_separator \"\"
      set -g @catppuccin_status_fill \"icon\"
      set -g @catppuccin_status_connect_separator \"no\"
      set -g @catppuccin_directory_text \"#{pane_current_path}\"
      # tmux.nix extraConfig
    ";
    keyMode = "vi";
    mouse = true;
    plugins = with pkgs; [
      tmuxPlugins.vim-tmux-navigator
      tmuxPlugins.catppuccin
    ];
    prefix = "C-a";
    sensibleOnTop = false;
    shell = "\${pkgs}/bin/zsh";
    terminal = "screen-256color";
  };
}

