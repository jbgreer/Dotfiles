{ config, pkgs, ... }:

{
  # Configure Kitty
  programs.kitty = {
    enable = true;
    package = pkgs.kitty;
    font = {
      name = "JetBrainsMono Nerd Font";
      size = 16;
    };
    settings = {
      background_opacity = "0.85";
      confirm_os_window_close = 0;
      enable_audio_bell = false;
      scrollback_lines = 2000;
      wheel_scroll_min_lines = 1;
      window_padding_width = 6;
    };
    #theme = "One Dark";
  };
}
