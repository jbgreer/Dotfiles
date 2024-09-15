# neovim/options.nix
{
  programs.nixvim = {

    clipboard = {

      # Always system clipboard
      register = "unnamedplus";

      # 2024-09-14 currently nixvim doesn't explicitly support many of these
      # Wayland (if WAYLAND_DISPLAY set)
      #providers.wl-copy.enable = true;
      #providers.wl-paste.enable = true;
      # MacOS
      #providers.pbcopy.enable = true;
      #providers.pbpaste.enable = true;
      # tmux
      #providers.tmux.enable = true;
    };
  };
}

