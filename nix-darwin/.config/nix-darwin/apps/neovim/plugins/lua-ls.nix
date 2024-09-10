{
  programs.nixvim.plugins = {

    lsp.servers.lua-ls = {
      enable = true;
      settings.telemetry.enable = false;
    };
  };
}
