{
  programs.nixvim.plugins = {

    lsp.servers.rust-analyzer = {
      enable = true;
      installRustc = false;
      installCargo = false;
    };
  };
}
