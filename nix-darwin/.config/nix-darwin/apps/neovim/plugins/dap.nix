{
  programs.nixvim.plugins.dap = {
    enable = true;

    adapters = {
      executables = {
        mix_task = {
          args = null;
          command = "elixir-debugger";
          #options = {
          #  cwd =  null;
          #  detached =  true;
          #  env = null;
          #};
        };
      };

      #servers = {
        #elixir = {
          #executable = {
            #args = null;
            #command = "";
            #cwd = "";
            #detached = true;
          #};
          #options.maxRetries = 14;
        #};
      #};
    };

    configurations = {
      elixir = [
        {
          type = "mix_task";
          name = "mix (Default tasks)";
          request = "launch";
          startApps = true;
        }
        {
          type = "mix_task";
          name = "mix test";
          request = "launch";
          taskArgs = [ "--trace" ];
          startApps = false;
          #projectDir = "${workspaceRoot}";
          requireFiles = [
            "test/**/test_helper.exs"
            "test/**/*_test.exs"
          ];
        }
      ];
    };
  };

  programs.nixvim.plugins.dap.extensions.dap-ui.enable = true;
}
