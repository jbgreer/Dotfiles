{
  description = "jbgreer Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager }:
  let
    configuration = { pkgs, ... }: {
      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment.systemPackages =
        [ 
          pkgs.git    # distributed version control system
          pkgs.vim    # the most popular clone of the vi editor
          pkgs.wget   # tool for retrieving files via FTP, HTTP, HTTPS
        ];

      # Auto upgrade nix package and the daemon service.
      services.nix-daemon.enable = true;

      # Necessary for using flakes on this system.
      nix.settings.experimental-features = "nix-command flakes";

      # Create /etc/zshrc that loads the nix-darwin environment.
      programs.zsh.enable = true;  # default shell on catalina

      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # $ darwin-rebuild changelog  # WARNING DO NOT CHANGE
      system.stateVersion = 4;

      # Platform should be either "x86_64-darwin" or "aarch64-darwin"
      nixpkgs.hostPlatform = "aarch64-darwin";

      users.users.jbgreer = {
        name = "jbgreer";
        home = "/Users/jbgreer";
      };
      home-manager.backupFileExtension = "backup";
      nix.configureBuildUsers = true;
      nix.useDaemon = true;

      # MacOS / Darwin settings
      system.defaults = {
        dock.autohide = true;
        dock.mru-spaces = false;
        finder.AppleShowAllExtensions = true;
        finder.FXPreferredViewStyle = "clmv";
        loginwindow.LoginwindowText = "Ishiguro";
        screencapture.location = "~/Pictures/screenshots/";
        screensaver.askForPasswordDelay = 10;
      };

      # homebrew
      homebrew.enable = true;
      homebrew.casks = [
        "1password-cli"
        "alfred"
        "google-chrome"
        "iterm2"
        "rectangle"
        "spotify"
      ];
      homebrew.brews = [
        "exercism"
      ];
    };
  in
  {
    # $ darwin-rebuild build --flake .#
    darwinConfigurations."Ishiguro" = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";

      modules = [ 
        configuration
        home-manager.darwinModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.jbgreer = import ./home.nix;
        }
      ];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations."Ishiguro".pkgs;
  };
}
