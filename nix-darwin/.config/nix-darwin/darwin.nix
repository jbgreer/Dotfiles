# darwin.nix
{
  self,
  pkgs,
  ...
}: {
  # List of system packages. To search by name, run: $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
      git              # distributed version control system
      hugo             # fast and modern static website engine
      sass             # Tools and Ruby libraries for the CSS3 extension langs SASS and SCSS
      wget             # tool for retrieving files via FTP, HTTP, HTTPS
    ];

  fonts = {
    packages = with pkgs; [
      fira-code
      fira-code-symbols
      font-awesome
      (nerdfonts.override { fonts = [ "FiraCode" ]; })
    ];
  };

  # homebrew
  homebrew.enable = true;
  homebrew.casks = [
    # "1password" separately installed
    "spotify"
    "karabiner-elements"
  ];
#homebrew.brews = [
#  "exercism"
#];

# Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  nix.configureBuildUsers = true;
  nix.useDaemon = true;

# Platform should be either "x86_64-darwin" or "aarch64-darwin"
  nixpkgs.hostPlatform = "aarch64-darwin";
# needed for Google Chrome
  nixpkgs.config.allowUnfree = true;

# Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina

# Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

# Set Git commit hash for darwin-version.
  system.configurationRevision = self.rev or self.dirtyRev or null;

# MacOS / Darwin settings
  system.defaults = {
    dock = {
      autohide = true;
      orientation = "right";
      mru-spaces = false;
      persistent-apps = [
        "/Applications/Google Chrome.app"
        "/Applications/Spotify.app"
        "/Applications/Visual Studio Code.app"
        "/Applications/Firefox.app"
        "/Applications/Mozilla VPN.app"
      ];
      show-recents = false;
      tilesize = 48;          # 64 max
      # 1: Disabled
      # 2: Mission Control
      # 3: Application Windows
      # 4: Desktop
      # 5: Start Screen Saver
      # 6: Disable Screen Saver
      # 7: Dashboard
      # 10: Put Display to Sleep
      # 11: Launchpad
      # 12: Notification Center
      # 13: Lock Screen
      # 14: Quick Note
      wvous-bl-corner = 1;
      wvous-br-corner = 1;
      wvous-tl-corner = 1;
      wvous-tr-corner = 1;
    };
    finder = {
      AppleShowAllExtensions = true;
      AppleShowAllFiles = true;
      FXPreferredViewStyle = "Nisv";
      ShowPathbar = true;
      ShowStatusBar = true;
    };
    loginwindow.LoginwindowText = "Ishiguro";
    menuExtraClock = {
      Show24Hour = true;
      ShowDate = 1;           # 0 = when space alows, 1 = always, 2 = never
      ShowDayOfMonth = true;
      ShowDayOfWeek = true;
      ShowSeconds = true;
    };
    NSGlobalDomain = {
      AppleShowAllExtensions = true;
      AppleShowAllFiles = true;
      AppleShowScrollBars = "Always";
      NSTableViewDefaultSizeMode = 2;
    };
    screencapture.location = "~/Pictures/screenshots/";
    screensaver.askForPasswordDelay = 10;
  };

  # $ darwin-rebuild changelog  # WARNING DO NOT CHANGE
  system.stateVersion = 4;

  users.users.jbgreer = {
    name = "jbgreer";
    home = "/Users/jbgreer";
  };
}
