# flake.nix
{
  description = "jbgreer nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self, 
    nix-darwin, 
    nixpkgs, 
    home-manager,
  }: let 
    lib = inputs.nixpkgs.lib;
  in {
    # $ darwin-rebuild switch --flake .
    darwinConfigurations."Ishiguro" = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = { inherit inputs; };

      modules = [ 
        ./darwin.nix
        home-manager.darwinModules.home-manager {
          home-manager.backupFileExtension = "backup";
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.jbgreer = import ./jbgreer.nix;
        }
      ];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations."Ishiguro".pkgs;
  };
}
