# flake.nix
{
  description = "jbgreer nix-darwin system flake";

  nixConfig = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    substituters = [ "https://cache.nixos.org" ];
    extra-substituters = [ "https://nix-community.cachix.org" ];
    extra-trusted-users = [ "jbgreer" ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    catppuccin.url = "github:catppuccin/nix";
  };

  outputs = inputs @ {
    self,
    nix-darwin,
    nixpkgs,
    home-manager,
    nixvim,
    catppuccin,
    ...
  }: let
    lib = inputs.nixpkgs.lib;
  in {
    # $ darwin-rebuild switch --flake .
    darwinConfigurations."Ishiguro" = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = {
        inherit self;
        inherit nixpkgs;
        inherit nixvim;
        inherit inputs;
      };

      modules = [
        ./darwin.nix

        home-manager.darwinModules.home-manager {
          home-manager.backupFileExtension = "backup";
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = {
              inherit inputs;
              inherit nixpkgs;
          };
          home-manager.users.jbgreer = {
            imports = [
              ./jbgreer.nix
              catppuccin.homeManagerModules.catppuccin
              nixvim.homeManagerModules.nixvim
            ];
          };
        }
      ];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations."Ishiguro".pkgs;
  };
}
