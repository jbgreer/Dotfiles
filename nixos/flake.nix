{
  description = "jbgreer nixos flake";

  nixConfig = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];

    substituters = [ "https://cache.nixos.org" ];

    extra-substituters = [ "https://nix-community.cachix.org" ];

    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland";

    nix-index-database = {
      url = "github:Mic92/nix-index-database";
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
      nixpkgs,
      nixos-hardware,
      home-manager,
      hyprland,
      nix-index-database,
      nixvim,
      catppuccin,
      ...
    }: let
      system = "x86_64-linux";
    in {

      nixosConfigurations = {
        saint-exupery = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit system;
            inherit inputs;
          };
          modules = [
            nixos-hardware.nixosModules.framework-13-7040-amd
            ./hosts/saint-exupery/configuration.nix

            home-manager.nixosModules.home-manager {
              home-manager.backupFileExtension = "backup";
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.jbgreer = {
                imports = [
                  ./users/jbgreer.nix
                  nix-index-database.hmModules.nix-index
                  catppuccin.homeManagerModules.catppuccin
                  nixvim.homeManagerModules.nixvim
                ];
              };
            }
          ];
        };
      };
    };
}
