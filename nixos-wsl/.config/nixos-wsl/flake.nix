{
  description = "jbgreer WSL NixOS flake";

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
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL/main";
    };

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

  outputs = {
    self,
    nixpkgs,
    nixos-wsl,
    home-manager,
    nixvim,
    catppuccin,
    ...
  }: {
    nixosConfigurations = {
      vonnegut = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          nixos-wsl.nixosModules.default {
            system.stateVersion = "24.05";
            wsl.enable = true;
          }

          ./hosts/vonnegut.nix

          home-manager.nixosModules.home-manager {
            home-manager.backupFileExtension = "backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jbgreer = {
              imports = [
                ./users/jbgreer.nix
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
