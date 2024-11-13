{
  description = "nixos config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    quickshell =
      {
        url = "github:quickshell-mirror/quickshell";
        inputs.nixpkgs.follows = "nixpkgs";
      };

    nix-qml-support = {
      url = "git+https://git.outfoxxed.me/outfoxxed/nix-qml-support";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, emacs-overlay, quickshell, nix-qml-support, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;

        config = {
          allowUnfree = true;
        };
        overlays = [
          emacs-overlay.overlays.default
        ];
      };
    in
    {
      nixosConfigurations = {
        desktop = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; inherit pkgs; inherit system; };
          modules = [
            ./nixos/hosts/desktop/configuration.nix
            inputs.home-manager.nixosModules.default
          ];
        };
        laptop = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [
            ./nixos/hosts/laptop/configuration.nix
            inputs.home-manager.nixosModules.default
          ];
        };
      };
    };
}
