{ inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/general.nix
    inputs.home-manager.nixosModules.default
  ];

  networking.hostName = "sll"; # Define your hostname.
}
