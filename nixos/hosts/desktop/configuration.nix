{ inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/general.nix
    ../../modules/amd.nix
    ../../modules/hyprland.nix
    inputs.home-manager.nixosModules.default
  ];

  networking.hostName = "slp"; # Define your hostname.

  system.stateVersion = "23.11"; # Did you read the comment?
}
