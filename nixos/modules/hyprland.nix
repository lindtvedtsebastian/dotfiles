{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    dunst
    libnotify
    swww
    grim
    slurp
    wl-clipboard
    rofi-wayland
    hypridle
    hyprlock
    hyprls
  ];

  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  security.pam.services.hyprlock = { };

  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
  };
}
