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
  ];

  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
  };
  
}
