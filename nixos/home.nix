{ pkgs, ... }:

{
  home.username = "sl";
  home.homeDirectory = "/home/sl";
  home.stateVersion = "23.11";

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 16;
  };

  gtk = {
    enable = true;
  };

}
