{ pkgs, inputs, ... }:
{

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.efiInstallAsRemovable = true;

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  hardware.graphics.enable = true;

  networking.networkmanager.enable = true;

  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sl = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };


  programs.zsh.enable = true;

  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    boost
    (emacs-git.override { withPgtk = true; })
    git
    firefox
    chromium
    vim
    stow
    wget
    waybar
    dunst
    libnotify
    swww
    kitty
    rofi-wayland
    liquidctl
    orca-slicer
    freecad
    inkscape
    blender-hip
    gimp
    via
    obs-studio
    direnv
    tree-sitter
    psmisc
    nil
    slack
    discord
    ripgrep
    gnupg
    nixpkgs-fmt
    neofetch
    grim
    slurp
    unzip
    wl-clipboard
    lshw
    clinfo
    spotify
    ffmpeg
    htop
    minikube
    kubernetes-helm
    kubectl
    calibre
    texlab
    texliveFull
    libreoffice
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

  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    users = {
      "sl" = import ../home.nix;
    };
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    polkitPolicyOwners = [ "sl" ];
  };

  fonts.packages = with pkgs; [
    iosevka
    inter
    quicksand
    helvetica-neue-lt-std
    dm-sans
  ];

  services.syncthing = {
    enable = true;
    user = "sl";
    group = "users";
    dataDir = "/home/sl/st";
    configDir = "/home/sl/.config/syncthing";
    overrideDevices = true; # overrides any devices added or deleted through the web interface
    overrideFolders = true; # overrides any folders added or deleted through the web interface
    settings = {
      devices = {
        "sll" = {id = "BMXYPPC-3RQLZGD-H5AXDXK-KXKIZQJ-TV2XGUG-OBWHW2M-KEIKMGQ-BPO4CA5"; };
        "slp" = { id = "N52DELU-B54YMUO-PA5EQZE-BQWRCT6-PI6OUI2-IMBNEFR-T6DWFNX-SNOUEAA"; };
      };
      folders = {
        "st" = {
          path = "/home/sl/st";
          devices = ["sll" "slp"];
        };
      };
    };
  };


}
