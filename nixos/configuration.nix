# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ pkgs, inputs, ... }:


{
  imports =
    [
      ./hardware-configuration.nix
      inputs.home-manager.nixosModules.default
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  boot.initrd.kernelModules = [ "amdgpu" ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.efiInstallAsRemovable = true;

  networking.hostName = "slp"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        rocmPackages.clr.icd
        amdvlk
      ];
    };
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
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

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    (emacs.override { withPgtk = true; })
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
    rocmPackages.rocminfo
    pywal
  ];

  systemd.tmpfiles.rules = [
    "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
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
      "sl" = import ./home.nix;
    };
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  programs._1password-gui = {
    enable = true;
    polkitPolicyOwners = [ "sl" ];
  };

  fonts.packages = with pkgs; [
    iosevka
    inter
    quicksand
    helvetica-neue-lt-std
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
      folders = {
        "st" = {
          path = "/home/sl/st";
        };
      };
    };
  };
  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "23.11"; # Did you read the comment?

}

