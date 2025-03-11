{ pkgs, system, inputs, ... }:
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

  environment.systemPackages = [
    (pkgs.emacs-git.override {
      withPgtk = true;
    })
    pkgs.git
    pkgs.firefox
    pkgs.chromium
    pkgs.vim
    pkgs.stow
    pkgs.wget
    pkgs.kitty
    pkgs.liquidctl
    pkgs.orca-slicer
    pkgs.freecad
    pkgs.inkscape
    pkgs.blender-hip
    pkgs.gimp
    pkgs.via
    pkgs.obs-studio
    pkgs.direnv
    pkgs.tree-sitter
    pkgs.psmisc
    pkgs.nil
    pkgs.slack
    pkgs.discord
    pkgs.ripgrep
    pkgs.gnupg
    pkgs.nixpkgs-fmt
    pkgs.neofetch
    pkgs.unzip
    pkgs.lshw
    pkgs.clinfo
    pkgs.spotify
    pkgs.ffmpeg
    pkgs.htop
    pkgs.minikube
    pkgs.kubernetes-helm
    pkgs.kubectl
    pkgs.calibre
    pkgs.texlab
    pkgs.texliveFull
    pkgs.libreoffice
    pkgs.nautilus
    pkgs.vlc
    pkgs.fontforge
    pkgs.godot_4
    inputs.quickshell.packages.${system}.default
  ];


  programs.steam = {
    enable = true;
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

  programs.virt-manager.enable = true;
  users.groups.libvirtd.members = [ "sl" ];
  virtualisation.libvirtd.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;
}
