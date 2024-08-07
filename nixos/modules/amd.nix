{ pkgs, inputs, ... }:
{

  boot.initrd.kernelModules = [ "amdgpu" ];

  hardware = {
    graphics = {
      extraPackages = with pkgs; [
        rocmPackages.clr.icd
        amdvlk
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    rocmPackages.rocminfo
  ];

  systemd.tmpfiles.rules = [
    "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
  ];
}
