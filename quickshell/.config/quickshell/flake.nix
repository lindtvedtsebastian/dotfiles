{
  description = "quickshell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    quickshell =
      {
        url = "github:quickshell-mirror/quickshell";
        inputs.nixpkgs.follows = "nixpkgs";
      };

  };

  outputs = { self, nixpkgs, quickshell }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default =
        pkgs.mkShell {
          buildInputs = [
            pkgs.kdePackages.qtdeclarative
            quickshell.packages.${system}.default
          ];
          QML2_IMPORT_PATH = "${pkgs.kdePackages.qtdeclarative}/lib/qt-6/qml:${quickshell.packages.${system}.default}/lib/qt-6/qml";
        };
    };
}
