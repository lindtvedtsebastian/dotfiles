{
  description = "ags";

  inputs =
    {
      nixpkgs.url = "github:nixos/nixpkgs/master";
    };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          typescript
          nodePackages.prettier
          nodePackages.eslint
          nodePackages.typescript-language-server
        ];
      };
    };
}
