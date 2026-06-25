{
  description = "Entorno de desarrollo para Kat (Haskell + LLVM 9)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.stack
          pkgs.haskell.compiler.ghc884
          pkgs.llvmPackages_9.llvm
          pkgs.zlib
          pkgs.ncurses
          pkgs.libffi
        ];

        shellHook = ''
          export LLVM_CONFIG=${pkgs.llvmPackages_9.llvm}/bin/llvm-config
        '';
      };
    };
}
