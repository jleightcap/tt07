let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  inherit (pkgs) mkShell callPackage;
in mkShell {
  packages = with pkgs.haskellPackages; [
    haskell-language-server
    hlint
    ormolu
  ];
  inputsFrom = [ (callPackage ./. { }) ];
}
