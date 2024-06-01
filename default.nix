let
  pkgs = import (
  fetchTarball
  {
    name = "nixpkgs-unstable-2022-07-19";
    url = "https://github.com/nixos/nixpkgs/archive/2df37941652c28e0858b9a9520ce5763c43c2ec1.tar.gz";
    sha256 = "sha256:12d5w1bvhjlxrvdhsc44gq1lv5s3z1lv18s39q1702hwmp2bz071";
  }
  ) {};
in pkgs.stdenv.mkDerivation {
  name = "tt07";

  nativeBuildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages
      (p: with p; [ clash-lib clash-ghc clash-prelude ]))
    # TODO: nativeCheckInputs more idiomatic for these,
    # but only nativeBuildInputs available in shell.nix $PATH via `inputsFrom`
    (python3.withPackages (p: with p; [ pytest cocotb pylint ]))
    verilator
    verilog
  ];

  src = ./.;
}
