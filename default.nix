{ pkgs }:
let

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
