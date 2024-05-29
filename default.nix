{ pkgs ? import <nixpkgs> { } }:
pkgs.python3Packages.buildPythonPackage rec {
  name = "tt07";
  src = ./.;
  nativeBuildInputs = with pkgs; [ yosys verilog ];
}
