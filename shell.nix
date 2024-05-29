# the full tinytapeout build process, in full, would be quite painful to orchestrate at the moment via nix
# tracking development and local build+test flow with shell.nix
with import <nixpkgs> { };
mkShell {
  packages = [
    # dev
    xdot
    gtkwave
    verible
    # test
    yosys
    verilog
    (python3.withPackages (p: [ p.pytest p.cocotb ]))
  ];
}
