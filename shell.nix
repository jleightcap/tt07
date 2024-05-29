with import <nixpkgs> { };
mkShell {
  packages = [ xdot gtkwave verible ];
  inputsFrom = [ (callPackage ./. { }) ];
}
