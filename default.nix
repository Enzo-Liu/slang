let
pkgs = import <nixpkgs> {};
#hls = pkgs.haskellPackages.haskell-language-server.override { supportedGhcVersions = [ "8102" ]; };
in
with pkgs;
stdenv.mkDerivation {
  name = "mllib";
  nativeBuildInputs = [
    #hls
    pkgs.haskellPackages.haskell-language-server
    llvm_9
  ];
  buildInputs = [
  ];
}

