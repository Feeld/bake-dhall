self: super:
if !super.stdenv.hostPlatform.isMusl then {} else
{
  # Build tools which we don't need to build with musl
  inherit (self.pkgsGlibc)
    python pythonPackages
    python2 python2Packages
    python3 python3Packages
    cabal2nix
    ;
}
