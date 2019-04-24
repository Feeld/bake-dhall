# This overlay extends the nixpkgs' haskell package set with our stuff
pkgs: self: super:
with pkgs.haskell.lib;
let
  inherit (pkgs.lib)
    cleanSource
    foldl
    flip;
  dhall-src = pkgs.fetchFromGitHub
    { owner = "dhall-lang";
      repo = "dhall-haskell";
      rev = "58ae94df0ea28f4688edaf72c59ee19d4f3f284a";
      sha256 = "1fdrpdkv23l8052kwjzh2kib72dhmbj3wxraw5vvj3rw8h0k6ypw";
    };

in
{
  bake-dhall =
    let
      drv = self.callCabal2nix "bake-dhall" (cleanSource ../.) {};
    in foldl (acc: f: f acc) drv
      [ (if (pkgs.stdenv.targetPlatform.isMusl || pkgs.stdenv.isDarwin) then (x: x) else linkWithGold)
        (flip appendConfigureFlag "--ghc-option=-Werror")
      ];

  # dontCheck because tests require network access
  dhall =
    let
      drv = self.callCabal2nix "dhall" "${dhall-src}/dhall" {};
    in dontCheck drv;
  dhall-json =
    let
      drv = self.callCabal2nix "dhall-json" "${dhall-src}/dhall-json" {};
    in drv;


  # This hack provides a configured hoogle instance inside nix-shell
  ghcWithPackages = super.ghcWithHoogle;
}
