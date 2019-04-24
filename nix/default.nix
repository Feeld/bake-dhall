{ pkgsPath ? null
, compiler ? "ghc864"
, fullyStatic ? true
}:
let
  # We pin the nixpkgs version here to ensure build reproducibility
  pinnedNixpkgs =
    import ./fetch-nixpkgs.nix
      { # Latest HEAD of the master branch as of 2019-03-31
        rev = "c637aeff19b57b620a27029188060c76c6022dc";
        # This sha256 can be obtained with:
        # `$ nix-prefetch-url https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz`
        sha256 = "189fqxp7v72nak6y8qnqw8vzmv51wmbz4xj081kd2jf7d4icjjvz";
        # This one with:
        # `$ nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz`
        outputSha256 = "08vij68df530xfnid700my1f9pj0k8iyb1zzplv2yidls204k70b";
      };

  # Use pkgsPath if provided, else the pinned checkout
  realPkgsPath =
    if pkgsPath == null then pinnedNixpkgs else pkgsPath;

  # Musl is not supported on osx so don't attempt to link fully statically there
  linkStatically = fullyStatic && !pkgsGlibc.stdenv.isDarwin;

  # This overlay extends the nixpkgs' package set with our stuff
  overlay = self: super:
    let pkgs = if linkStatically then self.pkgsMusl else self;
    in
    {
      inherit pkgsGlibc;

      # This is an overlayed haskell package set. See
      # 'nix/overlays/ghcPackages.nix' for the overrides
      myHaskellPackages = pkgs.haskell.packages."${compiler}".override (_ : {
        overrides = import ./ghcPackages.nix pkgs;
        });

      # This is our production executable.
      # It's wrapped with justReallyStaticExecutables so it is
      # linked statically against Haskell and system libraries instead of dynamically
      # (nixpkgs' default). This produces an executable
      # which can run on any x86_64 linux system with a kernel >= 2.6.39
      bake-dhall =
        let drv =
          if linkStatically
            then
              pkgs.haskell.lib.justReallyStaticExecutables self.myHaskellPackages "bake-dhall"
            else
              self.haskell.lib.justStaticExecutables self.myHaskellPackages.bake-dhall;
        in self.haskell.lib.generateOptparseApplicativeCompletion "bake-dhall" drv;
    };

  pkgsGlibc = import realPkgsPath
    {
      overlays =
        [ overlay
          (import ./utilities.nix)
          (import ./muslPkgs.nix)
        ];
      config.allowUnfree = true;
    };
in pkgsGlibc
