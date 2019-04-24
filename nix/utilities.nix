self: super: {

  lib = super.lib //
    {
      # This function filters out stuff we don't want to consider part of the source
      # when building with nix. Any change in one of these files would cause a
      # re-build otherwise
      cleanSource =
        let
          fldSourceFilter = name: type: let baseName = baseNameOf (toString name); in ! (
            # Filter out Subversion and CVS directories.
            (type == "directory" &&
              ( baseName == ".git" ||
                baseName == ".circleci" ||
                baseName == ".nix-cache" ||
                baseName == ".cache" ||
                baseName == "deploy" ||
                baseName == "kubernetes" ||
                baseName == "secrets" ||
                baseName == "nix" ||
                baseName == "dist" ||
                baseName == "dist-newstyle"
              )
            ) ||
            # Filter out editor backup / swap files.
            self.lib.hasSuffix "~" baseName ||
            builtins.match "^\\.sw[a-z]$" baseName != null ||
            builtins.match "^\\..*\\.sw[a-z]$" baseName != null ||

            # filter out .ghc.environment
            builtins.match "^\\.ghc.environment.*" baseName != null ||

            # Filter out nix-build result symlinks
            (type == "symlink" && self.lib.hasPrefix "result" baseName) ||

            # Filter other random crap we have lying around for development
            # which we don't need to properly build
            (baseName == "develop.sh") ||
            (baseName == "Setup") ||
            (baseName == "Setup.o") ||
            (baseName == "Setup.hi") ||
            (baseName == ".bash_history") ||
            (baseName == "README.md")
          );
        in builtins.filterSource fldSourceFilter;

    };

  haskell = super.haskell // {
    lib = super.haskell.lib // {
      justReallyStaticExecutables = pkgSet: name:
        let
          drv = pkgSet.${name};
          inherit (self.haskell.lib)
            appendConfigureFlags justStaticExecutables
            disableLibraryProfiling overrideCabal dontHaddock;
          # This version of Cabal supports --enable-executable-static.
          Cabal_head =
            let
              src =
                self.fetchFromGitHub {
                  owner = "haskell";
                  repo = "cabal";
                  rev = "7b8e6e5aa7d851d4caa132140673b17fe85970e3";
                  sha256 = "0p8mk7ry31ykk9q68mvif6yva36cyamfbhqffsalrzxkqbv9j20m";
                };
              drv = (pkgSet.callCabal2nix "Cabal" "${src}/Cabal" {}).overrideDerivation (old: {
                # Fixes the parsing of pkgId which now contains more tha one space
                # between "id:" and the package id
                installPhase = ''
                  ./Setup copy
                  local packageConfDir="$out/lib/${pkgSet.ghc.name}/package.conf.d"
                  local packageConfFile="$packageConfDir/${old.pname}-${old.version}.conf"
                  mkdir -p "$packageConfDir"
                  ./Setup register --gen-pkg-config=$packageConfFile
                  if [ -d "$packageConfFile" ]; then
                    mv "$packageConfFile/"* "$packageConfDir"
                    rmdir "$packageConfFile"
                  fi
                  find $packageConfDir
                  for packageConfFile in "$packageConfDir/"*; do
                    local pkgId=$( ${self.gnused}/bin/sed -n -e 's|^id: *||p' $packageConfFile )
                    mv $packageConfFile $packageConfDir/$pkgId.conf
                  done

                  # delete confdir if there are no libraries
                  find $packageConfDir -maxdepth 0 -empty -delete;
                  '';
              });

            in disableLibraryProfiling (dontHaddock drv);
          enableStatic = drv: drv.overrideAttrs (old: {
            dontDisableStatic = true;
            configureFlags = (old.configureFlags or []) ++ [
              "--enable-static"
              ];
            });
        in overrideCabal (justStaticExecutables drv) (old: {
          enableSharedLibraries = true;
          enableSharedExecutables = false;
          setupHaskellDepends = (old.setupHaskellDepends or []) ++ [ Cabal_head ];
          passthru = (old.passthru or {}) // { inherit Cabal_head; };
          configureFlags = (old.configureFlags or []) ++ [
            "--enable-executable-static"
            # Pass lib location of common libraries with static .a library
            # available
            "--extra-lib-dirs=${self.gmp6.override { withStatic = true;}}/lib"
            "--extra-lib-dirs=${self.zlib.static}/lib"
            "--extra-lib-dirs=${(self.bzip2.override { linkStatic=true;}).out}/lib"
            "--extra-lib-dirs=${self.ncurses.override { enableStatic=true;}}/lib"
            "--extra-lib-dirs=${(enableStatic self.xz).out}/lib"
            "--extra-lib-dirs=${enableStatic self.libffi}/lib"
          ];
      });
    };
  };
}
