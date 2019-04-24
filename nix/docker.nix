{ tag ? "latest"
, name ? "fld-production/bake-dhall"
, port ? 8080
, pkgs ? import ./. {}
}:
let
  suExec = pkgs.pkgsMusl.su-exec.overrideAttrs(o:{CFLAGS="--static";});
in with pkgs;
dockerTools.buildImage {
  inherit tag name;
  created = "now";

  contents = [ bake-dhall suExec ];

  config = {
    Cmd = [
      "su-exec"
      "30000:30000" # uid/gid to run the service as
      "bake-dhall"
      "serve"
      "+RTS"   # the following options are for the RTS...
      "-N"     # use all capabilities (processors)
      "-qn1"   # Only use one capability for parallel GC
    ];
    Env = [
      "PORT=${toString port}"
      "TMPDIR=/"
    ];
    ExposedPorts = {
      "${toString port}/tcp" = {};
    };
  };
}
