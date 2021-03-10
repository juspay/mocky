{ pkgs ? import <nixpkgs> {}
, name ? "mocky"
, tag ? "0.0.0.1"
, production ? false # not currently used
# , drv
}:
with pkgs;
let
  nixFromDockerHub = dockerTools.pullImage {
    imageName = "busybox";
    imageDigest = "sha256:c6b45a95f932202dbb27c31333c4789f45184a744060f6e569cc9d2bf1b9ad6f";
    sha256 = "00v7r0562fhf2fvqx4kii4clyrnhcfvn8kbsdy7kfpyii44n36qp";
    finalImageTag = "stable";
  };

  mocky-static = haskellPackages.callPackage (import ./.) {};

  Entrypoint = [ "${pkgs.tini}/bin/tini" "--" ];
  Cmd = [ "${mocky-static}/bin/mocky" ];
in
dockerTools.buildImage {
  inherit name tag;
  created = "now";
  fromImage = nixFromDockerHub;
  contents = [
    # extra image binaries like:
    # bash
  ];
  config = {
    inherit Entrypoint;
    inherit Cmd;
  };
}
