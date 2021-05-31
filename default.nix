{ mkDerivation, aeson, base, base64, bsb-http-chunked, bytestring
, case-insensitive, containers, either, generic-lens, http-types
, lib, text, wai, wai-extra, warp, warp-tls
}:
mkDerivation {
  pname = "mocky";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base64 bsb-http-chunked bytestring case-insensitive
    containers either generic-lens http-types text wai wai-extra warp
    warp-tls
  ];
  homepage = "https://github.com/githubuser/mocky#readme";
  license = lib.licenses.bsd3;
}
