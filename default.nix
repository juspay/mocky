{ mkDerivation, aeson, base, bsb-http-chunked, bytestring
, case-insensitive, containers, generic-lens, http-types, lib, text
, wai, wai-extra, warp
}:
mkDerivation {
  pname = "mocky";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bsb-http-chunked bytestring case-insensitive containers
    generic-lens http-types text wai wai-extra warp
  ];
  homepage = "https://github.com/githubuser/mocky#readme";
  license = lib.licenses.bsd3;
}
