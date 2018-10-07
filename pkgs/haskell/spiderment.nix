{ mkDerivation, fetchhgPrivate, aeson, ansi-wl-pprint, async, base, bytestring
, containers, directory, http-client, lens, mtl, network-uri
, regex-applicative, stdenv, stm, tagsoup, text, time, transformers
, wreq
}:
mkDerivation {
  pname   = "spiderment";
  version = "0.1";
  src = fetchhgPrivate {
    url = "ssh://hg@bitbucket.org/Shimuuar/spiderment";
    rev = "3a008267dcabffd26110fd5b7e95f700ff3d0fc0";
  };
  libraryHaskellDepends = [
    aeson ansi-wl-pprint async base bytestring containers directory
    http-client lens mtl network-uri regex-applicative stm tagsoup text
    time transformers wreq
  ];
  description = "Kind of web scrapper";
  license = stdenv.lib.licenses.bsd3;
}
