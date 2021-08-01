{ mkDerivation, fetchhgPrivate, aeson, ansi-wl-pprint, async, base, bytestring
, containers, directory, http-client, lens, mtl, network-uri
, regex-applicative, lib, stm, tagsoup, text, time, transformers
, wreq
}:
mkDerivation {
  pname   = "spiderment";
  version = "0.1";
  src = fetchhgPrivate {
    url = "ssh://hg@bitbucket.org/Shimuuar/spiderment";
    rev = "58c02373f82f9742e612c419fac0935e5fb1cbad";
  };
  libraryHaskellDepends = [
    aeson ansi-wl-pprint async base bytestring containers directory
    http-client lens mtl network-uri regex-applicative stm tagsoup text
    time transformers wreq
  ];
  description = "Kind of web scrapper";
  license = lib.licenses.bsd3;
}
