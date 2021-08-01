{ mkDerivation, fetchhgPrivate, aeson, async, base, bytestring, containers, lens
, network-uri, regex-applicative, spiderment, lib, text, time
}:
mkDerivation {
  pname = "comic-scrapper";
  version = "0.1";
  src = fetchhgPrivate {
    url = "ssh://hg@bitbucket.org/Shimuuar/comic-scraper";
    rev = "9f97e87be657d0cf1d2a47f0d32639e229bfb504";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring containers lens network-uri
    regex-applicative spiderment text time
  ];
  description = "Scrapper for comicss";
  license = lib.licenses.bsd3;
}
