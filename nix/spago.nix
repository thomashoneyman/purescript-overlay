{
  callPackage,
  purs,
}: rec {
  spago = spago-0_93_4;

  spago-0_93_4 = callPackage ./mkSpagoDerivation.nix {
    version = "0.93.4";
    purs = purs.purs-0_15_9;
    rev = "116aaa37d6616c0a4721da268bdf9e6fb4219ae4";
  };
}
