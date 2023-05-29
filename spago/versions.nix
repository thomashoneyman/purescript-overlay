{
  callPackage,
  compilers,
}: rec {
  spago = spago-0_93_4;
  spago-0_93_4 = callPackage ./0.93.4.nix {purs = compilers.purs-0_15_9;};
}
