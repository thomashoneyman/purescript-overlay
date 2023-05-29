{
  callPackage,
  ncurses5,
}: rec {
  purs = purs-0_15_9;
  purs-0_15_9 = callPackage ./0.15.9.nix {ncurses = ncurses5;};
}
