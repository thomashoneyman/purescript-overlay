{
  callPackage,
  ncurses5,
}: rec {
  purs-unstable = purs-0_15_10-0;
  purs-0_15_10-0 = callPackage ./0.15.10-0.nix {ncurses = ncurses5;};

  purs = purs-0_15_9;
  purs-0_15_9 = callPackage ./0.15.9.nix {ncurses = ncurses5;};
}
