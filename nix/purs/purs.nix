{mkPursDerivation}: rec {
  purs = purs-0_15_9;
  purs-unstable = purs-0_15_10-0;
  purs-0_15_10-0 = mkPursDerivation (import ./0.15.10-0.nix);
  purs-0_15_9 = mkPursDerivation (import ./0.15.9.nix);
}
