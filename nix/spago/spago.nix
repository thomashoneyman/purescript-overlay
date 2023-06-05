{mkSpagoDerivation}: rec {
  spago = spago-0_93_4;
  spago-0_93_4 = mkSpagoDerivation (import ./0.93.4.nix);
}
