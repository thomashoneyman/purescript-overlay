_final: prev:
let
  packages = prev.callPackage ./manifests { };
in
{
  inherit (packages)
    purs-bin
    spago-bin
    purs-tidy-bin
    purs-backend-es-bin
    purescript-language-server-bin
    purs
    purs-unstable
    spago
    spago-unstable
    purs-tidy
    purs-tidy-unstable
    purs-backend-es
    purs-backend-es-unstable
    purescript-language-server
    purescript-language-server-unstable
    ;
}
