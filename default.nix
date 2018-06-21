with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = (lib.foldl' (s: p: s // (import p).hsSrcSet) {} [
    ../Lib-ADPfusion
    ../Lib-PrimitiveArray
  ]) // {FormalGrammars = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.FormalGrammars ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      ADPfusion
      PrimitiveArray
    ];
  };
}
