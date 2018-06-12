with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsPkgs = haskellPackages.extend (packageSourceOverrides {
    ADPfusion = ../Lib-ADPfusion;
    DPutils = ../Lib-DPutils;
    FormalGrammars = ./.;
    OrderedBits = ../Lib-OrderedBits;
    PrimitiveArray = ../Lib-PrimitiveArray;
  });
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.FormalGrammars ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      ADPfusion
      DPutils
      OrderedBits
      PrimitiveArray
    ];
  };
}
