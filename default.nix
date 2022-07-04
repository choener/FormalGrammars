{ mkDerivation, ADPfusion, base, bytestring, containers
, data-default, fused-effects, fused-effects-lens, lens, lib, mtl
, parsers, prettyprinter, prettyprinter-ansi-terminal
, PrimitiveArray, semigroups, template-haskell, text, transformers
, trifecta, unordered-containers, vector
}:
mkDerivation {
  pname = "FormalGrammars";
  version = "0.4.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ADPfusion base bytestring containers data-default fused-effects
    fused-effects-lens lens mtl parsers prettyprinter
    prettyprinter-ansi-terminal PrimitiveArray semigroups
    template-haskell text transformers trifecta unordered-containers
    vector
  ];
  testHaskellDepends = [
    ADPfusion base bytestring containers data-default fused-effects
    fused-effects-lens lens mtl parsers prettyprinter
    prettyprinter-ansi-terminal PrimitiveArray semigroups
    template-haskell text transformers trifecta unordered-containers
    vector
  ];
  homepage = "https://github.com/choener/FormalGrammars";
  description = "(Context-free) grammars in formal language theory";
  license = lib.licenses.gpl3Only;
}
