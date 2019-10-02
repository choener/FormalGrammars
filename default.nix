{ mkDerivation, ADPfusion, ansi-wl-pprint, base, bytestring
, containers, data-default, lens, mtl, parsers, PrimitiveArray
, semigroups, stdenv, template-haskell, text, transformers
, trifecta, unordered-containers, vector
}:
mkDerivation {
  pname = "FormalGrammars";
  version = "0.4.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ADPfusion ansi-wl-pprint base bytestring containers data-default
    lens mtl parsers PrimitiveArray semigroups template-haskell text
    transformers trifecta unordered-containers vector
  ];
  testHaskellDepends = [
    ADPfusion ansi-wl-pprint base bytestring containers data-default
    lens mtl parsers PrimitiveArray semigroups template-haskell text
    transformers trifecta unordered-containers vector
  ];
  homepage = "https://github.com/choener/FormalGrammars";
  description = "(Context-free) grammars in formal language theory";
  license = stdenv.lib.licenses.gpl3;
}
