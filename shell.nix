(import ./.).shellFor {
  packages = p: [ p.DPutils p.OrderedBits p.PrimitiveArray p.ADPfusion p.FormalGrammars ];
  withHoogle = true;
}

