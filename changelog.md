0.3.2.0
-------

- parsers 0.12.5 added mtl instances for Unlined, etc. We remove our own
  instances and require 0.12.5 as the new minimal version
- bang -> notStrict (GHC 8 only)

0.3.1.1
-------

- removed most upper bounds, and version bumped stuff

0.3.1.0
-------

- This version only works together with ADPfusion-0.5.2, this update improves
  performance for all grammars.

0.3.0.0
-------

- Major change in terminal symbol handling: In 0.2.x.y versions, when you named
  a terminal (say 'c') then each occurance of 'c' had the same type. This was
  *independent* of the tape the symbol occured on. Starting with 0.3.0.0 if you
  have a terminal with the same name on different tapes, then you will have to
  give each type explicitly. This requires a bit more typing for homogenous
  grammars, but makes heterogenous multi-tape grammars much more flexible.

0.2.1.0
-------

- indexed rules and grammars (revamped system)
- *multi*-context free grammars available!
- new travis.yml

0.2.0.0
-------

- completely rewritten parsing system
- makes use of newer ADPfusion 0.4.0
- travis-ci integration

0.1.0.0
-------

- fixed dependencies
- major version bump
- new TH code generator using ADPfusion 0.3

0.0.0.2
-------

- cleaned up the TH generator. Should be easier now to build more complex
  auto-generators

0.0.0.1
-------

- basic operations on context-free formal grammars.
