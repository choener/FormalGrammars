
-- | Testing area

module Tests where



data SynVar = S
data Term   = T

-- we use the above to be able to give arguments as:
--
-- grammar Algebra{..} (S:.a:.b) (T:.c:.d)

[formalLanguage|
-- we provide an index {i} at the grammar level. This makes the index {i}
-- available at the grammar level. Each grammar here is at a proto stage.
-- Can this be implicit? If an index is given, expand ALL syntactic
-- variables with indices?
Grammar: Global{i}
N: X{i}
T: c
T: e
E: z
-- use '-' as epsilon symbol ?!
[X,X] -> done  <<< [e,e]
[X,X] -> align <<< [X,X] [c,c]
[X,X] -> indel <<< [X,X] [z,c]
[X,X] -> delin <<< [X,X] [c,z]
//

Grammar: Casino
-- two syntactic variables, honest and dishonest
N: H
N: D
-- two terminals, read a character, empty production
T: c
T: e
-- start symbol for the grammar.
S: S

-- rules from the start symbol
S -> idd <<< H
S -> idd <<< D

-- rules from honest synvar
H -> nil <<<   e
H -> hfh <<< H c
H -> hfd <<< D c

-- rules from dishonest synvar
D -> dfd <<< D c
D -> dfh <<< H c
//

-- The fromfile directive reads the given file, which should contain valid
-- grammars.
FromFile: 'tests.gra'

-- Only grammars that we 'write' are written as Haskell code. This allows
-- us to expand one or more grammars into code.
Write: Global{1}
Write: Global >< Global -- for grammar products only
|]

