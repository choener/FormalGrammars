module FormalLanguage.CFG.Test where

import           FormalLanguage.CFG


[formalLanguage|
Verbose

Grammar: Global
N: X
T: c      -- NOTE that each tape with a 'c' uses its own type!
S: [X,X]
[X,X] -> done  <<< [e,e]
[X,X] -> align <<< [X,X] [c,c]
[X,X] -> indel <<< [X,X] [-,c]
[X,X] -> delin <<< [X,X] [c,-]
//

Outside: O
Source: Global
//
Emit: Global
Emit: O
|]
