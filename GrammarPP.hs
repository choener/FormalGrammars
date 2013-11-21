
-- | This small utility allows us to turn a formal language description into
-- either a LaTeX source file or a Haskell module.

module Main where



data Options
  = LaTeX { select :: Maybe String    -- Select one of the grammars to print (or all)
          }

main = return ()
