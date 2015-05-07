
module FormalLanguage.CFG.PrettyPrint.LaTeX
  ( renderFile
  , renderLaTeX
  ) where

import Control.Lens hiding ((&), to)
import Data.List (intersperse)
import Data.Set (toList)
import Data.Text (pack)
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath hiding (align)

import FormalLanguage.CFG.Grammar



renderLaTeX :: Int -> Grammar -> LaTeX
renderLaTeX = error "renderLaTeX" -- renderGrammar

{-

-- | Transform a grammar to some LaTeX code.

renderGrammar :: LaTeXC l => Int -> Grammar -> l
renderGrammar k g -- (Grammar ps gname)
  | k == 1 = align xs
  | k == 2 = align2 xs
  where -- subsubsection (raw $ pack gname) <> raw "\n" <> align2 xs <> raw "\n" where
    xs = [ (renderNtT l, mconcat (map renderNtT r)) | Rule l _ r <- toList (g^.rules) ]

-- | Transform a single 'Symb'. This will produce a column of terminal /
-- non-terminal symbols.
--
-- TODO inside/outside

renderNtT :: LaTeXC l => Symb -> l
renderNtT (Symb io xs) = ll <> (mci $ map go xs) <> rr
  where
    go (T s  ) = render s
    go (N s e)
      | Singular        <- e = render s
      | IntBased   k zs <- e = render s !: (raw . pack $ show k)
    go (E    ) = render "-"
--      | Enumerated k zs <- e = render s !: (raw . pack $ k)
    ll = raw "\\begingroup \\left ( \\begin{smallmatrix}"
    rr = raw "\\end{smallmatrix} \\right ) \\endgroup" where
    render x
      | x == "empty" = varepsilon
      | null x       = epsilon -- raw $ pack "-"
      | otherwise    = raw $ pack x

mci = mconcat . intersperse (raw "\\\\\n")

align :: LaTeXC l => [(l,l)] -> l
align = (liftL $ TeXEnv "align*" []) . go where
  go xs = mci [ l & to <> r | (l,r) <- xs ]

align2 :: LaTeXC l => [(l,l)] -> l
align2 = (liftL $ TeXEnv "align*" []) . go where
  go xs = let len     = length xs
              (as,bs) = splitAt ((len +1) `div` 2) $ xs ++ repeat ("","")
              to' c = if c > len `div` 2 then "" else to
          in
              mci [ ll & to <> lr & rl & to' c <> rr | (ll,lr) <- as | ((rl,rr),c) <- zip bs [1..] ]
-}

