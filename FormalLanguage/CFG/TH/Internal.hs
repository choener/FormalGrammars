
-- | Support for different Template Haskell versions.

module FormalLanguage.CFG.TH.Internal where

import qualified Language.Haskell.TH.Lib as TH
import           Language.Haskell.TH


#if MIN_VERSION_base(4,10,0)
dataD ctxt tc tvs cons = TH.dataD ctxt tc tvs Nothing cons []
#else
#if MIN_VERSION_base(4,9,0)
--dataD :: CxtQ -> Name -> [TyVarBndr] -> [ConQ] -> CxtQ -> DecQ
--
-- NOTE the last argument would be @mapM conT xs@ if we actually had any
-- @xs@ to work with.
dataD ctxt tc tvs cons = TH.dataD ctxt tc tvs Nothing cons (return [])
#else
dataD ctxt tc tvs cons = TH.dataD ctxt tc tvs cons []
#endif
#endif

