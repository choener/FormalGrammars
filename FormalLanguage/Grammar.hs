
-- |

module FormalLanguage.Grammar where



-- | Grammar indices are enumerable objects
--
-- TODO should we always assume operations "modulo"?

data Enumerable = Enumerable
  { index :: String
  , indices :: [String]
  }

indicesFromIntegral :: Integral a => a -> [String]
indicesFromIntegral k
  | k < 1 = error "indicesFromIntegral: <1"
  | otherwise = map show [0..(k-1)]
