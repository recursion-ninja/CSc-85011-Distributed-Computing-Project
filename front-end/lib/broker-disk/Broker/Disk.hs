{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language StrictData #-}

module Broker.Disk
  ( -- * Data-type
    Disk()
    -- ** Constructor
  , fromBytes
    -- ** Accessor
  , toBytes
    -- ** Mutator
  ) where

import Data.Aeson.Encoding (word64)
import Data.Aeson.Types
import Data.Foldable (fold)
import Data.Word (Word64)


newtype Disk = Disk Word64


deriving newtype instance Enum Disk


deriving newtype instance Eq Disk


deriving newtype instance FromJSON Disk


deriving newtype instance Integral Disk


deriving newtype instance Num Disk


deriving newtype instance Ord Disk


deriving newtype instance Read Disk


deriving newtype instance Real Disk


instance Show Disk where

    {-# INLINABLE show #-}
    show (Disk bytes) =
        let basis = 1024
            
            considerMagnitude n x =
                let (q, r) = bytes `quotRem` (basis ^ n)
                in  (q, r `quot` (basis ^ (n - 1)), x)

            magnitudes =
                [ (1, "KiB")
                , (2, "MiB")
                , (3, "GiB")
                ]

            options = uncurry considerMagnitude <$> magnitudes

            (num, frac, unit) = case dropWhile (\(v,_,_) -> v >= basis) options of
                [] -> let (t, _, _) = last options in (t `quot` (basis ^ 5), 0, "TiB") 
                v:_ -> v

            showFrac fVal =
                let rounded :: Word
                    rounded = round $ fromIntegral fVal / fromIntegral basis
                in  show rounded

        -- max size of 12 = 4 + 1 + 3 + 1 + 3
        in  if   bytes < basis
            then fold [ show bytes, ".",         "000", " ", "  B" ] 
            else fold [ show   num, ".", showFrac frac, " ",  unit ] 


instance ToJSON Disk where

    toJSON = Number . fromIntegral . diskMibiB

    toEncoding = word64 . diskMibiB


diskMibiB :: Disk -> Word64
diskMibiB (Disk bytes) =
    let
        mibiB = case bytes `divMod` (1024 * 1024) of
            (q,0) -> q
            (q,_) -> q + 1
    in  mibiB


fromBytes :: Integral i => i -> Disk
fromBytes bytes = Disk $ fromIntegral bytes


toBytes :: Integral i => Disk -> i
toBytes (Disk bytes) = fromIntegral bytes
