{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

module Broker.Job.Input
  ( -- * JobInput
    JobInput()
    -- ** Constructor
  , blankJobInput
    -- ** Lenses
  , jobInputEmail
  , jobInputEdict
  , jobInputLevel
    -- * Other Data-types
  , Disk()
  , fromBytes
  , Email()
  , Minutes()
  , Priority(..)
  ) where


import Control.Monad (replicateM, when)
import Data.Aeson.Types
import Data.Char (isSpace)
import Data.Text (Text, pack, unpack)
import Data.Word
import GHC.Generics
import Lens.Micro.TH
import Text.Read (get, look, pfail, readPrec, readListPrec, readListPrecDefault)


data  JobInput
    = JobInput
    { _jobInputEmail :: Email
    , _jobInputEdict :: Minutes
    , _jobInputLevel :: Priority
    }


blankJobInput :: JobInput
blankJobInput = JobInput (Email "") 30 Low -- ""


newtype Disk = Disk Word64


newtype Email = Email Text


newtype Minutes = Minutes Word64


data  Priority
    = Nil
    | Low
    | Mid
    | Max


deriving newtype instance Enum Disk


deriving newtype instance Enum Minutes


deriving newtype instance Eq Disk


deriving newtype instance Eq Email


deriving stock   instance Eq JobInput


deriving newtype instance Eq Minutes


deriving stock   instance Eq Priority


deriving newtype instance FromJSON Disk


deriving newtype instance FromJSON Email


deriving newtype instance FromJSON Minutes


deriving stock   instance Generic Priority


deriving newtype instance Integral Disk


deriving newtype instance Integral Minutes


deriving newtype instance Num Disk


deriving newtype instance Num Minutes


deriving newtype instance Ord Disk


deriving newtype instance Ord Email


deriving stock   instance Ord JobInput


deriving newtype instance Ord Minutes


deriving stock   instance Ord Priority


deriving stock   instance Read JobInput


deriving newtype instance Read Disk


deriving newtype instance Read Minutes


deriving stock   instance Read Priority


deriving newtype instance Real Disk


deriving newtype instance Real Minutes


deriving stock   instance Show JobInput


deriving newtype instance Show Minutes


deriving stock   instance Show Priority


deriving newtype instance ToJSON Disk


deriving newtype instance ToJSON Email


deriving newtype instance ToJSON Minutes


instance FromJSON Priority where

    parseJSON v@(Number n) =
        case n of
            0 -> pure Nil
            1 -> pure Low
            2 -> pure Mid
            3 -> pure Max
            _ -> prependFailure "parsing Priority failed, " $ unexpected v

    parseJSON invalid = prependFailure "parsing Priority failed, " $
       typeMismatch "Number" invalid



instance ToJSON Priority where

    toJSON Nil = Number 0
    toJSON Low = Number 1
    toJSON Mid = Number 2
    toJSON Max = Number 3

    toEncoding = genericToEncoding defaultOptions


instance Read Email where

    {-# INLINABLE readPrec #-}
    readPrec = do
        input <- look
        let token = takeWhile (not . isSpace) input
        _ <- replicateM (length token) get
        let (a,b) = break (== '@') token
        let (c,d) = break (== '.') b
        let e     = drop 1 d
        when (null e) pfail
        pure . Email . pack $ token

    readListPrec = readListPrecDefault


instance Show Disk where

    {-# INLINABLE show #-}
    show (Disk kibiB) =
        let (num,unit) = case kibiB `quotRem` 1024 of
                (0,r)     -> (r  , "KiB")
                (mebiB,r) ->
                    case (mebiB + 1) `quotRem` 1024 of
                      (0,r) -> (r, "MiB") 
                      (g,_) -> (g, "GiB")
            
        in  unwords [ show num, unit ]


instance Show Email where

    {-# INLINABLE show #-}
    show (Email txt) = unpack txt


fromBytes :: Integral i => i -> Disk
fromBytes bytes = Disk . fromIntegral $ bytes `div` 1024


makeLenses ''JobInput
