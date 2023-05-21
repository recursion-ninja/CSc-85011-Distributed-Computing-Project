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
  , toBytes
  , Email()
  , Minutes()
  , Priority(..)
  ) where


import Broker.Disk
import Control.Monad (replicateM, when)
import Data.Aeson.Encoding (string)
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


newtype Email = Email Text


newtype Minutes = Minutes Word64


data  Priority
    = Low
    | Mid
    | Max


deriving newtype instance Enum Minutes


deriving newtype instance Eq Email


deriving stock   instance Eq JobInput


deriving newtype instance Eq Minutes


deriving stock   instance Eq Priority


deriving newtype instance FromJSON Email


deriving newtype instance FromJSON Minutes


deriving stock   instance Generic Priority


deriving newtype instance Integral Minutes


deriving newtype instance Num Minutes


deriving newtype instance Ord Email


deriving stock   instance Ord JobInput


deriving newtype instance Ord Minutes


deriving stock   instance Ord Priority


deriving stock   instance Read JobInput


deriving newtype instance Read Minutes


deriving stock   instance Read Priority


deriving newtype instance Real Minutes


deriving stock   instance Show JobInput


deriving newtype instance Show Minutes


deriving stock   instance Show Priority


deriving newtype instance ToJSON Email


deriving newtype instance ToJSON Minutes


instance FromJSON Priority where

    parseJSON v@(String n) = 
        case n of
            "LOW"    -> pure Low
            "MEDIUM" -> pure Mid
            "HIGH"   -> pure Max
            _ -> prependFailure "parsing Priority failed, " $ unexpected v

    parseJSON invalid = prependFailure "parsing Priority failed, " $
       typeMismatch "String" invalid


instance ToJSON Priority where

    toJSON Low = String "LOW"
    toJSON Mid = String "MEDIUM"
    toJSON Max = String "HIGH"

    toEncoding Low = string "LOW"
    toEncoding Mid = string "MEDIUM"
    toEncoding Max = string "HIGH"


instance Read Email where

    {-# INLINABLE readPrec #-}
    readPrec = do
        input <- look
        let token = takeWhile (not . isSpace) input
        _ <- replicateM (length token) get
        let (_,b) = break (== '@') token
        let (_,d) = break (== '.') b
        let e     = drop 1 d
        when (null e) pfail
        pure . Email . pack $ token

    readListPrec = readListPrecDefault


instance Show Email where

    {-# INLINABLE show #-}
    show (Email txt) = unpack txt


makeLenses ''JobInput

