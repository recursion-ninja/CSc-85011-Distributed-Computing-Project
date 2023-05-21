{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StrictData #-}

module Broker.Job.Allocation
  ( -- * Data-type JobAllocation
    JobAllocation()
    -- ** Data-type JobUploadLocation
  , JobUploadLocation()
  , getFileUploadSet
  ) where

import Data.Aeson.Key (toString)
import Data.Aeson.KeyMap (traverseWithKey)
import Data.Aeson.Types
import Data.Foldable
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import GHC.Exts (fromList)


newtype JobAllocation = JobAllocation [JobUploadLocation]


newtype JobUploadLocation = JobUploadLocation { getUploadLocation :: (FilePath, Text) }


deriving stock instance Eq JobAllocation


deriving stock instance Eq JobUploadLocation


deriving stock instance Generic JobAllocation


deriving stock instance Generic JobUploadLocation


deriving stock instance Show JobAllocation


deriving stock instance Show JobUploadLocation


instance FromJSON JobAllocation where

    parseJSON = 
        let consumer = fmap (JobAllocation . fold) . traverseWithKey makePair

            makePair key = withText "FilePath" $ \val -> pure [ JobUploadLocation (toString key, val) ]

        in  withObject "JobAllocation" consumer


getFileUploadSet :: JobAllocation -> Set (FilePath, Text)
getFileUploadSet (JobAllocation locs) = fromList $ getUploadLocation <$> toList locs  
