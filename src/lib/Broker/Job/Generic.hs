{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

module Broker.Job.Generic
  ( -- * Data-types
    JobSpecificationT(..)
  , JobBinaryStreamsT(..)
  , JobMetadataT(..)
    -- * Setters
  , setJobMetadata
  , setJobStreams
  ) where

import Broker.Job.Class
import Data.ByteString (ByteString)
import Data.Foldable (fold, toList)
import Data.Functor.Classes (Eq1, Ord1, Show1)
import Data.Word
import Data.Text (Text)
import Lens.Micro (Lens', lens)
import Data.Aeson.Types


data  JobBinaryStreamsT f
    = JobBinaryStreamsT
    { jobStreamTask :: f ByteString
    , jobStreamData :: f [ByteString]
    }


data  JobMetadataT f
    = JobMetadataT
    { jobMetaMail :: Text
    , jobMetaTime :: f Word64
    , jobMetaDisk :: f Word64
    , jobMetaRAM  :: f Word64
    , jobMetaCPUs :: f Word64
    , jobMetaGPUs :: f Word64
    }


data  JobSpecificationT f
    = JobSpecificationT
    { getJobMetadata :: JobMetadataT f
    , getJobStreams  :: JobBinaryStreamsT f
    }


deriving stock instance Eq1 f => Eq (JobBinaryStreamsT f)


deriving stock instance Eq1 f => Eq (JobMetadataT f)


deriving stock instance Eq1 f => Eq (JobSpecificationT f)


deriving stock instance Ord1 f => Ord (JobBinaryStreamsT f)


deriving stock instance Ord1 f => Ord (JobMetadataT f)


deriving stock instance Ord1 f => Ord (JobSpecificationT f)


deriving stock instance Show1 f => Show (JobBinaryStreamsT f)


deriving stock instance Show1 f => Show (JobMetadataT f)


deriving stock instance Show1 f => Show (JobSpecificationT f)


instance HasJobCPUs (JobMetadataT f) (f Word64) where

    {-# INLINE jobCPUs #-}
    jobCPUs :: Lens' (JobMetadataT f) (f Word64)
    jobCPUs = lens jobMetaCPUs $ \jm x -> jm { jobMetaCPUs = x }


instance HasJobData (JobBinaryStreamsT f) (f [ByteString]) where

    {-# INLINE jobData #-}
    jobData :: Lens' (JobBinaryStreamsT f) (f [ByteString])
    jobData f (JobBinaryStreamsT t d) = (JobBinaryStreamsT t) <$> (f d)


instance HasJobDisk (JobMetadataT f) (f Word64) where

    {-# INLINE jobDisk #-}
    jobDisk :: Lens' (JobMetadataT f) (f Word64)
    jobDisk f jm =
        let so x = jm { jobMetaDisk = x }
        in  fmap so . f $ jobMetaDisk jm


instance HasJobGPUs (JobMetadataT f) (f Word64) where

    {-# INLINE jobGPUs #-}
    jobGPUs :: Lens' (JobMetadataT f) (f Word64)
    jobGPUs f jm =
        let so x = jm { jobMetaGPUs = x }
        in  fmap so . f $ jobMetaGPUs jm


instance HasJobMail (JobMetadataT f) Text where

    {-# INLINE jobMail #-}
    jobMail :: Lens' (JobMetadataT f) Text
    jobMail f jm =
        let so x = jm { jobMetaMail = x }
        in  fmap so . f $ jobMetaMail jm


instance HasJobRAM  (JobMetadataT f) (f Word64) where

    {-# INLINE jobRAM  #-}
    jobRAM  :: Lens' (JobMetadataT f) (f Word64)
    jobRAM  f jm =
        let so x = jm { jobMetaRAM = x }
        in  fmap so . f $ jobMetaRAM jm


instance HasJobTask (JobBinaryStreamsT f) (f ByteString) where

    {-# INLINE jobTask #-}
    jobTask :: Lens' (JobBinaryStreamsT f) (f ByteString)
    jobTask f (JobBinaryStreamsT t d) = (flip JobBinaryStreamsT d) <$> (f t)


instance HasJobTime (JobMetadataT f) (f Word64) where

    {-# INLINE jobTime #-}
    jobTime :: Lens' (JobMetadataT f) (f Word64)
    jobTime f jm =
        let so x = jm { jobMetaTime = x }
        in  fmap so . f $ jobMetaTime jm


instance HasJobCPUs (JobSpecificationT f) (f Word64) where

    {-# INLINE jobCPUs #-}
    jobCPUs :: Lens' (JobSpecificationT f) (f Word64)
    jobCPUs = lens (jobMetaCPUs . getJobMetadata) $ \js x ->
        let jm = getJobMetadata js
        in  js { getJobMetadata = jm { jobMetaCPUs = x } }


instance HasJobData (JobSpecificationT f) (f [ByteString]) where

    {-# INLINE jobData #-}
    jobData :: Lens' (JobSpecificationT f) (f [ByteString])
    jobData = lens (jobStreamData . getJobStreams) $ \js x ->
        let jm = getJobStreams js
        in  js { getJobStreams = jm { jobStreamData = x } }


instance HasJobDisk (JobSpecificationT f) (f Word64) where

    {-# INLINE jobDisk #-}
    jobDisk :: Lens' (JobSpecificationT f) (f Word64)
    jobDisk = lens (jobMetaDisk . getJobMetadata) $ \js x ->
        let jm = getJobMetadata js
        in  js { getJobMetadata = jm { jobMetaDisk = x } }


instance HasJobGPUs (JobSpecificationT f) (f Word64) where

    {-# INLINE jobGPUs #-}
    jobGPUs :: Lens' (JobSpecificationT f) (f Word64)
    jobGPUs = lens (jobMetaGPUs . getJobMetadata) $ \js x ->
        let jm = getJobMetadata js
        in  js { getJobMetadata = jm { jobMetaGPUs = x } }


instance HasJobMail (JobSpecificationT f) Text where

    {-# INLINE jobMail #-}
    jobMail :: Lens' (JobSpecificationT f) Text
    jobMail = lens (jobMetaMail . getJobMetadata) $ \js x ->
        let jm = getJobMetadata js
        in  js { getJobMetadata = jm { jobMetaMail = x } }


instance HasJobRAM  (JobSpecificationT f) (f Word64) where

    {-# INLINE jobRAM  #-}
    jobRAM  :: Lens' (JobSpecificationT f) (f Word64)
    jobRAM  = lens (jobMetaRAM . getJobMetadata) $ \js x ->
        let jm = getJobMetadata js
        in  js { getJobMetadata = jm { jobMetaRAM = x } }


instance HasJobTask (JobSpecificationT f) (f ByteString) where

    {-# INLINE jobTask #-}
    jobTask :: Lens' (JobSpecificationT f) (f ByteString)
    jobTask = lens (jobStreamTask . getJobStreams) $ \js x ->
        let jm = getJobStreams js
        in  js { getJobStreams = jm { jobStreamTask = x } }


instance HasJobTime (JobSpecificationT f) (f Word64) where

    {-# INLINE jobTime #-}
    jobTime :: Lens' (JobSpecificationT f) (f Word64)
    jobTime = lens (jobMetaTime . getJobMetadata) $ \js x ->
        let jm = getJobMetadata js
        in  js { getJobMetadata = jm { jobMetaTime = x } }


instance Foldable f => ToJSON (JobMetadataT f) where

    toJSON = object . metadataInputKeyValues

    toEncoding = pairs . fold . metadataInputKeyValues


instance Foldable f => ToJSON (JobSpecificationT f) where

    toJSON (JobSpecificationT jMeta _jBins) = object $ metadataInputKeyValues jMeta

    toEncoding (JobSpecificationT jMeta _jBins) = pairs . fold $ metadataInputKeyValues jMeta


setJobMetadata :: JobMetadataT f -> JobSpecificationT f -> JobSpecificationT f
setJobMetadata v js = js { getJobMetadata = v }


setJobStreams :: JobBinaryStreamsT f -> JobSpecificationT f -> JobSpecificationT f
setJobStreams v js = js { getJobStreams = v }


metadataInputKeyValues :: forall f kv. (Foldable f, KeyValue kv) => JobMetadataT f -> [kv]
metadataInputKeyValues obj =
    let prefix :: [kv]
        prefix = [ "Mail" .= jobMetaMail obj ]

        suffix :: [kv]
        suffix = gatherKeyValues obj
            [ ("Time", jobMetaTime)
            , ("Disk", jobMetaDisk)
            , ("RAM" , jobMetaRAM )
            , ("CPUs", jobMetaCPUs)
            , ("GPUs", jobMetaGPUs)
            ]

    in  prefix <> suffix


gatherKeyValues :: forall a f g kv v. (Foldable f, Foldable g, KeyValue kv, ToJSON v) => a -> g (Key, a -> f v) -> [kv]
gatherKeyValues obj =
    let link :: (Key, a -> f v) -> [kv]
        link (k, op) = fmap (k .=) . take 1 . toList $ op obj
    in  foldMap link
