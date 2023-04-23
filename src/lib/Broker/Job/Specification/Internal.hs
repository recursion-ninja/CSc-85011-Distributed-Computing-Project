{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

module Broker.Job.Specification.Internal
  ( -- * Data-types
    JobSpecification(..)
--  , JobBinaryStreams()
--  , JobMetadata()
  ) where


import Broker.Job.Class
import Broker.Job.Generic
import Data.Aeson.Types
import Data.ByteString (ByteString)
--import Data.Foldable (fold, toList)
import Data.Functor.Identity
import Data.Word
import Lens.Micro (Lens', (^.), (.~), lens)


newtype JobBinaryStreams = JobBinaryStreams { unBins :: JobBinaryStreamsT Identity }


newtype JobMetadata      = JobMetadata      { unMeta :: JobMetadataT Identity }


newtype JobSpecification = JobSpecification { unSpec :: JobSpecificationT Identity }


deriving newtype instance Eq JobBinaryStreams


deriving newtype instance Eq JobMetadata


deriving newtype instance Eq JobSpecification


deriving newtype instance Ord JobBinaryStreams


deriving newtype instance Ord JobMetadata


deriving newtype instance Ord JobSpecification


deriving newtype instance Show JobBinaryStreams


deriving newtype instance Show JobMetadata


deriving newtype instance Show JobSpecification


instance HasJobCPUs JobMetadata Word64 where

    {-# INLINE jobCPUs #-}
    jobCPUs :: Lens' JobMetadata Word64
    jobCPUs = lensMeta jobCPUs


instance HasJobCPUs JobSpecification Word64 where

    {-# INLINE jobCPUs #-}
    jobCPUs :: Lens' JobSpecification Word64
    jobCPUs = lensSpec getJobMetadata setJobMetadata jobCPUs


instance HasJobData JobBinaryStreams [ByteString] where

    {-# INLINE jobData #-}
    jobData :: Lens' JobBinaryStreams [ByteString]
    jobData = lensBins jobData


instance HasJobData JobSpecification [ByteString] where

    {-# INLINE jobData #-}
    jobData :: Lens' JobSpecification [ByteString]
    jobData = lensSpec getJobStreams setJobStreams jobData


instance HasJobDisk JobMetadata Word64 where

    {-# INLINE jobDisk #-}
    jobDisk :: Lens' JobMetadata Word64
    jobDisk = lensMeta jobDisk


instance HasJobDisk JobSpecification Word64 where

    {-# INLINE jobDisk #-}
    jobDisk :: Lens' JobSpecification Word64
    jobDisk = lensSpec getJobMetadata setJobMetadata jobDisk


instance HasJobGPUs JobMetadata Word64 where

    {-# INLINE jobGPUs #-}
    jobGPUs :: Lens' JobMetadata Word64
    jobGPUs = lensMeta jobGPUs


instance HasJobGPUs JobSpecification Word64 where

    {-# INLINE jobGPUs #-}
    jobGPUs :: Lens' JobSpecification Word64
    jobGPUs = lensSpec getJobMetadata setJobMetadata jobGPUs


instance HasJobMail JobMetadata String where

    {-# INLINE jobMail #-}
    jobMail :: Lens' JobMetadata String
    jobMail = lensMeta jobMail


instance HasJobMail JobSpecification String where

    {-# INLINE jobMail #-}
    jobMail :: Lens' JobSpecification String
    jobMail = lensSpec getJobMetadata setJobMetadata jobMail


instance HasJobRAM  JobMetadata Word64 where

    {-# INLINE jobRAM  #-}
    jobRAM  :: Lens' JobMetadata Word64
    jobRAM  = lensMeta jobRAM


instance HasJobRAM  JobSpecification Word64 where

    {-# INLINE jobRAM  #-}
    jobRAM  :: Lens' JobSpecification Word64
    jobRAM  = lensSpec getJobMetadata setJobMetadata jobRAM


instance HasJobTask JobBinaryStreams ByteString where

    {-# INLINE jobTask #-}
    jobTask :: Lens' JobBinaryStreams ByteString
    jobTask = lensBins jobTask


instance HasJobTask JobSpecification ByteString where

    {-# INLINE jobTask #-}
    jobTask :: Lens' JobSpecification ByteString
    jobTask = lensSpec getJobStreams setJobStreams jobTask


instance HasJobTime JobMetadata Word64 where

    {-# INLINE jobTime #-}
    jobTime :: Lens' JobMetadata Word64
    jobTime = lensMeta jobTime


instance HasJobTime JobSpecification Word64 where

    {-# INLINE jobTime #-}
    jobTime :: Lens' JobSpecification Word64
    jobTime = lensSpec getJobMetadata setJobMetadata jobTime


instance ToJSON JobMetadata where

    toJSON = toJSON . unMeta

    toEncoding = toEncoding . unMeta


instance ToJSON JobSpecification where

    toJSON = toJSON . unSpec

    toEncoding = toEncoding . unSpec


lensBins
  :: Lens' (JobBinaryStreamsT Identity) (Identity a)
  -> Lens' JobBinaryStreams a
lensBins lensT =
    let get = runIdentity . (^. lensT) . unBins
        set jb x = JobBinaryStreams . (lensT .~ Identity x) . unBins $ jb
    in  lens get set


lensMeta
  :: Lens' (JobMetadataT Identity) (Identity a)
  -> Lens' JobMetadata a
lensMeta lensT =
    let get = runIdentity . (^. lensT) . unMeta
        set jm x = JobMetadata . (lensT .~ Identity x) . unMeta $ jm
    in  lens get set


lensSpec
  :: (JobSpecificationT Identity -> b)
  -> (b -> JobSpecificationT Identity -> JobSpecificationT Identity)
  -> Lens' b (Identity a)
  -> Lens' JobSpecification a
lensSpec getter setter lensT =
    let get = runIdentity . (^. lensT) . getter . unSpec
        set (JobSpecification js) x = JobSpecification . (`setter` js) . (lensT .~ Identity x) . getter $ js
    in  lens get set

