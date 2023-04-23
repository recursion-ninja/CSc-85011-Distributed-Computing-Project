{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

module Broker.Job.Input.Internal
  ( -- * Data-types
    UserInputJob(..)
    -- ** Constructor
  ) where


import Broker.Job.Class
import Broker.Job.Generic
import Data.ByteString (ByteString)
import Data.Word
import Lens.Micro (Lens', (^.), (.~), lens)


newtype UserInputJob = UserInputJob { unUIJ :: JobSpecificationT Maybe }


deriving newtype instance Eq UserInputJob


deriving newtype instance Ord UserInputJob


deriving newtype instance Show UserInputJob


instance HasJobCPUs UserInputJob (Maybe Word64) where

    {-# INLINE jobCPUs #-}
    jobCPUs :: Lens' UserInputJob (Maybe Word64)
    jobCPUs = lensSpec getJobMetadata setJobMetadata jobCPUs


instance HasJobData UserInputJob (Maybe [ByteString]) where

    {-# INLINE jobData #-}
    jobData :: Lens' UserInputJob (Maybe [ByteString])
    jobData = lensSpec getJobStreams setJobStreams jobData


instance HasJobDisk UserInputJob (Maybe Word64) where

    {-# INLINE jobDisk #-}
    jobDisk :: Lens' UserInputJob (Maybe Word64)
    jobDisk = lensSpec getJobMetadata setJobMetadata jobDisk


instance HasJobGPUs UserInputJob (Maybe Word64) where

    {-# INLINE jobGPUs #-}
    jobGPUs :: Lens' UserInputJob (Maybe Word64)
    jobGPUs = lensSpec getJobMetadata setJobMetadata jobGPUs


instance HasJobMail UserInputJob (Maybe String) where

    {-# INLINE jobMail #-}
    jobMail :: Lens' UserInputJob (Maybe String)
    jobMail = lensSpec getJobMetadata setJobMetadata jobMail


instance HasJobRAM  UserInputJob (Maybe Word64) where

    {-# INLINE jobRAM  #-}
    jobRAM  :: Lens' UserInputJob (Maybe Word64)
    jobRAM  = lensSpec getJobMetadata setJobMetadata jobRAM


instance HasJobTask UserInputJob (Maybe ByteString) where

    {-# INLINE jobTask #-}
    jobTask :: Lens' UserInputJob (Maybe ByteString)
    jobTask = lensSpec getJobStreams setJobStreams jobTask


instance HasJobTime UserInputJob (Maybe Word64) where

    {-# INLINE jobTime #-}
    jobTime :: Lens' UserInputJob (Maybe Word64)
    jobTime = lensSpec getJobMetadata setJobMetadata jobTime


lensSpec
  :: (JobSpecificationT Maybe -> b)
  -> (b -> JobSpecificationT Maybe -> JobSpecificationT Maybe)
  -> Lens' b (Maybe a)
  -> Lens' UserInputJob (Maybe a)
lensSpec getter setter lensT =
    let get = (^. lensT) . getter . unUIJ
        set (UserInputJob js) x = UserInputJob . (`setter` js) . (lensT .~ x) . getter $ js
    in  lens get set
