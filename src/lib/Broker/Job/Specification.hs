{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

module Broker.Job.Specification
  ( -- * JobSpecification
    JobSpecification()
    -- ** Constructor
  , fromValidUserInput
    -- * JobError
  , JobError(..)
    -- * Lenses
  , HasJobCPUs(..)
  , HasJobData(..)
  , HasJobDisk(..)
  , HasJobGPUs(..)
  , HasJobMail(..)
  , HasJobRAM(..)
  , HasJobTask(..)
  , HasJobTime(..)
  ) where


import Broker.Job.Class
import Broker.Job.Input.Internal
import Broker.Job.Generic
import Broker.Job.Specification.Internal
import Data.Functor.Identity
import Data.List.NonEmpty(NonEmpty(..))
import Data.Validation
import Lens.Micro (Getting, (^.))


newtype JobError = JobError { getJobErrors :: NonEmpty String }


deriving newtype instance Eq JobError


deriving newtype instance Ord JobError


deriving newtype instance Semigroup JobError


deriving newtype instance Show JobError


fromValidUserInput :: UserInputJob -> Validation JobError JobSpecification
fromValidUserInput (UserInputJob spec) =
    let meta :: JobMetadataT Maybe
        meta = getJobMetadata spec

        bins :: JobBinaryStreamsT Maybe
        bins = getJobStreams spec

        check :: (String, Getting (Maybe a) s (Maybe a)) -> s -> Validation JobError (Identity a)
        check (key, getter) obj = case obj ^. getter of
            Nothing -> Failure . JobError $ pure key
            Just v  -> Success $ Identity v

        checkMeta :: JobMetadataT Maybe -> Validation JobError (JobMetadataT Identity)
        checkMeta obj = JobMetadataT
            <$> check ( "Mail", jobMail ) obj
            <*> check ( "Time", jobTime ) obj
            <*> check ( "Disk", jobDisk ) obj
            <*> check ( "RAM" , jobRAM  ) obj
            <*> check ( "CPUs", jobCPUs ) obj
            <*> check ( "GPUs", jobGPUs ) obj

        checkBins :: JobBinaryStreamsT Maybe -> Validation JobError (JobBinaryStreamsT Identity)
        checkBins obj = JobBinaryStreamsT
            <$> check ( "Task", jobTask ) obj
            <*> check ( "Data", jobData ) obj


    in  fmap JobSpecification $ JobSpecificationT
            <$> checkMeta meta
            <*> checkBins bins
