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
    UserInput(..)
  , UserInputJob(..)
    -- ** Constructor
  , emptyUserInput
  ) where


import Broker.Job.Class
import Broker.Job.Generic
import Data.Functor.Classes
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word
import Lens.Micro (Lens', (^.), (.~), lens)


newtype UserInput a = UI { getUserInput :: Maybe a }


newtype UserInputJob = UserInputJob { unUIJ :: JobSpecificationT UserInput }


deriving newtype instance Eq a => Eq (UserInput a)


deriving newtype instance Eq UserInputJob


deriving newtype instance Eq1 UserInput


deriving newtype instance Foldable UserInput


deriving newtype instance Functor UserInput


deriving newtype instance Ord a => Ord (UserInput a)


deriving newtype instance Ord UserInputJob


deriving newtype instance Ord1 UserInput


deriving newtype instance Read a => Read (UserInput a)


deriving newtype instance Read1 UserInput


deriving newtype instance Show UserInputJob


deriving newtype instance Show1 UserInput


instance Show a => Show (UserInput a) where

    show (UI x) = case x of
        Nothing -> ""
        Just v  -> show v


instance HasJobCPUs UserInputJob (UserInput Word64) where

    {-# INLINE jobCPUs #-}
    jobCPUs :: Lens' UserInputJob (UserInput Word64)
    jobCPUs = lensSpec getJobMetadata setJobMetadata jobCPUs


instance HasJobData UserInputJob (UserInput [ByteString]) where

    {-# INLINE jobData #-}
    jobData :: Lens' UserInputJob (UserInput [ByteString])
    jobData = lensSpec getJobStreams setJobStreams jobData


instance HasJobDisk UserInputJob (UserInput Word64) where

    {-# INLINE jobDisk #-}
    jobDisk :: Lens' UserInputJob (UserInput Word64)
    jobDisk = lensSpec getJobMetadata setJobMetadata jobDisk


instance HasJobGPUs UserInputJob (UserInput Word64) where

    {-# INLINE jobGPUs #-}
    jobGPUs :: Lens' UserInputJob (UserInput Word64)
    jobGPUs = lensSpec getJobMetadata setJobMetadata jobGPUs


instance HasJobMail UserInputJob Text where

    {-# INLINE jobMail #-}
    jobMail :: Lens' UserInputJob Text
    jobMail = 
        let get = (^. jobMail) . getJobMetadata . unUIJ

            set :: HasJobMail (JobMetadataT UserInput) b => UserInputJob -> b -> UserInputJob
            set (UserInputJob js) x = UserInputJob . (`setJobMetadata` js) . (jobMail .~ x) . getJobMetadata $ js

        in  lens get set


instance HasJobRAM  UserInputJob (UserInput Word64) where

    {-# INLINE jobRAM  #-}
    jobRAM  :: Lens' UserInputJob (UserInput Word64)
    jobRAM  = lensSpec getJobMetadata setJobMetadata jobRAM


instance HasJobTask UserInputJob (UserInput ByteString) where

    {-# INLINE jobTask #-}
    jobTask :: Lens' UserInputJob (UserInput ByteString)
    jobTask = lensSpec getJobStreams setJobStreams jobTask


instance HasJobTime UserInputJob (UserInput Word64) where

    {-# INLINE jobTime #-}
    jobTime :: Lens' UserInputJob (UserInput Word64)
    jobTime = lensSpec getJobMetadata setJobMetadata jobTime


emptyUserInput :: UserInput a
emptyUserInput = UI Nothing


lensSpec
  :: (JobSpecificationT UserInput -> b)
  -> (b -> JobSpecificationT UserInput -> JobSpecificationT UserInput)
  -> Lens' b (UserInput a)
  -> Lens' UserInputJob (UserInput a)
lensSpec getter setter lensT =
    let get = (^. lensT) . getter . unUIJ
        set (UserInputJob js) x = UserInputJob . (`setter` js) . (lensT .~ x) . getter $ js
    in  lens get set
