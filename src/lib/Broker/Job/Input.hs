{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

module Broker.Job.Input
  ( -- * Data-types
    UserInputJob()
    -- ** Constructor
  , userInputEmptyJob
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
import Broker.Job.Input.Internal(UserInputJob(..))
import Broker.Job.Generic(JobBinaryStreamsT(..), JobMetadataT(..), JobSpecificationT(JobSpecificationT))


userInputEmptyJob :: UserInputJob
userInputEmptyJob =
    let emptyStreams = JobBinaryStreamsT
            { jobStreamTask = Nothing
            , jobStreamData = Nothing
            }

        emptyMetadata = JobMetadataT
            { jobMetaMail = Nothing
            , jobMetaTime = Nothing
            , jobMetaDisk = Nothing
            , jobMetaRAM  = Nothing
            , jobMetaCPUs = Nothing
            , jobMetaGPUs = Nothing
            }

    in  UserInputJob $ JobSpecificationT emptyMetadata emptyStreams
