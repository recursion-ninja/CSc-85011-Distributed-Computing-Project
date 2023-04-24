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
  , UserInput(..)
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
import Broker.Job.Input.Internal(UserInputJob(..), UserInput(..), emptyUserInput)
import Broker.Job.Generic(JobBinaryStreamsT(..), JobMetadataT(..), JobSpecificationT(JobSpecificationT))


userInputEmptyJob :: UserInputJob
userInputEmptyJob =
    let emptyStreams = JobBinaryStreamsT
            { jobStreamTask = emptyUserInput
            , jobStreamData = emptyUserInput
            }

        emptyMetadata = JobMetadataT
            { jobMetaMail = ""
            , jobMetaTime = emptyUserInput
            , jobMetaDisk = emptyUserInput
            , jobMetaRAM  = emptyUserInput
            , jobMetaCPUs = emptyUserInput
            , jobMetaGPUs = emptyUserInput
            }

    in  UserInputJob $ JobSpecificationT emptyMetadata emptyStreams
