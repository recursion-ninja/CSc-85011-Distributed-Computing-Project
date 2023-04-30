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
  , buildJobSpecification
    -- * Lenses
  , jobSpecEmail
  , jobSpecTime
  , jobSpecPriority
  , jobSpecFiles
  ) where


import Broker.Job.Input
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Foldable
import Lens.Micro ((^.))
import Lens.Micro.TH


data  JobSpecification
    = JobSpecification
    { _jobSpecEmail    :: Email
    , _jobSpecTime     :: Minutes
    , _jobSpecPriority :: Priority
    , _jobSpecFiles    :: [(FilePath, Disk)]
    }


buildJobSpecification :: Foldable f => f (FilePath, Disk) -> JobInput -> JobSpecification
buildJobSpecification files input =
    JobSpecification
    { _jobSpecEmail    = input ^. jobInputEmail
    , _jobSpecTime     = input ^. jobInputEdict
    , _jobSpecPriority = input ^. jobInputLevel
    , _jobSpecFiles    = toList files
    }


makeLenses ''JobSpecification


$(deriveJSON defaultOptions{fieldLabelModifier = drop 8 . map toLower, constructorTagModifier = map toLower} ''JobSpecification)
