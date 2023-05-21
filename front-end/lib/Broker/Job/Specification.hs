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
  , jobPartDiskSpace
  , jobPartFileName
  , jobPartPriority
  , jobPartUserEmail
  ) where


import Broker.Job.Input
import Broker.Job.Specification.Internal (camelCasing)
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Foldable
import GHC.Generics
import Lens.Micro ((^.))
import Lens.Micro.TH


data  JobPart
    = JobPart
    { _jobPartUserEmail :: Email
    , _jobPartPriority  :: Priority
    , _jobPartFileName  :: FilePath
    , _jobPartDiskSpace :: Disk
    }


makeLenses ''JobPart


newtype JobSpecification = JobSpecification [JobPart]


deriving stock instance Eq JobPart


deriving stock instance Eq JobSpecification


deriving stock instance Generic JobPart


instance ToJSON JobPart where

    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelCasing . drop 8 }

    toEncoding jobPart = pairs $ fold
        [ "fileName"  .= ( jobPart ^. jobPartFileName  )
        , "diskSpace" .= ( jobPart ^. jobPartDiskSpace )
        , "priority"  .= ( jobPart ^. jobPartPriority  )
        , "userEmail" .= ( jobPart ^. jobPartUserEmail )
        ]


buildJobSpecification :: Foldable f => f (FilePath, Disk) -> JobInput -> JobSpecification
buildJobSpecification files input =
    let makePart = JobPart (input ^. jobInputEmail) (input ^. jobInputLevel)
    in  JobSpecification $ uncurry makePart <$> toList files


$(deriveToJSON defaultOptions ''JobSpecification)
