module Broker.Job
  ( -- * Data-types
    JobSpecification(..)
  , JobBinaryStreams()
  , JobMetadata()
    -- * Constructors
  , bundleMetadata
  , bundleStreams
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

import Broker.Job.Internal
