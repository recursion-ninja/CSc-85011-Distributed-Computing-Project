{-# Language DerivingStrategies #-}

module Broker.Job.Class
  ( -- * Lenses
   HasJobCPUs(..)
  , HasJobData(..)
  , HasJobDisk(..)
  , HasJobGPUs(..)
  , HasJobMail(..)
  , HasJobRAM(..)
  , HasJobTask(..)
  , HasJobTime(..)
  ) where

import Lens.Micro (Lens')


class HasJobCPUs s a where

    jobCPUs :: Lens' s a


class HasJobData s a where

    jobData :: Lens' s a


class HasJobDisk s a where

    jobDisk :: Lens' s a


class HasJobGPUs s a where

    jobGPUs :: Lens' s a


class HasJobMail s a where

    jobMail :: Lens' s a


class HasJobRAM s a where

    jobRAM :: Lens' s a


class HasJobTask s a where

    jobTask :: Lens' s a


class HasJobTime s a where

    jobTime :: Lens' s a
