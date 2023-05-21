{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language FunctionalDependencies #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language MultiParamTypeClasses #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}

module FrontEnd.Core
  ( -- * Data-types
    MyEvent
  , Tick(..)
  , FrontEndCore(..)
    -- * Type-classes
  , HasCurrentTick(..)
  , HasEventChannel(..)
  , HasFileStore(..)
  , HasGivenArgs(..)
  , HasWorkingDirectory(..)
  ) where

import Brick.BChan
import Client.CommandLineOptions
import Client.FileStore
import Data.Word (Word8)
import Lens.Micro (Lens, Lens', lens)


{- |
The event type of the functionally reactive TUI.
-}
type MyEvent = Either Tick (JobFile, FileProcessing FileDataProgress -> FileProcessing FileDataProgress)


{- |
A value continuously fed to the processing queue for intermittent polling.
-}
newtype Tick = Tick Word8
    deriving newtype (Eq, Ord, Show)


{- |
Shared sub-state type between the two phases of the TUI operations.
-}
data  FrontEndCore a
    = FrontEndCore
    { _givenArgs    :: CommandLineOptions
    , _eventChannel :: BChan MyEvent
    , _fileStore    :: FileStore a
    , _currPath     :: FilePath
    , _currTick     :: Tick
    }


class HasCurrentTick s  where

    currentTick :: Lens' s Tick


class HasEventChannel s  where

    eventChannel :: Lens' s (BChan MyEvent)


class HasFileStore s t a b | s -> a, t -> b, s b -> t, t a -> s where

    fileStore :: Lens s t (FileStore a) (FileStore b)


class HasGivenArgs s where

    givenArgs :: Lens' s CommandLineOptions


class HasWorkingDirectory s  where

    workingDirectory :: Lens' s FilePath


instance Bounded Tick where

    minBound = Tick 0

    maxBound = Tick 19


instance Enum Tick where

    fromEnum (Tick w) = fromEnum w
  
    toEnum i
        | 0 <= i && i < maxBound = Tick $ toEnum i
        | otherwise = minBound

    succ t@(Tick w) 
        | t == maxBound = minBound
        | otherwise     = Tick $ w + 1

    pred t@(Tick w)
        | t == minBound = maxBound
        | otherwise     = Tick $ w - 1


instance HasCurrentTick (FrontEndCore a) where

    {-# INLINE currentTick #-}
    currentTick = lens _currTick $ (\st fv -> st { _currTick = fv })


instance HasEventChannel (FrontEndCore a) where

    {-# INLINE eventChannel #-}
    eventChannel = lens _eventChannel $ (\st fv -> st { _eventChannel = fv })


instance HasFileStore (FrontEndCore a) (FrontEndCore b) a b where

    {-# INLINE fileStore #-}
    fileStore = lens _fileStore (\st fv -> st { _fileStore = fv })


instance HasGivenArgs (FrontEndCore a) where

    {-# INLINE givenArgs #-}
    givenArgs = lens _givenArgs $ (\st fv -> st { _givenArgs = fv })


instance HasWorkingDirectory (FrontEndCore a) where

    {-# INLINE workingDirectory #-}
    workingDirectory = lens _currPath $ (\st fv -> st { _currPath = fv })


tickLimit :: Word8
tickLimit = 19


{-
{-# INLINE fileStore #-}
fileStore :: forall a. Lens'
    (FrontEndCore a)
    (FileStore a)
fileStore f st = (\v -> st { _fileStore = v }) <$> f (_fileStore st)


{-# INLINE givenArgs #-}
givenArgs :: forall a. Lens' (FrontEndCore a) CommandLineOptions
givenArgs f st = (\v -> st { _givenArgs = v }) <$> f (_givenArgs st)
-}
