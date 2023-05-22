{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}
{-# Language TypeFamilies #-}

module FrontEnd.Types
  ( FrontEndState(..)
  , FrontEndStateInputing()
  , FrontEndStateQueueing()
  , FileReference
  , FileDataRatio
  , MyEvent
    -- ** Constructors
  , initializeFrontEndState
  , initializeTicks
  , transitionFrontEndState
    -- ** Lenses
  , dataProgressUploading
  , dataProgressDownloading
  , inputFocused
  , inputJobForm
  , fileSelector
  , whichMode
    -- ** Data Progress
  , FileDataProgress(..)
  , DataProgressUploading
  , DataProgressDownloading
  , FileQueuedStatus(..)
  , updatingDataProgress
  , finalizeDataProgress
    -- * Other
  , FocusedPane(..)
  , Name(..)
  ) where

import Brick.BChan
import Brick.Forms
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.Widgets.Core hiding (Max)
import Brick.Widgets.FileBrowser
import Brick.Types (Widget)
import Broker.Disk
import Broker.Job.Input
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Monad (forever)
import Control.Monad.IO.Class
import Client.CommandLineOptions
import Client.FileStore
import Control.Monad.State.Class
import Data.Map (Map)
import Data.IORef
import FrontEnd.Core
import Lens.Micro (Lens', (^.), (.~), lens)
import System.Directory (getCurrentDirectory)


newtype FrontEndState = FrontEndState { _whichMode :: Either (FrontEndStateInputing) FrontEndStateQueueing }


{-# INLINE whichMode #-}
whichMode :: Lens' FrontEndState (Either (FrontEndStateInputing) FrontEndStateQueueing)
whichMode f st = (\v -> st { _whichMode = v }) <$> f (_whichMode st)


type FileReference = FileProcessing FileDataProgress


type FileDataRatio = Either (Word, Word) Word


data  FrontEndStateInputing
    = FrontEndStateInputing
    { _inputFocused :: FocusedPane
    , _inputJobForm :: Form JobInput MyEvent Name
    , _fileSelector :: FileBrowser Name
    , _inputingCoreStateTUI :: FrontEndCore Disk
    }


data  FrontEndStateQueueing
    = FrontEndStateQueueing
    { _progressUploads      :: IORef DataProgressUploading
    , _progressDownloads    :: IORef DataProgressDownloading
    , _queueingCoreStateTUI :: FrontEndCore FileReference
    }


{-# INLINE dataProgressUploading #-}
dataProgressUploading :: Lens' FrontEndStateQueueing (IORef DataProgressUploading)
dataProgressUploading f st = (\v -> st { _progressUploads = v }) <$> f (_progressUploads st)


{-# INLINE dataProgressDownloading #-}
dataProgressDownloading :: Lens' FrontEndStateQueueing (IORef DataProgressDownloading)
dataProgressDownloading f st = (\v -> st { _progressDownloads = v }) <$> f (_progressDownloads st)


type DataProgressUploading = Map FilePath FileDataProgress


type DataProgressDownloading = Map FilePath FileDataProgress


updatingDataProgress :: Word -> Word -> FileDataProgress -> FileDataProgress
updatingDataProgress num den = \case
    done@Done {} -> done
    _            -> Part num den


finalizeDataProgress :: Word -> FileDataProgress -> FileDataProgress
finalizeDataProgress num = const $ Done num


-- |
-- Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
--type Name = ()
data  Name
    = InputField_Mail
    | InputField_Time
    | InputField_Nil
    | InputField_Low
    | InputField_Mid
    | InputField_Max
    | InputField_List
    | Browser_FileSystem
    | Queueing_Uploads
    | Queueing_Polling
    | Queueing_Downloads
    | Queueing_Complete


data  FocusedPane
    = PaneMetadata
    | PaneFileData
    | PaneSendTask


data  FileQueuedStatus
    = SEND
    | WAIT
    | WORK
    | DOWN
    | DONE
    | FAIL


instance Show FileQueuedStatus where

    show = \case
        SEND -> "SEND"
        WAIT -> "WAIT"
        WORK -> "WORK"
        DOWN -> "DOWN"
        DONE -> "DONE"
        FAIL -> "FAIL"


deriving stock instance Eq Name


deriving stock instance Eq FocusedPane


deriving stock instance Ord Name


deriving stock instance Ord FocusedPane


deriving stock instance Show Name


deriving stock instance Show FocusedPane


instance HasCurrentTick FrontEndState where

    {-# INLINE currentTick #-}
    currentTick =
        let get' :: FrontEndState -> Tick
            get' (FrontEndState st) = case st of
                    Left  x -> x ^. currentTick
                    Right x -> x ^. currentTick

            set' :: FrontEndState -> Tick -> FrontEndState
            set' (FrontEndState st) fv = FrontEndState $ case st of
                    Left  x -> Left  . (currentTick .~ fv) $ x
                    Right x -> Right . (currentTick .~ fv) $ x

        in  lens get' set'


instance HasCurrentTick FrontEndStateInputing where

    {-# INLINE currentTick #-}
    currentTick =
        let get' :: FrontEndStateInputing -> Tick
            get' = (^. currentTick) . _inputingCoreStateTUI

            set' :: FrontEndStateInputing -> Tick -> FrontEndStateInputing
            set' st fv = st { _inputingCoreStateTUI = (currentTick .~ fv) (_inputingCoreStateTUI st) }

        in  lens get' set'


instance HasCurrentTick FrontEndStateQueueing where

    {-# INLINE currentTick #-}
    currentTick =
        let get' = (^. currentTick) . _queueingCoreStateTUI
            set' st fv = st { _queueingCoreStateTUI = (currentTick .~ fv) (_queueingCoreStateTUI st) }
        in  lens get' set'


instance HasEventChannel FrontEndState where

    {-# INLINE eventChannel #-}
    eventChannel =
        let get' :: FrontEndState -> (BChan MyEvent)
            get' (FrontEndState st) = case st of
                    Left  x -> x ^. eventChannel
                    Right x -> x ^. eventChannel

            set' :: FrontEndState -> (BChan MyEvent) -> FrontEndState
            set' (FrontEndState st) fv = FrontEndState $ case st of
                    Left  x -> Left  . (eventChannel .~ fv) $ x
                    Right x -> Right . (eventChannel .~ fv) $ x

        in  lens get' set'


instance HasEventChannel FrontEndStateInputing where

    {-# INLINE eventChannel #-}
    eventChannel =
        let get' :: FrontEndStateInputing -> (BChan MyEvent)
            get' = (^. eventChannel) . _inputingCoreStateTUI

            set' :: FrontEndStateInputing -> (BChan MyEvent) -> FrontEndStateInputing
            set' st fv = st { _inputingCoreStateTUI = (eventChannel .~ fv) (_inputingCoreStateTUI st) }

        in  lens get' set'


instance HasEventChannel FrontEndStateQueueing where

    {-# INLINE eventChannel #-}
    eventChannel =
        let get' = (^. eventChannel) . _queueingCoreStateTUI
            set' st fv = st { _queueingCoreStateTUI = (eventChannel .~ fv) (_queueingCoreStateTUI st) }
        in  lens get' set'


instance HasFileStore FrontEndStateInputing FrontEndStateInputing Disk Disk where

    {-# INLINE fileStore #-}
    fileStore =
        let get' :: FrontEndStateInputing -> FileStore Disk
            get' = (^. fileStore) . _inputingCoreStateTUI

            set' :: FrontEndStateInputing -> FileStore Disk -> FrontEndStateInputing
            set' st fv = st { _inputingCoreStateTUI = (fileStore .~ fv) (_inputingCoreStateTUI st) }

        in  lens get' set'


instance HasFileStore FrontEndStateQueueing FrontEndStateQueueing FileReference FileReference where

    {-# INLINE fileStore #-}
    fileStore =
        let get' = (^. fileStore) . _queueingCoreStateTUI
            set' st fv = st { _queueingCoreStateTUI = (fileStore .~ fv) (_queueingCoreStateTUI st) }
        in  lens get' set'


instance HasGivenArgs (FrontEndState) where

    {-# INLINE givenArgs #-}
    givenArgs =
        let get' :: FrontEndState -> CommandLineOptions
            get' (FrontEndState st) = case st of
                    Left  x -> x ^. givenArgs
                    Right x -> x ^. givenArgs

            set' :: FrontEndState -> CommandLineOptions -> FrontEndState
            set' (FrontEndState st) fv = FrontEndState $ case st of
                    Left  x -> Left  . (givenArgs .~ fv) $ x
                    Right x -> Right . (givenArgs .~ fv) $ x

        in  lens get' set'


instance HasGivenArgs FrontEndStateInputing where

    {-# INLINE givenArgs #-}
    givenArgs =
        let get' :: FrontEndStateInputing -> CommandLineOptions
            get' = (^. givenArgs) . _inputingCoreStateTUI

            set' :: FrontEndStateInputing -> CommandLineOptions -> FrontEndStateInputing
            set' st fv = st { _inputingCoreStateTUI = (givenArgs .~ fv) (_inputingCoreStateTUI st) }

        in  lens get' set'


instance HasGivenArgs FrontEndStateQueueing where

    {-# INLINE givenArgs #-}
    givenArgs =
        let get' = (^. givenArgs) . _queueingCoreStateTUI
            set' st fv = st { _queueingCoreStateTUI = (givenArgs .~ fv) (_queueingCoreStateTUI st) }
        in  lens get' set'


instance HasWorkingDirectory (FrontEndState) where

    {-# INLINE workingDirectory #-}
    workingDirectory =
        let get' :: FrontEndState -> FilePath
            get' (FrontEndState st) = case st of
                    Left  x -> x ^. workingDirectory
                    Right x -> x ^. workingDirectory

            set' :: FrontEndState -> FilePath -> FrontEndState
            set' (FrontEndState st) fv = FrontEndState $ case st of
                    Left  x -> Left  . (workingDirectory .~ fv) $ x
                    Right x -> Right . (workingDirectory .~ fv) $ x

        in  lens get' set'


instance HasWorkingDirectory FrontEndStateInputing where

    {-# INLINE workingDirectory #-}
    workingDirectory =
        let get' :: FrontEndStateInputing -> FilePath
            get' = (^. workingDirectory) . _inputingCoreStateTUI

            set' :: FrontEndStateInputing -> FilePath -> FrontEndStateInputing
            set' st fv = st { _inputingCoreStateTUI = (workingDirectory .~ fv) (_inputingCoreStateTUI st) }

        in  lens get' set'


instance HasWorkingDirectory FrontEndStateQueueing where

    {-# INLINE workingDirectory #-}
    workingDirectory =
        let get' = (^. workingDirectory) . _queueingCoreStateTUI
            set' st fv = st { _queueingCoreStateTUI = (workingDirectory .~ fv) (_queueingCoreStateTUI st) }
        in  lens get' set'


{-# INLINE inputFocused #-}
inputFocused :: Lens' FrontEndStateInputing FocusedPane
inputFocused f st = (\v -> st { _inputFocused = v }) <$> f (_inputFocused st)


{-# INLINE inputJobForm #-}
inputJobForm :: Lens' FrontEndStateInputing (Form JobInput MyEvent Name)
inputJobForm f st = (\v -> st { _inputJobForm = v }) <$> f (_inputJobForm st)


{-# INLINE fileSelector #-}
fileSelector :: Lens' FrontEndStateInputing (FileBrowser Name)
fileSelector f st = (\v -> st { _fileSelector = v }) <$> f (_fileSelector st)


initializeFrontEndState :: BChan MyEvent -> IO (FrontEndState)
initializeFrontEndState channel = do
    options <- getCommandLineOptions
    currDir <- getCurrentDirectory
    browser <- newFileBrowser selectNonDirectories Browser_FileSystem Nothing
    pure . FrontEndState . Left $ FrontEndStateInputing
        { _inputFocused = PaneMetadata
        , _inputJobForm = jobSpecificationForm blankJobInput
        , _fileSelector = browser
        , _inputingCoreStateTUI = FrontEndCore options channel mempty currDir $ Tick 0
        }


initializeTicks :: IO (BChan MyEvent)
initializeTicks = do
    tVar <- atomically . newTVar $ Tick 0
    chan <- newBChan 1024
    _pid <- forkIO . forever $ do
        tick <- atomically $ readTVar tVar
        atomically $ modifyTVar' tVar succ
        writeBChan chan $ Left tick
        threadDelay (250 * 1000)
    pure chan


transitionFrontEndState
  :: ( MonadIO m
     , MonadState (Either FrontEndStateInputing FrontEndStateQueueing) m
     )
  => FileStore FileReference
  -> m ()
transitionFrontEndState files =
    let decideWhich
          :: MonadIO m
          => Either FrontEndStateInputing FrontEndStateQueueing
          -> m (Either FrontEndStateInputing FrontEndStateQueueing)
        decideWhich = \case
            Right queueing -> pure . Right $ (fileStore .~ files) queueing
            Left  inputing -> do
                ups <- liftIO $ newIORef mempty
                dls <- liftIO $ newIORef mempty
                pure . Right . FrontEndStateQueueing ups dls $ FrontEndCore
                    (inputing ^. givenArgs)
                    (inputing ^. eventChannel)
                    files
                    (inputing ^. workingDirectory)
                    (inputing ^. currentTick)
    in  get >>= decideWhich >>= put


jobSpecificationForm :: JobInput -> Form JobInput e Name
jobSpecificationForm =
    let -- valueBox = withBorderStyle unicodeBold . hLimit 36
        label :: String -> (t -> Widget n) -> t -> Widget n
        label s f w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 7 $ str (s <> ":") <+> fill ' ') <+> f w

        emailBox :: Widget n -> Widget n
        emailBox = withBorderStyle unicodeBold . hLimit 26

        edictBox :: Widget n -> Widget n
        edictBox = withBorderStyle unicodeBold . (<+> str "minutes") . padRight (Pad 1) . hLimit 8

    in  newForm
            [ label "Email" emailBox @@= editShowableField (jobInputEmail :: Lens' JobInput Email  ) InputField_Mail
            , label "Edict" edictBox @@= editShowableField (jobInputEdict :: Lens' JobInput Minutes) InputField_Time
            , label "Level" emailBox @@= radioField         jobInputLevel
                [ ( Low, InputField_Low, "Low")
                , ( Mid, InputField_Mid, "Med")
                , ( Max, InputField_Max, "Max")
                ]
            ]
