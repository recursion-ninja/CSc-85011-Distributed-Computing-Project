{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

module Main
  ( main
  ) where

import Brick hiding (Direction, Max)
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as BS
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.FileBrowser
import Brick.Widgets.FileBrowser qualified as FB
import Broker.CommandLineOptions
import Broker.Job.Input
import Broker.Job.Specification
import Control.Exception (displayException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (encode)
import Data.Aeson.Types
import Data.Foldable (fold, toList)
import Data.Functor
import Data.Set (Set)
import Data.IORef
import Data.Text (Text, pack, unpack)
import Data.Vector (Vector)
import GHC.Exts (fromList)
import Graphics.Vty (Event(..), Key(..), Modifier(..))
import Graphics.Vty qualified as V
import Lens.Micro (Lens', (^.), (.~), (%~))
import Network.HTTP.Client
import Network.HTTP.Types.Status
import System.IO
import System.IO.Unsafe


-- | Named resources
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
    deriving (Eq, Ord, Show)


data  FocusedPane
    = PaneMetadata
    | PaneFileData
    | PaneSendTask


deriving stock instance Eq FocusedPane


deriving stock instance Ord FocusedPane


deriving stock instance Show FocusedPane


data  FrontEndTermState e
    = FrontEndTermState
    { _inputFocused :: FocusedPane
    , _inputJobForm :: Form JobInput e Name
    , _fileSelector :: FileBrowser Name
    , _choosenFiles :: Set (FilePath, Disk)
    , _managerHTTPS :: Manager
    , _givenArgsCLI :: CommandLineOptions
    }


{-# INLINE inputFocused #-}
inputFocused :: forall a. Lens' (FrontEndTermState a) FocusedPane
inputFocused f st = (\v -> st { _inputFocused = v }) <$> f (_inputFocused st)


{-# INLINE inputJobForm #-}
inputJobForm :: forall a. Lens'
    (FrontEndTermState a)
    (Form JobInput a Name)
inputJobForm f st = (\v -> st { _inputJobForm = v }) <$> f (_inputJobForm st)


{-# INLINE fileSelector #-}
fileSelector :: forall a. Lens'
    (FrontEndTermState a)
    (FileBrowser Name)
fileSelector f st = (\v -> st { _fileSelector = v }) <$> f (_fileSelector st)


{-# INLINE choosenFiles #-}
choosenFiles :: forall a. Lens'
    (FrontEndTermState a)
    (Set (FilePath, Disk))
choosenFiles f st = (\v -> st { _choosenFiles = v }) <$> f (_choosenFiles st)


{-# INLINE managerHTTPS #-}
managerHTTPS :: forall a. Lens' (FrontEndTermState a) Manager
managerHTTPS f st = (\v -> st { _managerHTTPS = v }) <$> f (_managerHTTPS st)


{-# INLINE givenArgsCLI #-}
givenArgsCLI :: forall a. Lens' (FrontEndTermState a) CommandLineOptions
givenArgsCLI f st = (\v -> st { _givenArgsCLI = v }) <$> f (_givenArgsCLI st)


main :: IO ()
main =
    let initializeState :: CommandLineOptions -> Manager -> IO (FrontEndTermState e)
        initializeState opts netMan = newFileBrowser selectNonDirectories Browser_FileSystem Nothing >>=
            \browser ->
                pure FrontEndTermState
                { _inputFocused = PaneMetadata
                , _inputJobForm = jobSpecificationForm blankJobInput
                , _fileSelector = browser
                , _choosenFiles = mempty
                , _managerHTTPS = netMan
                , _givenArgsCLI = opts
                }

        frontendApp :: App (FrontEndTermState e) e Name
        frontendApp = App
            { appAttrMap      = const theMap
            , appChooseCursor = showFirstCursor
            , appDraw         = drawTUI
            , appHandleEvent  = handleEvent
            , appStartEvent   = return ()
            }

        builder = V.mkVty V.defaultConfig

    in  do  optionsCLI <- getCommandLineOptions
            netManager <- newManager defaultManagerSettings
            initialVty <- builder
            initState  <- initializeState optionsCLI netManager
            void $ customMain initialVty builder Nothing frontendApp initState


handleEvent :: BrickEvent Name e -> EventM Name (FrontEndTermState e) ()
handleEvent bEvent = do
    pane <- gets (^. inputFocused)

    case bEvent of
        VtyEvent (EvResize    {}) -> return ()
        VtyEvent (EvKey KEsc   []) -> halt
        VtyEvent (EvKey (KChar '|') _)   -> modify $ (inputFocused .~ PaneSendTask)
        VtyEvent (EvKey KLeft  [MShift]) -> modify $ (inputFocused .~ PaneMetadata)
        VtyEvent (EvKey KRight [MShift]) -> modify $ (inputFocused .~ PaneFileData)
{-
        -- Enter quits only when we aren't in the multi-line editor.
        VtyEvent (V.EvKey V.KEnter [])
            | focusGetCurrent f /= Just AddressField -> halt

        VtyEvent (EvKey (V.KChar '\t') [])
            | curr == final -> pure () -- TODO go to FileSelector
-}
        _ -> case pane of
            PaneMetadata -> zoom inputJobForm $ do
                handleFormEvent bEvent

            PaneFileData -> do
                markedFiles <- zoom fileSelector $ do
                    case bEvent of
                        VtyEvent event -> do
                            handleFileBrowserEvent event
                            case event of
                                V.EvKey V.KEnter [] -> getMarkedFiles
                                _ -> pure mempty
                modify (choosenFiles %~ (<> markedFiles))

            PaneSendTask -> case bEvent of
                VtyEvent (EvKey KEnter _) -> do
                    liftIO $ putStrLn "ENTERED"
                    mangr <- gets (^. managerHTTPS)
                    servr <- gets (gateway . (^. givenArgsCLI))
                    files <- gets (^. choosenFiles)
                    input <- gets (formState . (^. inputJobForm))
                    liftIO $ sendJobRequest mangr servr input files
                _ -> pure ()


-- Drawing

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ( E.editFocusedAttr   , V.black `on` V.yellow )
    , ( invalidFormInputAttr, V.white `on` V.red    )
    , ( focusedFormInputAttr, V.black `on` V.yellow )
    , ( readySend           , V.blue  `on` V.green )
    ]


errorAttr :: AttrName
errorAttr = attrName "error"


readySend :: AttrName
readySend = attrName "readySend"


jobSpecificationForm :: JobInput -> Form JobInput e Name
jobSpecificationForm =
    let valueBox = withBorderStyle BS.unicodeBold . hLimit 36
        label s f w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 7 $ str (s <> ":") <+> fill ' ') <+> f w
        emailBox = withBorderStyle BS.unicodeBold . hLimit 36
        edictBox = withBorderStyle BS.unicodeBold . (<+> str "minutes") . padRight (Pad 1) . hLimit 8
    in  newForm
            [ label "Email" emailBox @@= editShowableField (jobInputEmail :: Lens' JobInput Email  ) InputField_Mail
            , label "Edict" edictBox @@= editShowableField (jobInputEdict :: Lens' JobInput Minutes) InputField_Time
            , label "Level" emailBox @@= radioField         jobInputLevel
                [ ( Nil, InputField_Nil, "Nil")
                , ( Low, InputField_Low, "Low")
                , ( Mid, InputField_Mid, "Mid")
                , ( Max, InputField_Max, "Max")
                ]
            ]


drawTUI :: FrontEndTermState e -> [Widget Name]
drawTUI st =
    let barTitle = hBox
            [ drawSTREAM st
            , drawHelper st
            ]
        barInput = hBox
            [ drawUserInput st
            , drawFileBrowser st
            ]
    in  pure . C.center $ vBox
            [ barTitle
            , barInput
            , drawChoosenFiles st
            , drawSendButton st
            ]


drawSTREAM :: FrontEndTermState e -> Widget Name
drawSTREAM =
    let boxTitle = drawTitle
            [ "Simple"
            , "Transparent"
            , "Resource"
            , "Exchange"
            , "And"
            , "Management"
            ]

        drawLine x =
            let cap = str . pure $ head x
                sep = padLeftRight 2 $ str "-"
                end = str x
            in  hLimit 18 $ cap <+> sep <+> end

        drawTitle = id
          . joinBorders
          . withBorderStyle BS.unicodeBold
          . B.borderWithLabel (str "STREAM User Endpoint")
          . freezeBorders
          . hLimit 47
          . C.hCenter
          . padTopBottom 2
          . vBox
          . fmap drawLine

    in  const boxTitle


drawHelper :: FrontEndTermState e -> Widget Name
drawHelper st =
    let
        boxLine (k,v) = keyBox k <+> valBox v

        keyMax = maximum $ length . fst <$> pairs st
        keyPad x = x <> replicate (keyMax - length x) ' '
        keyBox x = padLeftRight 1 . hLimit keyMax . str $ keyPad x

        valMax = maximum $ length . snd <$> pairs st
        valPad x = x <> replicate (valMax - length x) ' '
        valBox x = padLeftRight 1 . hLimit valMax . str $ valPad x

        pairs x = fold
            [ helpInfoConst x
            , helpInfoMetadata
            , helpInfoFileData x
            ]

        helpInfoConst x =
            [ ("Escape:"     , "Exit")
            , ("Shift+Enter:", "Send job")
            ] <> helpInfoDirection x


        helpInfoDirection x =
            let goFileData = ("Shift+Right:", "Switch to file selector")
                goMetadata = ("Shift+Left:" , "Switch to job metadata")
                goSendTask = ("'|':" , "Switch to job submission")
            in  case x ^. inputFocused of
                    PaneMetadata -> [ goFileData, goSendTask ]
                    PaneFileData -> [ goMetadata, goSendTask ]
                    PaneSendTask -> [ goMetadata, goFileData ]

        helpInfoMetadata =
            [ ("Tab:"      , "Next input field")
            , ("Shift+Tab:", "Previous input field")
            ]

        helpInfoFileData x = helpInfoFileErr x <>
            [ ("Enter:"  , "select file, change folder")
            , ("Up/Down:", "scroll" )
            , ("/:"      , "search, escape to cancel")
            ]

        helpInfoSendData =
            [ ("Enter:", "submit job for processing")
            ]

        helpInfoFileErr x =
            case FB.fileBrowserException $ x ^. fileSelector of
                Nothing -> []
                Just e  -> [ ("Error:", displayException e) ]

        helpBoxFormat = padTop (Pad 1) . vBox . fmap boxLine

        boxHelp x =
            let boxConstant = helpBoxFormat $ helpInfoConst x
                boxContext  = helpBoxFormat $ case x ^. inputFocused of
                    PaneMetadata -> helpInfoMetadata
                    PaneFileData -> helpInfoFileData x
                    PaneSendTask -> helpInfoSendData
            in  boxConstant <=> boxContext

        boxPadding = padBottom $ case st ^. inputFocused of
            PaneMetadata -> Pad 2
            PaneFileData -> Pad 1
            PaneSendTask -> Pad 3

    in  id
          . joinBorders
          . withBorderStyle BS.unicodeBold
          . B.borderWithLabel (str "Input Controls")
          . freezeBorders
          . hLimit 47
          . C.hCenter
          . boxPadding
          $ boxHelp st


drawUserInput :: FrontEndTermState e -> Widget Name
drawUserInput st =
    let formBox = id
            . joinBorders
            . borderWithLabel (txt "Job Specification")
            . freezeBorders
            . padTop (Pad 1)
            . padLeftRight 2
            . hLimit 48
            . renderForm
            . (^. inputJobForm)
    in  formBox st


drawFileBrowser :: FrontEndTermState e -> Widget Name
drawFileBrowser st =
    let
        browser = id
            . vLimit 12
            . hLimit 50
            . borderWithLabel (txt "File Selector")
            . FB.renderFileBrowser True
            . (^. fileSelector)
    in browser st


drawSendButton :: FrontEndTermState e -> Widget Name
drawSendButton st =
    let focusing = case st ^. inputFocused of
            PaneSendTask -> withAttr readySend
            _ -> id

        bordering = case st ^. inputFocused of
            PaneSendTask -> withBorderStyle BS.unicodeBold
            _ -> id

    in  id
        . padLeftRight 36
--        . hCenter
        . hLimit 32
        . vLimit 5
        . bordering
        . border
        . freezeBorders
        . padLeftRight 5
        . padTopBottom 1
        . focusing
        $ str " S U B M I T "

drawChoosenFiles :: FrontEndTermState e -> Widget Name
drawChoosenFiles st =
    let fileLine (path, disk) =
            let strDisk = show disk
                padDisk = padLeft (Pad (8 - length strDisk)) $ str strDisk
                boxDisk = padLeftRight  1  padDisk
                boxPath = padRight (Pad 1) $ str path
            in  vLimit 1 $ boxDisk <+> boxPath

        sizing [] = [ hCenter $ str "( None )" ]
        sizing xs = xs

        fileList = id
--            . hCenter
            . vLimit 16
            . hLimit 99
            . borderWithLabel (txt "Choosen Files")
            . freezeBorders
            . padTopBottom 1
            . vBox
            . sizing
            . fmap fileLine
            . toList
            . (^. choosenFiles)
    in fileList st


consumeSelectedFiles :: [FileInfo] -> Set (FilePath, Disk)
consumeSelectedFiles fs =
    let consumeFile fInfo = case fileStatusSize <$> fileInfoFileStatus fInfo of
          Left _ -> []
          Right size -> [(fileInfoFilePath fInfo, fromBytes size)]
    in  fromList $ foldMap consumeFile fs


getMarkedFiles :: EventM Name (FileBrowser Name) (Set (FilePath, Disk))
getMarkedFiles = consumeSelectedFiles . FB.fileBrowserSelection <$> get


sendJobRequest :: Manager -> Gateway -> JobInput -> Set (FilePath, Disk) -> IO ()
sendJobRequest manager gateway input files =
    let requestObject = toJSON $ buildJobSpecification files input
        request connection = connection
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode requestObject
            }
        handler :: Response BodyReader -> IO ()
        handler response =
            putStrLn $ "The status code was: " <> (show $ statusCode $ responseStatus response)

    in  do  initialRequest <- parseRequest $ show gateway
            traceToFile "Gateway" gateway
            traceToFile "JSON object to send" $ toJSON $ buildJobSpecification files input
            print . toJSON $ buildJobSpecification files input
            print (request initialRequest)
            withResponse (request initialRequest) manager handler


logFileHandle :: IORef Handle
logFileHandle = unsafePerformIO $ openFile "brick.log" WriteMode >>= newIORef


traceToFile :: (MonadIO m, Show a) => String -> a -> m ()
traceToFile key val =
    let writeOut :: Handle -> IO ()
        writeOut h = hPutStrLn h (key <> ":\t" <> show val) *> hFlush h

        logKeyValPair :: IO ()
        logKeyValPair = readIORef logFileHandle >>= writeOut

    in  liftIO logKeyValPair
