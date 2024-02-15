{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main,
) where

import Brick hiding (Direction, Max)
import Brick.BChan
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.FileBrowser
import Brick.Widgets.FileBrowser qualified as FB
import Brick.Widgets.ProgressBar
import Broker.Gateway
import Broker.Job.Allocation
import Broker.Job.Input
import Broker.Job.Poll
import Broker.Job.Specification
import Client.CommandLineOptions
import Client.FileStore
import Control.Concurrent
import Control.Exception (bracket, displayException, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Types
import Data.Bifunctor
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Foldable (fold, toList, traverse_)
import Data.Functor (void, ($>))
import Data.IORef
import Data.List (intersperse)
import FrontEnd.Core
import FrontEnd.Types
import GHC.Exts (fromList)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes.Color
import Graphics.Vty.CrossPlatform qualified as V
import Lens.Micro ((%~), (.~), (^.), _Left, _Right)
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Req
import Network.HTTP.Types.Status
import System.FilePath
import System.IO
import System.IO.Unsafe
import Text.Read (readMaybe)


debugging :: Bool
debugging = False


main :: IO ()
main =
    let frontendApp :: App (FrontEndState) MyEvent Name
        frontendApp =
            App
                { appAttrMap = const theMap
                , appChooseCursor = showFirstCursor
                , appDraw = drawTUI
                , appHandleEvent = handleEvent
                , appStartEvent = return ()
                }

        builder = V.mkVty V.defaultConfig
    in  do
            tickStream <- initializeTicks
            initState <- initializeFrontEndState tickStream
            initialVty <- builder
            void $ customMain initialVty builder (Just tickStream) frontendApp initState


handleEvent :: BrickEvent Name MyEvent -> EventM Name (FrontEndState) ()
handleEvent = \case
    -- Immediately handle terminal control events
    VtyEvent (EvResize{}) -> pure ()
    VtyEvent (EvKey KEsc []) -> halt
    -- Next check which mode we are in
    bEvent -> zoom whichMode $ do
        sent <- zoom _Left $ handleInputing bEvent
        case sent of
            Just files -> transitionFrontEndState files
            Nothing -> pure ()

        zoom _Right $ handleQueueing bEvent


sendJobRequest :: Gateway -> JobInput -> FileStore Disk -> IO JobAllocation
sendJobRequest gateway input files =
    let (reqURL, reqOpt) = gatewayEndpoint gateway

        fjNames = first tayloredName <$> fileStoreList files
        jobSpec = buildJobSpecification fjNames input
        jobJSON = BSL.toStrict . encodingToLazyByteString $ toEncoding jobSpec
        optJSON = header "Content-Type" "application/json; charset=utf-8"
        request = req POST (reqURL /: "jobs") (ReqBodyBs jobJSON) jsonResponse (reqOpt <> optJSON)

        response :: IO JobAllocation
        response = responseBody <$> runReq defaultHttpConfig request
    in  do
            result <- response
            traceToFile "Input File List" $ show fjNames
            pure result


handleInputing :: BrickEvent Name MyEvent -> EventM Name FrontEndStateInputing (Maybe (FileStore FileReference))
handleInputing = \case
    AppEvent (Left tick) -> liftIO (traceToFile "AppEvent" $ show tick) *> modify (currentTick .~ tick) $> Nothing
    VtyEvent (EvKey (KChar '|') _) -> modify (inputFocused .~ PaneSendTask) $> Nothing
    VtyEvent (EvKey KLeft [MShift]) -> modify (inputFocused .~ PaneMetadata) $> Nothing
    VtyEvent (EvKey KRight [MShift]) -> modify (inputFocused .~ PaneFileData) $> Nothing
    bEvent ->
        gets (^. inputFocused) >>= \case
            PaneMetadata -> Nothing <$ zoom inputJobForm (handleFormEvent bEvent)
            PaneFileData -> (Nothing <$) $ do
                workDir <- gets (^. workingDirectory)
                markedFiles <- zoom fileSelector $ do
                    case bEvent of
                        VtyEvent event -> do
                            handleFileBrowserEvent event
                            case event of
                                V.EvKey V.KEnter [] -> getMarkedFiles workDir
                                _ -> pure mempty
                        _ -> pure mempty

                when (markedFiles /= mempty) $
                    modify (fileStore %~ (<> markedFiles))
            PaneSendTask -> case bEvent of
                VtyEvent (EvKey KEnter _) -> do
                    servr <- gets (gateway . (^. givenArgs))
                    files <- gets (^. fileStore)
                    input <- gets (formState . (^. inputJobForm))
                    alloc <- liftIO $ sendJobRequest servr input files
                    liftIO . traceToFile "Results" . show $ getFileUploadSet alloc
                    let newStore = internalizeAllocation files alloc
                    liftIO . traceToFile "New F-Store" $ show newStore
                    pure $ Just newStore
                _ -> pure Nothing


handleQueueing :: BrickEvent Name MyEvent -> EventM Name FrontEndStateQueueing ()
handleQueueing = \case
    AppEvent (Left tick) -> do
        liftIO (traceToFile "AppEvent" $ show tick) *> modify (currentTick .~ tick)

        liftIO $ traceToFile "Queueing:" "ENTRY"
        handleFileUploads
        liftIO $ traceToFile "Queueing:" "UPLOADED"
        liftIO $ traceToFile "Queueing:" "POLLING?"
        handleFilePolling
        liftIO $ traceToFile "Queueing:" "POLLED!"

        liftIO $ traceToFile "Queueing:" "Downloads"
        handleFileDownloads
        liftIO $ traceToFile "Queueing:" "Downloaded!"

        modify $ currentTick .~ tick
        fs <- gets (^. fileStore)
        liftIO $ traceToFile "FILE Store" $ show fs
    AppEvent (Right (path, updater)) -> do
        modify $ fileStore %~ (path `fileAdjustBy` updater)
    _ -> do
        liftIO $ traceToFile "Queueing:" "ENTRY"
        handleFileUploads
        liftIO $ traceToFile "Queueing:" "UPLOADED"


refreshUploadFile :: FileDataProgress -> FileReference -> FileReference
refreshUploadFile (Fail{}) = id
refreshUploadFile part@(Part{}) = fileUploadTo part
refreshUploadFile (Done num) = filePollWith num


handleFileUploads :: EventM Name FrontEndStateQueueing ()
handleFileUploads =
    let blankFileDataSize = Part 0 0

        uploadOneFile channel (FileQuery path (_size, opt, url)) = do
            let filePath = originalPath path
            let fileName = tayloredName path
            liftIO $ traceToFile "File-path" filePath
            liftIO $ traceToFile "File-name" fileName

            let listener response = do
                    chunks <- brReadSome (HTTP.responseBody response) 12
                    let resStr = BSL.unpack chunks
                    case readMaybe resStr of
                        Nothing -> throwIO . JsonHttpException $ "Not a number: " <> show resStr
                        Just v -> liftIO . void . writeBChanNonBlocking channel $ Right (path, refreshUploadFile $ Done v)

            let handler request manager = liftIO $ withResponse request manager listener

            let fileProgress :: StreamFileStatus -> IO ()
                fileProgress (StreamFileStatus d n _) =
                    let n' = fromIntegral n
                        d' = fromIntegral d
                        v = Part n' d'
                    in  void . writeBChanNonBlocking channel $ Right (path, refreshUploadFile v)

            filePart <- liftIO $ do
                p <- partFileRequestBody "file" fileName <$> observedStreamFile fileProgress filePath
                traceToFile "Part File-name" . show $ partFilename p
                pure p

            reqBody <- reqBodyMultipart [filePart]

            let request = req' POST url reqBody opt handler

            modify $ fileStore %~ (path `fileAdjustBy` refreshUploadFile blankFileDataSize)
            liftIO . forkIO $ runReq defaultHttpConfig request
    in  do
            fStore <- gets (^. fileStore)
            channel <- gets (^. eventChannel)
            liftIO . traceToFile "BEGIN UPLOAD FILE Store" . show $ getQueriedFile <$> toList (queryUploadReady fStore)
            traverse_ (uploadOneFile channel) $ queryUploadReady fStore


handleFilePolling :: EventM Name FrontEndStateQueueing ()
handleFilePolling =
    let -- pollOneFile :: FileQuery (Option 'Http, Url 'Http) ->
        pollOneFile channel (FileQuery path (_, opt, url)) =
            let request = req GET url NoReqBody bsResponse opt

                response :: IO JobPolledStatus
                response = do
                    resStr <- BS.unpack . responseBody <$> runReq defaultHttpConfig request
                    case readMaybe resStr of
                        Just v -> pure v
                        Nothing -> throwIO . JsonHttpException $ "Not a valid status: " <> show resStr

                handleStatus :: JobPolledStatus -> IO ()
                handleStatus =
                    let writeChannel = void . writeBChanNonBlocking channel
                    in  \case
                            Waiting -> pure ()
                            Processing -> writeChannel $ Right (path, fileNowGoing)
                            Finished -> writeChannel $ Right (path, fileNowReady)
            in  liftIO . forkIO $ (response >>= (\s -> traceToFile ("Polled - " <> readableName path) (show s) *> handleStatus s))
    in  do
            fStore <- gets (^. fileStore)
            channel <- gets (^. eventChannel)
            let pollThese = toList $ queryPolling fStore
            liftIO . traceToFile "BEGIN UPLOAD FILE Store" . show $ getQueriedFile <$> pollThese
            traverse_ (pollOneFile channel) pollThese


httpFailure :: Status -> FileDataProgress
httpFailure = (Fail <$> statusCode <*> BS.unpack . statusMessage)


handleFileDownloads :: EventM Name FrontEndStateQueueing ()
handleFileDownloads =
    let downloadOneFile channel (FileQuery jobFile (opt, url)) = do
            let filePath = originalPath jobFile
            let outputPath = (`addExtension` "mp3") . fst $ splitExtension filePath
            liftIO $ traceToFile "Filename" filePath
            liftIO $ traceToFile "File out" outputPath

            let listener response = do
                    traceToFile "Listener -- ENTRY" ""
                    let writeChannel = void . writeBChanNonBlocking channel

                    let resStatus = responseStatus response
                    if resStatus /= ok200
                        then writeChannel $ Right (jobFile, fileFailWith $ httpFailure resStatus)
                        else do
                            traceToFile "Listener -- Header" "(pre)"
                            let resHeads = responseHeaders response
                            let resMay = responseHeader (void response) "Content-Length"
                            traceToFile "Listener -- Header" . show $ BS.unpack <$> resMay
                            traceToFile "Listener -- Header (all)\n" . unlines $ show . bimap show BS.unpack <$> resHeads
                            traceToFile "Listener -- Header" "(post)"

                            let bodyReader = HTTP.responseBody response
                            let acquireFile = outputPath `openFile` ReadWriteMode
                            let releaseFile = hClose

                            bracket acquireFile releaseFile $ \outputHandle -> do
                                traceToFile "Bracket -- ENTRY" outputPath

                                let writeOutChunk bs = do
                                        let len = BS.length bs
                                        traceToFile "Bracket -- Chunk" $ "Write -- " <> show len
                                        BS.hPut outputHandle bs
                                        writeChannel $ Right (jobFile, fileAddBytes $ incrementDataProgress len)

                                traceToFile "Bracket -- B-Channel" "write (pre)"
                                writeChannel $ Right (jobFile, fileDownload $ Part 0 0)
                                traceToFile "Bracket -- B-Channel" "write (post)"
                                chunks <- brConsume bodyReader
                                traverse_ writeOutChunk chunks
                                writeChannel $ Right (jobFile, fileComplete)

            let handler request manager = liftIO $ withResponse request manager listener

            let request = req' GET url NoReqBody opt handler

            liftIO . void . forkIO $ runReq defaultHttpConfig request
    in  do
            fStore <- gets (^. fileStore)
            channel <- gets (^. eventChannel)
            let downloadables = toList (queryDownloadReady fStore)
            liftIO . traceToFile "BEGIN DOWNload F-Store" . show $ getQueriedFile <$> downloadables
            liftIO $ traverse_ (downloadOneFile channel) downloadables


consumeSelectedFiles :: FilePath -> [FileInfo] -> FileStore Disk
consumeSelectedFiles workDir fs =
    let makeJobFile = fromFullPath workDir . fileInfoFilePath
        consumeFile fInfo = case fileStatusSize <$> fileInfoFileStatus fInfo of
            Left _ -> []
            Right size -> [(makeJobFile fInfo, fromBytes size)]
    in  fromList $ foldMap consumeFile fs


getMarkedFiles :: FilePath -> EventM Name (FileBrowser Name) (FileStore Disk)
getMarkedFiles workDir = consumeSelectedFiles workDir . FB.fileBrowserSelection <$> get


logFileHandle :: IORef Handle
logFileHandle = unsafePerformIO $ openFile "brick.log" WriteMode >>= newIORef


traceToFile :: (MonadIO m) => String -> String -> m ()
traceToFile key val =
    let writeOut :: Handle -> IO ()
        writeOut h = hPutStrLn h (key <> ":\t" <> val) *> hFlush h

        logKeyValPair :: IO ()
        logKeyValPair = readIORef logFileHandle >>= writeOut
    in  when debugging $ liftIO logKeyValPair


internalizeAllocation :: FileStore Disk -> JobAllocation -> FileStore FileReference
internalizeAllocation oldStore =
    let processFile (fileName, host) = case oldStore `fileStoreQuery` fileName of
            Nothing -> mempty
            Just (FileQuery jobFile size) ->
                jobFile `fileStoredWith` fileHostedBy host size
    in  foldMap processFile . getFileUploadSet


encaseFileRow :: Widget n -> Widget n
encaseFileRow = vLimit 1 . hLimit 77 . padLeftRight 1


queryBy
    :: ( Foldable t
       , HasFileStore s s a a
       )
    => s
    -> (FileStore a -> t b)
    -> [b]
queryBy st f = toList . f $ st ^. fileStore


{-
 Drawing
-}

theMap :: AttrMap
theMap =
    attrMap
        V.defAttr
        [ (E.editFocusedAttr, V.black `on` V.yellow)
        , (invalidFormInputAttr, V.white `on` V.red)
        , (focusedFormInputAttr, V.black `on` V.yellow)
        , (readySubmit, V.blue `on` V.green)
        , (readySubmitOuter, V.green `on` V.green)
        , (progressIncompleteAttr, fg uploadedBackground)
        , (progressCompleteAttr, V.black `on` uploadedBackground)
        , (statusSEND, V.black `on` uploadedBackground)
        , (statusWAIT, V.black `on` linearColor 150 210 225)
        , (statusWORK, V.black `on` linearColor 150 215 220)
        , (statusDOWN, V.black `on` linearColor 150 220 215)
        , (statusDONE, V.black `on` linearColor 150 225 210)
        , (statusFAIL, V.black `on` V.red)
        ]


uploadedBackground :: Color
uploadedBackground = linearColor 150 210 225


errorAttr :: AttrName
errorAttr = attrName "error"


readySubmit :: AttrName
readySubmit = attrName "readySubmit"


readySubmitOuter :: AttrName
readySubmitOuter = attrName "readySubmitOuter"


progressPollingAttr :: AttrName
progressPollingAttr = attrName "progressPolling"


progressDownloadAttr :: AttrName
progressDownloadAttr = attrName "progressDownloading"


statusSEND, statusWAIT, statusWORK, statusDOWN, statusDONE, statusFAIL :: AttrName
statusSEND = attrName "statusSEND"
statusWAIT = attrName "statusWAIT"
statusWORK = attrName "statusWORK"
statusDOWN = attrName "statusDOWN"
statusDONE = attrName "statusDONE"
statusFAIL = attrName "statusFAIL"


queuedStatusTinting :: FileQueuedStatus -> Widget n -> Widget n
queuedStatusTinting = \case
    SEND -> withAttr statusSEND
    WAIT -> withAttr statusWAIT
    WORK -> withAttr statusWORK
    DOWN -> withAttr statusDOWN
    DONE -> withAttr statusDONE
    FAIL -> withAttr statusFAIL


drawTUI :: FrontEndState -> [Widget Name]
drawTUI (FrontEndState st) = case st of
    Left inputing -> drawInputingTUI inputing
    Right queueing -> drawQueueingTUI queueing


drawQueueingTUI :: FrontEndStateQueueing -> [Widget Name]
drawQueueingTUI st =
    let upload :: [FileQuery (Disk, FileDataProgress)]
        upload = queryBy st queryUploading

        polling :: [FileQuery (Bool, Option 'Http, Url 'Http)]
        polling = queryBy st queryPolling

        download :: [FileQuery FileDataProgress]
        download = queryBy st queryDownloading

        complete :: [FileQuery ()]
        complete = queryBy st queryComplete

        drawFileUpload :: FileQuery (Disk, FileDataProgress) -> Widget n
        drawFileUpload (FileQuery jobFile (_, dataRatio)) =
            let (note, tinting, percentage) = case dataRatio of
                    Done _ -> (WAIT, withAttr progressCompleteAttr, 1)
                    Fail _ _ -> (FAIL, withAttr progressIncompleteAttr, 1)
                    Part n d -> (SEND, withAttr progressIncompleteAttr, fromIntegral n / fromIntegral d)
            in  drawQueuedStatusBox note
                    . encaseFileRow
                    . tinting
                    $ progressBar (Just $ readableName jobFile) percentage

        drawFilePolling :: FileQuery (Bool, a, b) -> Widget n
        drawFilePolling (FileQuery jobFile (working, _, _)) =
            let note
                    | working = WORK
                    | otherwise = WAIT
                tinting = withAttr progressPollingAttr
            in  drawQueuedStatusBox note . encaseFileRow . tinting $ jobFileWidget jobFile []

        drawFileDownload :: FileQuery FileDataProgress -> Widget n
        drawFileDownload (FileQuery jobFile dataRatio) =
            let tinting = withAttr progressDownloadAttr
                (note, suffix) = case dataRatio of
                    Done _ -> (DONE, [])
                    Part bytes _ -> (DOWN, ["--", show (fromBytes bytes) `leftSpacedTo` 8])
                    Fail code msg -> (FAIL, ["--", "Status", "(" <> show code <> ")", msg])

                leftSpacedTo s i = let n = length s in replicate (max 0 (i - n)) ' ' <> s
            in  drawQueuedStatusBox note . encaseFileRow . tinting $ jobFileWidget jobFile suffix

        drawFileComplete :: FileQuery () -> Widget n
        drawFileComplete (FileQuery jobFile _) =
            let tinting = withAttr progressDownloadAttr
            in  drawQueuedStatusBox DONE . encaseFileRow . tinting $ jobFileWidget jobFile []

        jobFileWidget :: JobFile -> [String] -> Widget n
        jobFileWidget jobFile suffix =
            let strWords = [readableName jobFile] <> suffix
            in  str (unwords strWords) <+> fill ' '

        drawQueuedStatusBox queuedStatus x =
            let tiniting = queuedStatusTinting queuedStatus
                prefix = padLeftRight 1 . str $ show queuedStatus
            in  vLimit 1 $ hFoldWith vBorder [tiniting prefix, x]

        fileListing :: [Widget n]
        fileListing =
            fold
                [ drawFileUpload <$> upload
                , drawFilePolling <$> polling
                , drawFileDownload <$> download
                , drawFileComplete <$> complete
                ]

        fileCount = length fileListing

        drawFileListing :: Widget n
        drawFileListing = finalizeBox . vLimit (2 * fileCount - 1) $ vFoldWith hBorder fileListing

        jobIsComplete = null upload && null polling && null download

        continuedQueueing :: [Widget n]
        continuedQueueing =
            [ drawRefresh st
            , drawFileListing
            ]

        completedQueueing :: [Widget n]
        completedQueueing =
            [ drawJobCompleteBox
            , drawFileListing
            ]
    in  drawAppWidgetFromRegions $
            [drawSTREAM <+> drawQueueHelper]
                <> if jobIsComplete
                    then completedQueueing
                    else continuedQueueing


drawJobCompleteBox :: Widget n
drawJobCompleteBox =
    let shortNoteBox = padLeftRight 1 $ str "Stop"

        notificationBox =
            let msg = "Job Complete"
                x = length msg
                n = fromEnum (maxBound :: Tick)
                (q, r) = (n - x) `quotRem` 2
                lPad = q + r
                rPad = q
            in  str $
                    fold
                        [ replicate lPad ' '
                        , msg
                        , replicate rPad ' '
                        ]

        messageBox = encaseFileRow $ str "All Files Downloaded" <+> fill ' '
    in  finalizeBox . vLimit 1 $
            hFoldWith
                vBorder
                [ shortNoteBox
                , notificationBox
                , messageBox
                ]


drawQueueHelper :: Widget Name
drawQueueHelper =
    let boxLine (k, v) = keyBox k <+> valBox v

        keyMax = maximum $ length . fst <$> pairings
        keyPad x = x <> replicate (keyMax - length x) ' '
        keyBox x = padLeftRight 1 . hLimit keyMax . str $ keyPad x

        valMax = maximum $ length . snd <$> pairings
        valPad x = x <> replicate (valMax - length x) ' '
        valBox x = padLeftRight 1 . hLimit valMax . str $ valPad x

        pairings =
            fold
                [ helpQueueing
                ]

        helpQueueing = [("Escape:", "Exit")]

        helpBoxFormat = padTop (Pad 1) . vBox . fmap boxLine

        boxHelp = helpBoxFormat helpQueueing <=> fill ' '
    in  id
            . joinBorders
            . withBorderStyle unicodeBold
            . B.borderWithLabel (str "Input Controls")
            . freezeBorders
            . hLimit 47
            . vLimit 10
            . C.hCenter
            $ boxHelp


drawRefresh
    :: ( HasCurrentTick a
       , HasFileStore a a (FileProcessing x) (FileProcessing x)
       )
    => a
    -> Widget n
drawRefresh st =
    let drawRefreshLabel = padLeftRight 1 $ str "Poll"

        drawRefreshGauge =
            let tick = case st `queryBy` queryPolling of
                    [] -> toEnum 0
                    _ -> st ^. currentTick
            in  tickToStr tick

        tickToStr (Tick w) =
            let x = fromEnum w
                n = fromEnum (maxBound :: Tick)
            in  str $ replicate x '█' <> replicate (n - x) ' '
    in  finalizeBox . vLimit 1 $ drawRefreshLabel <+> vBorder <+> drawRefreshGauge


drawInputingTUI :: FrontEndStateInputing -> [Widget Name]
drawInputingTUI st =
    let barTitle =
            hBox
                [ drawSTREAM
                , drawHelper st
                ]
        barInput =
            hBox
                [ drawUserInput st
                , drawFileBrowser st
                ]
    in  drawAppWidgetFromRegions
            [ barTitle
            , barInput
            , drawChoosenFiles st
            , drawSendButton st
            ]


drawSTREAM :: Widget Name
drawSTREAM =
    let boxTitle =
            drawTitle
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
            in  hLimit 17 $ cap <+> sep <+> end

        drawLogo rows =
            let logo = padAll 2 $ vBox rows
                containment = vLimit 10 . hLimit 35
            in  containment $ fill ' ' <+> logo <+> fill ' '

        drawEnclosure =
            id
                . withBorderStyle unicodeBold
                . B.borderWithLabel (str "STREAM User Endpoint")

        drawTitle =
            id
                . drawEnclosure
                . drawLogo
                . fmap drawLine
    in  boxTitle


drawHelper :: FrontEndStateInputing -> Widget Name
drawHelper st =
    let boxLine (k, v) = keyBox k <+> valBox v

        keyMax = maximum $ length . fst <$> pairings st
        keyPad x = x <> replicate (keyMax - length x) ' '
        keyBox x = padLeftRight 1 . hLimit keyMax . str $ keyPad x

        valMax = maximum $ length . snd <$> pairings st
        valPad x = x <> replicate (valMax - length x) ' '
        valBox x = padLeftRight 1 . hLimit valMax . str $ valPad x

        pairings x =
            fold
                [ helpInfoConst x
                , helpInfoMetadata
                , helpInfoFileData x
                ]

        helpInfoConst x =
            [ ("Escape:", "Exit")
            , ("Shift+Enter:", "Send job")
            ]
                <> helpInfoDirection x

        helpInfoDirection x =
            let goFileData = ("Shift+Right:", "Switch to file selector")
                goMetadata = ("Shift+Left:", "Switch to job metadata")
                goSendTask = ("'|':", "Switch to job submission")
            in  case x ^. inputFocused of
                    PaneMetadata -> [goFileData, goSendTask]
                    PaneFileData -> [goMetadata, goSendTask]
                    PaneSendTask -> [goMetadata, goFileData]

        helpInfoMetadata =
            [ ("Tab:", "Next input field")
            , ("Shift+Tab:", "Previous input field")
            ]

        helpInfoFileData x =
            helpInfoFileErr x
                <> [ ("Enter:", "select file, change folder")
                   , ("Up/Down:", "scroll")
                   , ("/:", "search, escape to cancel")
                   ]

        helpInfoSendData =
            [ ("Enter:", "submit job for processing")
            ]

        helpInfoFileErr x =
            case FB.fileBrowserException $ x ^. fileSelector of
                Nothing -> []
                Just e -> [("Error:", displayException e)]

        helpBoxFormat = padTop (Pad 1) . vBox . fmap boxLine

        boxHelp x =
            let boxConstant = helpBoxFormat $ helpInfoConst x
                boxContext = helpBoxFormat $ case x ^. inputFocused of
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
            . withBorderStyle unicodeBold
            . B.borderWithLabel (str "Input Controls")
            . freezeBorders
            . hLimit 47
            . C.hCenter
            . boxPadding
            $ boxHelp st


drawUserInput :: FrontEndStateInputing -> Widget Name
drawUserInput st =
    let formBox =
            id
                . joinBorders
                . borderWithLabel (txt "Job Specification")
                . freezeBorders
                . padTop (Pad 1)
                . padLeftRight 2
                . vLimit 8
                . hLimit 31
                . (<+> fill ' ')
                . renderForm
                . (^. inputJobForm)
    in  formBox st


drawFileBrowser :: FrontEndStateInputing -> Widget Name
drawFileBrowser st =
    let browser =
            id
                . vLimit 11
                . hLimit 49
                . borderWithLabel (txt "File Selector")
                . FB.renderFileBrowser True
                . (^. fileSelector)
    in  browser st


drawSendButton :: FrontEndStateInputing -> Widget Name
drawSendButton st =
    let coloring :: Widget Name -> Widget Name
        coloring = case st ^. inputFocused of
            PaneSendTask -> withAttr readySubmitOuter
            _ -> id

        focusing = case st ^. inputFocused of
            PaneSendTask -> withAttr readySubmit
            _ -> id

        bordering = case st ^. inputFocused of
            PaneSendTask -> withBorderStyle unicodeBold
            _ -> id
    in  id
            . hLimit 86
            . hCenter
            . vLimit 5
            . hLimit 24
            . freezeBorders
            . bordering
            . border
            . coloring
            . padLeftRight 4
            . coloring
            . padTopBottom 1
            . focusing
            $ str " S U B M I T "


drawDiskBox :: Disk -> Widget n
drawDiskBox disk =
    let maxDiskStrLen = 12
    in  hLimit maxDiskStrLen $ fill ' ' <+> str (show disk)


drawChoosenFiles :: FrontEndStateInputing -> Widget Name
drawChoosenFiles st =
    let fileLine (jobFile, disk) =
            let boxDisk = padLeftRight 1 $ drawDiskBox disk
                boxPath = padRight (Pad 1) . str $ readableName jobFile
            in  vLimit 1 $ boxDisk <+> boxPath <+> fill ' '

        sizing [] = [hCenter $ str "( None )"]
        sizing xs = xs

        fileList =
            id
                . hLimit 86
                . borderWithLabel (txt "Choosen Files")
                . freezeBorders
                . padTopBottom 1
                . vBox
                . sizing
                . fmap fileLine
                . fileStoreList
                . (^. fileStore)
    in  fileList st


drawAppWidgetFromRegions :: [Widget n] -> [Widget n]
drawAppWidgetFromRegions = pure . padAll 1 . C.hCenter . vBox


finalizeBox :: Widget n -> Widget n
finalizeBox = joinBorders . freezeBorders . border


hFoldWith :: (Foldable f) => Widget n -> f (Widget n) -> Widget n
hFoldWith sep = foldr1 (<+>) . intersperse sep . toList


vFoldWith :: (Foldable f) => Widget n -> f (Widget n) -> Widget n
vFoldWith sep = foldr1 (<=>) . intersperse sep . toList
