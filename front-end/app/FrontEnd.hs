{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}

module Main
  ( main
  ) where

import Data.Bits (Bits(..), FiniteBits(..))
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Functor
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set (Set, fromList, toAscList)
import Data.Tuple (swap)
import Data.Word
--import GHC.Exts (fromList)
--import Numeric (showIntAtBase)
import Text.Printf


import Brick hiding (Direction)
import Brick.BChan (newBChan, writeBChan)
import Brick.Forms
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as BS
import Brick.Widgets.Center qualified as C

import Broker.Job.Input
import Broker.Job.Specification

import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay)
--import Control.Concurrent.STM.TVar (readTVar)
--import Control.Monad.STM (atomically)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Graphics.Vty qualified as V
--import Data.Sequence (Seq)
--import qualified Data.Sequence as S
--import Linear.V2 (V2(..))
--import Lens.Micro ((^.))

import Data.IORef
import Lens.Micro (Lens', lens)
import System.IO
import System.IO.Unsafe

import System.Posix.Signals


-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick


-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
--type Name = ()
data  Name
    = InputField_Mail
    | InputField_Time
    | InputField_Disk
    | InputField_RAM
    | InputField_CPUs
    | InputField_GPUs
     deriving (Eq, Ord, Show)


data  FocusedField


data  FrontEndTermState
    = FrontEndTermState
    { inputJob   :: UserInputJob
--    , inputFocus :: FocusedField
    }


data  BinarySI
    = Kibi
    | Mibi
    | Gibi
    | Tibi
    | Pibi
    | Eibi
    | Zibi
    | Yibi


data  DecimalSI
    = Kilo
    | Mega
    | Giga
    | Tera
    | Peta
    | Exa
    | Zetta
    | Yotta


main :: IO ()
main = do
    chan <- newBChan 10
    void . forkIO . forever $ do
        writeBChan chan Tick
        threadDelay 100000000
    let builder = V.mkVty V.defaultConfig
    initialVty <- builder
--    void $ customMain initialVty builder (Just chan) app' initState
    void $ customMain initialVty builder Nothing frontendApp initState


initState :: FrontEndTermState
initState = FrontEndTermState userInputEmptyJob


frontendApp :: App FrontEndTermState Tick Name
frontendApp = App
    { appAttrMap      = const theMap
    , appChooseCursor = showFirstCursor
    , appDraw         = drawTUI
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
    }


jobSpecificationForm :: UserInputJob -> Form UserInputJob e Name
jobSpecificationForm =
    let valueBox = withBorderStyle BS.unicodeBold . hLimit 42
        label s w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 6 $ str (s<>":") <+> fill ' ') <+> valueBox w
    in  newForm
            [ label "Mail" @@= editTextField jobMail InputField_Mail (Just 1)
            , label "Time" @@= editShowableField (jobTime :: Lens' UserInputJob (UserInput Word64)) InputField_Time
            , label "Disk" @@= editShowableField (jobDisk :: Lens' UserInputJob (UserInput Word64)) InputField_Disk
            , label "RAM"  @@= editShowableField (jobRAM  :: Lens' UserInputJob (UserInput Word64)) InputField_RAM
            , label "CPUs" @@= editShowableField (jobCPUs :: Lens' UserInputJob (UserInput Word64)) InputField_CPUs
            , label "GPUs" @@= editShowableField (jobGPUs :: Lens' UserInputJob (UserInput Word64)) InputField_GPUs
--            , editNumericField jobTime InputField_Time
--            , editNumericField jobDisk InputField_Disk
--            , editNumericField jobRAM  InputField_RAM
--            , editNumericField jobCPUs InputField_CPUs
--            , editNumericField jobGPUs InputField_GPUs
            ]


drawTUI :: FrontEndTermState -> [Widget Name]
drawTUI st = pure . C.center $ vBox
    [ drawPreamble
    , drawUserInput st
    ]


drawPreamble :: Widget Name
drawPreamble =
    let drawLine  = hLimit 16 . str
        drawTitle = padTopBottom 2
          . withBorderStyle BS.unicodeBold
          . B.borderWithLabel (str "STREAM User Endpoint")
          . hLimit 26
          . C.hCenter
          . padTopBottom 1
          . vBox
          . fmap drawLine
    in  padTop (Pad 2) $ drawTitle
            [ "S - Simple"
            , "T - Transparent"
            , "R - Resource"
            , "E - Exchange"
            , "A - And"
            , "M - Management"
            ]
          


drawUserInput :: FrontEndTermState -> Widget Name
drawUserInput = id 
    . hLimit 48
    . C.hCenter
    . padTopBottom 1
    . renderForm
    . jobSpecificationForm
    . inputJob


{-
drawState :: PrintfArg b => SketchState b -> Widget Name
drawState st =
    let drawIndex :: PrintfArg b => b -> Widget Name
        drawIndex = withBorderStyle BS.unicodeBold
          . B.borderWithLabel (str "Stored")
          . C.hCenter
          . padAll 1
          . str . printf shapeIndexformatStr

        drawShift :: PrintfArg b => b -> Widget Name
        drawShift = withBorderStyle BS.unicodeBold
          . B.borderWithLabel (str "Shifts")
          . C.hCenter
          . padAll 1
          . str . printf shapeIndexformatStr

    in  hLimit 24 $ vBox
         [ drawIndex $ sketchIndex st
         , padTop (Pad 2) $ drawShift $ sketchShift st
         ]


drawGrid :: FiniteBits b => SketchState b -> Widget Name
drawGrid st =
    let focus = fromEnum $ hoveringAt st

        renderQueriedBit False = "0"
        renderQueriedBit True  = "1"
        
        renderFiniteBitCells :: FiniteBits b => b -> [Widget n]
        renderFiniteBitCells b = drawCell b <$> [ 0 .. finiteBitSize b - 1 ]

--        cellWidget :: FiniteBits b => b -> String
--        cellWidget i = 

        drawCell :: Bits p => p -> Int -> Widget n
        drawCell b i =
            let c :: Widget n
                c = str . renderQueriedBit $ testBit b i

                f :: AttrName -> Widget n
                f = flip withAttr c
            in  if   i == focus
                then f focusAttr
                else f emptyAttr

    in  withBorderStyle BS.unicodeBold
-}

handleEvent :: BrickEvent Name Tick -> EventM Name FrontEndTermState ()
--handleEvent (AppEvent Tick)                      = pure () --continue $ step g
--handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
--handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
--handleEvent (VtyEvent (V.EvKey V.KLeft []))       = modify (move dirLeft)
--handleEvent (VtyEvent (V.EvKey V.KRight []))      = modify (move dirRight)
--handleEvent (VtyEvent (V.EvKey V.KEnter []))      = modify commit
--handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = modify toggle *> (get >>= paint)
--handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
--handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
--handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East  g
--handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West  g
--handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = put initState
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleEvent _                                    = pure ()


-- Drawing

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (focusAttr, (V.red `on` V.white) `V.withStyle` V.bold)
  , (focusedFormInputAttr, (V.red `on` V.white) `V.withStyle` V.bold)
  ]


emptyAttr :: AttrName
emptyAttr = attrName "NormalIndex"


focusAttr :: AttrName
focusAttr = attrName "FocusedIndex"


{-
-- x : number you want rounded, n : number of decimal places you want...
roundToDigits :: Int -> Double -> Double
roundToDigits n x = (fromIntegral (floor (x * t))) / t
    where t = 10 ^ n


buildFractal :: Generator Double -> QDiagram B V2 Double Any
buildFractal gen =
    let genPrefix = showGenerator gen # scale (1 / 3)
        imgSuffix = arrangeTransforms gen :: Diagram B
    in vcat [genPrefix, imgSuffix ]


defineOutputDomain :: [Word64]
defineOutputDomain =
  let begin  = 0 :: Word64
--      stop1  = (2 ^ 6) - 1
      stop2  = (2 ^ 5) - 1
      lower  = [ bestMask ]
      upper  = (`shiftL` 21) <$> [ begin .. stop2 ]
  in  liftA2 (.|.) upper lower


outputFractalTo :: FilePath -> Diagram B -> IO ()
outputFractalTo path =
          let dist = 1024
              spec = dims2D dist dist
          in  renderSVG path spec

{-
outputFractalTo :: Bits b => FilePath -> b -> IO ()
outputFractalTo path i =
          let dist = 1024
              draw = buildFractal $ sliceSpikeBy i
              spec = dims2D dist dist
          in  renderSVG path spec draw
-}

outputFractal :: (Bits b, PrintfArg b) => b -> IO ()
outputFractal = outputFractalTo
    <$> (printf "slice-spike-%09x.svg")
    <*> (buildFractal . sliceSpikeBy)


outputManyFractals :: (Bits b, Foldable f, PrintfArg b) => f b -> IO ()
outputManyFractals = traverse_ outputFractal


{-
The above is enough to generate a Heighway dragon of arbitrary level of detail, but let's go a little further to show the relation of successive curves in the sequence.

`withPrevious` combines each diagram in a list with a shadow of the previous one.
-}

withPrevious :: (Monoid c, HasStyle c) => Bool -> [c] -> [c]
withPrevious focusPrev diagrams =
    let opacities :: (Double, Double)
        opacities = (1.0, 0.2)

        selection :: (a, a) -> (a, a)
        selection
          | focusPrev = id
          | otherwise = swap

        (prev, curr) = selection opacities
        diedDiagrams = (diagrams #) . opacity
    in  zipWith (<>)
            (diedDiagrams curr)
            (mempty : diedDiagrams prev)

{-
We remember the order of the diagrams by giving them names, so that we can lay them out and then show the order with arrows.
-}

rememberOrder :: [Diagram B] -> [Diagram B]
rememberOrder = zipWith named [0 :: Int ..]


showOrder :: Diagram B -> Diagram B
showOrder diagram =
    let addArrow :: Int -> QDiagram B V2 Double Any -> QDiagram B V2 Double Any
        addArrow n = connectOutside' opts n (n + 1)

        opts = with & gaps .~ normalized (0.005 :: Double)
                    & headLength .~ tiny

    in  diagram # applyAll (map addArrow [ 0 .. length (names diagram) ])

{-
Finally, we put all of the above together, with some layout tricks to make the diagrams and arrows align properly. `gridSnake` lays out the diagrams in a "snaking" grid, so that each diagram is adjacent to the previous one.
-}

arrangeTransforms :: Generator Double -> QDiagram B V2 Double Any
arrangeTransforms gen =
    let generatedFrames :: [Trail' Line V2 Double]
        generatedFrames = iterGeneratorWithScaling (1 / 4) gen

        renderedDiagram :: TrailLike c => Trail' l (V c) (N c) -> c
        renderedDiagram = trailLike . (`at` origin) . Trail

--        makeHalfOpacity :: HasStyle x => x -> x
--        makeHalfOpacity = (# opacity 0.5)

    in  generatedFrames
            # map renderedDiagram
            # withPrevious False
            # take 4
            # sameBoundingRect
            # rememberOrder
            # map (frame 0.1)
            # gridSnake
            # showOrder
            # lw ultraThin

data  Candidate
    = Candidate
    { _candidateIndex  :: ShapeIndex
    , _candidatePoints :: Set (P2 Double)
    }


instance Show Candidate where

    show (Candidate (ShapeIndex i) ps) =
        let indent = ("  " <>)
            prefix = "Index: " <> printf shapeIndexformatStr i
            suffix = indent <$> case toAscList ps of
              []    -> pure $ "{ None }"
              x:[] -> pure $ "{ " <> show x <> " }"
              x:xs -> pure "{" <> (show <$> x:xs) <> pure "}"
        in  unlines $ prefix : suffix


forgeShuriken :: Generator Double -> QDiagram B V2 Double Any
forgeShuriken =
    let makeImage :: Trail V2 Double -> QDiagram B V2 Double Any
        makeImage = (# lw ultraThin) . trailLike . (`at` origin)
    in  makeImage . flip shurikenUsing 4


paint :: (Bits b, MonadIO m) => SketchState b -> m ()
paint =
    let rendering :: Bits i => i -> QDiagram B V2 Double Any
        rendering i = vcat
            [ buildFractal  $ sliceSpikeBy i
            , forgeShuriken $ sliceSpikeBy i
            ]
    in  liftIO . outputFractalTo fractalFilePath . rendering . candidate


bestMask :: Integral i => i

-- arrangement12
--bestMask = 0x100a

-- arrangement11
bestMask = 0x00000000c56c
--0xc149
--bestMask = 0x000000008173

-- arrangement10
--bestMask = 0x00c2a8a0020c
--bestMask = 0x00c7acb00208

-- arrangement9
--bestMask = 0x0352e7ca0003

-- arrangement7
--bestMask = 0x0352e73c9586
--bestMask = 0x0352e72c9586
--bestMask = 0x0352e73d8187
--bestMask = 0x0352e73d9587

--bestMask = 0x0352e63c9587
--bestMask = 0x0352e8999555
--bestMask = 0x0352ea699555
--bestMask = 0x0352ea68d555
--bestMask = 0x0352e328c000
--bestMask = 0x0352e3688000
--bestMask = 0x0352e0dcf0c0
--bestMask = 0x000101837
--bestMask = 0x000101C37
--bestMask = 0x000100C37
--bestMask = 0x1e007d610
--bestMask = 0x3c05f9f3365
--bestMask = 0x3c05f981507
--bestMask = 0x3c475f55577
--bestMask = 0x0352e08c2000
--bestMask = 0x0352e08e980a
--bestMask = 0x0352e082900f
--bestMask = 0x0352e081a008
--bestMask = 0x0352e082a500
--bestMask = 0x0352e0df30c0


main :: IO ()
main =
    let x = 1
    in case x of
       0 -> main0
       1 -> main1
       _ -> main2


main0 :: IO ()
main0 = outputFractalTo fractalFilePath $ forgeShuriken sliceSpikeSeries4
    -- outputManyFractals $ [ bestMask :: Word64 ]


main2 :: IO ()
main2 =
  let
{-
      intersections :: Located (Trail V2 Double) -> [Point V2 Double]
      intersections t =
          let -- ps :: _
              ps = trailPoints t
          in  ps
-}

{-
      pointsToSVG :: Candidate -> IO ()
      pointsToSVG =
         let pntCirc  :: Trail V2 Double -- V2 Double
             pntCirc  = unitCircle

             pointDot :: Point V2 Double -> QDiagram B V2 Double Any -- P2 Double -> _
             pointDot = trailLike . at (scale (1 / 72) pntCirc)

             pointImg :: [Point V2 Double] -> QDiagram B V2 Double Any -- [P2 Double] -> Diagram B
             pointImg = foldMap pointDot

         in  outputFractalTo "slice-spike.svg" . pointImg . toAscList . _candidatePoints
-}
{-
      locate =
          let check = null . _candidatePoints
          in  getFirst . foldMap (First . Just) . filter check
-}
      domain :: [Word]
      domain = [ 0x000000005d4b .. 2 ^ 36 ]
--                    let i = (<> " ") . show . fromEnum $ _candidateIndex c
--                        b =
--                    in  hPutStr stdout i *> hFlush stdout $> b


  in  performSearch domain   -- pointsToSVG =<< considerIndex (bestMask :: Word)



performSearch :: (Bits i, Integral i) => [i] -> IO ()
performSearch =
    let ponder :: (Bits i, Integral i) => i -> Arg Int Candidate
        ponder = (Arg <$> length . _candidatePoints <*> id) . considerIndex
        update :: (Bits p, Integral p) => IORef (Arg Int Candidate) -> p -> IO ()
        update ref key =
            let val@(Arg len _) = ponder key
            in  writeIORef ref val *> print len
    in  \case
            [] -> putStrLn "( None )"
            x:xs -> do
                tID <- myThreadId
                currentMinima <- newIORef $ ponder x
                let printMin :: IO ()
                    printMin = readIORef currentMinima >>= print

                let handler :: IO ()
                    handler = printMin *> killThread tID

                let go :: (Bits i, Integral i) => [i] -> IO ()
                    go cs = do
                        minima <- readIORef currentMinima
                        case dropWhile ((>= minima) . ponder) cs of
                          [] -> pure ()
                          m:ms -> update currentMinima m *> go ms

                void $ installHandler keyboardSignal (Catch handler) Nothing
                putStrLn "Searching ..."
                update currentMinima x
                go xs *> printMin


--            traverse_ (print <=< considerIndex) [ bestMask :: Word ]


considerIndex :: (Bits i, Integral i) => i -> Candidate
considerIndex =
    let genTrailT2 :: Bits i => i -> Located (Trail V2 Double)
        genTrailT2 = (`at` origin) . Trail . (!! 2) . iterGeneratorWithScaling (1 / 4) . sliceSpikeBy

        intersectionPoints :: Located (Trail V2 Double) -> Set (P2 Double)
        intersectionPoints = foldMap intersectionCheck . segmentPairings

        --intersectionPoints :: MonadIO m => Located (Trail V2 Double) -> m (Set (P2 Double))
        --intersectionPoints = fmap fold . zipWithM intersectionCheck [ 0 .. ] . segmentPairings

        -- trailLocSegments :: Located (Trail v n) -> [Located (Segment Closed v n)]
        -- fromLocSegments  :: TrailLike t => Located [Segment Closed (V t) (N t)] -> t

        intersectionCheck
            :: ([Located (Segment Closed V2 Double)], Located (Segment Closed V2 Double))
            -> (Set (P2 Double))
        intersectionCheck (prevSegs, currSeg) =
            let prevTrail = fromLocSegments . (`at` origin) $ unLoc <$> prevSegs
                currTrail = fromLocSegments $ (:[]) `mapLoc` currSeg
  --              makeWithOpacity val tr = trailLike tr # opacity val
  --              outputPath = "slice-spike-" <> show i <> ".svg"
  --              diagramFig = makeWithOpacity 0.2 prevTrail <> makeWithOpacity 0.6 currTrail
                --opIO = outputFractalTo outputPath
            in   --liftIO $ pure diagramFig
                fromList $ fmap (roundToDigits 5) <$> intersectPointsT prevTrail currTrail

        segmentPairings
            :: ( Floating n, Ord n )
            => Located (Trail V2 n)
            -> [ ( [Located (Segment Closed V2 n)], Located (Segment Closed V2 n) ) ]
        segmentPairings trail =
            let go :: ([a] -> [a]) -> NonEmpty a -> [([a], a)]
                go pref (x1:|xss) = case xss of
                  []     -> []
                  x2:xs ->
                      let next = go (pref [x1] <>) $ x2:|xs
                          curr = (pref [], x2)
                      in  curr : next

            in  case trailLocSegments trail of
                  [] -> []
                  x:xs -> go id $ x:|xs

    in   Candidate
            <$> (toEnum . fromIntegral)
            <*> (intersectionPoints . genTrailT2)
-}
