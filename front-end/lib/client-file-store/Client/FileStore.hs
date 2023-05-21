{-# Language DataKinds #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language Strict #-}
{-# Language TypeFamilies #-}

module Client.FileStore
  ( -- * FileStore
    FileStore(..)
    -- ** Constructor
  , fileStoredWith
  , fileStoreFromPaths
    -- ** Getter
  , fileStoreQuery
  , fileStoreList
--  , fileStoreRelativize
    -- ** Mutator
  , fileAdjustBy
    -- ** Querying
  , FileQuery(..)
  , getQueriedFile
    -- *
  , JobFile()
    -- ** Constructor
  , fromFullPath
    -- ** Getters
  , originalPath
  , tayloredName
  , readableName
    -- * FileProcessing
    -- ** Data-types
  , FileDataProgress(..)
    -- ** Constructors
  , FileProcessing()
  , fileHostedBy
  , fileUploadTo
  , filePollWith
  , fileNowGoing
  , fileNowReady
  , fileDownload
  , fileAddBytes
  , fileComplete
  , fileFailWith
  , incrementDataProgress

  , queryUploadReady
  , queryUploading
  , queryPolling
  , queryDownloadReady
  , queryDownloading
  , queryComplete

  ) where

import Broker.Disk (Disk)
import Data.Char (isSpace)
import Data.Foldable (asum, fold)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map.Strict (Map, (!?), adjust, foldrWithKey', keysSet, singleton)
import Data.Map.Merge.Strict (SimpleWhenMatched, merge, preserveMissing', zipWithMatched)
import Data.Text (Text)
import GHC.Exts
import Network.HTTP.Req
import System.FilePath


newtype FileStore a = FileStore (Map JobFile a)
    deriving newtype (Eq, Functor, Ord, Show)


data FileDataProgress
   = Done Word
   | Part Word Word
   | Fail Int String
   deriving stock (Eq, Ord, Show)


data FileProcessingState x
    = Upload   {-# UNPACK #-} Disk (Maybe x)
    | Polling  {-# UNPACK #-} Word Bool
    | Download {-# UNPACK #-} Word (Maybe x)
    | Complete
    deriving stock (Eq, Ord, Show)


data  FileProcessing x
    = FileProcessing {-# UNPACK #-} Text (FileProcessingState x)
    deriving stock (Eq, Ord, Show)


data FileQuery x = FileQuery JobFile x


{- |
Nice wrapper for files utilized in Science Broker Jobs.

This type contains the follwing:

  1. The full filepath on the local TUI client's system
  2. The filename, useful for nice rendering
  3. The /taylored/ filename, replacing all space characters with underscores

The last stored object in necissary as the Science Broker service forbits each
filename from containing /any spaces!/
-}
data  JobFile
    = JobFile
    { originalPath :: FilePath
    , readableName :: FilePath
    , tayloredName :: FilePath
    }


instance Eq (FileQuery x) where

    (FileQuery x _) == (FileQuery y _) = x == y


deriving stock instance Eq JobFile


instance Ord (FileQuery x) where

    (FileQuery x _) `compare` (FileQuery y _) = x `compare` y


deriving stock instance Ord JobFile


instance IsList (FileStore a) where

    type Item (FileStore a) = (JobFile, a)

    fromList = FileStore . fromList

    toList (FileStore m) = toList m


instance Monoid (FileStore a) where

    mempty = FileStore mempty


instance Semigroup (FileStore a) where

    (FileStore x) <> (FileStore y) =
        let new :: SimpleWhenMatched JobFile a a a
            new = zipWithMatched $ \_ _ v -> v
        in  FileStore $ merge preserveMissing' preserveMissing' new x y


deriving stock instance Show JobFile


fromFullPath :: FilePath -> FilePath -> JobFile
fromFullPath workingDir path =
    let replaceSpace c
            | isSpace c = '_'
            | otherwise = c

        fileName = takeBaseName path <> takeExtension path

    in  JobFile
        { originalPath = path
        , readableName = makeRelative workingDir path
        , tayloredName = replaceSpace <$> fileName
        }


fileHostedBy :: Text -> Disk -> FileProcessing x
fileHostedBy host size = FileProcessing host $ Upload size Nothing


fileUploadTo :: x -> FileProcessing x -> FileProcessing x
fileUploadTo x input@(FileProcessing host state) = case state of
    Upload size _ -> FileProcessing host $ Upload size $ Just x
    _ -> input


filePollWith :: Word -> FileProcessing x -> FileProcessing x
filePollWith num input@(FileProcessing host state) = case state of
    Upload _ (Just _) -> FileProcessing host $ Polling num False
    Polling _ b       -> FileProcessing host $ Polling num b
    _ -> input


fileNowGoing :: FileProcessing x -> FileProcessing x
fileNowGoing input@(FileProcessing host state) = case state of
    Polling num _ -> FileProcessing host $ Polling num True
    _ -> input


fileNowReady :: FileProcessing x -> FileProcessing x
fileNowReady input@(FileProcessing host state) = case state of
    Polling num _ -> FileProcessing host $ Download num Nothing
    _ -> input


fileDownload :: x -> FileProcessing x -> FileProcessing x
fileDownload x input@(FileProcessing host state) = case state of
    Download num _ -> FileProcessing host $ Download num $ Just x
    _ -> input


fileAddBytes :: (x -> x) -> FileProcessing x -> FileProcessing x
fileAddBytes f input@(FileProcessing host state) = case state of
    Download num (Just x) -> FileProcessing host $ Download num . Just $ f x
    _ -> input


fileComplete :: FileProcessing x -> FileProcessing x
fileComplete input@(FileProcessing host state) = case state of
    Download _ (Just _) -> FileProcessing host Complete
    _ -> input


fileFailWith :: x -> FileProcessing x -> FileProcessing x
fileFailWith x input@(FileProcessing host state) = case state of
    Upload   size _ -> FileProcessing host . Upload   size $ Just x
    Download num  _ -> FileProcessing host . Download num  $ Just x
    _ -> input


incrementDataProgress :: Integral i => i -> FileDataProgress -> FileDataProgress
incrementDataProgress addend (Part num den) = Part (fromIntegral addend + num) den
incrementDataProgress _ x = x


queryUploadReady :: FileStore (FileProcessing x) -> Set (FileQuery (Disk, Option 'Http, Url 'Http))
queryUploadReady (FileStore m) =
    let partitioning
          :: JobFile
          -> FileProcessing x
          -> [FileQuery (Disk, Option 'Http, Url 'Http)]
          -> [FileQuery (Disk, Option 'Http, Url 'Http)]
        partitioning path (FileProcessing host state) acc = case state of
          Upload size Nothing -> [ FileQuery path (size, baseOpts, baseURL host /: "files") ] <> acc
          _ -> acc
    in  fromList $ foldrWithKey' partitioning mempty m


queryUploading :: FileStore (FileProcessing x) -> Set (FileQuery (Disk, x))
queryUploading (FileStore m) =
    let partitioning
          :: JobFile
          -> FileProcessing b
          -> [FileQuery (Disk, b)]
          -> [FileQuery (Disk, b)]
        partitioning path (FileProcessing _ state) acc = case state of
          Upload size (Just x) -> [ FileQuery path (size, x) ] <> acc
          _ -> acc
    in  fromList $ foldrWithKey' partitioning mempty m


queryPolling :: FileStore (FileProcessing x) -> Set (FileQuery (Bool, Option 'Http, Url 'Http))
queryPolling (FileStore m) =
    let partitioning
          :: JobFile
          -> FileProcessing x
          -> [FileQuery (Bool, Option 'Http, Url 'Http)]
          -> [FileQuery (Bool, Option 'Http, Url 'Http)]
        partitioning path (FileProcessing host state) acc = case state of
          Polling num work -> [ FileQuery path (work, baseOpts, baseURL host /: "status" /~ num) ] <> acc
          _ -> acc
    in  fromList $ foldrWithKey' partitioning mempty m


queryDownloadReady :: FileStore (FileProcessing x) -> Set (FileQuery (Option 'Http, Url 'Http))
queryDownloadReady (FileStore m) =
    let partitioning
          :: JobFile
          -> FileProcessing x
          -> [FileQuery (Option 'Http, Url 'Http)]
          -> [FileQuery (Option 'Http, Url 'Http)]
        partitioning path (FileProcessing host state) acc = case state of
          Download num Nothing -> [ FileQuery path (baseOpts, baseURL host /: "files" /~ num) ] <> acc
          _ -> acc
    in  fromList $ foldrWithKey' partitioning mempty m


queryDownloading :: FileStore (FileProcessing x) -> Set (FileQuery x)
queryDownloading (FileStore m) =
    let partitioning :: JobFile -> FileProcessing x -> [FileQuery x] -> [FileQuery x]
        partitioning path (FileProcessing _ state) acc = case state of
          Download _ (Just x) -> [ FileQuery path x ] <> acc
          _ -> acc
    in  fromList $ foldrWithKey' partitioning mempty m


queryComplete :: FileStore (FileProcessing x) -> Set (FileQuery ())
queryComplete (FileStore m) =
    let partitioning :: JobFile -> FileProcessing x -> [FileQuery ()] -> [FileQuery ()]
        partitioning path (FileProcessing _ state) acc = case state of
          Complete -> [ FileQuery path () ] <> acc
          _ -> acc
    in  fromList $ foldrWithKey' partitioning mempty m


baseOpts :: Option 'Http
baseOpts = fold
    [ header "User-Agent" "STREAM TUI Client"
    , httpVersion 1 1
    , port 8080
    ]


baseURL :: Text -> Url 'Http
baseURL host = http host /: "api"


getQueriedFile :: FileQuery x -> JobFile
getQueriedFile (FileQuery path _) = path


fileStoreQuery :: FileStore a -> FilePath -> Maybe (FileQuery a)
fileStoreQuery (FileStore m) fileDesc =
    let fileJobSet     = keysSet m
        isOriginalPath = (fileDesc ==) . originalPath
        isReadableName = (fileDesc ==) . readableName
        isTayloredName = (fileDesc ==) . tayloredName
        keysSuchThat f = toList $ Set.filter f fileJobSet
        lookingUpKey k = FileQuery k <$> m !? k
        
        queryConsidering f = case keysSuchThat f of
            []   -> Nothing
            k:_ -> lookingUpKey k

    in  asum $ queryConsidering <$>
            [ isOriginalPath
            , isReadableName
            , isTayloredName
            ]


fileStoreList :: FileStore a -> [(JobFile, a)]
fileStoreList (FileStore m) = toList m


{-
fileStoreRelativize :: FilePath -> FileStore a -> FileStore a
fileStoreRelativize workingPath (FileStore m) = FileStore $ makeRelative workingPath `mapKeys` m
-}

fileStoreFromPaths :: FilePath -> FilePath -> a -> FileStore a
fileStoreFromPaths wDir path = FileStore . singleton (fromFullPath wDir path)


fileStoredWith :: JobFile -> a -> FileStore a
fileStoredWith jobFile = FileStore . singleton jobFile


fileAdjustBy :: JobFile -> (a -> a) -> FileStore a -> FileStore a
fileAdjustBy path f (FileStore m) = FileStore $ adjust f path m
