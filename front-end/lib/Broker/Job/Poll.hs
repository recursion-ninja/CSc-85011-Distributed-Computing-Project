{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

module Broker.Job.Poll
  ( -- * JobInput
    JobPolledStatus(..)
  ) where


import Data.Aeson.Types
import Data.Char (toUpper)
import Data.Text (unpack)
import GHC.Generics
import Text.Read (Lexeme(Ident), lexP, pfail, readPrec)


data  JobPolledStatus
    = Waiting
    | Processing
    | Finished


deriving stock instance Eq JobPolledStatus


deriving stock instance Generic JobPolledStatus


deriving stock instance Ord JobPolledStatus


deriving stock instance Show JobPolledStatus


instance Read JobPolledStatus where

    {-# INLINABLE readPrec #-}
    readPrec = do
        Ident str <- lexP
        case str of
            "WAITING"    -> pure Waiting
            "PROCESSING" -> pure Processing
            "FINISHED"   -> pure Finished
            _ -> pfail


instance FromJSON JobPolledStatus where

    parseJSON = withText "JobPolledStatus" $ \txt -> case toUpper <$> unpack txt of
        "WAITING"    -> pure Waiting
        "PROCESSING" -> pure Processing
        "FINISHED"   -> pure Finished
        str          -> fail $ "Unrecognized status: '" <> str <> "'"


instance ToJSON JobPolledStatus where

    toJSON = \case
        Waiting    -> String "WAITING"
        Processing -> String "PROCESSING"
        Finished   -> String "FINISHED"

    toEncoding = genericToEncoding defaultOptions


{-
pollJobPolledStatus :: Manager -> Gateway -> Word -> IO JobPolledStatus
pollJobPolledStatus manager gateway jobID =
    let 
        request connection = connection
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode requestObject
            }
        handler :: Response BodyReader -> IO ∅
        handler response =
            putStrLn $ "The status code was: " <> (show $ statusCode $ responseStatus response)

    in  do  initialRequest <- parseRequest $ show gateway <> "/api/status/" <> show jobID
            traceToFile "Gateway" $ initialRequest
--            traceToFile "JSON object to send" . BS.unpack . encodePretty $ toJSON $ buildJobSpecification files input
--            print . BS.unpack . encodePretty . toJSON $ buildJobSpecification files input
--            print (request initialRequest)
            withResponse (request initialRequest) manager handler


requestBulletinComponent :: (BulletinComponent a, FromJSON a) => Specification a -> IO a
requestBulletinComponent jobID =
    let getURL :: Req (JsonResponse a)
        getURL x = req GET (componentURL x) NoReqBody jsonResponse reqOpt

        reqOpt :: Option 'Https
        reqOpt = fold
            [ header "Cache-Control" "no-cache"
            , header "Pragma" "no-cache"
            , header "User-Agent" "Personal Daily Bulletin Spider -- Weather Thread"
            , httpVersion 2 0
            ]

    in  fmap responseBody . runReq defaultHttpConfig . getURL
-}
