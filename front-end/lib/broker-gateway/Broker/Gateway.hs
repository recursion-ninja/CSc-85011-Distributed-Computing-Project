{-# Language DataKinds #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}

module Broker.Gateway
  ( Gateway()
  , gatewayEndpoint
  ) where

import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.Text (pack, stripSuffix)
import Network.HTTP.Req
import Text.Read (get, look, pfail, readPrec, readListPrec, readListPrecDefault)
import Text.URI (mkURI)


newtype Gateway = Gateway (Url 'Http, Option 'Http)


instance Eq Gateway where

    (Gateway (url1, _)) == (Gateway (url2, _)) = url1 == url2


instance Ord Gateway where

    (Gateway (url1, _)) `compare` (Gateway (url2, _)) = url1 `compare` url2


instance Read Gateway where

    {-# INLINABLE readPrec #-}
    readPrec = do
        input <- look
        let token = takeWhile (not . isSpace) input
        _ <- replicateM (length token) get
        uri <- either (fail . show) pure . mkURI $ pack input
        maybe pfail (pure . Gateway) $ useHttpURI uri

    readListPrec = readListPrecDefault


instance Show Gateway where

    show (Gateway (url, _)) = show url


gatewayEndpoint :: Gateway -> (Url 'Http, Option 'Http)
gatewayEndpoint (Gateway (url, opt)) =
    let reqOpt :: Option 'Http
        reqOpt = fold
            [ opt
--            , header "Cache-Control" "no-cache"
--            , header "Pragma" "no-cache"
            , header "User-Agent" "STREAM TUI Client"
            , httpVersion 1 1
            ]

        reqURL :: Url 'Http
        reqURL = case "/api" `stripSuffix` renderUrl url of
            Just _  -> url
            Nothing -> url /: "api"

    in  (reqURL, reqOpt)
