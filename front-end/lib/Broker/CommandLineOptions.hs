{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}

module Broker.CommandLineOptions
  ( CommandLineOptions(..)
  , Gateway()
  , commandLineOptions
  , getCommandLineOptions
  ) where

import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Foldable
import Network.URI
import Options.Applicative
import System.Environment (getProgName)
import Text.Read (get, look, pfail, readPrec, readListPrec, readListPrecDefault)


data  CommandLineOptions
    = CommandLineOptions
    { gateway :: Gateway
    }


newtype Gateway = Gateway URI


deriving newtype instance Eq Gateway


deriving newtype instance Ord Gateway


deriving newtype instance Show Gateway


instance Read Gateway where

    {-# INLINABLE readPrec #-}
    readPrec = do
        input <- look
        let token = takeWhile (not . isSpace) input
        _ <- replicateM (length token) get
        maybe pfail (pure . Gateway) $ parseURI input

    readListPrec = readListPrecDefault


commandLineOptions :: Parser CommandLineOptions
commandLineOptions = CommandLineOptions
    <$> option auto (fold 
            [ long "gateway"
            , short 'g'
            , metavar "GATEWAY"
            , help "The address of STREAM service broker"
            ])


getCommandLineOptions :: IO CommandLineOptions
getCommandLineOptions =
    let opts name = info (commandLineOptions <**> helper) $ fold
            [ fullDesc
            , progDesc "User endpoint for submitting jobs to the STREAM service"
            , header $ name <> " - STREAM service TUI"
            ]
    in  getProgName >>= execParser . opts
