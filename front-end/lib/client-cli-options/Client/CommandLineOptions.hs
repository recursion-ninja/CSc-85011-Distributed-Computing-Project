{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}

module Client.CommandLineOptions
  ( CommandLineOptions(..)
  , commandLineOptions
  , getCommandLineOptions
  ) where

import Broker.Gateway
import Data.Foldable
import Options.Applicative
import System.Environment (getProgName)


data  CommandLineOptions
    = CommandLineOptions
    { gateway :: Gateway
    }


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
