{-# LANGUAGE OverloadedStrings #-}

module Binder.Commands.Parser where

import           Options.Applicative

data App = App Bool Bool Bool 

 
parser = App <$> switch (short 'i' <> 
                         long "init" <>
                         help "Set up default styles and config files")
             <*> switch (short 's' <>
                         long "styles" <>
                         help "Regenerate default styles (deletes existing style.css)")
             <*> switch (short 'v' <>
                         long "verbose" <>
                         help "Run in verbose mode (overwrites config.yaml setting)")
