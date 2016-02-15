{-# LANGUAGE OverloadedStrings #-}

module Binder.Config where

import           Data.Yaml
import           Data.Yaml.Config

defaultConfigPath :: FilePath
defaultConfigPath = "config.yaml"

defaultConfig :: Value
defaultConfig = undefined


getConfig :: ( FromJSON a ) => IO a
getConfig = loadYamlSettings [ defaultConfigPath ] [] ignoreEnv
