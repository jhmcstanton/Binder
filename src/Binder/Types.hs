{-# LANGUAGE DeriveFunctor              #-}

module Binder.Types (Binder (Binder), Settings (Settings)) where

import           System.Directory
import qualified Data.Text.Lazy as T
import           Data.Yaml
import           Data.Foldable

--newtype Note   = Note T.Text
--newtype Config = Config T.Text
                                     -- section name
data Binder conf note = Binder T.Text (Maybe conf) [ note ] [ Binder conf note ] 
     deriving (Show, Functor)

data Settings = Settings {
     verbose :: Bool,
     configs :: Object
}
