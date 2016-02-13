{-# LANGUAGE DeriveFunctor              #-}

module Binder.Types (Binder (Binder) ) where

import           System.Directory
import qualified Data.Text.Lazy as T

--newtype Note   = Note T.Text
--newtype Config = Config T.Text
                                     -- section name
data Binder conf note = Binder T.Text (Maybe conf) [ note ] [ Binder conf note ] 
     deriving (Show, Functor)

