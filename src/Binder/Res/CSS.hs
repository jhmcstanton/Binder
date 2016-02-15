{-# LANGUAGE OverloadedStrings #-}

module Binder.Res.CSS (writeStyle, defaultStyle) where

import           Clay
import           Clay.Geometry
import           Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Foldable (fold)

writeStyle :: FilePath -> T.Text -> IO ()
writeStyle path style = T.writeFile path style

defaultStyle :: T.Text
defaultStyle = fold . fmap render $ [bodyStyle, tocStyle, sectionStyle]

bodyStyle = binderBody where
  binderBody = body ? marginLeft (pct 5)

tocStyle = element "#toc-list" ? do
  paddingBottom (pct 80)

sectionStyle = element ".section" ? do
  paddingBottom (em 8)
