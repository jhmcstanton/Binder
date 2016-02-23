{-# LANGUAGE OverloadedStrings #-}

module Binder.Res.CSS (writeStyle, defaultStyle, generateTocStyle, Css, render, innerTocName) where

import           Clay
import           Clay.Geometry
import           Clay.Border
import           Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as ST
import           Data.Foldable (fold)

writeStyle :: FilePath -> T.Text -> IO ()
writeStyle path style = T.writeFile path style

defaultStyle :: T.Text
defaultStyle = fold . fmap render $ [bodyStyle, tocStyle, sectionStyle]

bodyStyle = binderBody where
  binderBody = body ? do
    marginLeft (pct 5)
    marginRight (pct 5)

tocStyle = element ".toc-list" ? do
  marginBottom (pct 2)
  paddingBottom (pct 2)
  borderBottom groove (em 0.2) gray


innerTocName :: Int -> String
innerTocName depth = ".toc-list-inner" <> show depth

generateTocStyle :: Int -> Css
generateTocStyle depth = element (ST.pack $ innerTocName depth) ? do
  marginLeft (pct . fromIntegral $ depth * 2)


sectionStyle = element ".section" ? do
  paddingBottom (pct 2)
  paddingBottom (pct 2)
  borderBottom groove (em 0.2) gray

