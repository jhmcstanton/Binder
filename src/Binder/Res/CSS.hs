{-# LANGUAGE OverloadedStrings #-}

module Binder.Res.CSS 
       ( writeStyle
       , defaultStyle
       , generateTocStyle
       , Css
       , render
       , innerTocName
       , notesName
       )
where

import           Clay
import           Clay.Geometry
import           Clay.List
import           Clay.Border
import           Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as ST
import           Data.Foldable (fold)
import           Prelude hiding ((**))

bodyName  = "#binder-content"
notesName = "note-body" 

writeStyle :: FilePath -> T.Text -> IO ()
writeStyle path style = T.writeFile path style

defaultStyle :: T.Text
defaultStyle = fold . fmap render $ [bodyStyle, tocStyle, sectionStyle, tocNoStyle, noteNameHeader] ++ shiftHTagSize

bodyStyle = binderBody where
  binderBody = element bodyName ? do
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

-- this is added so that markdown htags can be slightly more convenient to use while matching
-- the expected size (removes a single # from each tag)
shiftHTagSize = zipWith (\tag size -> element (ST.pack $ '#' : notesName) ** tag ? fontSize (em size)) tags fontSizes where
  tags      = [h1, h2, h3, h4, h5, h6]
  fontSizes = [1.5, 1.17, 1, 0.83, 0.67, 0.6]

tocNoStyle :: Css
tocNoStyle = element ".list-no-style" ? do
  listStyleType none

noteNameHeader :: Css
noteNameHeader = element ".note-name-header" ? do
    fontStyle italic
--  textDecoration underline

