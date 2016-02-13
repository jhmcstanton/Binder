{-# LANGUAGE OverloadedStrings #-}
module Binder.File where

import Binder.Types

import           System.Directory
import           Control.Monad
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T (readFile)
import           Data.List (isInfixOf)
import           Data.Monoid
import           Data.Yaml
import           Text.Markdown
import           Text.Blaze.Html
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as Attr
import           Text.Blaze
import           Data.Monoid

configFileName = "config.yaml"

collectBinder :: IO (Binder (Maybe Object) (FilePath, T.Text))
collectBinder = do
  base         <- getCurrentDirectory
  contents     <- getDirectoryContents base
  directories  <- filterM doesDirectoryExist contents
  noteNames    <- findFilesWith (return . isInfixOf ".note") contents "" 
  noteContents <- sequence . fmap T.readFile $ noteNames
  let notes    = zip (fmap (reverse . drop 5 . reverse) noteNames) noteContents
--  configExists <- doesFileExist configFileName
  config       <- decodeFile configFileName --if configExists then decodeFile configFileName else return Nothing                
  subBinders   <- mapM (\dir -> setCurrentDirectory (base <> dir) 
                          >> collectBinder 
                          >>= (\binder -> setCurrentDirectory base 
                          >> return binder)) contents
  return $ Binder (T.pack base) config notes subBinders

buildBinder :: Binder (Maybe Object) (FilePath, T.Text) -> Html
buildBinder binder = 
  let (contents, marks) = unzip (op binder) in mkToC (foldr appendToC mempty contents) <> foldr1 (<>) marks where 
  mkNote :: FilePath -> T.Text -> Html
  mkNote name markdownText = (h2 ! Attr.class_ "note" ! Attr.id (stringValue name) $ toHtml name) <> (markdown def markdownText)
  mkToC contents = (h2 ! Attr.id (textValue "ToC") $ toHtml (T.pack "Table of Contents")) <> ul contents
  appendToC :: T.Text -> Html -> Html -- at some point this will need to have section #s 
  appendToC name currentToC = currentToC <> (a ! Attr.href "name" $ li ! Attr.class_ "ToC_entry" $ toHtml name)
  op :: Binder (Maybe Object) (FilePath, T.Text) -> [(T.Text, Html)]
  op (Binder _ _ notes binders) = 
    fmap (\(name, mark) -> (T.pack name, mkNote name mark <> markdown def mark)) notes ++ foldr (\binder acc -> acc ++ op binder) [] binders

