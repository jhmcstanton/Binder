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
import           Data.Foldable
import           Prelude hiding (head, div)

import Debug.Trace --remove

configFileName = "config.yaml"

collectBinder :: IO (Binder (Maybe Object) (T.Text, T.Text))
collectBinder = do
  base         <- getCurrentDirectory
  allContents  <- getDirectoryContents base  
  let contents = drop 2 allContents
  directories  <- filterM doesDirectoryExist contents
  noteNames    <- filterM (return . isInfixOf ".note") contents 
  noteContents <- sequence . fmap T.readFile $ noteNames
  let notes    = zip (fmap (T.pack . reverse . drop 5 . reverse) noteNames) noteContents
  configExists <- doesFileExist configFileName
  config       <- if configExists then putStrLn "gettign conf" >> decodeFile configFileName else return Nothing                
  subBinders   <- mapM (\dir -> setCurrentDirectory (base <> "/" <> dir) 
                          >> collectBinder 
                          >>= (\binder -> setCurrentDirectory base 
                          >> return binder)) directories
  return $ Binder (T.pack base) config notes subBinders

buildBinder :: Binder (Maybe Object) (T.Text, T.Text) -> Html
buildBinder binder@(Binder base _ _ _) = 
  let (contents, marks) = unzip (op binder) in mkToC (foldr appendToC mempty contents) <> fold marks where 
  mkNote :: T.Text -> T.Text -> Html
  mkNote name markdownText = mkSection $ (h2 ! Attr.class_ "note" ! Attr.id (lazyTextValue name) $ toHtml name) <> (markdown def markdownText)
  mkToC contents = (h2 ! Attr.id (textValue "ToC") $ toHtml (T.pack "Table of Contents")) <> (ul ! Attr.id "toc-list"  $ contents)
  mkSection :: Html -> Html
  mkSection = div ! Attr.class_ "section" 
  mkJump :: T.Text -> T.Text
  mkJump name = "./binder.html#" <> name -- this will probably need to be updated
  appendToC :: T.Text -> Html -> Html -- at some point this will need to have section #s   
  appendToC name currentToC = (a ! Attr.href (lazyTextValue $ mkJump name ) $ li ! Attr.class_ "ToC_entry" $ toHtml name) <> currentToC
  op :: Binder (Maybe Object) (T.Text, T.Text) -> [(T.Text, Html)]
  op (Binder _ _ notes binders) = 
    fmap (\(name, mark) -> (name, mkNote name mark)) notes <> 
      foldr (\binder acc -> acc ++ op binder) [] binders

addHeader :: Html -> Html
addHeader html = (head $ link ! Attr.href "style.css" ! Attr.rel "stylesheet" ! Attr.type_ "text/css") <> html
