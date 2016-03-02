{-# LANGUAGE OverloadedStrings #-}
module Binder.File where

import Binder.Types
import Binder.Res.CSS

import           System.Directory
import           Control.Monad
import           Control.Monad.State
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

collectBinder :: StateT Int IO (Binder (Maybe Object) (T.Text, T.Text), [Css])
collectBinder = do
  depth         <- get
  base          <- liftIO $ getCurrentDirectory
  allContents   <- liftIO $ getDirectoryContents base  
  let contents  = drop 2 allContents
  directories   <- liftIO $ filterM doesDirectoryExist contents 
  let noteNames = filter (isInfixOf ".note" . reverse . take 5 . reverse) contents 
  noteContents  <- liftIO $ sequence . fmap T.readFile $ noteNames
  let notes     = zip (fmap (T.pack . reverse . drop 5 . reverse) noteNames) noteContents
  configExists  <- liftIO $ doesFileExist configFileName
  config        <- liftIO $ if configExists then putStrLn "getting conf" >> decodeFile configFileName else return Nothing                
  stateResults  <- liftIO $ mapM (\dir -> setCurrentDirectory (base <> "/" <> dir) 
                                             >> liftIO (runStateT collectBinder (depth + 1))
                                             >>= (\binderAndStyles -> setCurrentDirectory base 
                                             >> return binderAndStyles)) directories
  let (subBindersAndStyles, _ ) = unzip stateResults
  let (subBinders, styles     ) = unzip subBindersAndStyles
  return $ (Binder (T.pack base) config notes subBinders, generateTocStyle depth : fold styles)

buildBinder :: Binder (Maybe Object) (T.Text, T.Text) -> Html
buildBinder binder@(Binder base _ _ _) = 
  let (contents, marks) = unzip (op 0 binder) in mkToC (foldr (appendToC 0) mempty contents) <> fold marks where 
  mkNote :: T.Text -> T.Text -> Html
  mkNote name markdownText = mkSection $ (h1 ! Attr.class_ "note" ! Attr.id (lazyTextValue name) $ toHtml name) <> (markdown def markdownText)
  mkToC contents = (h2 ! Attr.id (textValue "ToC") $ toHtml (T.pack "Table of Contents")) <> (ul ! Attr.class_ "toc-list"  $ contents)
  mkSection :: Html -> Html
  mkSection = div ! Attr.class_ "section" 
  mkJump :: T.Text -> T.Text
  mkJump name = "./binder.html#" <> name -- this will probably need to be updated
  appendToC :: Int -> T.Text -> Html -> Html -- at some point this will need to have section #s   
  appendToC depth name currentToC =  (a 
     ! Attr.class_ (stringValue $ innerTocName depth)
     ! Attr.href (lazyTextValue $ mkJump name ) $ li 
     ! Attr.class_ "ToC_entry" $ toHtml name) <> currentToC
  op :: Int -> Binder (Maybe Object) (T.Text, T.Text) -> [(T.Text, Html)]
  op depth (Binder _ _ notes binders) = 
    fmap (\(name, mark) -> (name, mkNote name mark)) notes <> 
      foldr (\binder acc -> acc ++ op (depth + 1) binder) [] binders

wrapNotes :: Html -> Html
wrapNotes notes = div ! Attr.id (lazyTextValue "binder-content") $ notes

addHeader :: Html -> Html
addHeader html = header <> html where 
  header = head $ headerContents
  headerContents = link ! Attr.href "style.css" ! Attr.rel "stylesheet" ! Attr.type_ "text/css" <> title "Binder!"
  
