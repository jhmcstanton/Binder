{-# LANGUAGE OverloadedStrings #-}
module Binder.File 
       ( collectBinder
       , buildBinder
       , wrapNotes
       , addHeader
       ) 
where

import Binder.Types
import Binder.Res.CSS

import           System.Directory
import           Control.Monad
import           Control.Monad.State
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T (readFile)
import           Data.List (isInfixOf, intercalate, sortOn)
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
import           Data.Char (toUpper)

import Debug.Trace --remove

configFileName = "config.yaml"

collectBinder :: StateT Int IO (Binder (Maybe Object) (T.Text, T.Text), [Css])
collectBinder = do
  depth         <- get
  base          <- liftIO $ getCurrentDirectory
  allContents   <- liftIO $ do contents <- getDirectoryContents base 
                               modTimes <- mapM getModificationTime contents
                               return $ zip contents modTimes
  let contents  = fmap fst . reverse . sortOn snd . drop 2 $ allContents  
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
  let (contents, marks) = op binder in mkToC contents <> marks ! Attr.id (stringValue notesName) where 
  mkNote :: T.Text -> T.Text -> Html
  mkNote name markdownText = mkSection $ (h1 ! Attr.class_ "note-name-header" ! Attr.id (lazyTextValue name) $ toHtml name) <> (markdown def markdownText)
  mkToC contents = (h2 ! Attr.id (textValue "ToC") $ toHtml (T.pack "Table of Contents")) <> (ol ! Attr.class_ "toc-list"  $ contents)
  mkSection :: Html -> Html
  mkSection = div ! Attr.class_ "section" 
  mkJump :: T.Text -> T.Text
  mkJump name = "./binder.html#" <> name -- this will probably need to be updated
  appendToC :: Int -> T.Text -> Html -> Html -- at some point this will need to have section #s   
  appendToC depth name currentToC =  (a 
     ! Attr.class_ (stringValue $ innerTocName depth)
     ! Attr.href (lazyTextValue $ mkJump name ) $ li 
     ! Attr.class_ "ToC_entry" $ toHtml name) <> currentToC
  op :: Binder (Maybe Object) (T.Text, T.Text) -> (Html, Html)
  op (Binder base _ []    binders) = 
    foldr (\(a, b) (toc, notes) -> (toc <> a, notes <> b)) (mempty, mempty) . fmap op $ binders
  op (Binder base _ notes binders) = (titleli <> (ol $ toc0 <> ol toc1), h1 title <> notes0 <> notes1) where
   titleli        = li $ toHtml title 
   title          = toHtml . T.pack . cap . reverse . takeWhile (/= '/') . reverse . T.unpack $ base
   cap (c : cs)   = toUpper c : cs
   (toc0, notes0) = 
     foldr (\(name, mark) (toc, notes) -> (toc <> (a ! Attr.href (lazyTextValue . mkJump $ name) $ li $ (toHtml name)) , 
                                           notes <> mkNote name mark)) 
       (mempty, mempty) notes
   (toc1, notes1) = foldr (\(toc0, notes0) (toc1, notes1) -> (toc0 <> toc1, notes0 <> notes1)) (mempty, mempty) . fmap op $ binders  

wrapNotes :: Html -> Html
wrapNotes notes = div ! Attr.id (lazyTextValue "binder-content") $ notes

addHeader :: Html -> Html
addHeader html = header <> html where 
  header = head $ headerContents
  headerContents = link ! Attr.href "style.css" ! Attr.rel "stylesheet" ! Attr.type_ "text/css" <> title "Binder!"
  
