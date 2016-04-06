{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Binder.File 
       ( collectBinder
       , buildBinder
       , wrapNotes
       , addHeader
       ) 
where

import           Binder.Types
import           Binder.Res.CSS
import           Binder.Common
import           Binder.File.Filters.Diagrams

import           System.Directory
import           Control.Monad
import           Control.Monad.State
import qualified Control.Monad.Writer as W
import           Control.Monad.Reader
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T (readFile)
import           Data.List (isInfixOf, intercalate, sortOn, foldl')
import           Data.Monoid
import           Data.Yaml
import           Data.Map.Lazy (union)
import           Text.Pandoc
import           Text.Pandoc.Error (handleError)
--import           Text.Pandoc.Diagrams                 
import           Text.Blaze.Html
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as Attr
import           Text.Blaze
import           Data.Monoid
import           Data.Foldable
import           Prelude hiding (head, div)
import           Data.Char (toUpper)


configFileName = "config.yaml"
mathJaxUrl     = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"

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
  let notes     = zip (fmap (T.pack . fileNameOps) noteNames) noteContents
  configExists  <- liftIO $ doesFileExist configFileName
  config        <- liftIO $ if configExists then putStrLn "getting conf" >> decodeFile configFileName else return Nothing                
  stateResults  <- liftIO $ mapM (\dir -> setCurrentDirectory (base <> "/" <> dir) 
                                             >> liftIO (runStateT collectBinder (depth + 1))
                                             >>= (\binderAndStyles -> setCurrentDirectory base 
                                             >> return binderAndStyles)) directories
  let (subBindersAndStyles, _ ) = unzip stateResults
  let (subBinders, styles     ) = unzip subBindersAndStyles
  return $ (Binder (T.pack base) config notes subBinders, generateTocStyle depth : fold styles)

-- additional collectBinder helpers
dropExtension = reverse . drop 5 . reverse
_toSpace '_'  = ' '
_toSpace c    = c
capitalize :: String -> String
capitalize    = snd . foldl' (\(prevC, name) curC -> case prevC of
                                                       ' ' -> (curC, name <> [toUpper curC])
                                                       _   -> (curC, name <> [curC])) (' ', "")
fileNameOps = capitalize . fmap _toSpace . dropExtension

buildBinder :: Binder (Maybe Object) (T.Text, T.Text) -> (Html, [(FilePath, DiagramOpts)])
buildBinder binder@(Binder base _ _ _) = 
  let (contents, marks, imgs) = op binder in
  (mkToC contents <> marks ! Attr.id (stringValue notesName), imgs)
  where
    runHandler :: UniqueId -> Pandoc -> (Pandoc, [(FilePath, DiagramOpts)], UniqueId)
    runHandler n (Pandoc m blocks) = (Pandoc m blocks', imgs, uid) where
      ((blocks', imgs), uid) = runState (W.runWriterT (runReaderT (runBlockHandler $ mapM insertDiagrams blocks) imageDir)) n
    mkNote :: UniqueId -> T.Text -> T.Text -> (Html, [(FilePath, DiagramOpts)], UniqueId)
    mkNote uid name markdownText = (mkSection $ (h1 ! Attr.class_ "note-name-header" ! Attr.id (lazyTextValue name) $ toHtml name)
      <> (writeHtml def pandoc'), imgs, uid')
      where
        (pandoc', imgs, uid') = runHandler uid . handleError . readMarkdown def . T.unpack $ markdownText
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
    op :: Binder (Maybe Object) (T.Text, T.Text) -> (Html, Html, [(FilePath, DiagramOpts)])
    op (Binder base _ []    binders) = 
      foldr (\(a, b, imgs) (toc, notes, imgs') -> (toc <> a, notes <> b, imgs <> imgs')) (mempty, mempty, mempty) . fmap op $ binders
    op (Binder base _ notes binders) = (titleli <> (ol $ toc0 <> ol toc1), h1 title <> notes0 <> notes1, imgs0 <> imgs1)
      where
        titleli        = li $ toHtml title 
        title          = toHtml . T.pack . cap . reverse . takeWhile (/= '/') . reverse . T.unpack $ base
        cap (c : cs)   = toUpper c : cs
        (toc0, notes0, imgs0, _) = 
          foldr (\(name, mark) (toc, notes, imgs, uid) -> let (note', imgs', uid') = mkNote uid name mark in
                  (toc <> (a ! Attr.href (lazyTextValue . mkJump $ name) $ li $ (toHtml name)), 
                   notes <> note', imgs <> imgs', uid')) (mempty, mempty, mempty, 0) notes
        (toc1, notes1, imgs1) =
          foldr (\(toc0, notes0, imgs) (toc1, notes1, imgs') -> (toc0 <> toc1, notes0 <> notes1, imgs <> imgs'))
            (mempty, mempty, mempty) . fmap op $ binders  

wrapNotes :: Html -> Html
wrapNotes notes = body $ div ! Attr.id (lazyTextValue "binder-content") $ notes

addHeader :: [FilePath] -> Html -> Html
addHeader extraScripts html = header <> html where 
  scriptTag = script ! Attr.type_ "text/javascript"
  header = head $ headerContents
  headerContents = link ! Attr.href "style.css" ! Attr.rel "stylesheet" ! Attr.type_ "text/css"
    <> title "Binder!"
    -- extra scripts will include a local copy of mathjax to load, which will be overridden later 
    -- by the cdn copy if the servers are available
    <> fold (fmap (\sname -> scriptTag ! Attr.src (stringValue sname) $ mempty) extraScripts)
    <> (scriptTag ! Attr.async mempty 
                  ! Attr.src (lazyTextValue mathJaxUrl) $ mempty)
