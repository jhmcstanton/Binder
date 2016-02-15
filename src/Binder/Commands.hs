{-# LANGUAGE OverloadedStrings #-}
module Binder.Commands (entry) where

import           Binder.File
import           Binder.Res.CSS 
import           Binder.Commands.Parser

import           Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           System.Directory
import           Data.Monoid
import           Prelude hiding (init)
import           Options.Applicative
import           Control.Monad.Reader
import           Control.Monad.State

targetDir  = "target"
binderName = "binder.html"

-- pretty much all of this needs to be refactored for a reader environment

entry :: IO ()
entry = execParser opts >>= runWithOpts where
  opts = info (helper <*> parser)
     ( fullDesc
    <> progDesc "Binder is a note taking application that allows user to organize notes by folder and write them in markdown."
    <> header "Binder - a note taking tool")
  

runWithOpts :: App -> IO ()
runWithOpts (App True _ v)      = runReaderT init v
runWithOpts (App _ True _)      = mkStyles []
runWithOpts (App False False v) = 
  runIf v (putStrLn "Making styles...") >> runReaderT build v >>= mkStyles

build :: ReaderT Bool IO [Css]
build = do 
  verbose        <- ask
  ((binderContents, styles), _) <- liftIO $ runStateT collectBinder 0
  let binder = addHeader $ buildBinder binderContents
  liftIO $ createTargetIfNecessary targetDir
  let binderOut = targetDir <> "/" <> binderName
  fileExists <- liftIO $ doesFileExist binderOut
  runIf fileExists (runIf verbose (liftIO $ putStrLn "Cleaning previous binder.." >> removeFile binderOut))
  liftIO $ T.writeFile binderOut . renderHtml $ binder
  return styles

mkStyles :: [Css] -> IO ()
mkStyles styles = 
  createTargetIfNecessary targetDir 
  >> writeStyle stylesheet defaultStyle
  >> mapM_ (T.appendFile stylesheet . render) styles
  where
    stylesheet = targetDir <> "/style.css"

writeDefaultStyle = writeStyle (targetDir <> "/style.css") defaultStyle

init :: ReaderT Bool IO ()
init = do 
  verbose      <- ask
  targetExists <- liftIO $ doesDirectoryExist targetDir
  if targetExists
    then runIf verbose $ liftIO $ putStrLn "Target directory exists, stopping init"
    else do
      liftIO $ createDirectory targetDir
      liftIO $ mkStyles []

createTargetIfNecessary :: FilePath -> IO ()
createTargetIfNecessary path = do
  targetDirExists <- doesDirectoryExist targetDir
  runIf (not targetDirExists) $ createDirectory targetDir

runIf :: ( Monad m ) => Bool -> m () -> m ()
runIf False _ = return ()
runIf True m  = m
