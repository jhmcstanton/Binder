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
runWithOpts (App True _ _)      = init
runWithOpts (App _ True _)      = mkStyles
runWithOpts (App False False _) = mkStyles >> build

build :: IO ()
build = do 
  binderContents <- collectBinder 
  let binder = addHeader $ buildBinder binderContents
  createTargetIfNecessary targetDir
  let binderOut = targetDir <> "/" <> binderName
  fileExists <- doesFileExist binderOut
  if fileExists
    then putStrLn "Cleaning previous binder.." >> removeFile binderOut
    else return ()  
  T.writeFile binderOut . renderHtml $ binder

mkStyles :: IO ()
mkStyles = createTargetIfNecessary targetDir >> writeStyle (targetDir <> "/style.css") defaultStyle

init :: IO ()
init = do 
  targetExists <- doesDirectoryExist targetDir
  if targetExists
    then putStrLn "Target directory exists, stopping init"
    else do
      createDirectory targetDir
      mkStyles

createTargetIfNecessary :: FilePath -> IO ()
createTargetIfNecessary path = do
  targetDirExists <- doesDirectoryExist targetDir     
  if targetDirExists 
    then return () 
    else createDirectory targetDir
