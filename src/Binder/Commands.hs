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

targetDir  = "target"
binderName = "binder.html"

-- pretty much all of this needs to be refactored for a reader environment

entry :: IO ()
entry = execParser (info parser mempty) >>= runWithOpts where
  

runWithOpts :: App -> IO ()
runWithOpts (App True _)      = init
runWithOpts (App _ True)      = mkStyles
runWithOpts (App False False) = build

build :: IO ()
build = do 
  binderContents <- collectBinder 
  let binder = addHeader $ buildBinder binderContents
  targetDirExists <- doesDirectoryExist targetDir
  if not targetDirExists 
    then createDirectory targetDir
    else return ()
  let binderOut = targetDir <> "/" <> binderName
  fileExists <- doesFileExist binderOut
  if fileExists
    then putStrLn "Cleaning previous binder.." >> removeFile binderOut
    else return ()  
  T.writeFile binderOut . renderHtml $ binder

mkStyles :: IO ()
mkStyles = writeStyle (targetDir <> "/style.css") defaultStyle

init :: IO ()
init = do 
  targetExists <- doesDirectoryExist targetDir
  if targetExists
    then putStrLn "Target directory exists, stopping init"
    else do
      createDirectory targetDir
      mkStyles
