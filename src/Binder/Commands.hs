{-# LANGUAGE OverloadedStrings #-}
module Binder.Commands (build) where

import           Binder.File 

import           Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           System.Directory
import           Data.Monoid

targetDir  = "target"
binderName = "binder.html"

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
