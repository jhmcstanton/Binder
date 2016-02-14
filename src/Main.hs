{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Binder.File 

import           Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           System.Directory
import           Data.Monoid

main :: IO ()
main = do 
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

targetDir = "target"
binderName = "binder.html"
