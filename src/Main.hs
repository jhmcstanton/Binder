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
  let binder = buildBinder binderContents
  targetDirExists <- doesDirectoryExist targetDir
  if not targetDirExists 
    then createDirectory targetDir
    else return ()
  T.writeFile (targetDir <> "/" <> binderName) . renderHtml $ binder

targetDir = "target"
binderName = "binder.html"
