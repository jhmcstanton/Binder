module Binder.Common
  ( targetDir,
    imageDir,
    binderName,
    (</>)
  ) where

import           Data.Monoid

targetDir, imageDir, binderName :: FilePath
targetDir  = "target"
imageDir   = targetDir <> "/images"
binderName = "binder.html"

(</>) :: FilePath -> FilePath -> FilePath
dir </> next = dir' <> ('/' : next') where
  dir'  = reverse . dropWhile (== '/') . reverse $ dir
  next' = dropWhile (== '/') next 
