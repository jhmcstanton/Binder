module Binder.Common
  ( targetDir,
    imageDir,
    binderName
  ) where

import           Data.Monoid

targetDir  = "target"
imageDir   = targetDir <> "/images"
binderName = "binder.html"
