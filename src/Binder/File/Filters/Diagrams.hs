{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Binder.File.Filters.Diagrams
       ( UniqueId,
         insertDiagrams,
         BlockHandler (..),
         DiagramOpts
       ) where

import           Binder.Common

import qualified Diagrams.Builder as DB
import           Diagrams.Prelude ( (.~), (&), pad, centerXY)
import           Diagrams.TwoD.Size (mkWidth)
import           Diagrams.Backend.SVG
import           Text.Pandoc
--import           Text.Pandoc.Walk (walk)
import           Control.Monad.State
import qualified Control.Monad.Writer as W
import           Control.Monad.Reader
import           Data.Monoid ( (<>) )
import           System.IO
import           Linear (V2 (..), zero)
import qualified Data.Text as T (pack) 


type UniqueId    = Int
type DiagramOpts = DB.BuildOpts SVG V2 Integer

newtype BlockHandler a = BlockHandler {
    runBlockHandler :: ReaderT FilePath (W.WriterT [(FilePath, DiagramOpts)] (State UniqueId)) a
  } deriving (Functor, Applicative, Monad, MonadReader FilePath, W.MonadWriter [(FilePath, DB.BuildOpts SVG V2 Integer)], MonadState UniqueId)

-- arbitrarily chosen
defaultWidth = 250

-- used for naming diagrams 
getUnique :: MonadState UniqueId m => m UniqueId
getUnique = do
  curId <- get
  modify' (+1)
  return curId



-- | All of this is inspired, borrowed, or outright taken from Daniel Bergey's
--  diagrams-pandoc library, since I couldn't get it to work with stack
insertDiagrams :: Block -> BlockHandler Block
insertDiagrams block@(CodeBlock (ident, classes, attrs) code)
  | "diagram" `elem` classes = do
    uid <- get
    dir <- ask    
    let diagName = "_diag" <> show uid <> ".svg"
    let imgName = dir </> diagName
    let width = maybe defaultWidth read $ lookup "width" attrs
    let buildOpts =
          DB.mkBuildOpts SVG zero (SVGOptions (mkWidth width) Nothing $ T.pack imgName) 
          -- these imports are straight from Bergey's code, comments and all
          & DB.imports .~ [ "Diagrams.TwoD.Types"      -- WHY IS THIS NECESSARY =(
                          , "Diagrams.Core.Points"
                            -- GHC 7.2 bug?  need  V (Point R2) = R2  (see #65)
                          , "Diagrams.Backend.SVG"
--                          , "Diagrams.Backend.SVG.Internal"
                          , "Graphics.SVGFonts"
                          , "Data.Typeable"
                          ]
          & DB.diaExpr .~ code
          & DB.pragmas .~ ["DeriveDataTypeable"]
--          & DB.postProcess .~ (pad 1.1 . centerXY)
          -- this is *obviously* not ideal, but ok for right now.
          -- Binder should probably do some smart recompiling in general, not there yet
          & DB.decideRegen .~ DB.alwaysRegenerate
    W.tell [(imgName, buildOpts)]
    return $ Plain [Image ("", [], []) [] (dir </> diagName, "")]
                               
  | otherwise = return block
insertDiagrams block = return block    

-- mostly borred from Bergey again
writeImage :: FilePath -> DB.BuildResult a b c -> IO (Maybe FilePath)
writeImage imgName res = checkError res where
  checkError (DB.ParseErr err) = do
    hPutStrLn stderr ("Failed to parse " <> imgName)
    hPutStrLn stderr err
    return Nothing
  checkError (DB.InterpErr err) = do
    hPutStrLn stderr ("Failed to interpret " <> imgName)
    hPutStrLn stderr $ DB.ppInterpError err
    return Nothing
  checkError (DB.Skipped hash) = return Nothing
  checkError (DB.OK hash out ) = return $ Just imgName    

