module ConduitWordCount where

-- ConduitM                      i  o    m r -- Input, Output, Monad, Final Result
-- type Source m a    = ConduitM () a    m () -- no meaningful input or return value
-- type Conduit a m b = ConduitM a  b    m () -- no meaningful return value
-- type Sink a m b    = ConduitM a  Void m b -- no meaningful output value

import Prelude hiding (length, filter, concat)
import Data.Monoid
import Data.Conduit
import Data.Conduit.Binary
{- import Data.Conduit.List as CL -}
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.ByteString hiding (map, putStrLn, any, count, replicate)
import qualified Data.ByteString.Char8 as BW hiding (map, putStrLn, pack)
import System.IO
import Control.Applicative
import Control.Monad (forM_)

data Options = Options { countBytes      :: Bool
                       , countWords      :: Bool
                       , countLines      :: Bool
                       , inputFiles      :: Maybe [FilePath]
                       } deriving (Show)

data Counts = Counts { byteCount :: Sum Int
                     , wordCount :: Sum Int
                     , lineCount :: Sum Int
                     } deriving (Show)

instance Monoid Counts where
  mempty = Counts mempty mempty mempty
  mappend (Counts b1 w1 l1) (Counts b2 w2 l2) =
    Counts (b1 <> b2) (w1 <> w2) (l1 <> l2)

runWordCount :: Options -> IO ()
runWordCount opts =
  forM_ (inputs opts) (\i ->
    runResourceT $ i
                $= allCountsC
                $= combineCountsC
                $= showCountsC
                $$ output)
  where allCountsC = getZipConduit $ ZipConduit countByteC
                                  <* ZipConduit countWordsC
                                  <* ZipConduit countLinesC

inputs :: Options -> [Source (ResourceT IO) ByteString]
inputs opts = case inputFiles opts of
               Just files -> map sourceFile files
               Nothing    -> [ sourceHandle stdin ]

output :: Conduit String (ResourceT IO) o
output = awaitForever (liftIO . putStrLn)

countByteC :: Conduit ByteString (ResourceT IO) Counts
countByteC = awaitForever (yield . makeNewCount)
  where makeNewCount bs  = Counts (count bs) mempty mempty
        count = Sum . length

countWordsC :: Conduit ByteString (ResourceT IO) Counts
countWordsC = awaitForever (yield . makeNewCount)
  where makeNewCount bs  = Counts mempty (count bs) mempty
        count = Sum . length . BW.filter isSpace
        isSpace c = (c == ' ') || (c == '\t')

countLinesC :: Conduit ByteString (ResourceT IO) Counts
countLinesC = awaitForever (yield . makeNewCount)
  where makeNewCount bs  = Counts mempty mempty (count bs)
        count = Sum . length . BW.filter isNewline
        isNewline = (== '\n')

combineCountsC :: Conduit Counts (ResourceT IO) Counts
combineCountsC = do
  x <- await
  y <- await
  z <- await
  case mconcat [x,y,z] of
    Just result -> yield result
    Nothing     -> return ()

showCountsC :: ConduitM Counts String (ResourceT IO) ()
showCountsC = awaitForever (yield . show)


