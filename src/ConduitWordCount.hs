module ConduitWordCount where

import Prelude hiding (length, filter, concat)
import Data.Monoid
import Data.Conduit
import Data.Conduit.Binary
-- import Data.Conduit.List
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.ByteString hiding (map, putStrLn, any, count)
import qualified Data.ByteString.Char8 as BW hiding (map, putStrLn, pack)
import System.IO
import Control.Applicative

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
runWordCount opts = runResourceT $  input opts
                                $=  allCountsC
                                $$  showCountsC
                                =$  output
                    where allCountsC = getZipConduit $ ZipConduit countByteC
                                                    <* ZipConduit countWordsC
                                                    <* ZipConduit countLinesC
                    -- where allCountsC = sequenceConduits [ countByteC, countWordsC, countLines ]

input :: (MonadResource m) => Options -> Source m ByteString
input opts = case inputFiles opts of
               Just files -> sourceFile $ Prelude.head files
               Nothing    -> sourceHandle stdin

output :: ConduitM String o (ResourceT IO) ()
output = awaitForever (liftIO . putStrLn)

countByteC :: ConduitM ByteString Counts (ResourceT IO) ()
countByteC = awaitForever (yield . makeNewCount)
  where makeNewCount bs  = Counts (count bs) mempty mempty
        count = Sum . length

countWordsC :: ConduitM ByteString Counts (ResourceT IO) ()
countWordsC = awaitForever (yield . makeNewCount)
  where makeNewCount bs  = Counts mempty (count bs) mempty
        count = Sum . length . BW.filter isSpace
        isSpace c = (c == ' ') || (c == '\t')

countLinesC :: ConduitM ByteString Counts (ResourceT IO) ()
countLinesC = awaitForever (yield . makeNewCount)
  where makeNewCount bs  = Counts mempty mempty (count bs)
        count = Sum . length . BW.filter isNewline
        isNewline = (== '\n')

showCountsC :: ConduitM Counts String (ResourceT IO) ()
showCountsC = awaitForever (yield . show)


