module ConduitWordCount where

import Data.Monoid

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
    Counts (b1<>b2) (w1<>w2) (l1<>l2)


runWordCount :: Options -> IO ()
runWordCount opts = print opts



