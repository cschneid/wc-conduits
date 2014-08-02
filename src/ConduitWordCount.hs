module ConduitWordCount where

data Options = Options { countBytes      :: Bool
                       , countWords      :: Bool
                       , countLines      :: Bool
                       , inputFiles      :: Maybe [FilePath]
                       } deriving (Show)

