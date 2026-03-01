module Algorithms
    ( CompressionAlgorithm(..)
    , availableAlgorithms )
    where

import qualified BytePair as BP

import Data.ByteString (ByteString)


data CompressionAlgorithm = CompressionAlgorithm
    { longName      :: String
    , shortName     :: String
    , extention     :: String
    , compress      :: ByteString -> ByteString
    , decompress    :: ByteString -> ByteString
    }


availableAlgorithms :: [CompressionAlgorithm]
availableAlgorithms = 
    [ CompressionAlgorithm "Byte pair" "bp" "bp" BP.compress BP.decompress
    ]