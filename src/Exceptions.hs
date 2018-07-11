module Exceptions where

import Universum

data ClassifierExceptions
    = ReadZipFileException
    | DecompressionException
    deriving Show

instance Exception ClassifierExceptions