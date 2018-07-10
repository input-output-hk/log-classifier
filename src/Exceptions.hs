module Exceptions where

import Universum

data ClassifierExceptions
    = ReadZipFileException Text
    deriving Show

instance Exception ClassifierExceptions