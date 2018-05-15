module Lib
       ( someFunc
       ) where

import           Universum
-- TODO(ks): Here we want to import the functionality we require in cardano-report-server.
someFunc :: IO ()
someFunc = putTextLn "someFunc"
