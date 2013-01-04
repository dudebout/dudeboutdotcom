module Tools where

import Import
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)
import Data.Char (toLower)

deuniquifyName :: String -> String -> String
deuniquifyName prefix name = lowerInitial $ fromMaybe name $ stripPrefix prefix name

lowerInitial :: String -> String
lowerInitial []          = []
lowerInitial (initial:rest) = toLower initial:rest