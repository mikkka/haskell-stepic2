module Parsec where 

import Text.Parsec

getList :: Parsec String u [String]
getList = many1 digit `sepBy` char ';'

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces open close middle = open *> middle <* close