module MkEndo where

import Data.Monoid

mkEndo :: Foldable t => t (a -> a) -> Endo a
-- mkEndo tf = Endo $ foldr (.) id tf 
mkEndo = foldMap Endo