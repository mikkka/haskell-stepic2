module ZipList where
import Control.Applicative (ZipList(ZipList), getZipList)

(>$<) :: (a -> b) -> [a] -> [b]
f >$< xs = f <$> xs

(>*<) :: [a -> b] -> [a] -> [b]
f >*< xs = getZipList $ ZipList f <*> ZipList xs

{-
let x1s = [1,2,3]
let x2s = [4,5,6]
let x3s = [7,8,9]
let x4s = [10,11,12]
-}