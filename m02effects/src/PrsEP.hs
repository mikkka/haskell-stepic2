module PrsEP where
import Control.Applicative

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP f = 
  PrsEP (\pos str -> 
    let 
      newPos = pos + 1 
    in
    case str of 
      (x:xs)    -> if f x then (newPos, Right (x, xs)) else (newPos, Left ("pos " ++ show newPos ++ ": unexpected " ++ [x]))
      null      -> (newPos, Left ("pos " ++ show newPos ++ ": unexpected end of input")))

instance Functor PrsEP where
  fmap f (PrsEP prs) = PrsEP((fmap . fmap) (\(pos, res) -> 
    (pos, fmap (\(x, tail) -> (f x, tail)) res)) prs) 

instance Applicative PrsEP where
  pure x = PrsEP (\pos str -> (pos, Right (x, str)))
  (<*>) (PrsEP f0) (PrsEP prs0) = PrsEP fun where
    fun pos str = let
        (pos1, fE) = f0 pos str
        prsF = prs0 pos1
      in case fE of
        Left err -> (pos1, Left err)
        Right (f, tail) -> let
          (pos2, xE) = prsF tail
          in
            (pos2, fmap (\(x, tail) -> (f x, tail)) xE) 

-- import Control.Applicative
instance Alternative PrsEP where
  empty = PrsEP (\_ _ -> (0, Left "pos 0: empty alternative"))
  PrsEP lft <|> PrsEP rgt = PrsEP (\ pos str ->
    let 
      lftR = lft pos str
      rgtR = rgt pos str
    in case (lftR, rgtR) of
      ((pL, Left errL),(pR, Left errR))           -> if (pL >= pR) then (pL, Left errL) else (pR, Left errR)
      ((pL, Left errL),r)                         -> r
      (r,(pR, Left errR))                         -> r
      (lftR,rgtL)                                 -> lftR)




charEP c = satisfyEP (== c)

tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c