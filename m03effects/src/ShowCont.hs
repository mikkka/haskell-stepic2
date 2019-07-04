module ShowCont where 

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

evalCont :: Cont r r -> r
evalCont m = runCont m id

showCont :: Show a => Cont String a -> String
showCont m = runCont m show