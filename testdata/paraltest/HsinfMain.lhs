> module Main
> where

> import Hsinfernal.Probability
> import Hsinfernal.Grammar
> import Hsinfernal.AmbiPretty
> import Hsinfernal.Product
> import ADPTriCombinators


> import System.IO
> import System.Environment
> import Control.Monad

 main = print $ grammar (scoremax *** viennastring) "aaacccuuu"

> main = do
>        (a:b:[]) <- getArgs
>        when (a == "probability")
>          (putStrLn $ unlines $ map show $ grammar probability b)
>        when (a == "ambi")
>          (putStrLn $ unlines $ map show $ grammar ambipretty b)

>        when (a == "ambiprob")
>          (putStrLn $ repl '%' '/' $ unlines $ map show $ grammar (ambipretty *** probability) b)

>        when (a == "probpp")
>          (putStrLn $ unlines $ map show $ grammar (probability *** ambipretty ) b)


> repl a b [] = []
> repl a b (x:xs) = if (x == a) then b:repl a b xs else x:repl a b xs

