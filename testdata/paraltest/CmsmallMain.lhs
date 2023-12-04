> module Main
> where

> import ScoreMaxint
> import Grammarint
> import ViennaStringint
> import Product
> import ADPTriCombinators


> import System.IO
> import System.Environment
> import Control.Monad

 main = print $ grammar (scoremax *** viennastring) "aaacccuuu"

> main = do
>        (a:b:[]) <- getArgs
>        when (a == "score")
>          (putStrLn $ unlines $ map show $ grammar scoremax b)
>        when (a == "pretty")
>          (putStrLn $ unlines $ map show $ grammar viennastring b)

>        when (a == "scorepp")
>          (putStrLn $ unlines $ map show $ grammar (scoremax *** viennastring) b)

>        when (a == "ppscore")
>          (putStrLn $ unlines $ map show $ grammar (viennastring *** scoremax ) b)
