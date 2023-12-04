> module Main 
> where

> import Stef3
> import System.IO
> import System.Environment
> import Control.Monad

> main = do
>        (a:b:[]) <- getArgs
>        when (a == "count")
>          (putStrLn $ unlines $ map show $ canonicals_nonamb (-100.0) (count) b)
