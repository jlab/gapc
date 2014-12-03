> module Main 
> where

> import NussinovDurbin
> import System.IO
> import System.Environment
> import Control.Monad

> bpmax = pairmax

> main = do
>        (a:b:[]) <- getArgs
>        when (a == "bpmax")
>          (putStrLn $ unlines $ map show $ durbin bpmax b)
>        when (a == "pretty")
>          (putStrLn $ unlines $ map show $ durbin prettyprint b)
>        when (a == "bpmaxpp")
>          (putStrLn $ unlines $ map show $ durbin (bpmax *** prettyprint) b)
>        when (a == "count")
>          (putStrLn $ unlines $ map show $ durbin count b)
>        when (a == "bpmaxcnt")
>          (putStrLn $ unlines $ map show $ durbin (bpmax *** count) b)

