> module Main 
> where

> import Palloc
> import System.IO
> import System.Environment
> import Control.Monad

> main = do
>        (a:b:[]) <- getArgs
>        when (a == "count")
>          (putStrLn $ unlines $ map show $ pal count b)
>        when (a == "pretty")
>          (putStrLn $ unlines $ map show $ pal pretty b)
>        when (a == "score")
>          (putStrLn $ unlines $ map show $ pal score b)
>        when (a == "scorepp")
>          (putStrLn $ unlines $ map show $ pal (score *** pretty) b)
>        when (a == "scorecnt")
>          (putStrLn $ unlines $ map show $ pal (score *** count) b)
