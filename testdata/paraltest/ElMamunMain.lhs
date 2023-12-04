> module Main 
> where

> import ElMamun
> import System.IO
> import System.Environment
> import Control.Monad

> main = do
>        (a:b:[]) <- getArgs
>        when (a == "count")
>          (putStrLn $ unlines $ map show $ bill count b)
>        when (a == "pretty2")
>          (putStrLn $ unlines $ map show $ bill prettyprint b)
>        when (a == "seller")
>          (putStrLn $ unlines $ map show $ bill seller b)
>        when (a == "buyer")
>          (putStrLn $ unlines $ map show $ bill buyer b)
>        when (a == "buyerpp")
>          (putStrLn $ unlines $ map show $ bill (buyer *** prettyprint) b)
>        when (a == "sellercnt")
>          (putStrLn $ unlines $ map show $ bill (seller *** count) b)
>        when (a == "timebuyerpp")
>          (putStrLn $ unlines $ map show $ bill ((time *** buyer) *** prettyprint) b)
