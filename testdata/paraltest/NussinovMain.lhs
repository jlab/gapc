> module Main 
> where

> import Nussinov
> import System.IO
> import System.Environment
> import Control.Monad

> main = do
>        (a:b:[]) <- getArgs
>        when (a == "bpmax")
>          (putStrLn $ unlines $ map show $ nussinov bpmax b)
>        when (a == "bpmaxbpmax")
>          (putStrLn $ unlines $ map show $ nussinov (bpmax *** bpmax) b)
>        when (a == "pretty")
>          (putStrLn $ unlines $ map show $ nussinov prettyprint b)
>        when (a == "bpmaxpp")
>          (putStrLn $ unlines $ map show $ nussinov (bpmax *** prettyprint) b)
>        when (a == "count")
>          (putStrLn $ unlines $ map show $ nussinov count b)
>        when (a == "acount")
>          (putStrLn $ unlines $ map show $ nussinov count b)
>        when (a == "bpmaxcnt")
>          (putStrLn $ unlines $ map show $ nussinov (bpmax *** count) b)

>        when (a == "kbpmaxpp")
>          (putStrLn $ unlines $ map show $ nussinov (bpmax2 *** prettyprint) b)
