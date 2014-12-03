> module Main 
> where

> import ADPfold_always_dangle(fold)
> import Algebras_ad

> import System.IO
> import System.Environment
> import Control.Monad
> import Data.List

> main = do
>        (a:b:[]) <- getArgs
>        when (a == "count")
>          (putStrLn $ unlines $ map show $ fold b count)
>        when (a == "pretty")
>          (putStrLn $ unlines $ map show $ fold b prettyprintalg)
>        when (a == "mfe")
>          (putStrLn $ unlines $ map show $ nub $ fold b ((energyalg 0)))
>        when (a == "mfepp")
>          (putStrLn $ unlines $ map show $ fold b ((energyalg 0)***prettyprintalg))
>        when (a == "shape5")
>          (putStrLn $ unlines $ map show $ fold b (nubalg shape5))
>        when (a == "bpmaxpp")
>          (putStrLn $ unlines $ map show $ fold b ((maxalg bp) *** prettyprintalg))
>        when (a == "bpmax")
>          (putStrLn $ unlines $ map show $ fold b ((maxalg bp)))
>        when (a == "ppmfe")
>          (putStrLn $ unlines $ map show $ fold b ((prettyprintalg *** mfe)))


>        when (a == "shapemfepp")
>          (putStrLn $ unlines $ map show $ fold b (((nubalg shape5)***mfe)***prettyprintalg))

>        when (a == "prettyshape")
>          (putStrLn $ unlines $ map show $ fold b (prettyprintalg***(nubalg shape5)))
