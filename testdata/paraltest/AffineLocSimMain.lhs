> module Main 
> where

> import AffineLocSim
> import System.IO
> import System.Environment
> import Control.Monad


> parse (a:b:[]) = (a, b)
> parse (a:b:c:[]) = (a, b ++ "$" ++ reverse c)

> main = do
>        l <- getArgs
>        let (a, b) = parse l
>        when (a == "count")
>          (putStrLn $ unlines $ map show $ affinelocsim count b)
>        when (a == "pretty")
>          (putStrLn $ unlines $ map show $ affinelocsim prettyprint b)
>        when (a == "affine")
>          (putStrLn $ unlines $ map show $ affinelocsim affine b)
>        when (a == "affinepp")
>          (putStrLn $ unlines $ map show $ affinelocsim (affine *** prettyprint) b)
>        when (a == "affinecnt")
>          (putStrLn $ unlines $ map show $ affinelocsim (affine *** count) b)
