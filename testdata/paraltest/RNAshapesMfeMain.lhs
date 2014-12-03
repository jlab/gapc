> module Main where

> import System.Environment
> import System.IO
> import Numeric
> import Data.List(nub,sort,sortBy) 

> import RNAshapesMfe.Grammar_canonicals_nonamb
> import RNAshapesMfe.Algebra_prettyprint
> import RNAshapesMfe.AlgebraType
> import RNAshapesMfe.Algebra_mfe
> import RNAshapesMfe.AlgebraCrossProducts

> import RNAshapesMfe.Algebra_shape

> import Control.Monad

> main = do
>        (a:b:[]) <- getArgs
>        when (a == "mfepp")
>          (putStrLn $ unlines $ map show $ map (\((x,_,_), y) -> (round x, y) ) $canonicals_nonamb (-100.0) (mfe@@@dotBracket) b)
>        when (a == "ppmfe")
>          (putStrLn $ unlines $ map show $ map (\(y, (x,_,_)) -> (round x, y) ) $canonicals_nonamb (-100.0) (dotBracket***mfe) b)

>        when (a == "mfeppshape")
>          (putStrLn $ unlines $ map show $ map (\((a,_,_),x)  -> (round a, x)) $ canonicals_nonamb (-100.0) (mfe***(dotBracket *** shape5)) b)


