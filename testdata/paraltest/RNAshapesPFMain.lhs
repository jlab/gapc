> module Main where

> import System.Environment
> import System.IO
> import Numeric
> import Data.List(nub,sort,sortBy) 

> import RNAshapesMfe.Grammar_canonicals_nonambPF
> import RNAshapesMfe.Algebra_prettyprint
> import RNAshapesMfe.AlgebraTypePF
> import RNAshapesMfe.Algebra_mfe
> import RNAshapesMfe.AlgebraCrossProducts

> import RNAshapesMfe.Algebra_p_func

> import Control.Monad

> main = do
>        (a:b:[]) <- getArgs
>        when (a == "pf")
>          (putStrLn $ unlines $ map show $ map (\((x,_,_),_) -> x) $ canonicals_nonamb (-100.0) (p_func) b)

