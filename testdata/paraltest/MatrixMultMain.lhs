> module Main 
> where

> import MatrixMult
> import System.IO
> import System.Environment
> import Control.Monad


> sep = foldr (\b a -> if b == ',' then []:a else (b:(head a)):tail a ) [[]]

> tol :: [String] -> [(Int, Int)]
> tol [] = []
> tol [x] = []
> tol (x:y:xs) = (read x, read y):(tol xs)

> pp (a, b, c) = (b, a, c)

> main = do
>        (a:b:[]) <- getArgs
>        when (a == "minmult")
>          (putStrLn $ unlines $ map show $ map pp $ matrixmult minmult $ tol $ sep b)
