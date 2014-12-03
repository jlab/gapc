> module Nussinov where

> import Data.Array
> import Data.List
> import ADPCombinators

The signature:

> data Pairing = Nil                    |
>                Right' Pairing Char    |
>                Pair Char Pairing Char |
>                Split Pairing Pairing
>                                      deriving (Eq, Show)

Algebra type:

> type Nussinov_Algebra alphabet answer = (
>   () -> answer,                               -- nil
>   answer   -> alphabet -> answer,             -- right
>   alphabet -> answer   -> alphabet -> answer, -- pair
>   answer   -> answer   -> answer,             -- split
>   [answer] -> [answer]                        -- h
>   ) 

Enumeration algebra:

> enum :: Nussinov_Algebra Char Pairing
> enum = (nil,right,pair,split,h) where
>    nil _ = Nil
>    right = Right'
>    pair  = Pair
>    split = Split
>    h     = id

Pretty printing algebra:

> prettyprint :: Nussinov_Algebra Char String
> prettyprint = (nil,right,pair,split,h) where
>   nil _                 = ""
>   right  l b        = l ++ "."
>   pair a l b        = '(':l ++ ")"
>   split l1 l2 = l1 ++ l2
>   h                     = id

Counting algebra:

> count :: Nussinov_Algebra Char Integer
> count = (nil,right,pair,split,h) where
>   nil   _     = 1
>   right   i _ = i
>   pair  _ i _ = i
>   split i1 i2 = i1 * i2
>   h xs = [sum xs]

Base Pair Algebra:

> bpmax :: Nussinov_Algebra Char Int

> bpmax = (nil,right,pair,split,h) where
>    nil _        = 0
>    right   x _ = x
>    pair  _ x _ = x + 1
>    split x y   = x + y
>    h xs        = [maximum xs]

> bpmax2 :: Nussinov_Algebra Char Int
> bpmax2 = (nil,right,pair,split,h) where
>    nil _        = 0
>    right   x _ = x
>    pair  _ x _ = x + 1
>    split x y   = x + y
>    h xs        = take 2 $ reverse $ nub $ sort xs

Algebra product operation:

> infix ***
> alg1 *** alg2 = (nil,right,pair,split,h) where
>    (nil1,right1,pair1,split1,h1) = alg1
>    (nil2,right2,pair2,split2,h2) = alg2
>    nil     a             = (nil1 a,       nil2 a)
>    right     (x1,x2) a   = (right1 x1 a,  right2 x2 a)
>    pair    a (x1,x2) b   = (pair1 a x1 b, pair2 a x2 b)
>    split (x1,x2) (y1,y2) = (split1 x1 y1, split2 x2 y2) 
>    h xs = [(x1,x2)| x1 <- nub $ h1 [ y1 | (y1,y2) <- xs],
>                     x2 <-       h2 [ y2 | (y1,y2) <- xs, y1 == x1]]


The yield grammar:

> nussinov alg inp = axiom s where
>   (nil,right,pair,split,h) = alg

Durbin-style nussinov (introduces semantic ambiguity ...):

   s = tabulated (
	nil   <<< empty                |||
       left  <<< base -~~ s           |||
       right <<<          s ~~- base  |||
       (pair <<< base -~~ s ~~- base)
                   `with` basepairing |||
       split <<<       s +~+ s        ... h)

Real nussinov from 1978:

>   s = tabulated (
>         nil <<< empty |||
>         right <<< s ~~- base |||
>         split <<< s ~~+ t  ... h
>           )

>   t = tabulated (
>         (pair <<< base -~~ s ~~- base) `with` basepairing 
>          )



Bind input:

>   z         = mk (inp)
>   (_,n)     = bounds z

>   base      = achar' z
>   tabulated = table n
>   axiom     = axiom' n

>   basepairing :: Filter
>   basepairing  = match inp
>   match  inp (i,j) = i+1<j && basepair (z!(i+1), z!(j))
>   basepair ('a','u') = True
>   basepair ('u','a') = True
>   basepair ('c','g') = True
>   basepair ('g','c') = True
>   basepair ('g','u') = True
>   basepair ('u','g') = True

>   basepair ('A','U') = True
>   basepair ('U','A') = True
>   basepair ('C','G') = True
>   basepair ('G','C') = True
>   basepair ('G','U') = True
>   basepair ('U','G') = True

>   basepair ('a','t') = True
>   basepair ('t','a') = True
>   basepair ('A','T') = True
>   basepair ('T','A') = True

>   basepair ( x , y ) = False


