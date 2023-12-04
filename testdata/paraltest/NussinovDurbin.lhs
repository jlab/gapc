> module NussinovDurbin where

> import Data.Array
> import Data.List
> import ADPTriCombinators

The signature:

> data Pairing = Nil                    |
>                Left' Char Pairing     |
>                Right' Pairing Char    |
>                Pair Char Pairing Char |
>                Split Pairing Pairing
>                                      deriving (Eq, Show)

Algebra type:

> type Nussinov_Algebra alphabet answer = (
>   () -> answer,                               -- nil
>   alphabet -> answer   -> answer,             -- left
>   answer   -> alphabet -> answer,             -- right
>   alphabet -> answer   -> alphabet -> answer, -- pair
>   answer   -> answer   -> answer,             -- split
>   [answer] -> [answer]                        -- h
>   ) 

Enumeration algebra:

> enum :: Nussinov_Algebra Char Pairing
> enum = (nil,left,right,pair,split,h) where
>    nil _ = Nil
>    left  = Left'
>    right = Right'
>    pair  = Pair
>    split = Split
>    h     = id

Pretty printing algebra:

> prettyprint :: Nussinov_Algebra Char (String,String)
> prettyprint = (nil,left,right,pair,split,h) where
>   nil _                 = ("","")
>   left a (l,r)          = ('.':l, a:r)
>   right  (l,r) b        = (l++".", r++[b])
>   pair a (l,r) b        = ('(':l++")",a:r++[b])
>   split (l1,r1) (l2,r2) = (l1++l2,r1++r2)
>   h                     = id

Counting algebra:

> count :: Nussinov_Algebra Char Integer
> count = (nil,left,right,pair,split,h) where
>   nil   _     = 1
>   left  _ i   = i
>   right   i _ = i
>   pair  _ i _ = i
>   split i1 i2 = i1 * i2
>   h xs = [sum xs]

Base Pair Algebra:

> pairmax :: Nussinov_Algebra Char Int

> pairmax = (nil,left,right,pair,split,h) where
>    nil _        = 0
>    left  _ x   = x
>    right   x _ = x
>    pair  _ x _ = x + 1
>    split x y   = x + y
>    h xs        = [maximum xs]

Algebra product operation:

> infix ***
> alg1 *** alg2 = (nil,left,right,pair,split,h) where
>    (nil1,left1,right1,pair1,split1,h1) = alg1
>    (nil2,left2,right2,pair2,split2,h2) = alg2
>    nil     a             = (nil1 a,       nil2 a)
>    left    a (x1,x2)     = (left1 a x1,   left2 a x2)
>    right     (x1,x2) a   = (right1 x1 a,  right2 x2 a)
>    pair    a (x1,x2) b   = (pair1 a x1 b, pair2 a x2 b)
>    split (x1,x2) (y1,y2) = (split1 x1 y1, split2 x2 y2) 
>    h xs = [(x1,x2)| x1 <- nub $ h1 [ y1 | (y1,y2) <- xs],
>                     x2 <-       h2 [ y2 | (y1,y2) <- xs, y1 == x1]]


Nussinov's original grammar:

> nussinov78 alg inp = axiom s where
>   (nil,left,right,pair,split,h) = alg

>   s = tabulated (
>         nil <<< empty |||
>         right <<< s ~~- base |||
>         split <<< s ~~+ t  ... h
>       )

>   t = tabulated (
>         (pair <<< base -~~ s ~~- base) `with` basepairing 
>       )

Bind input:

>   z         = mk (inp)
>   (_,n)     = bounds z

>   base      = achar' z
>   tabulated = table n
>   axiom     = axiom' n

>   basepairing :: Filter
>   basepairing  = match inp
>   match  inp (i,j) = i+1<j && basepair (z!(i+1), z!(j))


Durbin's variant of nussinov78

> durbin alg inp = axiom s where
>   (nil,left,right,pair,split,h) = alg

>   s = tabulated (
>	nil   <<< empty                |||
>       left  <<< base -~~ s           |||
>       right <<<          s ~~- base  |||
>       (pair <<< base -~~ s ~~- base)
>                   `with` basepairing |||
>       split <<<       s +~+ s        ... h)

Bind input:

>   z         = mk (inp)
>   (_,n)     = bounds z

>   base      = achar' z
>   tabulated = table n
>   axiom     = axiom' n

>   basepairing :: Filter
>   basepairing  = match inp
>   match  inp (i,j) = i+1<j && basepair (z!(i+1), z!(j))

Baseparing function:

> basepair ('a','u') = True
> basepair ('u','a') = True
> basepair ('c','g') = True
> basepair ('g','c') = True
> basepair ('g','u') = True
> basepair ('u','g') = True
> basepair ( x , y ) = False
