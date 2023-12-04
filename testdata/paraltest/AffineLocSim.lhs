> module AffineLocSim where

> import Data.Array
> import ADPCombinators
> import Data.List

The signature:

> data Alignment = Nil (Int, Int)         |
>                  D  Char Alignment      |
>                  I  Alignment Char      |
>                  R  Char Alignment Char |
>                  Dx Char Alignment      |
>                  Ix Alignment Char
>                                            deriving (Eq, Show)

Algebra type:

> type AffineLocsim_Algebra alphabet answer = (
>   (Int, Int) -> answer,                     -- nil
>   alphabet -> answer -> answer,             -- d
>   answer -> alphabet -> answer,             -- i
>   alphabet -> answer -> alphabet -> answer, -- r
>   alphabet -> answer -> answer,             -- dx
>   answer -> alphabet -> answer,             -- ix
>   [answer] -> [answer]                      -- h
>   )

Enumeration algebra:

> enum :: AffineLocsim_Algebra Char Alignment
> enum = (nil, d, i, r, dx, ix, h) where
>    nil = Nil
>    d   = D
>    i   = I
>    r   = R
>    dx  = Dx
>    ix  = Ix
>    h   = id

Pretty printing algebra:

> prettyprint :: AffineLocsim_Algebra Char (String, String)
> prettyprint = (nil, d, i, r, dx, ix, h) where
>   nil _        = ("","")
>   d  x (l,r)   = (x:l, open:r)
>   i    (l,r) y = (open:l, y:r)
>   r  x (l,r) y = (x:l,y:r)
>   dx x (l,r) 	 = (x:l, extend:r)
>   ix   (l,r) y = (extend:l, y:r)
>   h            = id
>   open         = '='
>   extend	 = '-'

Counting algebra:

> count :: AffineLocsim_Algebra Char Int
> count = (nil, d, i, r, dx, ix, h) where
>    nil a   = 1
>    d x s   = s
>    i s y   = s
>    r a s b = s
>    dx x s  = s
>    ix s y  = s
>    h []    = []
>    h l     = [sum l]

Affine gap score algebra:

> affine :: AffineLocsim_Algebra Char Int
> affine = (nil, d, i, r, dx, ix, h) where
>    nil a   = 0
>    d x s   = s + open + extend
>    i s y   = s + open + extend
>    r a s b = s + w a b
>    dx x s  = s + extend
>    ix s y  = s + extend
>    h []    = []
>    h l     = [maximum l]

>  -- simple definitions for open, extend and w:
>    open   = (-15)
>    extend = (-1)
>    w a b  = if a==b then 4 else -3

Algebra product operation:

> infix ***
> (***) :: Eq answer1 =>
>          AffineLocsim_Algebra alphabet answer1 ->
>          AffineLocsim_Algebra alphabet answer2 ->
>          AffineLocsim_Algebra alphabet (answer1, answer2)
> alg1 *** alg2 = (nil, d, i, r, dx, ix, h) where
>    (nil1, d1, i1, r1, dx1, ix1, h1) = alg1
>    (nil2, d2, i2, r2, dx2, ix2, h2) = alg2
> 
>    nil a = (nil1 a, nil2 a)
>    d  x (s1,s2)   = (d1 x s1, d2 x s2)
>    i    (s1,s2) y = (i1 s1 y, i2 s2 y)
>    r  x (s1,s2) y = (r1 x s1 y, r2 x s2 y)
>    dx x (s1,s2)   = (dx1 x s1, dx2 x s2)
>    ix   (s1,s2) y = (ix1 s1 y, ix2 s2 y)
> 
>    h xs = [(x1,x2)| x1 <- nub $ h1 [ y1 | (y1,y2) <- xs],
>                     x2 <-       h2 [ y2 | (y1,y2) <- xs, y1 == x1]]

The yield grammar:

> affinelocsim alg f = axiom skipR where
>   (nil, d, i, r, dx, ix, h) = alg

>   skip_right a b = a
>   skip_left  a b = b

>   skipR     = skip_right <<<           skipR ~~- achar |||
>               skipL                                    ... h

>   skipL     = skip_left  <<< achar -~~ skipL           |||
>               alignment                                 ... h

>   alignment  = tabulated(
>                nil <<< astring                        |||
>                d   <<< achar  -~~ xDel                |||
>                i   <<<            xIns      ~~- achar |||
>                r   <<< achar  -~~ alignment ~~- achar ... h)

>   xDel      = tabulated (
>                alignment              |||
>                dx <<< achar  -~~ xDel ... h )

>   xIns      = tabulated (
>                alignment             |||
>                ix <<< xIns ~~- achar ... h )

Bind input:

>   z         = mk f
>   (_,n)     = bounds z
>   achar     = acharSep' z '$'
>   tabulated = table n
>   axiom     = axiom' n

