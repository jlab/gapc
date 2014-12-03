> module MatrixMult where

> import ADPCombinators
> import Data.Array
> import Data.List

The signature:

> data Matrixchain = Mult   Matrixchain Matrixchain |
>                    Single (Int, Int)
>                                       deriving (Show, Eq, Ord)

Algebra type:

> type Matrixmult_Algebra alphabet answer = (
>   alphabet -> answer,         -- single
>   answer -> answer -> answer, -- mult
>   [answer] -> [answer]        -- h
>   )

Enumeration algebra:

> enum :: Matrixmult_Algebra (Int, Int) Matrixchain
> enum  = (single, mult, h) where
>   single = Single
>   mult   = Mult
>   h      = id

Pretty printing algebra:

> prettyprint :: Matrixmult_Algebra (Int, Int) String
> prettyprint = (single, mult, h) where
>   single (a,b) = "|" ++ show a ++ "x" ++ show b ++ "|"
>   mult x y     = "(" ++ x ++ "*" ++ y ++ ")"
>   h            = id

Counting algebra:

> count :: Matrixmult_Algebra (Int, Int) Int
> count = (single, mult, h) where
>   single (r,c) = 1
>   mult x y     = x*y
>   h []         = []
>   h xs         = [sum xs]

Scoring algebra:

> minmult :: Matrixmult_Algebra (Int, Int) (Int, Int, Int)
> minmult = (single, mult, h) where
>   single (r,c)                = (r, 0 ,c)
>   mult (r, m, c) (r', m', c') = (r, m+m'+ r*c*c', c')
>   h []                        = []
>   h xs                        = [minimum xs]

Algebra product operation:

> infix ***
> (***) :: Eq answer1 =>
>          Matrixmult_Algebra alphabet answer1 ->
>          Matrixmult_Algebra alphabet answer2 ->
>          Matrixmult_Algebra alphabet (answer1, answer2)
> alg1 *** alg2 = (single, mult, h) where
>    (single1, mult1, h1) = alg1
>    (single2, mult2, h2) = alg2
> 
>    mult (x1,x2) (y1,y2) = (mult1 x1 y1, mult2 x2 y2)
>    single x             = (single1 x, single2 x)
> 
>    h xs = [(x1,x2)| x1 <- nub $ h1 [ y1 | (y1,y2) <- xs],
>                     x2 <-       h2 [ y2 | (y1,y2) <- xs, y1 == x1]]

The yield grammar:

> matrixmult alg f = axiom matrices where

>   (single, mult, h) = alg

>   matrices = tabulated (
>              single <<< achar                  |||
>              mult   <<< matrices +~+ matrices  ... h)

Bind input:

>   z         = mk f
>   (_,n)     = bounds z
>   axiom     = axiom' n
>   tabulated = table n
>   achar     = achar' z

Problem variation:
Minimizing intermediate storage:

> minmem :: Matrixmult_Algebra (Int, Int) (Int, Int, Int)
> minmem = (single, mult, h) where
>   single (r,c)              = (r,0,c) 
>   mult (r,m,c) (r',m',c')   = (r, minimum
>                                 [maximum [m,r*c+ m',r*c + r'* c' + r*c'], 
>                                  maximum [m',r'*c'+ m,r*c + r'* c' + r*c']],c')
>   h []                      = [] 
>   h l                       = [minimum(l)]

