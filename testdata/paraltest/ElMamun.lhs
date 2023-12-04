Haskell header:

> module ElMamun where

> import ADPCombinators
> import Data.Array
> import Data.List
 
The signature:

> data Bill = Mult Bill Char Bill |
>             Add  Bill Char Bill | 
>             Ext  Bill Char      | 
>             Val  Char  
>                                 deriving (Show, Eq)

Algebra type:

> type Bill_Algebra alphabet answer = (
>   alphabet -> answer,                       -- val
>   answer -> alphabet -> answer,             -- ext
>   answer -> alphabet -> answer -> answer,   -- add
>   answer -> alphabet -> answer -> answer,   -- mult
>   [answer] -> [answer]                      -- h
>   ) 

Enumeration algebra:

> enum :: Bill_Algebra Char Bill
> enum = (val, ext, add, mult, h) where
>   val       = Val  
>   ext       = Ext    
>   add       = Add      
>   mult      = Mult      
>   h         = id

Pretty printing algebra:

> prettyprint :: Bill_Algebra Char String
> prettyprint = (val, ext, add, mult, h) where
>   val   c     = [c]
>   ext   n c   = n ++ [c]
>   add   x c y = "(" ++ x ++ (c:y) ++ ")"
>   mult  x c y = "(" ++ x ++ (c:y) ++ ")"
>   h           = id

Counting algebra: 

> count :: Bill_Algebra Char Integer
> count = (val, ext, add, mult, h) where
>   val c       = 1
>   ext n c     = 1
>   add  x t y  = x * y
>   mult x t y  = x * y
>   h []        = []
>   h l         = [sum l]

The buyer's algebra:

> buyer :: Bill_Algebra Char Int
> buyer = (val, ext, add, mult, h) where
>   val c         = decode c
>   ext n c       = 10*n + decode c
>   add  x t y    = x + y
>   mult x t y    = x * y
>   h []          = []
>   h l           = [minimum l]

The seller's algebra:

> seller :: Bill_Algebra Char Int
> seller = (val, ext, add, mult, h) where
>   val c       = decode c
>   ext n c     = 10*n + decode c
>   add  x c y  = x + y
>   mult x c y  = x * y
>   h []        = []
>   h l         = [maximum l]

Computation time:

> time :: Bill_Algebra Char Int
> time = (val, ext, add, mult, h) where
>   val c       = 0
>   ext n c     = 0
>   add  x c y  = max x y + 2
>   mult x c y  = max x y + 5
>   h []        = []
>   h l         = [minimum l]

Algebra product operation:

> infix ***
> (***) :: Eq answer1 => 
>          Bill_Algebra alphabet answer1 -> Bill_Algebra alphabet answer2 ->
>          Bill_Algebra alphabet (answer1, answer2)

> alg1 *** alg2 = (val, ext, add, mult, h) where
>   (val1, ext1, add1, mult1, h1) = alg1
>   (val2, ext2, add2, mult2, h2) = alg2

>   val c                  = (val1 c,        val2 c)
>   ext (n1,n2) c          = (ext1 n1 c,     ext2 n2 c)
>   add (x1,x2) c (y1,y2)  = (add1 x1 c y1,  add2 x2 c y2)
>   mult (x1,x2) c (y1,y2) = (mult1 x1 c y1, mult2 x2 c y2)

>   h xs = [(x1,x2)| x1 <- nub $ h1 [ y1 | (y1,y2) <- xs],
>                    x2 <-       h2 [ y2 | (y1,y2) <- xs, y1 == x1]]

The yield grammar:

> bill :: Bill_Algebra Char answer -> [Char] -> [answer]
> bill alg f = axiom formula where
>   (val, ext, add, mult, h) = alg

>   formula = tabulated (
>             number |||
>             add  <<< formula  ~~-  plus   ~~~  formula  |||
>             mult <<< formula  ~~-  times  ~~~  formula  ... h)

>   number = val <<< digit ||| ext <<< number ~~- digit
>   digit  = char '0' ||| char '1' ||| char '2' ||| char '3' |||
>            char '4' ||| char '5' ||| char '6' ||| char '7' |||
>            char '8' ||| char '9'

>   plus  = char '+'
>   times = char '*'

Bind input:

>   z         = mk f
>   (_,n)     = bounds z
>   char      = char' z
>   tabulated = table n
>   axiom     = axiom' n

> decode c = fromEnum c - fromEnum '0'

