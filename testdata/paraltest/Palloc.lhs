Haskell header:

> module Palloc where

> import ADPCombinators
> import Data.Array
> import Data.List

> type Alphabet = Char
 
The signature:

> data Palindrome = Match Alphabet Palindrome Alphabet |
>                   Empty Subword             |
>                   Skipl Alphabet Palindrome |
>                   Skipr Palindrome Alphabet
>                     deriving (Show, Eq)


Algebra type:

> type Pal_Algebra alphabet answer = (
>   alphabet -> answer -> alphabet -> answer, -- match
>   Subword -> answer,                       -- empty 
>   alphabet -> answer -> answer, -- skipl
>   answer -> alphabet -> answer, -- skipr
>   [answer] -> [answer]                      -- h
>   ) 

Enumeration algebra:

> enum :: Pal_Algebra Alphabet Palindrome
> enum = (match, empty, skipl, skipr, h) where
>   match = Match
>   empty = Empty
>   skipl = Skipl
>   skipr = Skipr
>   h     = id

Pretty printing algebra:

> pretty :: Pal_Algebra Alphabet String
> pretty = (match, empty, skipl, skipr, h) where
>   match a b c = a:b
>   empty x     = []
>   skipl c l   = l
>   skipr l c   = l
>   h           = id


Counting algebra: 

> count :: Pal_Algebra Alphabet Integer
> count = (match, empty, skipl, skipr, h) where
>   match a b c = b
>   empty x     = 1
>   skipl c l   = l
>   skipr l c   = l
>   h []        = []
>   h l         = [sum l]

> score :: Pal_Algebra Alphabet Integer
> score = (match, empty, skipl, skipr, h) where
>   match a b c = b + 1
>   empty x     = 0
>   skipl c l   = l
>   skipr l c   = l
>   h []        = []
>   h l         = [maximum l]



Algebra product operation:

> infix ***
> (***) :: Eq answer1 => 
>          Pal_Algebra alphabet answer1 -> Pal_Algebra alphabet answer2 ->
>          Pal_Algebra alphabet (answer1, answer2)

> alg1 *** alg2 = (match, empty, skipl, skipr, h) where
>   (match1, empty1, skipl1, skipr1, h1) = alg1
>   (match2, empty2, skipl2, skipr2, h2) = alg2

>   match a (b,c) d = (match1 a b d, match2 a c d)
>   skipl a (b,c) = (skipl1 a b, skipl2 a c)
>   skipr (a,b) c = (skipr1 a c, skipr2 b c)
>   empty a         = (empty1 a, empty2 a)

>   h xs = [(x1,x2)| x1 <- nub $ h1 [ y1 | (y1,y2) <- xs],
>                    x2 <-       h2 [ y2 | (y1,y2) <- xs, y1 == x1]]

The yield grammar:

> pal :: Pal_Algebra Alphabet answer -> [Alphabet] -> [answer]
> pal alg f = axiom skip_l where
>   (match, empty, skipl, skipr, h) = alg

>   skip_l = tabulated(
>            skipl <<< achar -~~ skip_l |||
>            skip_r ... h
>            )

>   skip_r = tabulated(
>            skipr <<< skip_r ~~- achar |||
>            palin ... h
>            )


>   palin = tabulated(

>           (match <<< achar -~~ palin ~~- achar) `with` equal |||

           empty <<< astring

           empty <<< separated -- astring

>           empty <<< astring `with` (minsize 1)

>                          ... h
>                                )


Bind input:

>   z         = mk f
>   (_,n)     = bounds z
>   achar     = achar' z
>   tabulated = table n
>   axiom     = axiom' n

>   separated (i,j) = [(i,j) | i < j]

>   equal:: Filter
>   equal (i,j) = (i+1 < j) && (z!(i+1) == z!(j))

>   minsize k (i, j) = j-i > 0

> inp = "1abccba12xyzzyx2"


