----------------------------------------------------------------------
ADP combinators and functions from:

R. Giegerich, C. Meyer and P. Steffen. Towards a discipline of dynamic
programming.

----------------------------------------------------------------------

> module ADPTriCombinators where
> import Data.Array

Lexical parsers
----------------

> type Subword  = (Int,Int)
> type Parser b = Subword -> [b]

> empty        :: Parser ()
> empty  (i,j) =  [() | i == j]

> acharSep'           ::  Array Int Char -> Char -> Parser Char
> acharSep' z s (i,j) =  [z!j | i+1 == j, z!j /= s] 

> achar'         :: Array Int a -> Parser a
> achar' z (i,j) = [z!j | i+1 == j]

> char'           ::  Eq a => Array Int a -> a -> Parser a
> char' z c (i,j) =  [c | i+1 == j, z!j == c]

> astring       :: Parser Subword
> astring (i,j) =  [(i,j) | i <= j]

> string'           :: Eq a => Array Int a -> [a] -> Parser Subword
> string' z s (i,j) = [(i,j)| and [z!(i+k) == s!!(k-1) | k <-[1..(j-i)]]]

Parser combinators
-------------------

> infixr 6 ||| 
> (|||)           :: Parser b -> Parser b -> Parser b
> (|||) r q (i,j) = r (i,j) ++ q (i,j)

> infix  8 <<<
> (<<<)           :: (b -> c) -> Parser b -> Parser c
> (<<<) f q (i,j) =  map f (q (i,j))

> infixl 7 ~~~
> (~~~)           :: Parser (b -> c) -> Parser b -> Parser c
> (~~~) r q (i,j) =  [f y | k <- [i..j], f <- r (i,k), y <- q (k,j)]  

> infix  5 ...
> (...)           :: Parser b -> ([b] -> [b]) -> Parser b
> (...) r h (i,j) = h (r (i,j))


> type Filter    =  (Int, Int) -> Bool
> with           :: Parser b -> Filter -> Parser b
> with q c (i,j) =  if c (i,j) then q (i,j)  else []

> axiom'        :: Int -> Parser b -> [b]
> axiom' l ax   =  ax (0,l) 

Tabulation
-----------

> table     :: Int -> Parser b -> Parser b
> table n p = lockup where
>   lockup (i,j) =  if i <= j then t!adr (i,j) else []
>   t            = array (0,(n+1)*(n+2) `div` 2)
>                       [(adr (i,j),p (i,j)) | i<- [0..n], j<- [i..n]]
>   adr (i,j)    = n*i - (i*(i-1)) `div` 2 + j

Variants of the <<< and ~~~ Combinators
-------------------------------

> infix  8 ><<
> infixl 7 ~~, ~~*, *~~, *~*
> infixl 7 -~~, ~~-, +~~, ~~+, +~+

The operator ><< is the special case of <<< for a nullary function f

> (><<)           :: c -> Parser b -> Parser c
> (><<) f q (i,j) =  [f|a <- (q (i,j))]

Subwords on left and right of an explicit length range.

> (~~) :: (Int,Int) -> (Int,Int) 
>      -> Parser (b -> c) -> Parser b -> Parser c
> (~~) (l,u) (l',u') r q (i,j) 
>      = [x y | k <- [max (i+l) (j-u') .. min (i+u) (j-l')],
>               x <- r (i,k), y <- q (k,j)]

Subwords of explicit length range and unbounded length on one or on
either side.

> (~~*) :: (Int,Int) -> Int 
>       -> Parser (a -> b) -> Parser a -> Parser b 
> (~~*) (l, u) l' r q (i, j) 
>       = [x y | k <- [(i + l) .. min (i + u) (j - l')], 
>                x <- r (i, k), y <- q (k, j)] 

> (*~~) :: Int -> (Int,Int) 
>       -> Parser (a -> b) -> Parser a -> Parser b 
> (*~~) l (l', u') r q (i, j) 
>       = [x y | k <- [max (i + l) (j - u') .. (j - l')], 
>           x <- r (i, k), y <- q (k, j)] 

> (*~*) :: Int -> Int 
>       -> Parser (a -> b) -> Parser a -> Parser b 
> (*~*) l l' r q (i, j) 
>       = [x y | k <- [(i + l) .. (j - l')], 
>                x <- r (i, k), y <- q (k, j)] 

Single character on the lefthand (respecitvely righthand) side

> (-~~)           :: Parser (b -> c) -> Parser b -> Parser c
> (-~~) q r (i,j) = [x y | i<j, x <- q (i,i+1), y <- r (i+1,j)]

> (~~-)           :: Parser (b -> c) -> Parser b -> Parser c
> (~~-) q r (i,j) = [x y | i<j, x <- q (i,j-1), y <- r (j-1,j)]

Nonempty sequence on the lefthand (respecitvely righthand) side

> (+~~)           :: Parser (b -> c) -> Parser b -> Parser c
> (+~~) r q (i,j) =  [f y | k <- [i+1..j], f <- r (i,k), y <- q (k,j)]  

> (~~+)           :: Parser (b -> c) -> Parser b -> Parser c
> (~~+) r q (i,j) =  [f y | k <- [i..j-1], f <- r (i,k), y <- q (k,j)]  

Nonempty sequence on either side

> (+~+)           :: Parser (b -> c) -> Parser b -> Parser c
> (+~+) r q (i,j) = [f y | k <- [(i+1)..(j-1)], f <- r (i,k), y <- q (k,j)]  


Create array from List
-----------------------

> mk :: [a] -> Array Int a
> mk xs = array (1,n) (zip [1..n] xs) where n = length xs
