> module RNAshapesMfe.CommonFunctions where

> translate :: [Char] -> [Char]
> translate [] = []
> translate (x:xs) 
>	| x == 't'  = 'U' : translate xs 
>	| x == 'T'  = 'U' : translate xs
>       | x == 'u'  = 'U' : translate xs
>       | x == 'U'  = 'U' : translate xs
>	| x == 'a'  = 'A' : translate xs 
>	| x == 'A'  = 'A' : translate xs 
>	| x == 'c'  = 'C' : translate xs 
>	| x == 'C'  = 'C' : translate xs 
>	| x == 'g'  = 'G' : translate xs 
>       | x == 'G'  = 'G' : translate xs
>	| otherwise =  error ("Wrong Character in sequence: "++x: xs)

app verieinigt beim zusammenfuegen der strings aufeinanderfolgende  "_"s zu einem "_"

> app :: String -> String -> String
> app [] ys = ys
> app "_" "_" = "_"
> app (x:[]) (y:[]) = x:y:[]
> app (x:[]) (y:ys) = app (app (x:[]) (y:[])) ys
> app (x:xs) ys = x : app xs ys 

> sum_tuples (x1,x2,x3,x4,x5,x6) (y1,y2,y3,y4,y5,y6) = (x1+y1,x2+y2,x3+y3,x4+y4,x5+y5,x6+y6)
> sum_tuples' (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1+y1,x2+y2,x3+y3,x4+y4)

> sum_elems (x1,x2,x3,x4) = x1+x2+x3+x4
> sum_elems' (x1,x2,x3,x4,x5,x6) = x1+x2+x3+x4+x5+x6
