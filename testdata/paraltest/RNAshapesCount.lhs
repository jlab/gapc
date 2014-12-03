> module Main where

> import System.Environment
> import System.IO
> import Numeric
> import Data.Array
> import Data.List(nub,sort,sortBy) 
> import RnaI
> import ADPTriCombinators

> main :: IO()
> main  = do
>	  (_:arg1:[]) <- getArgs
>	  let input	 = arg1
>	      result	 = formatCount (canonicals_nonamb (-100.0) (count) input)

>	  putStr (result++"\n")

> formatCount :: [Integer] -> String
> formatCount [] = ""
> formatCount (x:xs) = show x

The signature:

> data Closed = Sadd Base Closed        |
>               Cadd Closed Closed      |
>               Ambd Closed Base Closed |
>               Nil                     |
>               Edl Base Closed                              |
>               Edr Closed Base                              |
>               Edlr Base Closed Base                        |
>               Drem Closed                                  |
>		Is Closed			             |  
>               Sr Base Closed Base                          |
>               Hl Base Base (Int,Int) Base Base             |
>               Sp Base Base Closed Base Base                |
>               Bl (Int,Int) Closed                          |
>               Br Closed (Int,Int)                          |
>               Il (Int,Int) Closed (Int,Int)                |
>               Ml Base Base Closed Base Base                |
>               Mldr Base Base Closed Base Base Base         |
>		Mladr Base Base Closed Base Base Base        |
>               Mldlr Base Base Base Closed Base Base Base   |
>               Mladlr Base Base Base Closed Base Base Base  |
>               Mldladr Base Base Base Closed Base Base Base  |
>               Mladldr Base Base Base Closed Base Base Base  |
>               Mldl Base Base Base Closed Base Base         |
>               Mladl Base Base Base Closed Base Base        |
>               Addss Closed (Int,Int)                       |
>               Ssadd (Int,Int) Closed                       |
>               Trafo Closed                                  |
>               Combine Closed Closed                        |
>               Acomb Closed Base Closed                     |
>               Incl Closed                                    |
>               CL                                           | -- Wird fuer shapes benoetigt
>               FK Closed Closed                             | -- Wird fuer shapes benoetigt
>               AD Closed Closed                             | -- Wird fuer shapes benoetigt
>               OP                                           | -- Wird fuer shapes benoetigt
>               E                                            deriving (Eq, Ord, Show) -- Wird fuer shapes benoetigt

Algebra type:

> type Canonical_Algebra alph1 alph2 closed answer pf_closed =
>  (alph1 -> closed -> closed,  --sadd
>   closed -> closed -> closed, --cadd
>   closed -> pf_closed -> closed, --cadd'
>   closed -> closed -> pf_closed, --cadd''
>   closed -> pf_closed -> pf_closed, --cadd'''
>   closed -> alph1 -> pf_closed -> closed, --ambd
>   closed -> alph1 -> pf_closed -> pf_closed, --ambd'
>   () -> closed,               --nil
>   () -> pf_closed,            --nil'
>   alph1 -> closed -> closed,  --edl
>   closed -> alph1 -> closed,  --edr
>   alph1 -> closed -> alph1 -> closed,  --edlr
>   closed -> closed,                    --drem   
>   closed -> closed,                    --is
>   alph1 -> closed -> alph1 -> closed,  --sr
>   alph1 -> alph1 -> alph2 -> alph1 -> alph1 -> closed,  --hl
>   alph1 -> alph1 -> closed -> alph1 -> alph1 -> closed, --sp
>   alph2 -> closed -> closed ,                           --bl         
>   closed -> alph2 -> closed,                            --br
>   alph2 -> closed -> alph2 -> closed,                   --il
>   alph1 -> alph1 -> answer -> alph1 -> alph1 -> closed,                   --ml
>   alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed,          --mldr
>   alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed,          --mladr
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed, --mldlr
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed, --mladlr
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed, --mldladr
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed, --mladldr
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> closed,          --mldl
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> closed,          --mladl
>   answer -> alph2 -> answer,  --addss
>   alph2 -> closed -> answer,  --ssadd
>   pf_closed -> closed, --trafo
>   closed -> answer,                   --incl
>   answer -> answer -> answer,  --combine
>   answer -> answer -> answer,  --lcombine
>   answer -> answer -> answer,  --lcombine'
>   answer -> answer -> answer,  --rcombine
>   answer -> answer -> answer,  --rcombine'
>   answer -> answer -> answer,  --lrcombine
>   answer -> alph1 -> answer -> answer, --acomb
>   answer -> alph1 -> answer -> answer, --lacomb
>   answer -> alph1 -> answer -> answer, --lacomb'
>   answer -> alph1 -> answer -> answer, --racomb
>   answer -> alph1 -> answer -> answer, --racomb'
>   answer -> alph1 -> answer -> answer, --lracomb
>   [closed] -> [closed], --h
>   [answer] -> [answer], --h_i
>   [closed] -> [closed], --h_l
>   [pf_closed] -> [pf_closed]  --h_s
>   )

Counting algebra:

> count :: a -> b -> Canonical_Algebra i (Int,Int) Integer Integer Integer
> count _ _ = (sadd,cadd,cadd',cadd'',cadd''',ambd,ambd',nil,nil',edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il,
>              ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,trafo,incl,combine,lcombine,lcombine',rcombine,rcombine',lrcombine,acomb,lacomb,lacomb',racomb,racomb',lracomb,h,h_i,h_l,h_s) where
>    sadd _ b = b
>    cadd a b = a*b
>    cadd' a b = a*b
>    cadd'' a b = a*b
>    cadd''' a b = a*b
>    ambd a _ b = a*b
>    ambd' a _ b = a*b
>    nil _ = 1
>    nil' _ = 1
>    edl _ b = b
>    edr a _ = a
>    edlr _ b _ = b
>    drem a = a
>    is a = a
>    sr _ b _ = b
>    hl _ _ _ _ _ = 1
>    sp _ _ c _ _ = c
>    bl _ d = d
>    br c _ = c
>    il _ c _ = c
>    ml _ _ c _ _ = c
>    mldr _ _ c _ _ _ = c
>    mladr _ _ c _ _ _ = c
>    mldlr _ _ _ d _ _ _ = d
>    mladlr _ _ _ d _ _ _ = d
>    mldladr _ _ _ d _ _ _ = d
>    mladldr _ _ _ d _ _ _ = d
>    mldl _ _ _ d _ _ = d
>    mladl _ _ _ d _ _ = d
>    addss a _ = a
>    ssadd _ b = b
>    trafo a  = a
>    incl a  = a
>    combine a b = a*b
>    lcombine a b = a*b
>    lcombine' a b = a*b
>    rcombine a b = a*b
>    rcombine' a b = a*b
>    lrcombine a b = a*b
>    acomb a _ b = a*b
>    lacomb a _ b = a*b
>    lacomb' a _ b = a*b
>    racomb a _ b = a*b
>    racomb' a _ b = a*b
>    lracomb a _ b = a*b
>    h [] = []
>    h xs = [sum xs]
>    h_i= h
>    h_l= h
>    h_s= h



The yield grammar for non-ambigous dangling bases:

> canonicals_nonamb takes alg inp_tr = axiom struct where
>  
>     (sadd,cadd,cadd',cadd'',cadd''',ambd,ambd',nil,nil',edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il,
>      ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,
>      ssadd,trafo,incl,combine,lcombine,lcombine',rcombine,rcombine',
>      lrcombine,acomb,lacomb,lacomb',racomb,racomb',lracomb,h,h_i,h_l,h_s) = alg baseArray takes

Bind input:

>     inp       = translate inp_tr
>     axiom     = axiom' n
>     z         = mk (inp)
>     (_,n)     = bounds z
>     baseArray     = (fst.str2inp) inp
>     base (i,j)= [ j | (i+1) == j ]
>     region (i,j) =  [(i,j) | i < j]
>     tabulated = table n
>     listed :: Parser a -> Parser a
>     listed p = q $ array (0,n) [(j, p (j,n)) | j <- [0..n]] where
>       q t (i,j) = if j==n then t!i else []
>     
>     infixl 7 ~~!
>     (~~!) = (~~*) (2,2) 3
>     infixl 7 ~~!!
>     (~~!!) = (~~*) (3,3) 14
>     minloopsize :: Int -> Filter
>     minloopsize n = match inp where
>       match inp (i,j) = i+n<=j
>     maxsize n = match inp where
>	match inp (i,j) = j-i<=n
>     stackpairing :: Filter
>     stackpairing  = match inp where
>       match inp (i,j) = i+3<j && basepair (z!(i+1), z!(j))
>                               && basepair (z!(i+2), z!(j-1))
>     basepairing :: Filter
>     basepairing  = match inp where
>       match inp (i,j) = i+1<j && basepair (z!(i+1), z!(j))
>     basepair ('A','U') = True
>     basepair ('U','A') = True
>     basepair ('C','G') = True
>     basepair ('G','C') = True
>     basepair ('G','U') = True
>     basepair ('U','G') = True
>     basepair ( x , y ) = False

grammar[struct]{

>     struct        =           left_dangle    ||| 
>	              (trafo <<< noleft_dangle) |||
>                               left_unpaired  ... h

>     left_unpaired = sadd <<< base    -~~ left_unpaired  |||
>                     sadd <<< base    -~~ left_dangle    ... h

>     left_dangle   = listed (
>                     ambd  <<< edanglel  ~~- base ~~~ noleft_dangle               ||| 
>                     cadd' <<< edanglel  ~~~ (noleft_dangle ||| (nil' <<< empty)) |||
>                     cadd  <<< edanglelr ~~~ (left_dangle ||| left_unpaired)      |||
>                     nil   <<< empty                                              ... h)

>     noleft_dangle = listed (
>                     cadd''  <<< edangler ~~~ (left_dangle  ||| left_unpaired)     |||
>	              cadd''' <<< nodangle ~~~ (noleft_dangle ||| (nil' <<< empty)) |||
>		      ambd'   <<< nodangle ~~- base ~~~ noleft_dangle               ... h_s)
             
>     edanglel      = edl  <<< base -~~ initstem          ... h
>     edangler      = edr  <<<          initstem ~~- base ... h
>     edanglelr     = edlr <<< base -~~ initstem ~~- base ... h
>     nodangle      = drem <<<          initstem          ... h

>     initstem      = is <<< closed ... h_l

>     closed        = tabulated (
>                     stack ||| hairpin ||| multiloop ||| leftB ||| rightB ||| iloop ... h) 

>                     
>     multiloop     = (mldl   <<< base -~~ base ~~-   base ~~!! ml_comps1            ~~- base ~~- base |||
>                      mladl  <<< base -~~ base ~~-   base ~~!! ml_comps2            ~~- base ~~- base |||  -- ambiguous dangle
>                      mldr   <<< base -~~ base ~~!             ml_comps3   ~~- base ~~- base ~~- base |||
>                      mladr  <<< base -~~ base ~~!             ml_comps2   ~~- base ~~- base ~~- base |||  -- ambiguous dangle
>                      mldlr  <<< base -~~ base ~~-   base ~~!! ml_comps4   ~~- base ~~- base ~~- base |||
>                      mladlr <<< base -~~ base ~~-   base ~~!! ml_comps2   ~~- base ~~- base ~~- base |||  -- ambiguous dangle both
>                      mldladr<<< base -~~ base ~~-   base ~~!! ml_comps1   ~~- base ~~- base ~~- base |||  -- ambiguous dangle right
>                      mladldr<<< base -~~ base ~~-   base ~~!! ml_comps3   ~~- base ~~- base ~~- base |||  -- ambiguous dangle left
>                      ml     <<< base -~~ base ~~!             ml_comps2            ~~- base ~~- base ) `with` stackpairing     -- ... h

>     ml_comps1     = tabulated (
>                     combine <<< block_dl  ~~~ no_dl_no_ss_end          |||
>	              combine <<< block_dlr ~~~ dl_or_ss_left_no_ss_end  |||
>		      acomb   <<< block_dl  ~~- base ~~~ no_dl_no_ss_end ... h_i)

>     ml_comps2     = tabulated (
>                     combine <<< (incl <<< nodangle) ~~~ no_dl_no_ss_end          ||| 
>	              combine <<< (incl <<< edangler) ~~~ dl_or_ss_left_no_ss_end  |||
>		      acomb   <<< (incl <<< nodangle) ~~- base ~~~ no_dl_no_ss_end ... h_i)

>     ml_comps3     = combine <<< (incl <<< edangler) ~~~ dl_or_ss_left_ss_end  |||
>	              combine <<< (incl <<< nodangle) ~~~ no_dl_ss_end          |||
>		      acomb   <<< (incl <<< nodangle) ~~- base ~~~ no_dl_ss_end ... h_i

>     ml_comps4     = combine <<< block_dl  ~~~  no_dl_ss_end        |||
>	              combine <<< block_dlr ~~~ dl_or_ss_left_ss_end |||
>		      acomb   <<< block_dl ~~- base ~~~ no_dl_ss_end ... h_i

>     block_dl      = tabulated(
>		      ssadd <<< region ~~~ edanglel |||
>		      incl    <<< edanglel            ... h_i)

>     block_dlr     = tabulated(
>		      ssadd <<< region ~~~ edanglelr |||
>		      incl    <<< edanglelr            ... h_i)

>     no_dl_no_ss_end =        ml_comps2 |||
>		        incl <<< nodangle  ... h_i

>     dl_or_ss_left_no_ss_end = ml_comps1 |||
>			        block_dl  ... h_i

>     no_dl_ss_end = tabulated (
>			          ml_comps3                    |||
>		        incl    <<< edangler                     |||
>		        addss <<< (incl <<< edangler) ~~~ region ... h_i)

>     dl_or_ss_left_ss_end = tabulated (
>				          ml_comps4            |||
>			                  block_dlr            |||
>			        addss <<< block_dlr ~~~ region ... h_i)

>     stack         = (sr  <<< base -~~ closed ~~- base) `with` basepairing

>     hairpin       = (hl  <<< base -~~ base ~~! (region `with` minloopsize 3)
>                          ~~- base ~~- base)
>                     `with` stackpairing

>     leftB         = (sp  <<< base -~~ base ~~! (bl <<< region  ~~~ initstem)
>                          ~~- base ~~- base)
>                     `with` stackpairing -- ... h

>     rightB        = (sp  <<< base -~~ base ~~! (br <<< initstem ~~~ region)
>                          ~~- base ~~- base)
>                     `with` stackpairing -- ... h

>     iloop         = (sp  <<< base -~~ base ~~! (il <<< (region `with` (maxsize 30)) ~~~ closed ~~~ (region `with` (maxsize 30)))
>	                   ~~- base ~~- base)
>	              `with` stackpairing -- ... h

}

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
