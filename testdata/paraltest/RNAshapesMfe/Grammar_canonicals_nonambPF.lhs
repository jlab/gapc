> module RNAshapesMfe.Grammar_canonicals_nonambPF where

> import System.Environment
> import System.IO
> import Numeric
> import Data.Array
> import Data.List(nub,sort,sortBy) 
> import RNAshapesMfe.RnaI
> import RNAshapesMfe.Energy
> import ADPTriCombinators

 import Random_number

> import RNAshapesMfe.CommonFunctions

The yield grammar for non-ambigous dangling bases:

> canonicals_nonamb takes alg inp_tr = axiom struct where
>  
>     (sadd,cadd,cadd',cadd'',cadd''',ambd,ambd',nil,nil',edl,edr,edlr,drem,is,sr,hl,hlChar,sp,bl,br,il,
>      ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,
>      ssadd,trafo,incl,combine,
>      acomb,h,h_i,h_l,h_s) = alg baseArray takes

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

>     leftB         = (sp  <<< base -~~ base ~~! (bl <<< region  ~~~ closed)
>                          ~~- base ~~- base)
>                     `with` stackpairing -- ... h

>     rightB        = (sp  <<< base -~~ base ~~! (br <<< closed ~~~ region)
>                          ~~- base ~~- base)
>                     `with` stackpairing -- ... h

>     iloop         = (sp  <<< base -~~ base ~~! (il <<< (region `with` (maxsize 30)) ~~~ closed ~~~ (region `with` (maxsize 30)))
>	                   ~~- base ~~- base)
>	              `with` stackpairing -- ... h

}

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
