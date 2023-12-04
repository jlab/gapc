> module RNAshapesMfe.Algebra_prettyprint where

--###Berechnet fuer jeden Kandidaten des Suchraums den Dot Bracket String seiner Struktur
--###[String] mehrelementige Liste

> import RNAshapesMfe.AlgebraType

Pretty printing algebra:

> prettyprint :: Char -> Char -> a -> b ->
>                Canonical_Algebra i (Int,Int) String String String
> prettyprint left right _ _ = 
>         (sadd,cadd,cadd',cadd'',cadd''',ambd,ambd',nil,nil',edl,edr,edlr,drem,is,sr,hl,hlChar,sp,bl,br,il,
>          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,
>	   ssadd,trafo,incl,combine,lcombine,lcombine',rcombine,rcombine',
>	   lrcombine,acomb,lacomb,lacomb',racomb,racomb',lracomb,h,h_i,h_l,h_s) where
>   sadd _  s  = '.':s
>   cadd s1 s2 = s1++s2
>   cadd' s1 s2 = s1++s2
>   cadd'' s1 s2 = s1++s2
>   cadd''' s1 s2 = s1++s2
>   ambd s1 b s2 = s1++('.':s2)
>   ambd' s1 b s2 = s1++('.':s2)
>   nil  _     = []
>   nil'  _     = []
>   edl  _  s   = left:s
>   edr  s  _   = s++right:[]
>   edlr _  s _ = left:s++right:[]
>   drem        = id
>   is          = id
>   sr   _  s _ = '(':s++")"
>   hl   _  _ (h1,h2) _ _   = '(':'(':dots (h2-h1)++"))"
>   hlChar   _  _ (h1,h2) _ _   = '(':'{':dots (h2-h1)++"})"
>   sp   _  _ s _ _ = '(':'(':s++"))"
>   bl   (l1,l2) s  = dots (l2-l1)++s
>   br   s (r1,r2)  = s++dots (r2-r1)
>   il  (l1,l2) s (r1,r2) = dots (l2-l1)++s++dots (r2-r1)
>   ml    _ _ s _ _     = '(':'(':s++"))"
>   mldr  _ _ s _ _ _   = '(':'(':s++left:"))"
>   mladr  _ _ s _ _ _   = '(':'(':s++left:"))"
>   mldlr _ _ _ s _ _ _ = '(':'(':right:s++left:"))"
>   mladlr _ _ _ s _ _ _ = '(':'(':right:s++left:"))"
>   mldladr _ _ _ s _ _ _ = '(':'(':right:s++left:"))"
>   mladldr _ _ _ s _ _ _ = '(':'(':right:s++left:"))"
>   mldl  _ _ _ s _ _   = '(':'(':right:s++"))"
>   mladl  _ _ _ s _ _   = '(':'(':right:s++"))"
>   addss s (r1,r2) = s++dots (r2-r1)
>   ssadd (l1,l2) s = dots (l2-l1)++s
>   trafo  s1    = s1
>   incl  s     = s
>   combine s1 s2 = s1 ++ s2
>   lcombine s1 s2 = s1 ++ s2
>   lcombine' s1 s2 = s1 ++ s2
>   rcombine s1 s2 = s1 ++ s2
>   rcombine' s1 s2 = s1 ++ s2
>   lrcombine s1 s2 = s1 ++ s2
>   acomb s1 b s2 = s1 ++ '.' : s2
>   lacomb s1 b s2 = s1 ++ '.' : s2
>   lacomb' s1 b s2 = s1 ++ '.' : s2
>   racomb s1 b s2 = s1 ++ '.' : s2
>   racomb' s1 b s2 = s1 ++ '.' : s2
>   lracomb s1 b s2 = s1 ++ '.' : s2
>   h   = id
>   h_i = id
>   h_l = id
>   h_s = id
>   dots i = replicate i '.'

> prettyprint1 = prettyprint '\\' '/'
> prettyprint2 = prettyprint '/' '\\'

DotBracket algebra:

> dotBracket = prettyprint '.' '.'
