          ********************************************
          *         Evaluation Algebras              *
          *                                          *
          *       Algebra type, enumeration,         *
          *   cross product, counting, energy,       *
          *       base pair maximization             *
          ********************************************

1. Algebra type
4. Energy maximization
5. base pair maximization
6. prettyprint (dot-bracket)
7. partition function
8. ***-operator
 
 
> module Algebras_ad where

> import Data.Array
> import Data.List(nub,sort)
> import System.IO.Unsafe
> import Data.Map(Map,toList,fromListWith,empty,insertWith',insertWith,member,insert)
> import Data.HashTable as HT

> import RNACombinators
> import Foldingspace
> import Energy2
> import Intloop
> import Intloop21
> import Intloop22
> import DeepSeq
> import RandomNumber


1. Algebra type

> type FS_Algebra base  comp cmpl = (   
>            base -> cmpl -> cmpl ,  -- sadd
>	     comp -> cmpl -> cmpl ,  -- cadd

>            base -> comp   -> base -> comp, --dlr
>	     base -> comp   -> base -> comp, --sr
>	     base -> base -> Region                   -> base -> base -> comp, --hl
>	     base -> base -> Region -> comp           -> base -> base -> comp, --bl
>	     base -> base ->           comp -> Region -> base -> base -> comp, --br
>	     base -> base -> Region -> comp -> Region -> base -> base -> comp, --il
>	     base -> base ->           cmpl           -> base -> base -> comp, --ml

>	     cmpl -> cmpl -> cmpl,     -- append
>	     comp ->  cmpl,	       -- ul
>	     cmpl -> Region -> cmpl,   -- addss
>	     Region -> comp -> cmpl,   -- ssadd

>	     cmpl,  --nil
>	     [comp] -> [comp],  --h
>	     [cmpl] -> [cmpl],  --h_l
>	     [cmpl] -> [cmpl]   --h_s
>	   ) 	      	     

> data Fold = Sadd Int Fold |
>             Cadd  Fold Fold |
>             Dlr Int Fold Int |
>             Sr Int Fold Int |
>             Hl Int Int Region Int Int |
>             Bl Int Int Region Fold Int Int |
>             Br Int Int Fold Region Int Int |
>             Il Int Int Region Fold Region Int Int |
>             Ml Int Int Fold Int Int |
>             Append Fold Fold |
>             Ul Fold |
>             Addss Fold Region |
>             Ssadd Region Fold |
>             Nil 
>               deriving (Show, Eq, Ord)

> enum = enumalg 

> enumalg :: RNAInput -> FS_Algebra Int Fold Fold
> enumalg inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	            append, ul, addss, ssadd, nil, h, h_l, h_s)
>     where
>       sadd =Sadd
>       cadd =Cadd

>       dlr =Dlr
>       sr =Sr
>       hl =Hl
>       bl =Bl
>       br =Br
>       il =Il
>       ml =Ml

>       append =Append
>       ul =Ul
>       addss =Addss
>       ssadd =Ssadd
>       nil =Nil

>       h = id
>       h_l = id
>       h_s = id
 
===================================
2. Counting Algebra
===================================

> count  = countalg

> countalg :: RNAInput -> FS_Algebra Int Integer Integer
> countalg inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	            append, ul, addss, ssadd, nil, h, h_l, h_s)
>     where
>       sadd  lb e = e
>       cadd  x  e = x * e

>       dlr lb e rb = e
>       sr  lb e rb = e
>       hl  lb _ r _ rb = 1
>       bl  bl _ x e _ br = e
>       br  bl _ e x _ br = e
>       il  _ _ _  x _ _ _ = x
>       ml  bl _     x _ br  = x

>       append c1 c = c1 * c
>       ul  c1 = c1
>       addss  c1 r = c1
>       ssadd  r x = x
>       nil = 1

we make the count algebra strict by adding the deepseq operator $!!

>       h [] = []
>       h es = id $!! [sum es]
>       h_l [] = []
>       h_l es = id $!! [sum es]
>       h_s [] = []
>       h_s es = id $!! [sum es]


=============================
3. Energy Algebra
=============================

> mfe:: RNAInput -> FS_Algebra Int Int Int
> mfe = energyalg 0

> subopt:: Int -> RNAInput -> FS_Algebra Int Int Int
> subopt k = energyalg k

> energyalg :: Int -> RNAInput -> FS_Algebra Int Int Int
> energyalg k inp =  (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	            append, ul, addss, ssadd, nil, h, h_l, h_s)
>     where
>       sadd  lb e = e
>	cadd  e1 e = e1 + e

>	dlr dl    e    dr  = e + dl_energy inp (dl+1,dr) + dr_energy inp (dl+1,dr) 
>                              + termaupenalty (inp!(dl+1)) (inp!dr)
>	sr  lb    e    rb  = e + sr_energy inp (lb,rb)
>	hl  llb lb  _      rb rrb =     hl_energy inp (lb,rb) +  sr_energy inp (llb,rrb)
>	bl  llb lb  x e    rb rrb = e + bl_energy inp lb x rb +  sr_energy inp (llb,rrb)
>	br  llb lb    e x  rb rrb = e + br_energy inp lb x rb +  sr_energy inp (llb,rrb)
>	il  llb lb lr e rr rb rrb = e + il_energy inp lr rr   +  sr_energy inp (llb,rrb)
>	ml  llb lb    e    rb rrb = 380 + e + termaupenalty (inp!lb) (inp!rb) +  sr_energy inp (llb,rrb)
>                                           + dli_energy inp (lb,rb) + dri_energy inp (lb,rb)

>	append  e1 e   = e1 + e
>	addss    e r =      e + ss_energy r
>	ul       e   = 40 + e
>	ssadd r  e   = 40 + e + ss_energy r
>	nil = 0

	h   [] = []
	h   es = [minimum es]
	h_l [] = []
	h_l es = [minimum es]
	h_s [] = []
	h_s es = [minimum es] 

>	h   [] = []
>	h   es = id $!! filter (<= (lowest + k)) es  --take k $ sort es -- [minimum es]
>                where lowest = minimum es

>	h_l [] = []
>	h_l es = id $!!  filter (<= (lowest + k)) es  --take k $ sort es -- [minimum es]
>                where lowest = minimum es
>	h_s [] = []
>	h_s es = h_l es

======================================
4. Basepairmaximization
======================================

> maxalg  f inp
>   = 
>     (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>          append, ul, addss, ssadd, nil, h', h_l', h_s')
>     where
>       h' [] = []
>       h' l = [maximum l]
>       h_l' [] = []
>       h_l' l = [maximum l]
>       h_s' [] = []
>       h_s' l = [maximum l]
>       (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>          append, ul, addss, ssadd, nil, h, h_l, h_s) = f inp

> bp :: RNAInput -> FS_Algebra Int Int Int
> bp = basepairalg 

> basepairalg :: RNAInput -> FS_Algebra Int Int Int
> basepairalg inp =  (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	            append, ul, addss, ssadd, nil, h, h_l, h_s)
>     where
>	sadd  lb e  = e		
>	cadd  x  e  = x + e

>	dlr lb e rb = e	
>	sr  lb e rb = e + 1	
>	hl  lb _ r _  rb  = 2	
>	bl  bl _ x e _ br  = 2 + e	
>	br  bl _ e x _ br  = 2 + e	
>	il  _ _ _  x _ _ _ = x + 2 
>	ml  bl _   x _ br  = x + 2

>	append  c1 c = c1 + c
>	ul  c1 = c1
>	addss  c1 r = c1	
>	ssadd  r x = x		
>	nil = 0
 
>	h [] = []
>	h es = es -- [maximum es]
>       h_l [] = []
>	h_l es = es --[maximum es]
>	h_s [] = []
>	h_s es = es -- [maximum es]


===================================
5. Prettyprint Algebra (dot bracket notation)
===================================

> pp :: RNAInput -> FS_Algebra Int [Char] [Char]

> pp  = prettyprintalg

> prettyprintalg :: RNAInput -> FS_Algebra Int [Char] [Char]
> prettyprintalg inp =  (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	                 append, ul, addss, ssadd, nil, h, h_l, h_s) 
>     where
>	sadd  lb e = '.': e		
>	cadd  x  e = x ++ e

>	dlr  _    x    _   =           x
>	sr  lb    x    rb  = '(':      x ++ ")"	
>	hl  lb _    x _    rb  = '(':'(': dots x  ++"))"	
>	bl  bl _    x e  _ br  = '(':'(': dots x  ++ e ++"))"
>	br  bl _  e x    _ br  = '(':'(':            e ++ dots x ++"))"
>	il  lb _ lr x rr _ rb  = '(':'(': dots lr ++ x ++ dots rr ++ "))" 
>	ml  bl _   x     _ br  = '(':'(':     x ++ "))" 

>	append c1 c = c1 ++ c
>	ul     c1   = c1
>	addss  c1 r = c1 ++ dots r
>	ssadd  r x  = dots r ++ x		
>	nil = []
 
>	h   = id 
>	h_l = id
>	h_s = id


======================================
6. Base pair distance - Classification algebra
======================================

> bpdistalg ::  Array Int Int -> RNAInput -> FS_Algebra Int Int Int
> bpdistalg struct inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	            append, ul, addss, ssadd, nil, h, h_l, h_s)
>     where
>       checkpair:: Int -> Int -> Int
>       checkpair lb rb 
>           | (struct!lb) == rb = 0    -- the same base pair 
>           | otherwise         = 1 + checkunpaired lb + checkunpaired rb  
>                                 -- check if rb or lb pair with another base
>       checkunpaired:: Int-> Int
>       checkunpaired lb 
>           | (struct!lb) == 0 = 0
>           | otherwise        = 1
  
>       checkloop:: Region -> Int
>       checkloop (l,r) = sum [checkunpaired k | k<-[l+1 ..r]]

>	sadd  lb e  = e	+ checkunpaired lb	
>	cadd  x  e  = x + e

>	dlr lb    e    rb = e	
>	sr  lb    e    rb = e + checkpair lb rb	
>	hl  llb lb    r    rb rrb =     checkpair lb rb + checkloop r  + checkpair llb rrb 
>	bl  llb bl    x e  br rrb = e + checkpair bl br + checkloop x  + checkpair llb rrb 
>	br  llb bl e  x    br rrb = e + checkpair bl br + checkloop x  + checkpair llb rrb 	
>	il  llb bl ll x rl br rrb = x + checkpair bl br + checkloop ll + checkloop rl + checkpair llb rrb 
>	ml  llb bl    x    br rrb = x + checkpair bl br + checkpair llb rrb 

>	append c1 c = c1 + c
>	ul     c1   = c1
>	addss  c1 r = c1 + checkloop r	
>	ssadd  r  x = x	 + checkloop r	
>	nil         = 0
 
>	h   = id 
>       h_l = id
>	h_s = id


The same algebra, but limited to a maximal distance of maxk.
All larger distances are subsumed under maxk

> bpdistalgk ::  Int -> Array Int Int -> RNAInput -> FS_Algebra Int Int Int
> bpdistalgk maxk struct inp =  (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	            append, ul, addss, ssadd, nil, h, h_l, h_s)
>     where
>       infixl 6 +%+   -- an intelligent, upper-bounded plus
>       (+%+) a b 
>             | a+b > maxk = maxk
>             | otherwise  = a + b

>       checkpair:: Int -> Int -> Int
>       checkpair lb rb 
>           | (struct!lb) == rb = 0    -- the same base pair 
>           | otherwise         = 1 + checkunpaired lb + checkunpaired rb  
>                                 -- check if rb or lb pair with another base
>       checkunpaired:: Int-> Int
>       checkunpaired lb 
>           | (struct!lb) == 0 = 0
>           | otherwise        = 1
  
>       checkloop:: Region -> Int
>       checkloop (l,r) = sum [checkunpaired k | k<-[l+1 ..r]]

>	sadd  lb e  = e	+%+ checkunpaired lb	
>	cadd  x  e  = x +%+ e

>	dlr lb    e    rb = e	
>	sr  lb    e    rb = e +%+ checkpair lb rb	
>	hl  llb lb    r    rb rrb =       checkpair lb rb +%+ checkloop r +%+ checkpair llb rrb
>	bl  llb bl    x e  br rrb = e +%+ checkpair bl br +%+ checkloop x +%+ checkpair llb rrb
>	br  llb bl e  x    br rrb = e +%+ checkpair bl br +%+ checkloop x +%+ checkpair llb rrb	
>	il  llb bl ll x rl br rrb = x +%+ checkpair bl br +%+ checkloop ll +%+ checkloop rl +%+ checkpair llb rrb
>	ml  llb bl    x    br rrb = x +%+ checkpair bl br +%+ checkpair llb rrb

>	append c1 c = c1 +%+ c
>	ul     c1   = c1
>	addss  c1 r = c1 +%+ checkloop r	
>	ssadd  r  x = x	 +%+ checkloop r	
>	nil         = 0
 
>	h   = id
>       h_l = id
>	h_s = id

=====================================
7. Partition function algebra:
====================================

> mean_nrg:: Double
> mean_nrg= -0.1843  -- mean energy for random sequences: 184.3*length cal
> mean_scale :: Double
> mean_scale = exp (-mean_nrg/(r_gas * temperature))

> r_gas = 0.00198717 -- [kcal/mol] <-- 1.98717 [cal/mol]
> temperature  = 310.15  -- [K]
> mk_pf:: Int -> Double
> mk_pf x = exp ((-(fromIntegral x::Double)/100) / (r_gas * temperature)) 
>                                  -- (-x/100) because energies are given multiplied by 100

> pfunc :: RNAInput -> FS_Algebra Int Double Double
> pfunc inp =  (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	          append, ul, addss, ssadd, nil, h, h_l, h_s)

>  where
>    (_,n)       = bounds inp
>    scale:: Array Int Double
>    scale  = array (0,n) ((0, 1.0) : [(i,  scale!(i-1)/ mean_scale) |i<-[1..n]])

>    sadd lb q = scale!1 * q
>    cadd q1 q = q1 * q
>    nil       = 1.0 

>    dlr dl        q    dr     =                                    q * mk_pf (dl_energy inp (dl+1,dr) + dr_energy inp (dl+1,dr) + termaupenalty (inp!(dl+1)) (inp!dr))
>    sr      lb    q    rb     = scale!2                          * q * mk_pf (sr_energy inp (lb,rb))
>    hl  llb lb  (i,j)  rb rrb = scale!(j-i+4)                        * mk_pf (hl_energy inp (lb,rb)) * mk_pf (sr_energy inp (llb,rrb)) 
>    bl  llb lb  x    q rb rrb = scale!(sizeof x +4)              * q * mk_pf (bl_energy inp lb x rb) * mk_pf (sr_energy inp (llb,rrb))  
>    br  llb lb    q  x rb rrb = scale!(sizeof x +4)              * q * mk_pf (br_energy inp lb x rb) * mk_pf (sr_energy inp (llb,rrb))  
>    il  llb lb lr q rr rb rrb = scale!(sizeof lr + sizeof rr +4) * q * mk_pf (il_energy inp lr rr) * mk_pf (sr_energy inp (llb,rrb))  
>    ml  llb lb    q    rb rrb = scale!4 * q * mk_pf (380 +termaupenalty (inp!lb) (inp!rb) + dli_energy inp (lb,rb) + dri_energy inp (lb,rb)) * mk_pf (sr_energy inp (llb,rrb))  

>    addss  q  (i,j)    = scale!(j-i) * q * mk_pf (ss_energy (i,j))
>    ssadd     (i,j) q  = scale!(j-i) * q * mk_pf (40  + ss_energy (i,j))
>    append q1       q2 = q1* q2
>    ul     q           = q * mk_pf 40
>    ss     r           = scale!(sizeof r) * mk_pf (ss_energy r)

>    h [] = []
>    h xs = id $!! [sum xs]
>    h_l [] = []
>    h_l xs = id $!! [sum xs]
>    h_s [] = []
>    h_s xs = id $!! [sum xs]

===================================
7.2. Sampling Algebra
===================================


Sampling this way does not follow the desired distribution:

                                 tgaaccacacagatacaacggtc
       Observed  Expected
        0.001   (0.000260)       ((...))....(((......)))
        0.001   (0.000354)       ((................))...
        0.004   (0.000953)       ((...))................
        0.01    (0.001602)       ((.............))......
        0.005   (0.001824)       ((.....))..(((......)))
        0.02    (0.006679)       ((.....))..............
        0.016   (0.006679)       ((.......))............
        0.002   (0.014088)       ...((...............)).
        0.104   (0.048350)       .((.((.............))))
        0.021   (0.078667)       ....((.............))..
        0.014   (0.089570)       ...........(((......)))
        0.223   (0.328012)       .......................
        0.579   (0.398519)       ...(((.............))).

> pfunc_sample_wrong :: RNAInput -> FS_Algebra Int Double Double
> pfunc_sample_wrong inp =  (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	          append, ul, addss, ssadd, nil, h, h_l, h_s)

>  where
>    (sadd, cadd, dlr, sr, hl, bl, br, il,ml, append, ul, addss, ssadd, nil, _, _, _) = pfunc inp

>    h   xs   = sample xs (getRandomDouble (sum xs)) 0 
>    h_l xs   = sample xs (getRandomDouble (sum xs)) 0 
>    h_s xs   = sample xs (getRandomDouble (sum xs)) 0 

>    sample:: [Double] -> Double -> Double -> [Double]
>    sample   []  rand  psum = []
>    sample   (x:xs) rand psum
>            | x+psum > rand = [x]
>            | otherwise     = sample xs rand (x+psum)


We need a mightier algebra.

This algebra includes two boltzman weights:
The weight of the actual sampled structure and the sum of all weights of the corresponding subsequence
The former value is needed to identify the sampled structure (for pp, mfe and so on)
The latter value is actually used for the sampling procedure.

> pfunc_sample :: RNAInput -> FS_Algebra Int (Double,Double) (Double,Double)
> pfunc_sample inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	                append, ul, addss, ssadd, nil, h, h_l, h_s)

>  where
>    (sadd1, cadd1, dlr1, sr1, hl1, bl1, br1, il1, ml1, append1, ul1, addss1, ssadd1, nil1, _, _, _) = pfunc inp

>    sadd lb       (q,q') = (sadd1 lb q, sadd1 lb  q')
>    cadd (q1,q1') (q,q') = (cadd1 q1 q, cadd1 q1' q')
>    nil                  = (nil1,       nil1)

>    dlr dl        (q,q')    dr     = (dlr1 dl        q    dr,    dlr1      dl    q'    dr    )
>    sr      lb    (q,q')    rb     = (sr1      lb    q    rb,     sr1      lb    q'    rb    )
>    hl  llb lb     (i,j)    rb rrb = (hl1  llb lb  (i,j)  rb rrb, hl1  llb lb  (i,j)   rb rrb)
>    bl  llb lb  x (q,q')    rb rrb = (bl1  llb lb  x q    rb rrb, bl1  llb lb  x q'    rb rrb)
>    br  llb lb    (q,q')  x rb rrb = (br1  llb lb    q  x rb rrb, br1  llb lb    q'  x rb rrb)
>    il  llb lb lr (q,q') rr rb rrb = (il1  llb lb lr q rr rb rrb, il1  llb lb lr q' rr rb rrb)
>    ml  llb lb    (q,q')    rb rrb = (ml1  llb lb    q    rb rrb, ml1  llb lb    q'    rb rrb)

>    addss  (q,q')  (i,j)          = (addss1 q (i,j), addss1 q' (i,j))
>    ssadd          (i,j) (q,q')   = (ssadd1 (i,j) q, ssadd1 (i,j) q') 
>    append (q1,q1')      (q2,q2') = (append1 q1 q2 , append1 q1' q2')
>    ul     (q,q')                 = (ul1 q,          ul1 q')


>    h []   = [] 
>    h xs   = [(sample xs (getRandomDouble newsum) 0, newsum)]
>            where newsum = sum (map snd xs)
>    h_l [] = []
>    h_l xs = [(sample xs (getRandomDouble newsum) 0, newsum)]
>            where newsum = sum (map snd xs)
>    h_s [] = []
>    h_s xs = [(sample xs (getRandomDouble newsum) 0, newsum)]
>             where newsum = sum (map snd xs)


>    sample:: [(Double,Double)] -> Double -> Double -> Double
>    sample   []  rand  psum = error "Empty sample"
>    sample   ((x,y):xs) rand psum
>            | y+psum > rand = x 
>            | otherwise     = sample xs rand (y+psum)
  


===================================
7.5. Shape Algebra
===================================

> nubalg  f inp
>   = 
>     (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>          append, ul, addss, ssadd, nil, h', h_l', h_s')
>     where
>       h' l = nub $ h l
>       h_l' l = nub $ h_l l
>       h_s' l = nub $ h_s l
>       (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>          append, ul, addss, ssadd, nil, h, h_l, h_s) = f inp

> shapes:: Int -> (RNAInput -> FS_Algebra Int String String)
> shapes 0 = shape0
> shapes 1 = shape1
> shapes 2 = shape2
> shapes 3 = shape3
> shapes 4 = shape4
> shapes 5 = shape5

> shape0, shape1, shape2, shape3, shape4, shape5::  (RNAInput -> FS_Algebra Int String String)

> shape0 = shapealg "[" "]" "[" "]" "_" "_"    -- + all external single strands
> shape1 = shapealg "[" "]" "[" "]" "_" "_"    -- not yet implemented
> shape2 = shapealg "[" "]" "[" "]" "_" ""     -- + single strands in bulge and iloops
> shape3 = shapealg "[" "]" "[" "]" ""  ""     -- + bulge loops
> shape4 = shapealg "[" "]" ""  ""  ""  ""     -- + internal loops
> shape5 = shapealg  ""  ""  ""  ""  ""  ""     -- hairpins and multiloops


> shapealg :: String -> String -> String -> String -> String -> String
>                    -> RNAInput -> FS_Algebra Int String String
> shapealg iloop_op iloop_cl bulge_op bulge_cl loop_ss ext_ss inp =
>     (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>          append, ul, addss, ssadd, nil, h, h_l, h_s)

>     where
>       sadd  lb e = app ext_ss e
>       cadd  x  e = app x e

>       dlr   dl x dr = app ext_ss (app x ext_ss)
>       sr  lb e rb = e
>       hl  llb lb r       rb rrb = "[]"
>       bl  llb bl  x e    br rrb = bulge_op ++ loop_ss ++ e ++ bulge_cl
>       br  llb bl    e x  br rrb = bulge_op ++ e ++ loop_ss ++ bulge_cl
>       il  llb lb lr x rr rb rrb = iloop_op ++ loop_ss ++ x ++ loop_ss ++ iloop_cl
>       ml  llb bl    x    br rrb = '[' :x ++ "]"

>       append c1 c  = app c1  c
>       ul      c1  = c1
>       addss  c1 r = app c1 ext_ss
>       ssadd  r x  = app ext_ss x
>       nil  = ext_ss


>       h [] = []
>       h es = es
>       h_l [] = []
>       h_l es = es
>       h_s [] = []
>       h_s es = es

> app :: String -> String -> String
> app [] ys = ys
> app "_" "_" = "_"
> app (x:[]) (y:[]) = x:y:[]
> app (x:[]) (y:ys) = app (app (x:[]) (y:[])) ys
> app (x:xs) ys = x : app xs ys


=============================
8. ***-operator - combines two algebras
=============================

> infixr ***

> (***) :: (Eq comp, Eq cmpl, Ord comp2, Ord cmpl2, Ord comp, Ord cmpl)
>                       => (RNAInput -> FS_Algebra Int comp cmpl) ->
>                                (RNAInput -> FS_Algebra Int comp2 cmpl2)
>                              -> RNAInput -> FS_Algebra Int (comp,comp2) (cmpl,cmpl2)

> (alg1 *** alg2) inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>                 append, ul, addss, ssadd, nil, h, h_l, h_s)
>     where
>       (sadd1, cadd1, dlr1, sr1, hl1, bl1, br1, il1,ml1,
>         append1, ul1, addss1, ssadd1, nil1, h1, h_l1, h_s1)= alg1 inp
>       (sadd2, cadd2, dlr2, sr2, hl2, bl2, br2, il2, ml2,
>         append2, ul2, addss2, ssadd2, nil2, h2, h_l2, h_s2)= alg2 inp
>
>       sadd  lb (c,d)            = (sadd1 lb c, sadd2 lb d)
>       cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
>       dlr        = com3 dlr1 dlr2
>       sr         = com3 sr1 sr2
>       hl  llb lb r rb rrb             = (hl1  llb lb      r  rb rrb, hl2  llb lb    r    rb rrb)
>       bl  llb bl x  (c,d)      br rrb = (bl1  llb bl    x c  br rrb, bl2  llb bl    x d  br rrb)
>       br  llb bl    (c,d) x    br rrb = (br1  llb bl    c x  br rrb, br2  llb bl d  x    br rrb)
>       il  llb lb lr (c,d)   rr rb rrb = (il1  llb lb lr c rr rb rrb, il2  llb lb lr d rr rb rrb)
>       ml  llb bl    (a,b)      br rrb = (ml1  llb bl    a    br rrb, ml2  llb bl    b    br rrb)

>       append  (a,c1) (b,c) = (append1   a b,append2   c1 c)
>       ul   (a,c1)        = (ul1     a  , ul2    c1  )

>       addss  (a,c1) r    = (addss1  a r, addss2 c1 r)
>       ssadd  r (c,d)     = (ssadd1  r c, ssadd2 r d )
>       nil = (nil1,nil2)

>       h xs   = [(x1,x2)| x1 <- nub $ h1 [ y1 | (y1,y2) <- xs],
>                          x2 <-       h2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>       h_l xs = [(x1,x2)| x1 <- nub $ h_l1 [ y1 | (y1,y2) <- xs],
>                          x2 <-       h_l2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>       h_s    =  h_l

=============================
8. ***-operator - combines two algebras (alg1 expected to be sampling algebra)
=============================

> infixr *$*

 (*$*) :: (Eq comp, Eq cmpl, Ord comp2, Ord cmpl2, Ord comp, Ord cmpl)
                       => (RNAInput -> FS_Algebra Int comp cmpl) ->
                                (RNAInput -> FS_Algebra Int comp2 cmpl2)
                              -> RNAInput -> FS_Algebra Int (comp,comp2) (cmpl,cmpl2)

> (alg1 *$* alg2) inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>                 append, ul, addss, ssadd, nil, h, h_l, h_s)
>     where
>       (sadd1, cadd1, dlr1, sr1, hl1, bl1, br1, il1,ml1,
>         append1, ul1, addss1, ssadd1, nil1, h1, h_l1, h_s1)= alg1 inp
>       (sadd2, cadd2, dlr2, sr2, hl2, bl2, br2, il2, ml2,
>         append2, ul2, addss2, ssadd2, nil2, h2, h_l2, h_s2)= alg2 inp
>
>       sadd  lb (c,d)            = (sadd1 lb c, sadd2 lb d)
>       cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
>       dlr        = com3 dlr1 dlr2
>       sr         = com3 sr1 sr2
>       hl  llb lb r rb rrb             = (hl1  llb lb      r  rb rrb, hl2  llb lb    r    rb rrb)
>       bl  llb bl x  (c,d)      br rrb = (bl1  llb bl    x c  br rrb, bl2  llb bl    x d  br rrb)
>       br  llb bl    (c,d) x    br rrb = (br1  llb bl    c x  br rrb, br2  llb bl d  x    br rrb)
>       il  llb lb lr (c,d)   rr rb rrb = (il1  llb lb lr c rr rb rrb, il2  llb lb lr d rr rb rrb)
>       ml  llb bl    (a,b)      br rrb = (ml1  llb bl    a    br rrb, ml2  llb bl    b    br rrb)

>       append  (a,c1) (b,c) = (append1   a b,append2   c1 c)
>       ul   (a,c1)        = (ul1     a  , ul2    c1  )

>       addss  (a,c1) r    = (addss1  a r, addss2 c1 r)
>       ssadd  r (c,d)     = (ssadd1  r c, ssadd2 r d )
>       nil = (nil1,nil2)


x1 is the boltzmann weight of the sampled substructure,
x1' ist the sum of all boltzmann weights for all alternative substructures of x1 and x1


>       h [] = []
>       h xs   = pick [((x1,x1'),x2)| (x1,x1') <- h1 [ y1 |  (y1,     y2) <- xs],
>                                         x2   <- h2 [ y2 | ((y1,y1'),y2) <- xs, y1 == x1]]
>       h_l [] = []
>       h_l xs = pick [((x1,x1'),x2)| (x1,x1') <- h_l1 [ y1 |  (y1,     y2) <- xs],
>                                         x2   <- h_l2 [ y2 | ((y1,y1'),y2) <- xs, y1 == x1]]

>       h_s    =  h_l




=============================
8.5 *%*-operator - combines two algebras 

Specialiced version for  shape *** pf
=============================

> infixr *%*

> (*%*) :: (Eq comp, Eq cmpl, Ord comp, Ord cmpl, DeepSeq comp, DeepSeq cmpl)
>                       => (RNAInput -> FS_Algebra Int comp cmpl) ->
>                          (RNAInput -> FS_Algebra Int Double Double)
>			       -> RNAInput -> FS_Algebra Int (comp,Double) (cmpl,Double) 

> (alg1 *%* alg2) inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	          append, ul, addss, ssadd, nil, h, h_l, h_s)
>     where
>       (sadd1, cadd1, dlr1, sr1, hl1, bl1, br1, il1,ml1,
>	  append1, ul1, addss1, ssadd1, nil1, h1, h_l1, h_s1)= alg1 inp
>       (sadd2, cadd2, dlr2, sr2, hl2, bl2, br2, il2, ml2,
>	  append2, ul2, addss2, ssadd2, nil2, h2, h_l2, h_s2)= alg2 inp
>
>	sadd  lb (c,d)            = (sadd1 lb c, sadd2 lb d)
>	cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
>	dlr	   = com3 dlr1 dlr2
>	sr 	   = com3 sr1 sr2 
>	hl  llb lb r rb rrb	        = (hl1  llb lb      r  rb rrb, hl2  llb lb    r    rb rrb)
>	bl  llb bl x  (c,d)      br rrb = (bl1  llb bl    x c  br rrb, bl2  llb bl    x d  br rrb)
>	br  llb bl    (c,d) x    br rrb = (br1  llb bl    c x  br rrb, br2  llb bl d  x    br rrb)
>	il  llb lb lr (c,d)   rr rb rrb = (il1  llb lb lr c rr rb rrb, il2  llb lb lr d rr rb rrb)
>	ml  llb bl    (a,b)      br rrb = (ml1  llb bl    a    br rrb, ml2  llb bl    b    br rrb)

>	append  (a,c1) (b,c) = (append1   a b,append2   c1 c)
>	ul   (a,c1)	   = (ul1     a  , ul2	  c1  )

>	addss  (a,c1) r	   = (addss1  a r, addss2 c1 r)
>	ssadd  r (c,d)	   = (ssadd1  r c, ssadd2 r d )
>	nil = (nil1,nil2)

        -- Hashtable version zu langsam wegen schlechtem Hashing

       h xs   = filter_low_probs $ unsafePerformIO $ buildHash xs
       h_l xs = filter_low_probs $ unsafePerformIO $ buildHash xs

>       h   xs = filter_low_probs $ Data.Map.toList $ buildMap xs
>       h_l xs = filter_low_probs $ Data.Map.toList $ buildMap xs
>	h_s    = h_l

>               -- id $!! toList $ fromListWith (+) xs
>               -- 2 times faster, but takes two times more memory (due to not strict?)


use a Map and update (add) the value for each key with each new value

> --buildMap:: (Num b, Ord a) => [(a,b)] -> Map a b
> buildMap:: (Ord a) => [(a, Double)] ->Map a Double
> buildMap []         = Data.Map.empty
> buildMap ((x,y):xs) = Data.Map.insertWith' (+) x y $! buildMap xs


Das hier scheint teuer zu sein!!

> buildMap':: (Num b, Ord a) => Map a b -> [(a,b)] -> Map a b
> buildMap' m []         = m
> buildMap' m ((x,y):xs) = buildMap' (Data.Map.insertWith' (+) x y m) xs



uses a hash for storage and plain addition for updating of values

> buildHash :: [(String,Double)] -> IO [(String,Double)]
> buildHash  xs = do
>       h <- HT.new (==) HT.hashString
>       mapM (myinsert h) xs
>       HT.toList h

> myinsert:: HashTable String Double -> (String, Double) -> IO Bool
> myinsert hash (x,y) = case unsafePerformIO (HT.lookup hash x) of
>                            Just a  -> HT.update hash x (y+a)
>                            Nothing -> HT.update hash x  y   


Given a list of (shape,boltzmann weights) remove all entries whose relative weight < thresh

> filter_low_probs:: [(a,Double)] -> [(a,Double)]
> filter_low_probs xs = filter ((>psum*thresh).snd) xs
>           where psum = sum (map snd xs)

> thresh::Double
> thresh = 0.000001

=============================
8.8. ***-operator - combines two algebras for classified DP
     alg 1 is supposed to be the classificiation algebra with h =id
     Here, we use Haskell Maps to store the values for each class.
=============================

> infixr *#*

> (*#*) :: (Eq comp, Eq cmpl, Ord comp2, Ord cmpl2, Ord comp, Ord cmpl,
>           Num comp2, Num cmpl2, DeepSeq comp, DeepSeq cmpl, DeepSeq comp2, DeepSeq cmpl2)
>                       => (RNAInput -> FS_Algebra Int comp cmpl) ->
>				 (RNAInput -> FS_Algebra Int comp2 cmpl2)
>			       -> RNAInput -> FS_Algebra Int (comp,comp2) (cmpl,cmpl2) 

> (alg1 *#* alg2) inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	          append, ul, addss, ssadd, nil, h, h_l, h_s)
>     where
>       (sadd1, cadd1, dlr1, sr1, hl1, bl1, br1, il1,ml1,
>	  append1, ul1, addss1, ssadd1, nil1, h1, h_l1, h_s1)= alg1 inp
>       (sadd2, cadd2, dlr2, sr2, hl2, bl2, br2, il2, ml2,
>	  append2, ul2, addss2, ssadd2, nil2, h2, h_l2, h_s2)= alg2 inp
>
>	sadd  lb (c,d)            = (sadd1 lb c, sadd2 lb d)
>	cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
>	dlr	   = com3 dlr1 dlr2
>	sr 	   = com3 sr1 sr2 
>	hl  llb lb r rb rrb	        = (hl1  llb lb      r  rb rrb, hl2  llb lb    r    rb rrb)
>	bl  llb bl x  (c,d)      br rrb = (bl1  llb bl    x c  br rrb, bl2  llb bl    x d  br rrb)
>	br  llb bl    (c,d) x    br rrb = (br1  llb bl    c x  br rrb, br2  llb bl d  x    br rrb)
>	il  llb lb lr (c,d)   rr rb rrb = (il1  llb lb lr c rr rb rrb, il2  llb lb lr d rr rb rrb)
>	ml  llb bl    (a,b)      br rrb = (ml1  llb bl    a    br rrb, ml2  llb bl    b    br rrb)

>	append  (a,c1) (b,c) = (append1   a b,append2   c1 c)
>	ul   (a,c1)	   = (ul1     a  , ul2	  c1  )

>	addss  (a,c1) r	   = (addss1  a r, addss2 c1 r)
>	ssadd  r (c,d)	   = (ssadd1  r c, ssadd2 r d )
>	nil = (nil1,nil2)

This looks quite generic!
We build a Map, where each entry stores a list of all members of one class.
Afterwards we apply h2 on each list separately.

Does not imply any restrictions on second choice function.

>       h xs   = [ (x,y)  | (x,ys) <- Data.Map.toList $ buildListMap Data.Map.empty xs, y <- h2 ys]

>	h_l xs = [ (x,y)  | (x,ys) <- Data.Map.toList $ buildListMap Data.Map.empty xs, y <- h_l2 ys]

>	h_s    = h_l

for each key store a list 

> buildListMap:: (Ord a) => Map a [b] -> [(a,b)] -> Map a [b]
> buildListMap m []         = m
> buildListMap m ((x,y):xs)
>           | member x m   = buildListMap (Data.Map.insertWith' (++) x [y] m) xs
>           | otherwise    = buildListMap (Data.Map.insert           x [y] m) xs
 




=============================
8. ///-operator - combines two algebras

second algebra must be a k-best algebra
=============================

> infixr ///

> (///) :: (Eq comp, Eq cmpl, Ord comp2, Ord cmpl2, Ord comp, Ord cmpl,DeepSeq comp2, DeepSeq cmpl2)
>                             => (RNAInput -> FS_Algebra Int comp cmpl)
>                                 -> (Int -> RNAInput -> FS_Algebra Int comp2 cmpl2)
>                                 ->  Int -> RNAInput -> FS_Algebra Int (comp,comp2) (cmpl,cmpl2) 

> (alg1 /// alg2) k inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	          append, ul, addss, ssadd, nil, h, h_l, h_s)
>     where

>       (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>	          append, ul, addss, ssadd, nil, _, _,  _) = (alg1 *** (alg2 k)) inp

>       (sadd1, cadd1, dlr1, sr1, hl1, bl1, br1, il1,ml1,
>	          append1, ul1, addss1, ssadd1, nil1, h1, h_l1, h_s1) = alg1 inp

>       (sadd2, cadd2, dlr2, sr2, hl2, bl2, br2, il2,ml2,
>	          append2, ul2, addss2, ssadd2, nil2, h2, h_l2, h_s2) = alg2 0 inp
>       (sadd3, cadd3, dlr3, sr3, hl3, bl3, br3, il3,ml3,
>	          append3, ul3, addss3, ssadd3, nil3, h3, h_l3, h_s3) = alg2 k inp

>   
>       h   xys = let l1 = nub $ h1 (map fst xys)                                  -- list of all classes
>                     l2 = [(x,y) | x <- l1, y <- h2 [y | (x',y) <- xys, x'== x]]  -- list of classes and its best e
>                     l3 = h3 (map snd l2 )
>                 in  [(x,y) | y <- l3, (x,y') <- l2, y'==y]

>       h_l xys = let l1 = nub $ h_l1 (map fst xys)
>                     l2 = [(x,y) | x <- l1, y <- h_l2 [y | (x',y) <- xys, x'== x]]
>                     l3 = h_l3 (map snd l2 )
>                 in  [(x,y) | y <- l3, (x,y') <- l2, y'==y]
>       h_s     = h_l





Everything below here is still unfinished.
Needs to add basepair in hl,il,bl,br,ml

-- =============================
-- 9. *!*-operator - combines two algebras in a strict way
-- =============================
-- A strict variant of ***
-- (It seems to be better to make the respective algebras itself strict.)

-- > infixr *!*

-- > (*!*) :: (DeepSeq comp, DeepSeq cmpl, DeepSeq cmpl2, DeepSeq comp2,
-- >           Eq comp, Eq cmpl, Ord comp2, Ord cmpl2, Ord comp, Ord cmpl)
-- >                       => (RNAInput -> FS_Algebra Int comp cmpl) ->
-- >				 (RNAInput -> FS_Algebra Int comp2 cmpl2)
-- >			       -> RNAInput -> FS_Algebra Int (comp,comp2) (cmpl,cmpl2) 

-- > (alg1 *!* alg2) inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
-- >	          append, ul, addss, ssadd, nil, h, h_l, h_s)
-- >     where
-- >       (sadd1, cadd1, dlr1, sr1, hl1, bl1, br1, il1,ml1,
-- >	  append1, ul1, addss1, ssadd1, nil1, h1, h_l1, h_s1)= alg1 inp
-- >       (sadd2, cadd2, dlr2, sr2, hl2, bl2, br2, il2, ml2,
-- >	  append2, ul2, addss2, ssadd2, nil2, h2, h_l2, h_s2)= alg2 inp
-- >
-- >	sadd  lb (c,d)            = (sadd1 lb c, sadd2 lb d)
-- >	cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
-- >	dlr	   = com3 dlr1 dlr2
-- >	sr 	   = com3 sr1 sr2 
-- >	hl  lb r rb 	       = (hl1  lb      r  rb , hl2  lb    r    rb )
-- >	bl  bl x (c,d) br      = (bl1  bl    x c  br , bl2  bl    x d  br )
-- >	br  bl (c,d) x br      = (br1  bl    c x  br , br2  bl d  x    br )
-- >	il  lb lr (c,d) rr rb  = (il1  lb lr c rr rb , il2  lb lr d rr rb )
-- >	ml  bl    (a,b)    br  = (ml1  bl    a    br , ml2  bl    b    br )

-- >	append  (a,c1) (b,c) = (append1   a b,append2   c1 c)
-- >	ul   (a,c1)	   = (ul1     a  , ul2	  c1  )

-- >	addss  (a,c1) r	   = (addss1  a r, addss2 c1 r)
-- >	ssadd  r (c,d)	   = (ssadd1  r c, ssadd2 r d )
-- >	nil = (nil1,nil2)

-- some tricks are neccessary here for making ghc more strict. Otherwise HUGE amounts of memory are needed.

-- >       h xs   = id  $!! [(x1,x2)| x1 <- nub $ h1 [ y1 | (y1,y2) <- xs],
-- >                                  x2 <-       h2 [ y2 | (y1,y2) <- xs, y1 == x1]]
-- >	h_l xs = id  $!! [(x1,x2)| x1 <- nub $ h_l1 [ y1 | (y1,y2) <- xs],
-- >                                  x2 <-       h_l2 [ y2 | (y1,y2) <- xs, y1 == x1]]
-- >	h_s    =  h_l

-- =============================
-- 10. *^*-operator - combines two algebras
-- =============================
-- A variant of ***, which uses buckets in the choice functions.
-- Only useful with an algebra, such as as diffalg as first component !!!
-- Reduces the runtime considerably (exact amount depends on the size of input list xs
-- and size of list returned by h1

-- > infixr *^*

-- > (*^*) :: (Ix comp, Ix cmpl,
-- >           --DeepSeq comp, DeepSeq cmpl, DeepSeq cmpl2, DeepSeq comp2,
-- >           Enum comp, Num comp, Enum cmpl, Num cmpl,
-- >           Eq comp, Eq cmpl, Ord comp2, Ord cmpl2, Ord comp, Ord cmpl)
-- >                       => (RNAInput -> FS_Algebra Int comp cmpl) ->
-- >				 (RNAInput -> FS_Algebra Int comp2 cmpl2)
-- >			       -> RNAInput -> FS_Algebra Int (comp,comp2) (cmpl,cmpl2) 

-- > (alg1 *^* alg2) inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
-- >	          append, ul, addss, ssadd, nil, h, h_l, h_s)
-- >     where
-- >       (sadd1, cadd1, dlr1, sr1, hl1, bl1, br1, il1,ml1,
-- >	  append1, ul1, addss1, ssadd1, nil1, h1, h_l1, h_s1)= alg1 inp
-- >       (sadd2, cadd2, dlr2, sr2, hl2, bl2, br2, il2, ml2,
-- >	  append2, ul2, addss2, ssadd2, nil2, h2, h_l2, h_s2)= alg2 inp
-- >
-- >	sadd  lb (c,d)            = (sadd1 lb c, sadd2 lb d)
-- >	cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
-- >	dlr	   = com3 dlr1 dlr2
-- >	sr 	   = com3 sr1 sr2 
-- >	hl  lb r rb 	       = (hl1  lb      r  rb , hl2  lb    r    rb )
-- >	bl  bl x (c,d) br      = (bl1  bl    x c  br , bl2  bl    x d  br )
-- >	br  bl (c,d) x br      = (br1  bl    c x  br , br2  bl d  x    br )
-- >	il  lb lr (c,d) rr rb  = (il1  lb lr c rr rb , il2  lb lr d rr rb )
-- >	ml  bl    (a,b)    br  = (ml1  bl    a    br , ml2  bl    b    br )

-- >	append  (a,c1) (b,c) = (append1   a b,append2   c1 c)
-- >	ul   (a,c1)	   = (ul1     a  , ul2	  c1  )

-- >	addss  (a,c1) r	   = (addss1  a r, addss2 c1 r)
-- >	ssadd  r (c,d)	   = (ssadd1  r c, ssadd2 r d )
-- >	nil = (nil1,nil2)

-- >       h []   = []
-- >       h xs   = [ (x1,y2) | let x = maximum $ h1 [ y1 | (y1,y2) <- xs],
-- >                            let buckets = accumArray (flip(:)) [] (0, x) xs,
-- >                             x1 <- [0..x],   y2 <- h2 (buckets!x1)]
-- >       h_l [] = []
-- >	   h_l xs = [(x1,y2)| let x = maximum $  h_l1 [ y1 | (y1,y2) <- xs],
-- >                          let buckets = accumArray (flip(:)) [] (0,x) xs,
-- >                           x1 <- [0 .. x], y2 <- h_l2 (buckets!x1)]
-- >
-- >	   h_s    =  h_l


-- =============================
-- 11. >-< -operator - cartesian product
-- =============================

-- > infixr >-<

-- > (>-<) :: (Eq comp, Eq cmpl, Ord comp2, Ord cmpl2, Ord comp, Ord cmpl)=> (RNAInput -> FS_Algebra Int comp cmpl) ->
-- >				 (RNAInput -> FS_Algebra Int comp2 cmpl2)
-- >			       -> RNAInput -> FS_Algebra Int (comp,comp2) (cmpl,cmpl2) 

-- > (alg1 >-< alg2) inp = (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
-- >	          append, ul, addss, ssadd, nil, h, h_l, h_s)
-- >     where
-- >       (sadd1, cadd1, dlr1, sr1, hl1, bl1, br1, il1,ml1,
-- >	  append1, ul1, addss1, ssadd1, nil1, h1, h_l1, h_s1)= alg1 inp
-- >       (sadd2, cadd2, dlr2, sr2, hl2, bl2, br2, il2, ml2,
-- >	  append2, ul2, addss2, ssadd2, nil2, h2, h_l2, h_s2)= alg2 inp
-- >
-- >	sadd  lb (c,d)            = (sadd1 lb c, sadd2 lb d)
-- >	cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
-- >	dlr	   = com3 dlr1 dlr2
-- >	sr 	   = com3 sr1 sr2 
-- >	hl  lb r rb 	       = (hl1  lb      r  rb , hl2  lb    r    rb )
-- >	bl  bl x (c,d) br      = (bl1  bl    x c  br , bl2  bl    x d  br )
-- >	br  bl (c,d) x br      = (br1  bl    c x  br , br2  bl d  x    br )
-- >	il  lb lr (c,d) rr rb  = (il1  lb lr c rr rb , il2  lb lr d rr rb )
-- >	ml  bl    (a,b)    br  = (ml1  bl    a    br , ml2  bl    b    br )

-- >	append  (a,c1) (b,c) = (append1   a b,append2   c1 c)
-- >	ul   (a,c1)	   = (ul1     a  , ul2	  c1  )

-- >	addss  (a,c1) r	   = (addss1  a r, addss2 c1 r)
-- >	ssadd  r (c,d)	   = (ssadd1  r c, ssadd2 r d )
-- >	nil = (nil1,nil2)

-- We take only one result from each algebra, otherwise It might give strange results

-- >       h xs =   [(x1,x2)|  x1 <- take 1 $ h1 [ y1 | (y1,y2) <- xs],
-- >                           x2 <- take 1 $ h2 [ y2 | (y1,y2) <- xs]]
-- >	h_l xs =  [(x1,x2)| x1 <- take 1 $ h_l1 [ y1 | (y1,y2) <- xs],
-- >                           x2 <- take 1 $ h_l2 [ y2 | (y1,y2) <- xs]]
-- >	h_s    =  h_l



> skipleft _ c   = c
> skipright c _ = c

> com3  f g a (c,d) b   = (f a c b, g a d b)
> compact [(SS (l,r))] | l == r    = []
>	     	       | otherwise = [SS (l,r)]
> compact xs         = xs    




