> module Stef3 where

> import System.Environment
> import System.IO
> import Numeric
> import Data.Array
> import Data.List(nub,sort,sortBy) 
> import RnaI
> import Energy
> import ADPTriCombinators

 import Random_number

 main :: IO()
 main  = do
	  [arg1,arg2] <- getArgs
	  let input	 = arg1
	      result	 = case  arg2 of
				 "1"  ->  formatCount (canonicals_nonamb (-100.0) (count) input)

                               "2"  ->  formatP_func (canonicals_nonamb (-100.0) (p_func) input)

                                "3"  ->  case getLevel of
 					       1 -> formatShapes (canonicals_nonamb (-100.0) (shape1) input)
 					       2 -> formatShapes (canonicals_nonamb (-100.0) (shape2) input)
 					       3 -> formatShapes (canonicals_nonamb (-100.0) (shape3) input)
 					       4 -> formatShapes (canonicals_nonamb (-100.0) (shape4) input)
 					       5 -> formatShapes (canonicals_nonamb (-100.0) (shape5) input)
				 otherwise -> error ("usage: <inputsequence> <type>\n<type>=\n\t1: Lists the number of structures in the searchspace for shape '"++getShape++"'\n\t2: Calculates the value of the partition function for shape '"++getShape++"'\n\t3: Returns all level "++ show getLevel ++" shapes an inputsequence can fold in. This results in just one shape if the matcher works correctly\n")
	  putStr (result++"\n")

> formatP_func :: [((Float,Int,Int),(Int,Int))] -> String
> formatP_func [] = ""
> formatP_func (x:xs) = "thermodynamic matcher for shape '"++ getShape ++"'\npartition function value: " ++ show pfValue
>             where ((pfValue, u1, u2),(u3,u4)) = x

> formatCount :: [Integer] -> String
> formatCount [] = ""
> formatCount (x:xs) = "thermodynamic matcher for shape '"++ getShape ++"'\nno. structures in searchspace: " ++ show x

> formatShapes :: [String] -> String
> formatShapes [] = ""
> formatShapes (x:xs) = show x ++ "\n" ++ formatShapes xs

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

Minimal free energy algebra:

 mfe :: Array Int Ebase -> Float ->      -- closed      answer   
        Canonical_Algebra Int (Int,Int) (Float,Int,Int) (Float,(Int,Int),(Int,Int)) (Float,Int,Int)
 mfe basearray takes = (sadd,cadd,cadd',cadd'',cadd''',ambd basearray,ambd' basearray,nil,nil',edl basearray,edr basearray,edlr basearray,drem,is basearray,sr basearray,
                    hl basearray,sp basearray,bl basearray,br basearray,il basearray,
                    ml basearray,mldr basearray,mladr basearray,mldlr basearray,mladlr basearray,mldladr basearray,mladldr basearray,
                    mldl basearray,mladl basearray,addss,ssadd,trafo,
                    incl,combine basearray,lcombine basearray,lcombine' basearray,rcombine basearray,rcombine' basearray,
		     lrcombine basearray,acomb basearray,lacomb basearray,lacomb' basearray,racomb basearray,racomb' basearray,
		     lracomb basearray,h,h_i,h_l,h_s)                where
    sadd lb (e,_,rb) = (e,lb,rb)
    cadd (e1,lb1,rb1) (e2,lb2,rb2)     = (e1 + e2,lb1,rb1)  -- uebergabe der indizes des ersten stems
    cadd' (e1,lb1,rb1) (e2,lb2,rb2)     = (e1 + e2,lb1,rb1)  -- uebergabe der indizes des ersten stems
    cadd'' (e1,lb1,rb1) (e2,lb2,rb2)     = (e1 + e2,lb1,rb1)  -- uebergabe der indizes des ersten stems
    cadd''' (e1,lb1,rb1) (e2,lb2,rb2)     = (e1 + e2,lb1,rb1)  -- uebergabe der indizes des ersten stems
    ambd inp (e1,lb1,rb1) db (e2,lb2,rb2) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),lb1,rb1) -- uebergabe der indizes des ersten stems
    ambd' inp (e1,lb1,rb1) db (e2,lb2,rb2) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),lb1,rb1) -- uebergabe der indizes des ersten stems
    nil _ = (0,n,n)
    nil' _ = (0,n,n)
    edl  inp dl (e,lb,rb)    = (e + dl_energy inp (lb,rb),lb,rb) -- uebergabe der indizes des ersten stems
    edr  inp    (e,lb,rb) dr = (e + dr_energy inp (lb,rb),lb,rb) -- uebergabe der indizes des ersten stems
    edlr inp dl (e,lb,rb) dr = (e + dl_energy inp (lb,rb) + dr_energy inp (lb,rb),lb,rb) -- uebergabe der indizes des ersten stems
    drem = id
    is inp    (e,lb,rb)    = (e + termaupenalty (inp!lb) (inp!rb),lb,rb)
    sr inp lb (e,_,_) rb = (e + sr_energy inp (lb,rb),lb,rb)
    hl inp llb lb loop rb rrb = (hl_energy inp (lb,rb) + sr_energy inp (llb,rrb),llb,rrb)
    sp inp llb lb (e,_,_) rb rrb = (e + sr_energy inp (llb,rrb), llb,rrb)
    bl inp (l,r) (e,lend,rend) = (e + bl_energy inp l    (l,r) (rend+1),l,rend)
    br inp (e,lend,rend) (l,r) = (e + br_energy inp (lend-1) (l,r) (r+1),lend,r)
    il inp (l1,l2) (e,l,r) (r1,r2) = (e + il_energy inp (l1,l2) (r1,r2), l1, r2)
    ml inp llb lb (e,_,_) rb rrb          = (380 + e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
    mldr inp llb lb (e,_,_) dr rb rrb     = (380 + e + dri_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
    mladr inp llb lb (e,_,(k,l)) dr rb rrb     = (380 + e + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb) 
					      where dangle_e = min (dri_energy inp (lb,rb)) (dr_energy inp (k,l))
    mldlr inp llb lb dl (e,_,_) dr rb rrb = (380 + e + dli_energy inp (lb,rb)  + dri_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
    mladlr inp llb lb dl (e,(i,j),(k,l)) dr rb rrb = (380 + e + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
				      where dangle_e = (min (dli_energy inp (lb,rb)) (dl_energy inp (i,j))) +  (min (dri_energy inp (lb,rb)) (dr_energy inp (k,l)))
    mldladr inp llb lb dl (e,(i,j),(k,l)) dr rb rrb = (380 + e + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
				      where dangle_e = dli_energy inp (lb,rb) + min (dri_energy inp (lb,rb)) (dr_energy inp (k,l))
    mladldr inp llb lb dl (e,(i,j),(k,l)) dr rb rrb = (380 + e + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
				      where dangle_e = (min (dli_energy inp (lb,rb)) (dl_energy inp (i,j))) + dri_energy inp (lb,rb)
    mldl inp llb lb dl (e,_,_) rb rrb     = (380 + e + dli_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb), llb,rrb)
    mladl inp llb lb dl (e,(i,j),_) rb rrb     = (380 + e + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb), llb,rrb)
					  where dangle_e = min (dli_energy inp (lb,rb)) (dl_energy inp (i,j))
    addss (e,(lb1,rb1),(lb2,rb2)) (i,j) = (e + ss_energy (i,j),(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems
    ssadd (i,j) (e,lb,rb) = (40 + e + ss_energy (i,j),(lb,rb),(lb,rb)) -- uebergabe der indizes des ersten und letzten stems
    trafo (e1,lb1,rb1) = (e1,lb1,rb1)
    incl (e,lb,rb) = (40 + e,(lb,rb),(lb,rb)) -- uebergabe der indizes des ersten und letzten stems
    combine inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
    lcombine inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
    lcombine' inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
    rcombine inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
    rcombine' inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
    lrcombine inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
    acomb inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))
    lacomb inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))
    lacomb' inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))
    racomb inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))
    racomb' inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))
    lracomb inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))

    h [] = []
    h xs = [minimum xs]
    h_i [] = []
    h_i xs = [minimum xs]

    h_l [] = []
    h_l xs = if takes < 0.0 then h xs else if (minE_xs < 0.0) then [(minE_xs,i,j)] else []
      where (minE_xs,i,j) = minimum xs

    h_s = h

    (_,n) = bounds basearray



 mfe_range :: Array Int Ebase -> Float ->      -- closed      answer   
              Canonical_Algebra Int (Int,Int) (Float,Int,Int) (Float,(Int,Int),(Int,Int)) (Float,Int,Int)
 mfe_range basearray takes = (sadd,cadd,cadd',cadd'',cadd''',ambd,ambd',nil,nil',edl ,edr ,edlr ,drem,is ,sr ,
                          hl ,sp ,bl ,br ,il ,ml ,mldr ,mladr ,mldlr ,mladlr ,mldladr ,mladldr ,
                          mldl ,mladl ,addss,ssadd,trafo,incl,combine,lcombine,lcombine',rcombine,rcombine',
			   lrcombine,acomb,lacomb,lacomb',racomb,racomb',lracomb,h,h_i,h_l,h_s) where
    (sadd1,cadd1,cadd1',cadd1'',cadd1''',ambd1,ambd1',nil1,nil1',edl1,edr1,edlr1,drem1,is1,sr1,hl1,sp1,bl1,br1,il1,
     ml1,mldr1,mladr1,mldlr1,mladlr1,mldladr1,mladldr1,mldl1,mladl1,addss1,ssadd1,
     trafo1,incl1,combine1,lcombine1,lcombine1',rcombine1,rcombine1',lrcombine1,acomb1,
     lacomb1,lacomb1',racomb1,racomb1',lracomb1,h1,h_i1,h_l1,h_s1) = mfe basearray takes
    sadd = sadd1
    cadd = cadd1
    cadd' = cadd1'
    cadd'' = cadd1''
    cadd''' = cadd1'''
    ambd = ambd1
    ambd' = ambd1'
    nil  = nil1
    nil'  = nil1'
    edl  = edl1
    edr  = edr1
    edlr = edlr1
    drem = drem1
    is   = is1
    sr   = sr1
    hl   = hl1
    sp   = sp1
    bl   = bl1
    br   = br1
    il   = il1
    ml   = ml1
    mldr = mldr1
    mladr = mladr1
    mldlr = mldlr1
    mladlr = mladlr1
    mldladr = mldladr1
    mladldr = mladldr1
    mldl = mldl1
    mladl = mladl1
    addss = addss1
    ssadd = ssadd1
    trafo = trafo1
    incl = incl1
    combine =combine1
    lcombine =lcombine1
    lcombine' =lcombine1'
    rcombine =rcombine1
    rcombine' =rcombine1'
    lrcombine =lrcombine1
    acomb = acomb1
    lacomb = lacomb1
    lacomb' = lacomb1'
    racomb = racomb1
    racomb' = racomb1'
    lracomb = lracomb1

    h [] = []
    h xs = filter_erange (lowest+abs(takes)) xs
       where (lowest,_,_) = minimum xs 
             filter_erange _ [] = []
	      filter_erange limit ((x1,x2,x3):xs) 
              | x1 <= limit = (x1,x2,x3): (filter_erange limit xs)
              | otherwise   = filter_erange limit xs

    h_l [] = []
    h_l xs = if takes < 0.0 then h xs else filter_erange (lowest+abs(takes)) xs
       where (lowest,_,_) = minimum xs 
             filter_erange _ [] = []
	      filter_erange limit ((x1,x2,x3):xs) 
              | x1 < 0.0 && x1 <= limit = (x1,x2,x3): (filter_erange limit xs)
              | otherwise   = filter_erange limit xs

    h_s = h
    h_i = h
    (_,n) = bounds basearray

Shape algebra:

> shape :: String -> String -> String -> String -> String -> String -> String -> String -> Array Int Ebase -> Float -> 
>                Canonical_Algebra Int (Int,Int) String String String

> shape edangle_op edangle_cl loop_ss loop_op loop_cl bulge_op bulge_cl singlestrand basearray takes  = 
>         (sadd,cadd,cadd',cadd'',cadd''',ambd,ambd',nil,nil',edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il,
>          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,
>	   ssadd,trafo,incl,combine,lcombine,lcombine',rcombine,rcombine',
>	   lrcombine,acomb,lacomb,lacomb',racomb,racomb',lracomb,h,h_i,h_l,h_s) where
>   sadd _ s     = if (singlestrand == "" && s == "") then "_" else app singlestrand s
>   cadd s1 s2   = if (singlestrand == "" && s2 =="_") then s1 else app s1 s2
>   cadd' s1 s2   = if (singlestrand == "" && s2 =="_") then s1 else app s1 s2
>   cadd'' s1 s2   = if (singlestrand == "" && s2 =="_") then s1 else app s1 s2
>   cadd''' s1 s2   = if (singlestrand == "" && s2 =="_") then s1 else app s1 s2
>   ambd s1 b s2 = app (app s1 singlestrand) s2
>   ambd' s1 b s2 = app (app s1 singlestrand) s2
>   nil  _       = ""
>   nil'  _       = ""
>   edl  _  s    = singlestrand++edangle_op++s++edangle_cl
>   edr  s _     = edangle_op++s++edangle_cl++singlestrand
>   edlr _ s _   = singlestrand++edangle_op++s++edangle_cl++singlestrand
>   drem    s    = edangle_op++s++edangle_cl
>   is           = id
>   sr _ s _     = s
>   hl _ _ _ _ _ = loop_op++loop_cl
>   sp _ _ s _ _ = s
>   bl _ s       = bulge_op++loop_ss++s++bulge_cl
>   br s _       = bulge_op++s++loop_ss++bulge_cl
>   il _ s _     = loop_op++loop_ss++s++loop_ss++loop_cl
>   ml _ _ s _ _ = loop_op++s++loop_cl
>   mldr _ _ s _ _ _  = loop_op++ (app s singlestrand) ++loop_cl
>   mladr _ _ s _ _ _ = loop_op++ (app s singlestrand) ++loop_cl
>   mldlr _ _ _ s _ _ _  = loop_op++ (app singlestrand (app s singlestrand)) ++loop_cl
>   mladlr _ _ _ s _ _ _ = loop_op++ (app singlestrand (app s singlestrand)) ++loop_cl
>   mldladr _ _ _ s _ _ _ = loop_op++ (app singlestrand (app s singlestrand)) ++loop_cl
>   mladldr _ _ _ s _ _ _ = loop_op++ (app singlestrand (app s singlestrand)) ++loop_cl
>   mldl _ _ _ s _ _  = loop_op++ (app singlestrand s) ++loop_cl
>   mladl _ _ _ s _ _ = loop_op++ (app singlestrand s) ++loop_cl
>   addss s _    = app s singlestrand
>   ssadd _ s    = app singlestrand s
>   trafo s1   = s1
>   incl s         = s
>   combine s1 s2= app s1 s2
>   lcombine s1 s2= app s1 s2
>   lcombine' s1 s2= app s1 s2
>   rcombine s1 s2= app s1 s2
>   rcombine' s1 s2= app s1 s2
>   lrcombine s1 s2= app s1 s2
>   acomb s1 b s2= app (app s1 singlestrand) s2
>   lacomb s1 b s2= app (app s1 singlestrand) s2
>   lacomb' s1 b s2= app (app s1 singlestrand) s2
>   racomb s1 b s2= app (app s1 singlestrand) s2
>   racomb' s1 b s2= app (app s1 singlestrand) s2
>   lracomb s1 b s2= app (app s1 singlestrand) s2
>   h   = nub
>   h_i = h
>   h_l = h
>   h_s = h

> -- edangle_op edangle_cl loop_ss loop_op loop_cl bulge_op bulge_cl singlestrand
> shape1 = shape "" "" "_" "[" "]" "[" "]" "_"  -- all loops are represented different
> shape2 = shape "" "" "_" "[" "]" "[" "]" ""   -- bulges and internal loops have same representation
> shape3 = shape "" "" ""  "[" "]" "[" "]" ""    -- bulges and internal loops have same representation, no external loop
> shape4 = shape "" "" ""  "[" "]" ""  "" ""     -- edangles (complete substructures) and external loop contribute
> shape5 = shape "[" "]" "" "" "" "" "" ""      -- only edangles contribute





Partition function algebra:

> mean_nrg:: Float
> mean_nrg= -0.1843  -- mean energy for random sequences: 184.3*length cal
> mean_scale :: Float
> mean_scale = exp (-mean_nrg/(r_gas * temperature))

> r_gas = 0.00198717 -- [kcal/mol] <-- 1.98717 [cal/mol]
> temperature  = 310.15  -- [K]
> mk_pf x = exp ((-x/100) / (r_gas * temperature)) -- (-x/100) because energies are given multiplied by 100

 p_func :: Array Int Ebase -> Float ->      -- closed                  answer                                 pf_closed
           Canonical_Algebra Int (Int,Int) ((Float,Int,Int),(Int,Int)) (Float,Float,Float,Float)  ((Float,Float,Float,Float,Float,Float),Int,Int)
 p_func basearray takes = (sadd,cadd,cadd', cadd'' basearray,cadd''' basearray,ambd basearray,ambd' basearray,nil,nil',edl basearray,edr basearray,
		     edlr basearray,drem,is basearray,sr basearray,
                    hl basearray,sp basearray,bl basearray,br basearray,il basearray,
                    ml basearray,mldr basearray,mladr basearray,mldlr basearray,mladlr basearray,mldladr basearray,mladldr basearray,
                    mldl basearray,mladl basearray,addss basearray,ssadd basearray,trafo,
                    incl basearray ,combine basearray ,lcombine basearray ,lcombine' basearray ,rcombine basearray ,rcombine' basearray ,
                    lrcombine basearray ,acomb basearray,lacomb basearray,lacomb' basearray,racomb basearray,racomb' basearray,
                    lracomb basearray,h,h_i,h_l,h_s)                where
    scale:: Array Int Float

    scale  = array (0,n) ((0, 1.0) : [(i,  scale!(i-1)/ mean_scale) |i<-[1..n]])

    scale  = array (0,n) ((0, 1.0) : [(i,1.0) |i<-[1..n]])

    sadd b ((q,i,j),(lb,rb)) = ((scale!1 * q,b,j),(lb,rb))
    cadd ((q1,i,_),(lb1,rb1)) ((q2,_,j),(lb2,rb2))   = ((q1 * q2,i,j),(lb1,rb1))  -- uebergabe der indizes des ersten stems
    cadd'((q,i,_),(lb1,rb1)) (t,_,j) = ((q*(sum_elems' t),i,j),(lb1,rb1))
    cadd'' inp ((q1,i,_),(lb1,rb1)) ((q2,_,j),(lb2,rb2)) =  (mk_tuple' (inp!lb1) (inp!rb1) (q1*q2),i,j)
    cadd''' inp ((q1,i,_),(lb1,rb1)) (t,_,j) = (mk_tuple' (inp!lb1) (inp!rb1) (q1*(sum_elems' t)),i,j)
    ambd inp ((q1,i,_),(lb1,rb1)) b (t,_,j) = ((scale!1 * check_tuple inp q1 lb1 rb1 b t,i,j),(lb1,rb1))
    ambd' inp ((q1,i,_),(lb1,rb1)) b (t,_,j)= (mk_tuple' (inp!lb1) (inp!rb1) (scale!1 * check_tuple inp q1 lb1 rb1 b t),i,j)
    nil _ = ((1.0,1,n),(n,n))
    nil' _ = ((1.0,0.0,0.0,0.0,0.0,0.0),1,n)
    edl  inp dl ((q,i,j),(lb,rb))    = ((scale!1 * q * mk_pf (dl_energy inp (lb,rb)),dl,j),(lb,rb))  -- uebergabe der indizes des ersten stems
    edr  inp    ((q,i,j),(lb,rb)) dr = ((scale!1 * q * mk_pf (dr_energy inp (lb,rb)),i,dr),(lb,rb))  -- uebergabe der indizes des ersten stems
    edlr inp dl ((q,_,_),(lb,rb)) dr = ((scale!2 * q * mk_pf (dl_energy inp (lb,rb) + dr_energy inp (lb,rb)),dl,dr),(lb,rb))  -- uebergabe der indizes des ersten stems
    drem = id
    is inp    ((q,i,j),(lb,rb))    = ((q * mk_pf (termaupenalty (inp!lb) (inp!rb)),i,j),(lb,rb))
    sr inp lb ((q,_,_),(_,_)) rb = ((scale!2 * q * mk_pf (sr_energy inp (lb,rb)),lb,rb),(lb,rb))
    hl inp llb lb (i,j) rb rrb = ((scale!(j-i+4) * mk_pf (hl_energy inp (lb,rb) + sr_energy inp (llb,rrb)),llb,rrb),(llb,rrb))
    sp inp llb lb ((q,_,_),(_,_)) rb rrb = ((scale!4 * q * mk_pf (sr_energy inp (llb,rrb)),llb,rrb),(llb,rrb))
    bl inp (l,r) ((q,_,j),(lend,rend)) = ((scale!(r-l) * q * mk_pf (bl_energy inp l    (l,r) (rend+1)),l,j),(l,rend))
    br inp ((q,i,_),(lend,rend)) (l,r) = ((scale!(r-l) * q * mk_pf (br_energy inp (lend-1) (l,r) (r+1)),i,r),(lend,r))
    il inp (l1,l2) ((q,i,j),(l,r)) (r1,r2) = ((scale!((l2-l1) + (r2-r1)) * q * mk_pf (il_energy inp (l1,l2) (r1,r2)),l1,r2),(l1, r2))
    ml inp llb lb q rb rrb          = ((scale!4 * (sum_elems q) * mk_pf (380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
    mldr inp llb lb q dr rb rrb     = ((scale!5 * (sum_elems q) * mk_pf (380 + dri_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
    mladr inp llb lb (q1,q2,q3,q4) dr rb rrb = ((scale!5 * amdangle * mk_pf(380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
				      where amdangle = q1 * mk_pf(min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
					               q2 * mk_pf(min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
					               q3 * mk_pf(min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
					               q4 * mk_pf(min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) 
    mldlr inp llb lb dl q dr rb rrb = ((scale!6 * (sum_elems q) * mk_pf (380 + dli_energy inp (lb,rb)  + dri_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
    mladlr inp llb lb dl (q1,q2,q3,q4) dr rb rrb = ((scale!6 * amdangle * mk_pf (380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
				      where amdangle = q1 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb)) + min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1)))  (inp!dr)) (dri_energy inp (lb,rb))) +
					               q2 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb)) + min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
					               q3 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))+ min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1)))  (inp!dr)) (dri_energy inp (lb,rb))) +
					               q4 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))+ min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb)))
    mldladr inp llb lb dl (q1,q2,q3,q4) dr rb rrb = ((scale!6 * amdangle * mk_pf (380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
				      where amdangle = mk_pf(dli_energy inp (lb,rb)) *
					               q1 * mk_pf(min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) + 
					               q2 * mk_pf(min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
					               q3 * mk_pf(min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
					               q4 * mk_pf(min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) 
    mladldr inp llb lb dl (q1,q2,q3,q4) dr rb rrb = ((scale!6 * amdangle * mk_pf (380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
				      where amdangle = q1 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
					               q2 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
					               q3 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
					               q4 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) *
						       mk_pf (dri_energy inp (lb,rb))
    mldl inp llb lb dl q rb rrb     = ((scale!5 * (sum_elems q) * mk_pf (380 + dli_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)), llb,rrb),(llb,rrb))
    mladl inp llb lb dl (q1,q2,q3,q4) rb rrb     = ((scale!5 * amdangle * mk_pf (380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)), llb,rrb),(llb,rrb))
				      where amdangle = q1 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
					               q2 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
					               q3 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
					               q4 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) 


    addss inp q (i,j) = mult_tup (scale!(j-i) * mk_pf(ss_energy (i,j))) q
    ssadd inp (i,j) ((q,_,j'),(l,r)) = mk_tuple (inp!l) (inp!r) (inp!l) (inp!r) (scale!(j-i) * q * mk_pf(40 + ss_energy (i,j)))
    trafo (t,i,j) = (((sum_elems' t),i,j),(i,j))
    incl inp ((q,i,j),(l,r)) = mk_tuple (inp!l) (inp!r) (inp!l) (inp!r) (q * mk_pf(40))
    combine inp q1 q2 = comb_tup q1 q2
    acomb inp q1 b q2 = mult_tup (scale!1)  (acomb_tup inp q1 b q2)

    lcombine  = combine
    lcombine' = combine
    rcombine = combine
    rcombine' = combine
    lrcombine = combine
    lacomb = acomb
    lacomb' = acomb
    racomb = acomb
    racomb' = acomb
    lracomb = acomb

    h []   = []
    h xs   = [foldl1 (sum_triples) xs]
         where sum_triples ((x1,i,j),(_,_)) ((x2,i',j'),(l,r)) 
			| i' == i && j' == j = ((x1+x2,i,j),(l,r))
			|otherwise           = error "Non-matching indeces h"
    h_l [] = []
    h_l xs = if takes < 0.0 then h xs else sum_triples xs
         where sum_triples [] = []
               sum_triples (((x1,i,j),(l,r)):[]) = if  x1 > scale!(j-i) then [((x1,i,j),(l,r))] else []
               sum_triples (((x1,i,j),(l,r)):((x2,i',j'),(l',r')):xs)
		        | i' == i && j' == j && x1 > scale!(j-i) && x2 > scale!(j-i)   = sum_triples (((x1+x2,i,j),(l,r)):xs)
			| i' == i && j' == j && x1 > scale!(j-i) && x2 <= scale!(j-i)  = sum_triples (((x1,i,j),(l,r)):xs)
			| i' == i && j' == j && x1 <= scale!(j-i) && x2 > scale!(j-i)  = sum_triples (((x2,i',j'),(l',r')):xs)
			| i' == i && j' == j && x1 <= scale!(j-i) && x2 <= scale!(j-i) = sum_triples xs
		        |otherwise                                                     = error "Non-matching indeces h"
    h_s []  = []
    h_s xs  = sum_tuples xs
         where sum_tuples (x:[]) = [x]
	        sum_tuples (((x1,x2,x3,x4,x5,x6),i,j):((y1,y2,y3,y4,y5,y6),i',j'):xs) = sum_tuples (((x1+y1,x2+y2,x3+y3,x4+y4,x5+y5,x6+y6),i,j):xs)

    h_i = id

    h_i []  = []
    h_i xs  = sum_tuples xs
         where sum_tuples (x:[]) = [x]
	        sum_tuples ((x1,x2,x3,x4):(y1,y2,y3,y4):xs) = sum_tuples ((x1+y1,x2+y2,x3+y3,x4+y4):xs)


    (_,n) = bounds basearray

> is_wobble G U = True
> is_wobble U G = True
> is_wobble _ _ = False

> wc_comp G = C
> wc_comp C = G
> wc_comp A = U
> wc_comp U = A
> wob_comp G = U
> wob_comp U = G
> wob_comp A = U -- identical to wc
> wob_comp C = G -- identical to wc

> check_tuple inp q i j b (t1,t2,t3,t4,t5,t6) = 
>                          q * t1 * mk_pf(min (dr_energy inp (i,j)) (dl_dangle_dg (inp!b) (A,U))) + 
>                          q * t2 * mk_pf(min (dr_energy inp (i,j)) (dl_dangle_dg (inp!b) (U,A))) + 
>                          q * t3 * mk_pf(min (dr_energy inp (i,j)) (dl_dangle_dg (inp!b) (G,C))) + 
>                          q * t4 * mk_pf(min (dr_energy inp (i,j)) (dl_dangle_dg (inp!b) (C,G))) + 
>                          q * t5 * mk_pf(min (dr_energy inp (i,j)) (dl_dangle_dg (inp!b) (G,U))) + 
>                          q * t6 * mk_pf(min (dr_energy inp (i,j)) (dl_dangle_dg (inp!b) (U,G)))

> mk_tuple i j i' j' x 
>              | (is_wobble i j) && (is_wobble i' j')         = (0,0,0,x)
>	       | (is_wobble i j) && not (is_wobble i' j')     = (0,0,x,0)
>	       | not (is_wobble i j) && (is_wobble i' j')     = (0,x,0,0)
>	       | not (is_wobble i j) && not (is_wobble i' j') = (x,0,0,0)

> comb_tup (q1,q2,q3,q4) (q1',q2',q3',q4') = ((q1+q2)*(q1'+q3'),(q1+q2)*(q2'+q4'),(q3+q4)*(q3'+q1'),(q4+q3)*(q4'+q2'))

> mult_tup x (q1,q2,q3,q4) = (x*q1,x*q2,x*q3,x*q4)

> acomb_tup s (q1,q2,q3,q4) b (q1',q2',q3',q4')
>	= (q1*(q1'*mk_pf(min (dr_dangle_dg ((wc_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wc_comp (s!(b+1)))))) 
>            + q3'*mk_pf(min (dr_dangle_dg ((wc_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wob_comp (s!(b+1))))))) + 
>          q2*(q1'*mk_pf(min (dr_dangle_dg ((wob_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wc_comp (s!(b+1)))))) 
>            + q3'*mk_pf(min (dr_dangle_dg ((wob_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wob_comp (s!(b+1))))))),
>	   q2*(q2'*mk_pf(min (dr_dangle_dg ((wob_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wc_comp (s!(b+1))))))
>            + q4'*mk_pf(min (dr_dangle_dg ((wob_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wob_comp (s!(b+1))))))) + 
>	   q1*(q2'*mk_pf(min (dr_dangle_dg ((wc_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wc_comp (s!(b+1))))))
>            + q4'*mk_pf(min (dr_dangle_dg ((wc_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wob_comp (s!(b+1))))))),
>	   q3*(q3'*mk_pf(min (dr_dangle_dg ((wc_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wob_comp (s!(b+1)))))) 
>            + q1'*mk_pf(min (dr_dangle_dg ((wc_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wc_comp (s!(b+1))))))) +
>	   q4*(q3'*mk_pf(min (dr_dangle_dg ((wob_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wob_comp (s!(b+1)))))) 
>            + q1'*mk_pf(min (dr_dangle_dg ((wob_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wc_comp (s!(b+1))))))),
>	   q4*(q4'*mk_pf(min (dr_dangle_dg ((wob_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wob_comp (s!(b+1)))))) 
>            + q2'*mk_pf(min (dr_dangle_dg ((wob_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wc_comp (s!(b+1))))))) +
>          q3*(q4'*mk_pf(min (dr_dangle_dg ((wc_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wob_comp (s!(b+1)))))) 
>            + q2'*mk_pf(min (dr_dangle_dg ((wc_comp (s!(b-1))),s!(b-1)) (s!b)) (dl_dangle_dg (s!b) (s!(b+1),(wc_comp (s!(b+1))))))))
>								      

> mk_tuple' A U x = (x,0,0,0,0,0)
> mk_tuple' U A x = (0,x,0,0,0,0)
> mk_tuple' G C x = (0,0,x,0,0,0)
> mk_tuple' C G x = (0,0,0,x,0,0)
> mk_tuple' G U x = (0,0,0,0,x,0)
> mk_tuple' U G x = (0,0,0,0,0,x)


> sum_elems (x1,x2,x3,x4) = x1+x2+x3+x4
> sum_elems' (x1,x2,x3,x4,x5,x6) = x1+x2+x3+x4+x5+x6

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


>
>     struct         = left_dangle1 ||| (trafo <<< noleft_dangle1) ||| left_unpaired1 ... h 
>
>     left_unpaired1 = sadd <<< base    -~~ left_unpaired1 |||
>                      sadd <<< base    -~~ left_dangle1   ... h
>
>     left_dangle1   = listed (
>                      ambd  <<< edanglel1  ~~- base ~~~ noleft_dangle4           ||| 
>                      cadd' <<< edanglel1  ~~~ noleft_dangle4                    |||
>                      cadd  <<< edanglelr1 ~~~ (left_dangle4 ||| left_unpaired4) ... h)
>
>     noleft_dangle1 = listed (
>                      cadd''  <<< edangler1 ~~~ (left_dangle4  ||| left_unpaired4) |||
>                      cadd''' <<< nodangle1 ~~~ noleft_dangle4                     |||
>                      ambd'   <<< nodangle1 ~~- base ~~~ noleft_dangle4            ... h_s)
>
>     edanglel1      = edl  <<< base -~~ motif1          ... h
>     edangler1      = edr  <<<          motif1 ~~- base ... h
>     edanglelr1     = edlr <<< base -~~ motif1 ~~- base ... h
>     nodangle1      = drem <<<          motif1          ... h
>
>     motif1         = initMultiloop1
>
>
>     initMultiloop1 = is <<< endMultiloop1 ... h_l
>
>     endMultiloop1  = tabulated (stack1 ||| multiloop1 ||| leftB1 ||| rightB1 ||| iloop1 ... h) 
>
>     stack1         = (sr  <<< base -~~ endMultiloop1 ~~- base) `with` basepairing
>
>     multiloop1     = (mldl   <<< base -~~ base ~~-   base ~~!! ml_comps12            ~~- base ~~- base |||
>                       mladl  <<< base -~~ base ~~-   base ~~!! ml_comps22            ~~- base ~~- base |||  -- ambiguous dangle
>                       mldr   <<< base -~~ base ~~!             ml_comps32   ~~- base ~~- base ~~- base |||
>                       mladr  <<< base -~~ base ~~!             ml_comps22   ~~- base ~~- base ~~- base |||  -- ambiguous dangle
>                       mldlr  <<< base -~~ base ~~-   base ~~!! ml_comps42   ~~- base ~~- base ~~- base |||
>                       mladlr <<< base -~~ base ~~-   base ~~!! ml_comps22   ~~- base ~~- base ~~- base |||  -- ambiguous dangle both
>                       mldladr<<< base -~~ base ~~-   base ~~!! ml_comps12   ~~- base ~~- base ~~- base |||  -- ambiguous dangle right
>                       mladldr<<< base -~~ base ~~-   base ~~!! ml_comps32   ~~- base ~~- base ~~- base |||  -- ambiguous dangle left
>                       ml     <<< base -~~ base ~~!             ml_comps22            ~~- base ~~- base ) `with` stackpairing     -- ... h
>
>     leftB1         = (sp  <<< base -~~ base ~~! (bl <<< region  ~~~ initMultiloop1)
>                           ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>
>     rightB1        = (sp  <<< base -~~ base ~~! (br <<< initMultiloop1 ~~~ region)
>                           ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>
>     iloop1         = (sp  <<< base -~~ base ~~! (il <<< (region `with` (maxsize 30)) ~~~ endMultiloop1 ~~~ (region `with` (maxsize 30)))
>                           ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>
>     ml_comps12     = tabulated (combine <<< block_dl2  ~~~ no_dl_no_ss_end2          |||
>                                 combine <<< block_dlr2 ~~~ dl_or_ss_left_no_ss_end2  |||
>                                 acomb   <<< block_dl2  ~~- base ~~~ no_dl_no_ss_end2 ... h_i)
>
>     ml_comps22     = tabulated (combine <<< (incl <<< nodangle2) ~~~ no_dl_no_ss_end2          ||| 
>                                 combine <<< (incl <<< edangler2) ~~~ dl_or_ss_left_no_ss_end2  |||
>                                 acomb   <<< (incl <<< nodangle2) ~~- base ~~~ no_dl_no_ss_end2 ... h_i)
>
>     ml_comps32     = combine <<< (incl <<< edangler2) ~~~ dl_or_ss_left_ss_end2  |||
>                      combine <<< (incl <<< nodangle2) ~~~ no_dl_ss_end2          |||
>                      acomb   <<< (incl <<< nodangle2) ~~- base ~~~ no_dl_ss_end2 ... h_i
>
>     ml_comps42     = combine <<< block_dl2  ~~~  no_dl_ss_end2        |||
>                      combine <<< block_dlr2 ~~~ dl_or_ss_left_ss_end2 |||
>                      acomb   <<< block_dl2 ~~- base ~~~ no_dl_ss_end2 ... h_i
>
>     no_dl_no_ss_end2         =            incl       <<< nodangle3                       ... h_i
>     dl_or_ss_left_no_ss_end2 =            block_dl3                                      ... h_i
>     no_dl_ss_end2            = tabulated (incl       <<< edangler3                       |||
>                                           addss      <<< (incl <<< edangler3) ~~~ region ... h_i)
>     dl_or_ss_left_ss_end2    = tabulated (block_dlr3                                     |||
>                                           addss      <<< block_dlr3           ~~~ region ... h_i)
>
>     block_dl2      = tabulated(ssadd <<< region ~~~ edanglel2 |||
>                                incl  <<< edanglel2            ... h_i)
>
>     block_dlr2     = tabulated(ssadd <<< region ~~~ edanglelr2 |||
>                                incl  <<< edanglelr2            ... h_i)
>
>     edanglel2      = edl  <<< base -~~ motif2          ... h
>     edangler2      = edr  <<<          motif2 ~~- base ... h
>     edanglelr2     = edlr <<< base -~~ motif2 ~~- base ... h
>     nodangle2      = drem <<<          motif2          ... h
>     motif2         = initHairpin
>
>
>     block_dl3      = tabulated(ssadd <<< region ~~~ edanglel3 |||
>                                incl  <<< edanglel3            ... h_i)
>
>     block_dlr3     = tabulated(ssadd <<< region ~~~ edanglelr3 |||
>                                incl  <<< edanglelr3            ... h_i)
>
>     edanglel3      = edl  <<< base -~~ motif3          ... h
>     edangler3      = edr  <<<          motif3 ~~- base ... h
>     edanglelr3     = edlr <<< base -~~ motif3 ~~- base ... h
>     nodangle3      = drem <<<          motif3          ... h
>     motif3         = initHairpin
>
>
>     left_unpaired4 = sadd <<< base    -~~ left_unpaired4 |||
>                      sadd <<< base    -~~ left_dangle4   ... h
>
>     left_dangle4   = listed (
>                      ambd  <<< edanglel4  ~~- base ~~~ noleft_dangle10           ||| 
>                      cadd' <<< edanglel4  ~~~ noleft_dangle10                    |||
>                      cadd  <<< edanglelr4 ~~~ (left_dangle10 ||| left_unpaired10) ... h)
>
>     noleft_dangle4 = listed (
>                      cadd''  <<< edangler4 ~~~ (left_dangle10  ||| left_unpaired10) |||
>                      cadd''' <<< nodangle4 ~~~ noleft_dangle10                     |||
>                      ambd'   <<< nodangle4 ~~- base ~~~ noleft_dangle10            ... h_s)
>
>     edanglel4      = edl  <<< base -~~ motif4          ... h
>     edangler4      = edr  <<<          motif4 ~~- base ... h
>     edanglelr4     = edlr <<< base -~~ motif4 ~~- base ... h
>     nodangle4      = drem <<<          motif4          ... h
>
>     motif4         = initMultiloop4
>
>
>     initMultiloop4 = is <<< endMultiloop4 ... h_l
>
>     endMultiloop4  = tabulated (stack4 ||| multiloop4 ||| leftB4 ||| rightB4 ||| iloop4 ... h) 
>
>     stack4         = (sr  <<< base -~~ endMultiloop4 ~~- base) `with` basepairing
>
>     multiloop4     = (mldl   <<< base -~~ base ~~-   base ~~!! ml_comps15            ~~- base ~~- base |||
>                       mladl  <<< base -~~ base ~~-   base ~~!! ml_comps25            ~~- base ~~- base |||  -- ambiguous dangle
>                       mldr   <<< base -~~ base ~~!             ml_comps35   ~~- base ~~- base ~~- base |||
>                       mladr  <<< base -~~ base ~~!             ml_comps25   ~~- base ~~- base ~~- base |||  -- ambiguous dangle
>                       mldlr  <<< base -~~ base ~~-   base ~~!! ml_comps45   ~~- base ~~- base ~~- base |||
>                       mladlr <<< base -~~ base ~~-   base ~~!! ml_comps25   ~~- base ~~- base ~~- base |||  -- ambiguous dangle both
>                       mldladr<<< base -~~ base ~~-   base ~~!! ml_comps15   ~~- base ~~- base ~~- base |||  -- ambiguous dangle right
>                       mladldr<<< base -~~ base ~~-   base ~~!! ml_comps35   ~~- base ~~- base ~~- base |||  -- ambiguous dangle left
>                       ml     <<< base -~~ base ~~!             ml_comps25            ~~- base ~~- base ) `with` stackpairing     -- ... h
>
>     leftB4         = (sp  <<< base -~~ base ~~! (bl <<< region  ~~~ initMultiloop4)
>                           ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>
>     rightB4        = (sp  <<< base -~~ base ~~! (br <<< initMultiloop4 ~~~ region)
>                           ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>
>     iloop4         = (sp  <<< base -~~ base ~~! (il <<< (region `with` (maxsize 30)) ~~~ endMultiloop4 ~~~ (region `with` (maxsize 30)))
>                           ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>
>     ml_comps15     = tabulated (combine <<< block_dl5  ~~~ no_dl_no_ss_end5          |||
>                                 combine <<< block_dlr5 ~~~ dl_or_ss_left_no_ss_end5  |||
>                                 acomb   <<< block_dl5  ~~- base ~~~ no_dl_no_ss_end5 ... h_i)
>
>     ml_comps25     = tabulated (combine <<< (incl <<< nodangle5) ~~~ no_dl_no_ss_end5          ||| 
>                                 combine <<< (incl <<< edangler5) ~~~ dl_or_ss_left_no_ss_end5  |||
>                                 acomb   <<< (incl <<< nodangle5) ~~- base ~~~ no_dl_no_ss_end5 ... h_i)
>
>     ml_comps35     = combine <<< (incl <<< edangler5) ~~~ dl_or_ss_left_ss_end5  |||
>                      combine <<< (incl <<< nodangle5) ~~~ no_dl_ss_end5          |||
>                      acomb   <<< (incl <<< nodangle5) ~~- base ~~~ no_dl_ss_end5 ... h_i
>
>     ml_comps45     = combine <<< block_dl5  ~~~  no_dl_ss_end5        |||
>                      combine <<< block_dlr5 ~~~ dl_or_ss_left_ss_end5 |||
>                      acomb   <<< block_dl5 ~~- base ~~~ no_dl_ss_end5 ... h_i
>
>     no_dl_no_ss_end5         = ml_comps22
>     dl_or_ss_left_no_ss_end5 = ml_comps12
>     no_dl_ss_end5            = ml_comps32
>     dl_or_ss_left_ss_end5    = ml_comps42
>
>     block_dl5      = tabulated(ssadd <<< region ~~~ edanglel5 |||
>                                incl  <<< edanglel5            ... h_i)
>
>     block_dlr5     = tabulated(ssadd <<< region ~~~ edanglelr5 |||
>                                incl  <<< edanglelr5            ... h_i)
>
>     edanglel5      = edl  <<< base -~~ motif5          ... h
>     edangler5      = edr  <<<          motif5 ~~- base ... h
>     edanglelr5     = edlr <<< base -~~ motif5 ~~- base ... h
>     nodangle5      = drem <<<          motif5          ... h
>     motif5         = initMultiloop5
>
>
>     initMultiloop5 = is <<< endMultiloop5 ... h_l
>
>     endMultiloop5  = tabulated (stack5 ||| multiloop5 ||| leftB5 ||| rightB5 ||| iloop5 ... h) 
>
>     stack5         = (sr  <<< base -~~ endMultiloop5 ~~- base) `with` basepairing
>
>     multiloop5     = (mldl   <<< base -~~ base ~~-   base ~~!! ml_comps16            ~~- base ~~- base |||
>                       mladl  <<< base -~~ base ~~-   base ~~!! ml_comps26            ~~- base ~~- base |||  -- ambiguous dangle
>                       mldr   <<< base -~~ base ~~!             ml_comps36   ~~- base ~~- base ~~- base |||
>                       mladr  <<< base -~~ base ~~!             ml_comps26   ~~- base ~~- base ~~- base |||  -- ambiguous dangle
>                       mldlr  <<< base -~~ base ~~-   base ~~!! ml_comps46   ~~- base ~~- base ~~- base |||
>                       mladlr <<< base -~~ base ~~-   base ~~!! ml_comps26   ~~- base ~~- base ~~- base |||  -- ambiguous dangle both
>                       mldladr<<< base -~~ base ~~-   base ~~!! ml_comps16   ~~- base ~~- base ~~- base |||  -- ambiguous dangle right
>                       mladldr<<< base -~~ base ~~-   base ~~!! ml_comps36   ~~- base ~~- base ~~- base |||  -- ambiguous dangle left
>                       ml     <<< base -~~ base ~~!             ml_comps26            ~~- base ~~- base ) `with` stackpairing     -- ... h
>
>     leftB5         = (sp  <<< base -~~ base ~~! (bl <<< region  ~~~ initMultiloop5)
>                           ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>
>     rightB5        = (sp  <<< base -~~ base ~~! (br <<< initMultiloop5 ~~~ region)
>                           ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>
>     iloop5         = (sp  <<< base -~~ base ~~! (il <<< (region `with` (maxsize 30)) ~~~ endMultiloop5 ~~~ (region `with` (maxsize 30)))
>                           ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>
>     ml_comps16     = tabulated (combine <<< block_dl6  ~~~ no_dl_no_ss_end6          |||
>                                 combine <<< block_dlr6 ~~~ dl_or_ss_left_no_ss_end6  |||
>                                 acomb   <<< block_dl6  ~~- base ~~~ no_dl_no_ss_end6 ... h_i)
>
>     ml_comps26     = tabulated (combine <<< (incl <<< nodangle6) ~~~ no_dl_no_ss_end6          ||| 
>                                 combine <<< (incl <<< edangler6) ~~~ dl_or_ss_left_no_ss_end6  |||
>                                 acomb   <<< (incl <<< nodangle6) ~~- base ~~~ no_dl_no_ss_end6 ... h_i)
>
>     ml_comps36     = combine <<< (incl <<< edangler6) ~~~ dl_or_ss_left_ss_end6  |||
>                      combine <<< (incl <<< nodangle6) ~~~ no_dl_ss_end6          |||
>                      acomb   <<< (incl <<< nodangle6) ~~- base ~~~ no_dl_ss_end6 ... h_i
>
>     ml_comps46     = combine <<< block_dl6  ~~~  no_dl_ss_end6        |||
>                      combine <<< block_dlr6 ~~~ dl_or_ss_left_ss_end6 |||
>                      acomb   <<< block_dl6 ~~- base ~~~ no_dl_ss_end6 ... h_i
>
>     no_dl_no_ss_end6         = ml_comps27
>     dl_or_ss_left_no_ss_end6 = ml_comps17
>     no_dl_ss_end6            = ml_comps37
>     dl_or_ss_left_ss_end6    = ml_comps47
>
>     block_dl6      = tabulated(ssadd <<< region ~~~ edanglel6 |||
>                                incl  <<< edanglel6            ... h_i)
>
>     block_dlr6     = tabulated(ssadd <<< region ~~~ edanglelr6 |||
>                                incl  <<< edanglelr6            ... h_i)
>
>     edanglel6      = edl  <<< base -~~ motif6          ... h
>     edangler6      = edr  <<<          motif6 ~~- base ... h
>     edanglelr6     = edlr <<< base -~~ motif6 ~~- base ... h
>     nodangle6      = drem <<<          motif6          ... h
>     motif6         = initHairpin
>
>
>     ml_comps17     = tabulated (combine <<< block_dl7  ~~~ no_dl_no_ss_end7          |||
>                                 combine <<< block_dlr7 ~~~ dl_or_ss_left_no_ss_end7  |||
>                                 acomb   <<< block_dl7  ~~- base ~~~ no_dl_no_ss_end7 ... h_i)
>
>     ml_comps27     = tabulated (combine <<< (incl <<< nodangle7) ~~~ no_dl_no_ss_end7          ||| 
>                                 combine <<< (incl <<< edangler7) ~~~ dl_or_ss_left_no_ss_end7  |||
>                                 acomb   <<< (incl <<< nodangle7) ~~- base ~~~ no_dl_no_ss_end7 ... h_i)
>
>     ml_comps37     = combine <<< (incl <<< edangler7) ~~~ dl_or_ss_left_ss_end7  |||
>                      combine <<< (incl <<< nodangle7) ~~~ no_dl_ss_end7          |||
>                      acomb   <<< (incl <<< nodangle7) ~~- base ~~~ no_dl_ss_end7 ... h_i
>
>     ml_comps47     = combine <<< block_dl7  ~~~  no_dl_ss_end7        |||
>                      combine <<< block_dlr7 ~~~ dl_or_ss_left_ss_end7 |||
>                      acomb   <<< block_dl7 ~~- base ~~~ no_dl_ss_end7 ... h_i
>
>     no_dl_no_ss_end7         =            incl       <<< nodangle3                       ... h_i
>     dl_or_ss_left_no_ss_end7 =            block_dl3                                      ... h_i
>     no_dl_ss_end7            = tabulated (incl       <<< edangler3                       |||
>                                           addss      <<< (incl <<< edangler3) ~~~ region ... h_i)
>     dl_or_ss_left_ss_end7    = tabulated (block_dlr3                                     |||
>                                           addss      <<< block_dlr3           ~~~ region ... h_i)
>
>     block_dl7      = tabulated(ssadd <<< region ~~~ edanglel7 |||
>                                incl  <<< edanglel7            ... h_i)
>
>     block_dlr7     = tabulated(ssadd <<< region ~~~ edanglelr7 |||
>                                incl  <<< edanglelr7            ... h_i)
>
>     edanglel7      = edl  <<< base -~~ motif7          ... h
>     edangler7      = edr  <<<          motif7 ~~- base ... h
>     edanglelr7     = edlr <<< base -~~ motif7 ~~- base ... h
>     nodangle7      = drem <<<          motif7          ... h
>     motif7         = motif1
>     left_unpaired10 = sadd <<< base    -~~ left_unpaired10 |||
>                       sadd <<< base    -~~ left_dangle10   ... h
>
>     left_dangle10   = listed (cadd' <<< edanglel10  ~~~ (nil' <<< empty) |||
>                               cadd  <<< edanglelr10 ~~~ ((nil <<< empty) ||| left_unpairedEnd) ... h)
>
>     noleft_dangle10 = listed (cadd''  <<< edangler10 ~~~ ((nil <<< empty) ||| left_unpairedEnd)     |||
>                         cadd''' <<< nodangle10 ~~~ (nil' <<< empty) ... h_s)
>
>     edanglel10      = edl  <<< base -~~ motif10          ... h
>     edangler10      = edr  <<<          motif10 ~~- base ... h
>     edanglelr10     = edlr <<< base -~~ motif10 ~~- base ... h
>     nodangle10      = drem <<<          motif10          ... h
>
>     motif10         = initHairpin
>
>
>     initHairpin    = is <<< endHairpin ... h_l
>
>     endHairpin     = tabulated (
>                      stack ||| hairpin ||| leftB ||| rightB ||| iloop ... h) 
>
>     stack          = (sr  <<< base -~~ endHairpin ~~- base) `with` basepairing
>
>     hairpin        = (hl  <<< base -~~ base ~~! (region `with` minloopsize 3)
>                           ~~- base ~~- base)
>                      `with` stackpairing
>
>     leftB          = (sp  <<< base -~~ base ~~! (bl <<< region  ~~~ initHairpin)
>                           ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>
>     rightB         = (sp  <<< base -~~ base ~~! (br <<< initHairpin ~~~ region)
>                           ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>
>     iloop          = (sp  <<< base -~~ base ~~! (il <<< (region `with` (maxsize 30)) ~~~ endHairpin ~~~ (region `with` (maxsize 30)))
>                       ~~- base ~~- base)
>                      `with` stackpairing -- ... h
>     left_unpairedEnd = sadd <<< base    -~~ left_unpairedEnd   |||
>                        sadd <<< base    -~~ (nil <<< empty)    ... h

}

> getShape :: String
> getShape = "[[][]][[[][[][]][]][][]][]"
> getLevel :: Int
> getLevel = 5

