> module RNAshapesMfe.Algebra_mfe where

--### Berechnet die minimale freie Energie fuer den Suchraum, d.h. der "beste" Kandidat wird gewaehlt und von ihm die MFE ausgegeben.
--### [(Float, Int, Int)] Liste mit einem Element

> import RNAshapesMfe.AlgebraType
> import Data.Array
> import Data.List 
> import RNAshapesMfe.RnaI
> import RNAshapesMfe.Energy

Minimal free energy algebra:

> mfe :: Array Int Ebase -> Float ->      -- closed      answer   
>        Canonical_Algebra Int (Int,Int) (Float,Int,Int) (Float,(Int,Int),(Int,Int)) (Float,Int,Int)
> mfe basearray takes = (sadd,cadd,cadd',cadd'',cadd''',ambd basearray,ambd' basearray,nil,nil',edl basearray,edr basearray,edlr basearray,drem,is basearray,sr basearray,
>                    hl basearray, hlChar basearray,sp basearray,bl basearray,br basearray,il basearray,
>                    ml basearray,mldr basearray,mladr basearray,mldlr basearray,mladlr basearray,mldladr basearray,mladldr basearray,
>                    mldl basearray,mladl basearray,addss,ssadd,trafo,
>                    incl,combine basearray,lcombine basearray,lcombine' basearray,rcombine basearray,rcombine' basearray,
>		     lrcombine basearray,acomb basearray,lacomb basearray,lacomb' basearray,racomb basearray,racomb' basearray,
>		     lracomb basearray,h,h_i,h_l,h_s)                where
>    sadd lb (e,_,rb) = (e,lb,rb)
>    cadd (e1,lb1,rb1) (e2,lb2,rb2)     = (e1 + e2,lb1,rb1)  -- uebergabe der indizes des ersten stems
>    cadd' (e1,lb1,rb1) (e2,lb2,rb2)     = (e1 + e2,lb1,rb1)  -- uebergabe der indizes des ersten stems
>    cadd'' (e1,lb1,rb1) (e2,lb2,rb2)     = (e1 + e2,lb1,rb1)  -- uebergabe der indizes des ersten stems
>    cadd''' (e1,lb1,rb1) (e2,lb2,rb2)     = (e1 + e2,lb1,rb1)  -- uebergabe der indizes des ersten stems
>    ambd inp (e1,lb1,rb1) db (e2,lb2,rb2) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),lb1,rb1) -- uebergabe der indizes des ersten stems
>    ambd' inp (e1,lb1,rb1) db (e2,lb2,rb2) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),lb1,rb1) -- uebergabe der indizes des ersten stems
>    nil _ = (0,n,n)
>    nil' _ = (0,n,n)
>    edl  inp dl (e,lb,rb)    = (e + dl_energy inp (lb,rb),lb,rb) -- uebergabe der indizes des ersten stems
>    edr  inp    (e,lb,rb) dr = (e + dr_energy inp (lb,rb),lb,rb) -- uebergabe der indizes des ersten stems
>    edlr inp dl (e,lb,rb) dr = (e + dl_energy inp (lb,rb) + dr_energy inp (lb,rb),lb,rb) -- uebergabe der indizes des ersten stems
>    drem = id
>    is inp    (e,lb,rb)    = (e + termaupenalty (inp!lb) (inp!rb),lb,rb)
>    sr inp lb (e,_,_) rb = (e + sr_energy inp (lb,rb),lb,rb)
>    hl inp llb lb loop rb rrb = (hl_energy inp (lb,rb) + sr_energy inp (llb,rrb),llb,rrb)
>    hlChar inp llb lb loop rb rrb = (hl_energy inp (lb,rb) + sr_energy inp (llb,rrb),llb,rrb)
>    sp inp llb lb (e,_,_) rb rrb = (e + sr_energy inp (llb,rrb), llb,rrb)
>    bl inp (l,r) (e,lend,rend) = (e + bl_energy inp l    (l,r) (rend+1),l,rend)
>    br inp (e,lend,rend) (l,r) = (e + br_energy inp (lend-1) (l,r) (r+1),lend,r)
>    il inp (l1,l2) (e,l,r) (r1,r2) = (e + il_energy inp (l1,l2) (r1,r2), l1, r2)
>    ml inp llb lb (e,_,_) rb rrb          = (380 + e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
>    mldr inp llb lb (e,_,_) dr rb rrb     = (380 + e + dri_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
>    mladr inp llb lb (e,_,(k,l)) dr rb rrb     = (380 + e + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb) 
>					      where dangle_e = min (dri_energy inp (lb,rb)) (dr_energy inp (k,l))
>    mldlr inp llb lb dl (e,_,_) dr rb rrb = (380 + e + dli_energy inp (lb,rb)  + dri_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
>    mladlr inp llb lb dl (e,(i,j),(k,l)) dr rb rrb = (380 + e + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
>				      where dangle_e = (min (dli_energy inp (lb,rb)) (dl_energy inp (i,j))) +  (min (dri_energy inp (lb,rb)) (dr_energy inp (k,l)))
>    mldladr inp llb lb dl (e,(i,j),(k,l)) dr rb rrb = (380 + e + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
>				      where dangle_e = dli_energy inp (lb,rb) + min (dri_energy inp (lb,rb)) (dr_energy inp (k,l))
>    mladldr inp llb lb dl (e,(i,j),(k,l)) dr rb rrb = (380 + e + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb),llb,rrb)
>				      where dangle_e = (min (dli_energy inp (lb,rb)) (dl_energy inp (i,j))) + dri_energy inp (lb,rb)
>    mldl inp llb lb dl (e,_,_) rb rrb     = (380 + e + dli_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb), llb,rrb)
>    mladl inp llb lb dl (e,(i,j),_) rb rrb     = (380 + e + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb), llb,rrb)
>					  where dangle_e = min (dli_energy inp (lb,rb)) (dl_energy inp (i,j))
>    addss (e,(lb1,rb1),(lb2,rb2)) (i,j) = (e + ss_energy (i,j),(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems
>    ssadd (i,j) (e,lb,rb) = (40 + e + ss_energy (i,j),(lb,rb),(lb,rb)) -- uebergabe der indizes des ersten und letzten stems
>    trafo (e1,lb1,rb1) = (e1,lb1,rb1)
>    incl (e,lb,rb) = (40 + e,(lb,rb),(lb,rb)) -- uebergabe der indizes des ersten und letzten stems
>    combine inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
>    lcombine inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
>    lcombine' inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
>    rcombine inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
>    rcombine' inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
>    lrcombine inp (e1,(lb1,rb1),_) (e2,_,(lb2,rb2)) = (e1 + e2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
>    acomb inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))
>    lacomb inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))
>    lacomb' inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))
>    racomb inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))
>    racomb' inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))
>    lracomb inp (e1,(lba,rba),(lb1,rb1)) b (e2,(lb2,rb2),(lbb,rbb)) = (e1 + e2 + (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))

>    h :: (Ord a) => [(a,b,c)] -> [(a,b,c)]
>    h [] = []
>    h xs = my_min xs

>    my_min xs = [minimumBy (\(x,_,_) (y,_,_) -> compare x y ) xs]

>    h_i :: (Ord a) => [(a,b,c)] -> [(a,b,c)]
>    h_i [] = []
>    h_i xs = h xs

>    h_l :: (Ord a) => [(a,b,c)] -> [(a,b,c)]
>    h_l [] = []
>    h_l xs = h xs


    h_l xs = if takes < 0.0 then h xs else if (minE_xs < 0.0) then [(minE_xs,i,j)] else []
      where (minE_xs,i,j) = minimum xs

>    h_s :: (Ord a) => [(a,b,c)] -> [(a,b,c)]
>    h_s = h

>    (_,n) = bounds basearray
