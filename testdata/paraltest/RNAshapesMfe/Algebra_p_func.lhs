> module RNAshapesMfe.Algebra_p_func where

--###Berechnet den Partition Function Value des gesamten Suchraums
--###[((Float, Int, Int), (Int, Int))] einelementige Liste

> import RNAshapesMfe.AlgebraTypePF
> import RNAshapesMfe.Energy
> import Data.Array
> import RNAshapesMfe.RnaI
> import RNAshapesMfe.CommonFunctions

Partition function algebra:

> mean_nrg:: Float
> mean_nrg= -0.1843  -- mean energy for random sequences: 184.3*length cal
> mean_scale :: Float
> mean_scale = exp (-mean_nrg/(r_gas * temperature))

> r_gas = 0.00198717 -- [kcal/mol] <-- 1.98717 [cal/mol]
> temperature  = 310.15  -- [K]
> mk_pf x = exp ((-x/100) / (r_gas * temperature)) -- (-x/100) because energies are given multiplied by 100

> p_func :: Array Int Ebase -> Float ->      -- closed                  answer                                 pf_closed
>           Canonical_Algebra Int (Int,Int) ((Float,Int,Int),(Int,Int)) (Float,Float,Float,Float)  ((Float,Float,Float,Float,Float,Float),Int,Int)
> p_func basearray takes = (sadd,cadd,cadd', cadd'' basearray,cadd''' basearray,ambd basearray,ambd' basearray,nil,nil',edl basearray,edr basearray,
>		     edlr basearray,drem,is basearray,sr basearray,
>                    hl basearray, hlChar basearray,sp basearray,bl basearray,br basearray,il basearray,
>                    ml basearray,mldr basearray,mladr basearray,mldlr basearray,mladlr basearray,mldladr basearray,mladldr basearray,
>                    mldl basearray,mladl basearray,addss basearray,ssadd basearray,trafo,
>                    incl basearray ,combine basearray,
>                    acomb basearray,h,h_i,h_l,h_s)                where
>    scale:: Array Int Float

>    scale  = array (0,n) ((0, 1.0) : [(i,  scale!(i-1)/ mean_scale) |i<-[1..n]])

    scale  = array (0,n) ((0, 1.0) : [(i,1.0) |i<-[1..n]])

>    sadd b ((q,i,j),(lb,rb)) = ((scale!1 * q,b,j),(lb,rb))
>    cadd ((q1,i,_),(lb1,rb1)) ((q2,_,j),(lb2,rb2))   = ((q1 * q2,i,j),(lb1,rb1))  -- uebergabe der indizes des ersten stems
>    cadd'((q,i,_),(lb1,rb1)) (t,_,j) = ((q*(sum_elems' t),i,j),(lb1,rb1))
>    cadd'' inp ((q1,i,_),(lb1,rb1)) ((q2,_,j),(lb2,rb2)) =  (mk_tuple' (inp!lb1) (inp!rb1) (q1*q2),i,j)
>    cadd''' inp ((q1,i,_),(lb1,rb1)) (t,_,j) = (mk_tuple' (inp!lb1) (inp!rb1) (q1*(sum_elems' t)),i,j)
>    ambd inp ((q1,i,_),(lb1,rb1)) b (t,_,j) = ((scale!1 * check_tuple inp q1 lb1 rb1 b t,i,j),(lb1,rb1))
>    ambd' inp ((q1,i,_),(lb1,rb1)) b (t,_,j)= (mk_tuple' (inp!lb1) (inp!rb1) (scale!1 * check_tuple inp q1 lb1 rb1 b t),i,j)
>    nil _ = ((1.0,1,n),(n,n))
>    nil' _ = ((1.0,0.0,0.0,0.0,0.0,0.0),1,n)
>    edl  inp dl ((q,i,j),(lb,rb))    = ((scale!1 * q * mk_pf (dl_energy inp (lb,rb)),dl,j),(lb,rb))  -- uebergabe der indizes des ersten stems
>    edr  inp    ((q,i,j),(lb,rb)) dr = ((scale!1 * q * mk_pf (dr_energy inp (lb,rb)),i,dr),(lb,rb))  -- uebergabe der indizes des ersten stems
>    edlr inp dl ((q,_,_),(lb,rb)) dr = ((scale!2 * q * mk_pf (dl_energy inp (lb,rb) + dr_energy inp (lb,rb)),dl,dr),(lb,rb))  -- uebergabe der indizes des ersten stems
>    drem = id
>    is inp    ((q,i,j),(lb,rb))    = ((q * mk_pf (termaupenalty (inp!lb) (inp!rb)),i,j),(lb,rb))
>    sr inp lb ((q,_,_),(_,_)) rb = ((scale!2 * q * mk_pf (sr_energy inp (lb,rb)),lb,rb),(lb,rb))
>    hl inp llb lb (i,j) rb rrb = ((scale!(j-i+4) * mk_pf (hl_energy inp (lb,rb) + sr_energy inp (llb,rrb)),llb,rrb),(llb,rrb))
>    hlChar inp llb lb (i,j) rb rrb = ((scale!(j-i+4) * mk_pf (hl_energy inp (lb,rb) + sr_energy inp (llb,rrb)),llb,rrb),(llb,rrb))
>    sp inp llb lb ((q,_,_),(_,_)) rb rrb = ((scale!4 * q * mk_pf (sr_energy inp (llb,rrb)),llb,rrb),(llb,rrb))
>    bl inp (l,r) ((q,_,j),(lend,rend)) = ((scale!(r-l) * q * mk_pf (bl_energy inp l    (l,r) (rend+1)),l,j),(l,rend))
>    br inp ((q,i,_),(lend,rend)) (l,r) = ((scale!(r-l) * q * mk_pf (br_energy inp (lend-1) (l,r) (r+1)),i,r),(lend,r))
>    il inp (l1,l2) ((q,i,j),(l,r)) (r1,r2) = ((scale!((l2-l1) + (r2-r1)) * q * mk_pf (il_energy inp (l1,l2) (r1,r2)),l1,r2),(l1, r2))
>    ml inp llb lb q rb rrb          = ((scale!4 * (sum_elems q) * mk_pf (380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
>    mldr inp llb lb q dr rb rrb     = ((scale!5 * (sum_elems q) * mk_pf (380 + dri_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
>    mladr inp llb lb (q1,q2,q3,q4) dr rb rrb = ((scale!5 * amdangle * mk_pf(380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
>				      where amdangle = q1 * mk_pf(min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
>					               q2 * mk_pf(min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
>					               q3 * mk_pf(min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
>					               q4 * mk_pf(min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) 
>    mldlr inp llb lb dl q dr rb rrb = ((scale!6 * (sum_elems q) * mk_pf (380 + dli_energy inp (lb,rb)  + dri_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
>    mladlr inp llb lb dl (q1,q2,q3,q4) dr rb rrb = ((scale!6 * amdangle * mk_pf (380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
>				      where amdangle = q1 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb)) + min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1)))  (inp!dr)) (dri_energy inp (lb,rb))) +
>					               q2 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb)) + min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
>					               q3 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))+ min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1)))  (inp!dr)) (dri_energy inp (lb,rb))) +
>					               q4 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))+ min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb)))
>    mldladr inp llb lb dl (q1,q2,q3,q4) dr rb rrb = ((scale!6 * amdangle * mk_pf (380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
>				      where amdangle = mk_pf(dli_energy inp (lb,rb)) *
>					               q1 * mk_pf(min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) + 
>					               q2 * mk_pf(min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
>					               q3 * mk_pf(min (dr_dangle_dg (wc_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) +
>					               q4 * mk_pf(min (dr_dangle_dg (wob_comp (inp!(dr-1)),(inp!(dr-1))) (inp!dr)) (dri_energy inp (lb,rb))) 
>    mladldr inp llb lb dl (q1,q2,q3,q4) dr rb rrb = ((scale!6 * amdangle * mk_pf (380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb),(llb,rrb))
>				      where amdangle = q1 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
>					               q2 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
>					               q3 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
>					               q4 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) *
>						       mk_pf (dri_energy inp (lb,rb))
>    mldl inp llb lb dl q rb rrb     = ((scale!5 * (sum_elems q) * mk_pf (380 + dli_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)), llb,rrb),(llb,rrb))
>    mladl inp llb lb dl (q1,q2,q3,q4) rb rrb     = ((scale!5 * amdangle * mk_pf (380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)), llb,rrb),(llb,rrb))
>				      where amdangle = q1 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
>					               q2 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wc_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
>					               q3 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) +
>					               q4 * mk_pf(min (dl_dangle_dg (inp!dl) ((inp!(dl+1)),wob_comp (inp!(dl+1)))) (dli_energy inp (lb,rb))) 
>    addss inp q (i,j) = mult_tup (scale!(j-i) * mk_pf(ss_energy (i,j))) q
>    ssadd inp (i,j) ((q,_,j'),(l,r)) = mk_tuple (inp!l) (inp!r) (inp!l) (inp!r) (scale!(j-i) * q * mk_pf(40 + ss_energy (i,j)))
>    trafo (t,i,j) = (((sum_elems' t),i,j),(i,j))
>    incl inp ((q,i,j),(l,r)) = mk_tuple (inp!l) (inp!r) (inp!l) (inp!r) (q * mk_pf(40))
>    combine inp q1 q2 = comb_tup q1 q2
>    acomb inp q1 b q2 = mult_tup (scale!1)  (acomb_tup inp q1 b q2)


>    h []   = []
>    h xs   = [foldl1 (sum_triples) xs]
>         where sum_triples ((x1,i,j),(_,_)) ((x2,i',j'),(l,r)) 
>			| i' == i && j' == j = ((x1+x2,i,j),(l,r))
>			|otherwise           = error "Non-matching indeces h"
>    h_l [] = []
>    h_l xs = if takes < 0.0 then h xs else sum_triples xs
>         where sum_triples [] = []
>               sum_triples (((x1,i,j),(l,r)):[]) = if  x1 > scale!(j-i) then [((x1,i,j),(l,r))] else []
>               sum_triples (((x1,i,j),(l,r)):((x2,i',j'),(l',r')):xs)
>		        | i' == i && j' == j && x1 > scale!(j-i) && x2 > scale!(j-i)   = sum_triples (((x1+x2,i,j),(l,r)):xs)
>			| i' == i && j' == j && x1 > scale!(j-i) && x2 <= scale!(j-i)  = sum_triples (((x1,i,j),(l,r)):xs)
>			| i' == i && j' == j && x1 <= scale!(j-i) && x2 > scale!(j-i)  = sum_triples (((x2,i',j'),(l',r')):xs)
>			| i' == i && j' == j && x1 <= scale!(j-i) && x2 <= scale!(j-i) = sum_triples xs
>		        |otherwise                                                     = error "Non-matching indeces h"
>    h_s []  = []
>    h_s xs  = sum_tuples xs
>         where sum_tuples (x:[]) = [x]
>	        sum_tuples (((x1,x2,x3,x4,x5,x6),i,j):((y1,y2,y3,y4,y5,y6),i',j'):xs) = sum_tuples (((x1+y1,x2+y2,x3+y3,x4+y4,x5+y5,x6+y6),i,j):xs)

    h_i = id

>    h_i []  = []
>    h_i xs  = sum_tuples xs
>         where sum_tuples (x:[]) = [x]
>	        sum_tuples ((x1,x2,x3,x4):(y1,y2,y3,y4):xs) = sum_tuples ((x1+y1,x2+y2,x3+y3,x4+y4):xs)


>    (_,n) = bounds basearray


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



