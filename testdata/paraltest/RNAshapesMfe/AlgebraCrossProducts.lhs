> module RNAshapesMfe.AlgebraCrossProducts where

> import RNAshapesMfe.AlgebraType
> import Data.List

Algebra cross product:

> infix @@@
> (alg1 @@@ alg2) basearray takes =
>         (sadd,cadd,cadd',cadd'',cadd''',ambd,ambd',nil,nil',edl,edr,edlr,drem,is,sr,hl,hlChar,sp,bl,br,il,
>          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,trafo,incl,
>          combine,lcombine,lcombine',rcombine,rcombine',lrcombine,acomb,lacomb,lacomb',
>	   racomb,racomb',lracomb,h,h_i,h_l,h_s) where
>    (sadd1,cadd1,cadd1',cadd1'',cadd1''',ambd1,ambd1',nil1,nil1',edl1,edr1,edlr1,drem1,is1,sr1,hl1,hlChar1,sp1,bl1,br1,il1,
>     ml1,mldr1,mladr1,mldlr1,mladlr1,mldladr1,mladldr1,mldl1,mladl1,addss1,ssadd1,trafo1,incl1,
>     combine1,lcombine1,lcombine1',rcombine1,rcombine1',lrcombine1,acomb1,lacomb1,lacomb1',
>     racomb1,racomb1',lracomb1,h1,h_i1,h_l1,h_s1) = alg1 basearray takes
>    (sadd2,cadd2,cadd2',cadd2'',cadd2''',ambd2,ambd2',nil2,nil2',edl2,edr2,edlr2,drem2,is2,sr2,hl2,hlChar2,sp2,bl2,br2,il2,
>     ml2,mldr2,mladr2,mldlr2,mladlr2,mldladr2,mladldr2,mldl2,mladl2,addss2,ssadd2,trafo2,incl2,
>     combine2,lcombine2,lcombine2',rcombine2,rcombine2',lrcombine2,acomb2,lacomb2,lacomb2',
>     racomb2,racomb2',lracomb2,h2,h_i2,h_l2,h_s2) = alg2 basearray takes

>    sadd b (a1,a2) = (sadd1 b a1, sadd2 b a2)
>    cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
>    cadd' (c1,c2) (a1,a2) = (cadd1' c1 a1, cadd2' c2 a2)
>    cadd'' (c1,c2) (a1,a2) = (cadd1'' c1 a1, cadd2'' c2 a2)
>    cadd''' (c1,c2) (a1,a2) = (cadd1''' c1 a1, cadd2''' c2 a2)
>    ambd (c1,c2) b (a1,a2) = (ambd1 c1 b a1, ambd2 c2 b a2)
>    ambd' (c1,c2) b (a1,a2) = (ambd1' c1 b a1, ambd2' c2 b a2)
>    nil a = (nil1 a, nil2 a)
>    nil' a = (nil1' a, nil2' a)
>    edl b (c1,c2) = (edl1 b c1, edl2 b c2)
>    edr (c1,c2) b = (edr1 c1 b, edr2 c2 b)
>    edlr b (c1,c2) b' = (edlr1 b c1 b', edlr2 b c2 b')
>    drem (c1,c2) = (drem1 c1, drem2 c2)
>    is (c1,c2)   = (is1 c1, is2 c2)
>    sr b (c1,c2) b' = (sr1 b c1 b', sr2 b c2 b')
>    hl b1 b2 u b2' b1' = (hl1 b1 b2 u b2' b1', hl2 b1 b2 u b2' b1')
>    hlChar b1 b2 u b2' b1' = (hlChar1 b1 b2 u b2' b1', hlChar2 b1 b2 u b2' b1')
>    sp b1 b2 (c1,c2) b2' b1' = (sp1 b1 b2 c1 b2' b1',sp2 b1 b2 c2 b2' b1') 
>    bl u (c1,c2) = (bl1 u c1, bl2 u c2)
>    br (c1,c2) u = (br1 c1 u, br2 c2 u)
>    il r1 (c1,c2) r2 = (il1 r1 c1 r2 ,il2 r1 c2 r2) 
>    ml b1 b2 (m1,m2) b2' b1' = (ml1 b1 b2 m1 b2' b1', ml2 b1 b2 m2 b2' b1')
>    mldr b1 b2 (m1,m2) d b2' b1' = (mldr1 b1 b2 m1 d b2' b1',
>                                    mldr2 b1 b2 m2 d b2' b1')
>    mladr b1 b2 (m1,m2) d b2' b1' = (mladr1 b1 b2 m1 d b2' b1',
>                                     mladr2 b1 b2 m2 d b2' b1')
>    mldlr b1 b2 d (m1,m2) d_ b2' b1' = (mldlr1 b1 b2 d m1 d_ b2' b1',
>                                        mldlr2 b1 b2 d m2 d_ b2' b1')
>    mladlr b1 b2 d (m1,m2) d_ b2' b1' = (mladlr1 b1 b2 d m1 d_ b2' b1',
>                                         mladlr2 b1 b2 d m2 d_ b2' b1')
>    mldladr b1 b2 d (m1,m2) d_ b2' b1' = (mldladr1 b1 b2 d m1 d_ b2' b1',
>                                          mldladr2 b1 b2 d m2 d_ b2' b1')
>    mladldr b1 b2 d (m1,m2) d_ b2' b1' = (mladldr1 b1 b2 d m1 d_ b2' b1',
>                                          mladldr2 b1 b2 d m2 d_ b2' b1')
>    mldl b1 b2 d (m1,m2) b2' b1' = (mldl1 b1 b2 d m1 b2' b1', mldl2 b1 b2 d m2 b2' b1')
>    mladl b1 b2 d (m1,m2) b2' b1' = (mladl1 b1 b2 d m1 b2' b1', mladl2 b1 b2 d m2 b2' b1')
>    addss (c1,c2) u = (addss1 c1 u, addss2 c2 u)
>    ssadd u (c1,c2) = (ssadd1 u c1, ssadd2 u c2)
>    trafo (c1,c2)= (trafo1 c1, trafo2 c2)
>    incl (c1,c2) = (incl1 c1, incl2 c2)
>    combine (c1,c2) (c_1,c_2) = (combine1 c1 c_1, combine2 c2 c_2)
>    lcombine (c1,c2) (c_1,c_2) = (lcombine1 c1 c_1, lcombine2 c2 c_2)
>    lcombine' (c1,c2) (c_1,c_2) = (lcombine1' c1 c_1, lcombine2' c2 c_2)
>    rcombine (c1,c2) (c_1,c_2) = (rcombine1 c1 c_1, rcombine2 c2 c_2)
>    rcombine' (c1,c2) (c_1,c_2) = (rcombine1' c1 c_1, rcombine2' c2 c_2)
>    lrcombine (c1,c2) (c_1,c_2) = (lrcombine1 c1 c_1, lrcombine2 c2 c_2)
>    acomb (c1,c2) b (c_1,c_2) = (acomb1 c1 b c_1, acomb2 c2 b c_2)
>    lacomb (c1,c2) b (c_1,c_2) = (lacomb1 c1 b c_1, lacomb2 c2 b c_2)
>    lacomb' (c1,c2) b (c_1,c_2) = (lacomb1' c1 b c_1, lacomb2' c2 b c_2)
>    racomb (c1,c2) b (c_1,c_2) = (racomb1 c1 b c_1, racomb2 c2 b c_2)
>    racomb' (c1,c2) b (c_1,c_2) = (racomb1' c1 b c_1, racomb2' c2 b c_2)
>    lracomb (c1,c2) b (c_1,c_2) = (lracomb1 c1 b c_1, lracomb2 c2 b c_2)

>    h xs   = [(x1,x2)| x1 <- nubBy (\(x,_,_) (y,_,_) -> x == y)  $   h1 [ y1 | (y1,y2) <- xs],
>                       x2 <-         h2 [ y2 | (y1,y2) <- xs, (\(x,_,_) (y,_,_) -> x == y) y1 x1]]


>    h_i xs   = [(x1,x2)| x1 <- nubBy (\(x,_,_) (y,_,_) -> x == y)  $   h_i1 [ y1 | (y1,y2) <- xs],
>                       x2 <-         h_i2 [ y2 | (y1,y2) <- xs, (\(x,_,_) (y,_,_) -> x == y) y1 x1]]

    h_i xs = [(x1,x2)| x1 <- nub $ h_i1 [ y1 | (y1,y2) <- xs],
                       x2 <-       h_i2 [ y2 | (y1,y2) <- xs, y1 == x1]]

>    h_l = h

>    h_s = h


    h_l xs = [(x1,x2)| x1 <- nub $ h_l1 [ y1 | (y1,y2) <- xs],
                       x2 <-       h_l2 [ y2 | (y1,y2) <- xs, y1 == x1]]

    h_s xs = [(x1,x2)| x1 <- nub $ h_s1 [ y1 | (y1,y2) <- xs],
                       x2 <-       h_s2 [ y2 | (y1,y2) <- xs, y1 == x1]]

> infix ***
> (alg1 *** alg2) basearray takes =
>         (sadd,cadd,cadd',cadd'',cadd''',ambd,ambd',nil,nil',edl,edr,edlr,drem,is,sr,hl,hlChar,sp,bl,br,il,
>          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,trafo,incl,
>          combine,lcombine,lcombine',rcombine,rcombine',lrcombine,acomb,lacomb,lacomb',
>	   racomb,racomb',lracomb,h,h_i,h_l,h_s) where
>    (sadd1,cadd1,cadd1',cadd1'',cadd1''',ambd1,ambd1',nil1,nil1',edl1,edr1,edlr1,drem1,is1,sr1,hl1,hlChar1,sp1,bl1,br1,il1,
>     ml1,mldr1,mladr1,mldlr1,mladlr1,mldladr1,mladldr1,mldl1,mladl1,addss1,ssadd1,trafo1,incl1,
>     combine1,lcombine1,lcombine1',rcombine1,rcombine1',lrcombine1,acomb1,lacomb1,lacomb1',
>     racomb1,racomb1',lracomb1,h1,h_i1,h_l1,h_s1) = alg1 basearray takes
>    (sadd2,cadd2,cadd2',cadd2'',cadd2''',ambd2,ambd2',nil2,nil2',edl2,edr2,edlr2,drem2,is2,sr2,hl2,hlChar2,sp2,bl2,br2,il2,
>     ml2,mldr2,mladr2,mldlr2,mladlr2,mldladr2,mladldr2,mldl2,mladl2,addss2,ssadd2,trafo2,incl2,
>     combine2,lcombine2,lcombine2',rcombine2,rcombine2',lrcombine2,acomb2,lacomb2,lacomb2',
>     racomb2,racomb2',lracomb2,h2,h_i2,h_l2,h_s2) = alg2 basearray takes

>    sadd b (a1,a2) = (sadd1 b a1, sadd2 b a2)
>    cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
>    cadd' (c1,c2) (a1,a2) = (cadd1' c1 a1, cadd2' c2 a2)
>    cadd'' (c1,c2) (a1,a2) = (cadd1'' c1 a1, cadd2'' c2 a2)
>    cadd''' (c1,c2) (a1,a2) = (cadd1''' c1 a1, cadd2''' c2 a2)
>    ambd (c1,c2) b (a1,a2) = (ambd1 c1 b a1, ambd2 c2 b a2)
>    ambd' (c1,c2) b (a1,a2) = (ambd1' c1 b a1, ambd2' c2 b a2)
>    nil a = (nil1 a, nil2 a)
>    nil' a = (nil1' a, nil2' a)
>    edl b (c1,c2) = (edl1 b c1, edl2 b c2)
>    edr (c1,c2) b = (edr1 c1 b, edr2 c2 b)
>    edlr b (c1,c2) b' = (edlr1 b c1 b', edlr2 b c2 b')
>    drem (c1,c2) = (drem1 c1, drem2 c2)
>    is (c1,c2)   = (is1 c1, is2 c2)
>    sr b (c1,c2) b' = (sr1 b c1 b', sr2 b c2 b')
>    hl b1 b2 u b2' b1' = (hl1 b1 b2 u b2' b1', hl2 b1 b2 u b2' b1')
>    hlChar b1 b2 u b2' b1' = (hlChar1 b1 b2 u b2' b1', hlChar2 b1 b2 u b2' b1')
>    sp b1 b2 (c1,c2) b2' b1' = (sp1 b1 b2 c1 b2' b1',sp2 b1 b2 c2 b2' b1') 
>    bl u (c1,c2) = (bl1 u c1, bl2 u c2)
>    br (c1,c2) u = (br1 c1 u, br2 c2 u)
>    il r1 (c1,c2) r2 = (il1 r1 c1 r2 ,il2 r1 c2 r2) 
>    ml b1 b2 (m1,m2) b2' b1' = (ml1 b1 b2 m1 b2' b1', ml2 b1 b2 m2 b2' b1')
>    mldr b1 b2 (m1,m2) d b2' b1' = (mldr1 b1 b2 m1 d b2' b1',
>                                    mldr2 b1 b2 m2 d b2' b1')
>    mladr b1 b2 (m1,m2) d b2' b1' = (mladr1 b1 b2 m1 d b2' b1',
>                                     mladr2 b1 b2 m2 d b2' b1')
>    mldlr b1 b2 d (m1,m2) d_ b2' b1' = (mldlr1 b1 b2 d m1 d_ b2' b1',
>                                        mldlr2 b1 b2 d m2 d_ b2' b1')
>    mladlr b1 b2 d (m1,m2) d_ b2' b1' = (mladlr1 b1 b2 d m1 d_ b2' b1',
>                                         mladlr2 b1 b2 d m2 d_ b2' b1')
>    mldladr b1 b2 d (m1,m2) d_ b2' b1' = (mldladr1 b1 b2 d m1 d_ b2' b1',
>                                          mldladr2 b1 b2 d m2 d_ b2' b1')
>    mladldr b1 b2 d (m1,m2) d_ b2' b1' = (mladldr1 b1 b2 d m1 d_ b2' b1',
>                                          mladldr2 b1 b2 d m2 d_ b2' b1')
>    mldl b1 b2 d (m1,m2) b2' b1' = (mldl1 b1 b2 d m1 b2' b1', mldl2 b1 b2 d m2 b2' b1')
>    mladl b1 b2 d (m1,m2) b2' b1' = (mladl1 b1 b2 d m1 b2' b1', mladl2 b1 b2 d m2 b2' b1')
>    addss (c1,c2) u = (addss1 c1 u, addss2 c2 u)
>    ssadd u (c1,c2) = (ssadd1 u c1, ssadd2 u c2)
>    trafo (c1,c2)= (trafo1 c1, trafo2 c2)
>    incl (c1,c2) = (incl1 c1, incl2 c2)
>    combine (c1,c2) (c_1,c_2) = (combine1 c1 c_1, combine2 c2 c_2)
>    lcombine (c1,c2) (c_1,c_2) = (lcombine1 c1 c_1, lcombine2 c2 c_2)
>    lcombine' (c1,c2) (c_1,c_2) = (lcombine1' c1 c_1, lcombine2' c2 c_2)
>    rcombine (c1,c2) (c_1,c_2) = (rcombine1 c1 c_1, rcombine2 c2 c_2)
>    rcombine' (c1,c2) (c_1,c_2) = (rcombine1' c1 c_1, rcombine2' c2 c_2)
>    lrcombine (c1,c2) (c_1,c_2) = (lrcombine1 c1 c_1, lrcombine2 c2 c_2)
>    acomb (c1,c2) b (c_1,c_2) = (acomb1 c1 b c_1, acomb2 c2 b c_2)
>    lacomb (c1,c2) b (c_1,c_2) = (lacomb1 c1 b c_1, lacomb2 c2 b c_2)
>    lacomb' (c1,c2) b (c_1,c_2) = (lacomb1' c1 b c_1, lacomb2' c2 b c_2)
>    racomb (c1,c2) b (c_1,c_2) = (racomb1 c1 b c_1, racomb2 c2 b c_2)
>    racomb' (c1,c2) b (c_1,c_2) = (racomb1' c1 b c_1, racomb2' c2 b c_2)
>    lracomb (c1,c2) b (c_1,c_2) = (lracomb1 c1 b c_1, lracomb2 c2 b c_2)
 
>    h xs   = [(x1,x2)| x1 <- nub $   h1 [ y1 | (y1,y2) <- xs],
>                       x2 <-         h2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>    h_i xs = [(x1,x2)| x1 <- nub $ h_i1 [ y1 | (y1,y2) <- xs],
>                       x2 <-       h_i2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>    h_l xs = [(x1,x2)| x1 <- nub $ h_l1 [ y1 | (y1,y2) <- xs],
>                       x2 <-       h_l2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>    h_s xs = [(x1,x2)| x1 <- nub $ h_s1 [ y1 | (y1,y2) <- xs],
>                       x2 <-       h_s2 [ y2 | (y1,y2) <- xs, y1 == x1]]
  
> infix *-*
> (alg1 *-* alg2) basearray takes =
 
>         (sadd,cadd,cadd',cadd'',cadd''',ambd,ambd',nil,nil',edl,edr,edlr,drem,is,sr,hl,hlChar,sp,bl,br,il,
>          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,trafo,incl,
>          combine,lcombine,lcombine',rcombine,rcombine',lrcombine,acomb,lacomb,lacomb',
>	   racomb,racomb',lracomb,h,h_i,h_l,h_s) where
>    (sadd1,cadd1,cadd1',cadd1'',cadd1''',ambd1,ambd1',nil1,nil1',edl1,edr1,edlr1,drem1,is1,sr1,hl1,hlChar1,sp1,bl1,br1,il1,
>     ml1,mldr1,mladr1,mldlr1,mladlr1,mldladr1,mladldr1,mldl1,mladl1,addss1,ssadd1,trafo1,incl1,
>     combine1,lcombine1,lcombine1',rcombine1,rcombine1',lrcombine1,acomb1,lacomb1,lacomb1',
>     racomb1,racomb1',lracomb1,h1,h_i1,h_l1,h_s1) = alg1 basearray takes
>    (sadd2,cadd2,cadd2',cadd2'',cadd2''',ambd2,ambd2',nil2,nil2',edl2,edr2,edlr2,drem2,is2,sr2,hl2,hlChar2,sp2,bl2,br2,il2,
>     ml2,mldr2,mladr2,mldlr2,mladlr2,mldladr2,mladldr2,mldl2,mladl2,addss2,ssadd2,trafo2,incl2,
>     combine2,lcombine2,lcombine2',rcombine2,rcombine2',lrcombine2,acomb2,lacomb2,lacomb2',
>     racomb2,racomb2',lracomb2,h2,h_i2,h_l2,h_s2) = alg2 basearray takes

>    sadd b (a1,a2) = (sadd1 b a1, sadd2 b a2)
>    cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
>    cadd' (c1,c2) (a1,a2) = (cadd1' c1 a1, cadd2' c2 a2)
>    cadd'' (c1,c2) (a1,a2) = (cadd1'' c1 a1, cadd2'' c2 a2)
>    cadd''' (c1,c2) (a1,a2) = (cadd1''' c1 a1, cadd2''' c2 a2)
>    ambd (c1,c2) b (a1,a2) = (ambd1 c1 b a1, ambd2 c2 b a2)
>    ambd' (c1,c2) b (a1,a2) = (ambd1' c1 b a1, ambd2' c2 b a2)
>    nil a = (nil1 a, nil2 a)
>    nil' a = (nil1' a, nil2' a)
>    edl b (c1,c2) = (edl1 b c1, edl2 b c2)
>    edr (c1,c2) b = (edr1 c1 b, edr2 c2 b)
>    edlr b (c1,c2) b' = (edlr1 b c1 b', edlr2 b c2 b')
>    drem (c1,c2) = (drem1 c1, drem2 c2)
>    is (c1,c2)   = (is1 c1, is2 c2)
>    sr b (c1,c2) b' = (sr1 b c1 b', sr2 b c2 b')
>    hl b1 b2 u b2' b1' = (hl1 b1 b2 u b2' b1', hl2 b1 b2 u b2' b1')
>    hlChar b1 b2 u b2' b1' = (hlChar1 b1 b2 u b2' b1', hlChar2 b1 b2 u b2' b1')
>    sp b1 b2 (c1,c2) b2' b1' = (sp1 b1 b2 c1 b2' b1',sp2 b1 b2 c2 b2' b1') 
>    bl u (c1,c2) = (bl1 u c1, bl2 u c2)
>    br (c1,c2) u = (br1 c1 u, br2 c2 u)
>    il r1 (c1,c2) r2 = (il1 r1 c1 r2 ,il2 r1 c2 r2) 
>    ml b1 b2 (m1,m2) b2' b1' = (ml1 b1 b2 m1 b2' b1', ml2 b1 b2 m2 b2' b1')
>    mldr b1 b2 (m1,m2) d b2' b1' = (mldr1 b1 b2 m1 d b2' b1',
>                                    mldr2 b1 b2 m2 d b2' b1')
>    mladr b1 b2 (m1,m2) d b2' b1' = (mladr1 b1 b2 m1 d b2' b1',
>                                     mladr2 b1 b2 m2 d b2' b1')
>    mldlr b1 b2 d (m1,m2) d_ b2' b1' = (mldlr1 b1 b2 d m1 d_ b2' b1',
>                                        mldlr2 b1 b2 d m2 d_ b2' b1')
>    mladlr b1 b2 d (m1,m2) d_ b2' b1' = (mladlr1 b1 b2 d m1 d_ b2' b1',
>                                         mladlr2 b1 b2 d m2 d_ b2' b1')
>    mldladr b1 b2 d (m1,m2) d_ b2' b1' = (mldladr1 b1 b2 d m1 d_ b2' b1',
>                                          mldladr2 b1 b2 d m2 d_ b2' b1')
>    mladldr b1 b2 d (m1,m2) d_ b2' b1' = (mladldr1 b1 b2 d m1 d_ b2' b1',
>                                          mladldr2 b1 b2 d m2 d_ b2' b1')
>    mldl b1 b2 d (m1,m2) b2' b1' = (mldl1 b1 b2 d m1 b2' b1', mldl2 b1 b2 d m2 b2' b1')
>    mladl b1 b2 d (m1,m2) b2' b1' = (mladl1 b1 b2 d m1 b2' b1', mladl2 b1 b2 d m2 b2' b1')
>    addss (c1,c2) u = (addss1 c1 u, addss2 c2 u)
>    ssadd u (c1,c2) = (ssadd1 u c1, ssadd2 u c2)
>    trafo (c1,c2) = (trafo1 c1, trafo2 c2)
>    incl (c1,c2) = (incl1 c1, incl2 c2)
>    combine (c1,c2) (c_1,c_2) = (combine1 c1 c_1, combine2 c2 c_2)
>    lcombine (c1,c2) (c_1,c_2) = (lcombine1 c1 c_1, lcombine2 c2 c_2)
>    lcombine' (c1,c2) (c_1,c_2) = (lcombine1' c1 c_1, lcombine2' c2 c_2)
>    rcombine (c1,c2) (c_1,c_2) = (rcombine1 c1 c_1, rcombine2 c2 c_2)
>    rcombine' (c1,c2) (c_1,c_2) = (rcombine1' c1 c_1, rcombine2' c2 c_2)
>    lrcombine (c1,c2) (c_1,c_2) = (lrcombine1 c1 c_1, lrcombine2 c2 c_2)
>    acomb (c1,c2) b (c_1,c_2) = (acomb1 c1 b c_1, acomb2 c2 b c_2)
>    lacomb (c1,c2) b (c_1,c_2) = (lacomb1 c1 b c_1, lacomb2 c2 b c_2)
>    lacomb' (c1,c2) b (c_1,c_2) = (lacomb1' c1 b c_1, lacomb2' c2 b c_2)
>    racomb (c1,c2) b (c_1,c_2) = (racomb1 c1 b c_1, racomb2 c2 b c_2)
>    racomb' (c1,c2) b (c_1,c_2) = (racomb1' c1 b c_1, racomb2' c2 b c_2)
>    lracomb (c1,c2) b (c_1,c_2) = (lracomb1 c1 b c_1, lracomb2 c2 b c_2)
>    h xs   = [(x1,x2)| x1 <- h1 [ y1 | (y1,y2) <- xs],
>                       x2 <- h2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>    h_i xs = [(x1,x2)| x1 <- h_i1 [ y1 | (y1,y2) <- xs],
>                       x2 <- h_i2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>    h_l xs = [(x1,x2)| x1 <- h_l1 [ y1 | (y1,y2) <- xs],
>                       x2 <- h_l2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>    h_s xs = [(x1,x2)| x1 <- h_s1 [ y1 | (y1,y2) <- xs],
>                       x2 <- h_s2 [ y2 | (y1,y2) <- xs, y1 == x1]]

Algebra cross product (sorting on 2nd argument):

> infix *+*
> (alg1 *+* alg2) basearray takes =
 
>         (sadd,cadd,cadd',cadd'',cadd''',ambd,ambd',nil,nil',edl,edr,edlr,drem,is,sr,hl,hlChar,sp,bl,br,il,
>          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,trafo,incl,
>          combine,lcombine,lcombine',rcombine,rcombine',lrcombine,acomb,lacomb,lacomb',
>	   racomb,racomb',lracomb,h,h_i,h_l,h_s) where
>    (sadd1,cadd1,cadd1',cadd1'',cadd1''',ambd1,ambd1',nil1,nil1',edl1,edr1,edlr1,drem1,is1,sr1,hl1,hlChar1,sp1,bl1,br1,il1,
>     ml1,mldr1,mladr1,mldlr1,mladlr1,mldladr1,mladldr1,mldl1,mladl1,addss1,ssadd1,trafo1,incl1,
>     combine1,lcombine1,lcombine1',rcombine1,rcombine1',lrcombine1,acomb1,lacomb1,lacomb1',
>     racomb1,racomb1',lracomb1,h1,h_i1,h_l1,h_s1) = alg1 basearray takes
>    (sadd2,cadd2,cadd2',cadd2'',cadd2''',ambd2,ambd2',nil2,nil2',edl2,edr2,edlr2,drem2,is2,sr2,hl2,hlChar2,sp2,bl2,br2,il2,
>     ml2,mldr2,mladr2,mldlr2,mladlr2,mldladr2,mladldr2,mldl2,mladl2,addss2,ssadd2,trafo2,incl2,
>     combine2,lcombine2,lcombine2',rcombine2,rcombine2',lrcombine2,acomb2,lacomb2,lacomb2',
>     racomb2,racomb2',lracomb2,h2,h_i2,h_l2,h_s2) = alg2 basearray takes

>    sadd b (a1,a2) = (sadd1 b a1, sadd2 b a2)
>    cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
>    cadd' (c1,c2) (a1,a2) = (cadd1' c1 a1, cadd2' c2 a2)
>    cadd'' (c1,c2) (a1,a2) = (cadd1'' c1 a1, cadd2'' c2 a2)
>    cadd''' (c1,c2) (a1,a2) = (cadd1''' c1 a1, cadd2''' c2 a2)
>    ambd (c1,c2) b (a1,a2) = (ambd1 c1 b a1, ambd2 c2 b a2)
>    ambd' (c1,c2) b (a1,a2) = (ambd1' c1 b a1, ambd2' c2 b a2)
>    nil a = (nil1 a, nil2 a)
>    nil' a = (nil1' a, nil2' a)
>    edl b (c1,c2) = (edl1 b c1, edl2 b c2)
>    edr (c1,c2) b = (edr1 c1 b, edr2 c2 b)
>    edlr b (c1,c2) b' = (edlr1 b c1 b', edlr2 b c2 b')
>    drem (c1,c2) = (drem1 c1, drem2 c2)
>    is (c1,c2)   = (is1 c1, is2 c2)
>    sr b (c1,c2) b' = (sr1 b c1 b', sr2 b c2 b')
>    hl b1 b2 u b2' b1' = (hl1 b1 b2 u b2' b1', hl2 b1 b2 u b2' b1')
>    hlChar b1 b2 u b2' b1' = (hlChar1 b1 b2 u b2' b1', hlChar2 b1 b2 u b2' b1')
>    sp b1 b2 (c1,c2) b2' b1' = (sp1 b1 b2 c1 b2' b1',sp2 b1 b2 c2 b2' b1') 
>    bl u (c1,c2) = (bl1 u c1, bl2 u c2)
>    br (c1,c2) u = (br1 c1 u, br2 c2 u)
>    il r1 (c1,c2) r2 = (il1 r1 c1 r2 ,il2 r1 c2 r2) 
>    ml b1 b2 (m1,m2) b2' b1' = (ml1 b1 b2 m1 b2' b1', ml2 b1 b2 m2 b2' b1')
>    mldr b1 b2 (m1,m2) d b2' b1' = (mldr1 b1 b2 m1 d b2' b1',
>                                    mldr2 b1 b2 m2 d b2' b1')
>    mladr b1 b2 (m1,m2) d b2' b1' = (mladr1 b1 b2 m1 d b2' b1',
>                                     mladr2 b1 b2 m2 d b2' b1')
>    mldlr b1 b2 d (m1,m2) d_ b2' b1' = (mldlr1 b1 b2 d m1 d_ b2' b1',
>                                        mldlr2 b1 b2 d m2 d_ b2' b1')
>    mladlr b1 b2 d (m1,m2) d_ b2' b1' = (mladlr1 b1 b2 d m1 d_ b2' b1',
>                                         mladlr2 b1 b2 d m2 d_ b2' b1')
>    mldladr b1 b2 d (m1,m2) d_ b2' b1' = (mldladr1 b1 b2 d m1 d_ b2' b1',
>                                          mldladr2 b1 b2 d m2 d_ b2' b1')
>    mladldr b1 b2 d (m1,m2) d_ b2' b1' = (mladldr1 b1 b2 d m1 d_ b2' b1',
>                                          mladldr2 b1 b2 d m2 d_ b2' b1')
>    mldl b1 b2 d (m1,m2) b2' b1' = (mldl1 b1 b2 d m1 b2' b1', mldl2 b1 b2 d m2 b2' b1')
>    mladl b1 b2 d (m1,m2) b2' b1' = (mladl1 b1 b2 d m1 b2' b1', mladl2 b1 b2 d m2 b2' b1')
>    addss (c1,c2) u = (addss1 c1 u, addss2 c2 u)
>    ssadd u (c1,c2) = (ssadd1 u c1, ssadd2 u c2)
>    trafo (c1,c2) = (trafo1 c1, trafo2 c2)
>    incl (c1,c2) = (incl1 c1, incl2 c2)
>    combine (c1,c2) (c_1,c_2) = (combine1 c1 c_1, combine2 c2 c_2)
>    lcombine (c1,c2) (c_1,c_2) = (lcombine1 c1 c_1, lcombine2 c2 c_2)
>    lcombine' (c1,c2) (c_1,c_2) = (lcombine1' c1 c_1, lcombine2' c2 c_2)
>    rcombine (c1,c2) (c_1,c_2) = (rcombine1 c1 c_1, rcombine2 c2 c_2)
>    rcombine' (c1,c2) (c_1,c_2) = (rcombine1' c1 c_1, rcombine2' c2 c_2)
>    lrcombine (c1,c2) (c_1,c_2) = (lrcombine1 c1 c_1, lrcombine2 c2 c_2)
>    acomb (c1,c2) b (c_1,c_2) = (acomb1 c1 b c_1, acomb2 c2 b c_2)
>    lacomb (c1,c2) b (c_1,c_2) = (lacomb1 c1 b c_1, lacomb2 c2 b c_2)
>    lacomb' (c1,c2) b (c_1,c_2) = (lacomb1' c1 b c_1, lacomb2' c2 b c_2)
>    racomb (c1,c2) b (c_1,c_2) = (racomb1 c1 b c_1, racomb2 c2 b c_2)
>    racomb' (c1,c2) b (c_1,c_2) = (racomb1' c1 b c_1, racomb2' c2 b c_2)
>    lracomb (c1,c2) b (c_1,c_2) = (lracomb1 c1 b c_1, lracomb2 c2 b c_2)
>    h xs   = sortIt [(x1,x2)| x1 <- nub $   h1 [ y1 | (y1,y2) <- xs],
>                              x2 <-         h2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>      where sortIt = sortBy (\b -> \a -> compare (snd a) (snd b))
>    h_i xs = sortIt [(x1,x2)| x1 <- nub $ h_i1 [ y1 | (y1,y2) <- xs],
>                              x2 <-       h_i2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>      where sortIt = sortBy (\b -> \a -> compare (snd a) (snd b))
>    h_l xs = sortIt [(x1,x2)| x1 <- nub $ h_l1 [ y1 | (y1,y2) <- xs],
>                              x2 <-       h_l2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>      where sortIt = sortBy (\b -> \a -> compare (snd a) (snd b))
>    h_s xs = sortIt [(x1,x2)| x1 <- nub $ h_s1 [ y1 | (y1,y2) <- xs],
>                              x2 <-       h_s2 [ y2 | (y1,y2) <- xs, y1 == x1]]
>      where sortIt = sortBy (\b -> \a -> compare (snd a) (snd b))
