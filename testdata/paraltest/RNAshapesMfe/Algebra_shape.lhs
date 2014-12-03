> module RNAshapesMfe.Algebra_shape where

--###Berechnet alle moeglichen, verschiedenen Shapes (als Shapestring) des Suchraums. Da viele Kandidaten denselben Shapestring haben, werden hier Kandidaten zusammen gefasst.
--###[[Char]] mehrelementige Liste

> import RNAshapesMfe.AlgebraType
> import RNAshapesMfe.CommonFunctions

> import Data.Array
> import RNAshapesMfe.RnaI
> import Data.List(nub,sort,sortBy) 

Shape algebra:

> shape :: String -> String -> String -> String -> String -> String -> String -> String -> Array Int Ebase -> Float -> 
>                Canonical_Algebra Int (Int,Int) String String String

> shape edangle_op edangle_cl loop_ss loop_op loop_cl bulge_op bulge_cl singlestrand basearray takes  = 
>         (sadd,cadd,cadd',cadd'',cadd''',ambd,ambd',nil,nil',edl,edr,edlr,drem,is,sr,hl,hlChar,sp,bl,br,il,
>          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,
>	   ssadd,trafo,incl,combine,combine,combine,combine,combine,combine,
>	   acomb,acomb,acomb,acomb,acomb,acomb,h,h_i,h_l,h_s) where




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
>   hlChar _ _ _ _ _ = loop_op++loop_cl
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
>   acomb s1 b s2= app (app s1 singlestrand) s2
>   h   = nub
>   h_i = h
>   h_l = h
>   h_s = h

> shape1 = shape "" "" "_" "[" "]" "[" "]" "_"  -- all loops are represented different
> shape2 = shape "" "" "_" "[" "]" "[" "]" ""   -- bulges and internal loops have same representation
> shape3 = shape "" "" ""  "[" "]" "[" "]" ""    -- bulges and internal loops have same representation, no external loop
> shape4 = shape "" "" ""  "[" "]" ""  "" ""     -- edangles (complete substructures) and external loop contribute
> shape5 = shape "[" "]" "" "" "" "" "" ""      -- only edangles contribute
