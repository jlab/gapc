 	***************************************
	*	    An ADP version of 	      *	
	*     Zukers RNA folding algorithm    *
      	*       Canonical RNA Structures      *
	*          in O(n^3) time and         *
	*             O(n^2) space.	      *
	*************************************** 

Neue Grammatik, die sich komplett sparse machen laesst.
dangles are always added even  base is already consumed!

> module ADPfold_always_dangle where

> import Data.Array
> import System.Environment
> import Text.Printf

> import Foldingspace
> import RNACombinators
> import Algebras_ad


> usage = "ADPfold_ad -[h|p|mfe|c|pp|sample|sp_gen|sp_spe|sc] sequence erange\n\n"
>         ++"\t-h       shows this help\n"
>         ++"\t-p       computes the total partition function value Z\n"
>         ++"\t-mfe     computes minimum free energy structure\n"
>         ++"\t-c       counts the whole search space\n"
>         ++"\t-pp      shows all possible structres (Warning: Huge output)\n"
>         ++"\t-sample  samples a structure from the partition function\n"
>         ++"\t-smfe    normal shape folding requires the third argument erange\n"
>         ++"\t-sp_gen  computes accumulated shape probabilities with the generic classified DP combinator\n"
>         ++"\t-sp_spe  computes accumulated shape probabilities with the specialized classified DP combinator\n"  
>         ++"\t-sc      counts the size of each shape space\n\n"

> main ::  IO()
> main  = do
>	  [arg1,arg2,arg3] <- getArgs
>	  let input    = arg2
>             range ::Int
>             range = read arg3
>             z = head $ fold input pfunc
>	      result = case arg1 of
>                           "-h" -> usage
>                           "-p" ->     show z++"\n"
>                           "-mfe"    -> format $ head                $ fold input (mfe *** pp)
>                           "-c"      -> show   ( head                $ fold input count) ++"\n"
>                           "-pp"     -> foldr ((++).(++"\n")) [] $ fold input pp
>                           "-sampleold" -> concat $ map (formatsample z) (fold input (pfunc_sample_wrong *** pp))
>                           "-sampletest" -> show (fold input pfunc_sample)
>                           "-sample" -> concat $ map (formatsample' z) (fold input (pfunc_sample *$* ((mfe *** pp) *** (shapes 5))))
>                           "-sp_gen" -> concat $ map (formatsp z)     (fold input ((shapes 5) *#* pfunc))
>                           "-sp_spe" -> concat $ map (formatsp z)     (fold input ((shapes 5) *%* pfunc))
>                           "-smfe"   -> concat $ map format_shape   (fold input ((((shapes 5) /// energyalg) range) *** pp))
>                           "-smfe2"  -> concat $ map format_shape2  (fold input (((shapes 5) /// energyalg) range))
>                           "-sc"     -> concat $ map formatsc         (fold input ((shapes 5) *** count))
>                           otherwise -> usage in
>	      putStr (input ++"\n" ++ result)

> format::(Int,String) -> String
> format (e,p) =  p ++"  (" ++ show (fromIntegral e/100) ++ ")\n"

> formatsp::Double -> (String,Double) -> String
> formatsp z (shape,pf) =  printf "%.6f\t%s\n" (pf/z) shape

> formatsample::Double -> (Double,String) -> String
> formatsample z (pf, pp) =  printf "%.6f\t%s\n" (pf/z) pp

> formatsample'::Double -> ((Double,Double),((Int,String),String)) -> String
> formatsample' z ((pf, pfsum),((e,pp),shape)) =  printf "%s   %10s  %s  %.6f\n" pp shape (show (fromIntegral e/100)) (pf/z)  

> formatsc::(String, Integer) -> String
> formatsc (shp, num) = shp ++ "\t "++ show num ++"\n"

> format_shape:: ((String,Int),String) -> String
> format_shape ((shp, e),pp) =  printf "%s\t(%3.2f)\t%s\n" pp ((fromIntegral e::Float) /100) shp
> format_shape2:: (String,Int) -> String
> format_shape2 (shp, e) =  printf "%s \t(%3.2f)\n" shp((fromIntegral e::Float) /100) 



> fold :: [Char] -> (RNAInput -> FS_Algebra  Int a b) -> [b]
> fold sequence algebra = axiom struct where
>
>   tabulated		= table  n 
>   listed		= table1 n
>   n			= length sequence	
>   axiom		= axiom' n
>   inp			= mk (rna sequence)
>   basepair (i,j)      = basepair'  (inp,(i,j))
>   stackpair (i,j)     = stackpair' (inp,(i,j))
>   minloopsize m (i,j) = minloopsize' m (inp,(i,j))

>   (sadd, cadd, dlr, sr, hl, bl, br, il,ml,
>    append, ul, addss, ssadd, nil, h, h_l, h_s)= algebra inp
> 
>   struct        = listed (
>	            sadd <<< base   +~~ struct |||
>                   cadd <<< dangle ~~~ struct |||
>                   empty nil ... h_s)
>              where   
>    dangle      =   dlr <<< loc .~~ closed ~~. loc
>                where
>     closed      = tabulated (
>		     (stack ||| hairpin ||| leftB ||| rightB ||| iloop ||| multiloop) `with` stackpair ... h)
>                   
>                 where
>       stack     =  sr <<< base +~~ closed ~~+ base
>       hairpin   =  hl <<< base +~~ base ++~ (region `with` (minloopsize 3))~~+ base ~~+ base
>       leftB     = (bl <<< base +~~ base ++~  region !~~ closed             ~~+ base ~~+ base) ... h
>       rightB    = (br <<< base +~~ base ++~	          closed ~~!  region ~~+ base ~~+ base) ... h
>	iloop	  = (il	<<< base +~~ base ++~  region !~~ closed ~~! region ~~+ base ~~+ base) ... h
>       multiloop =  ml <<< base +~~ base ++~ 	          ml_comps           ~~+ base ~~+ base
>                    where
>	  ml_comps  =  tabulated (
>		       sadd   <<< base            +~~ ml_comps  |||
>		       append <<< (ul <<< dangle) ~~~ ml_comps1 ... h_l)			   
>		     where
>	       ml_comps1  =  tabulated (
>		       sadd   <<< base            +~~ ml_comps1 |||
>		       append <<< (ul <<< dangle) ~~~ ml_comps1 ||| 			   
>		                   ul <<< dangle                |||
>		       addss  <<< (ul <<< dangle) ~~~ region    ... h_l)

>   infixl 7 ~~!,!~~
>   (~~!) = (~~<) 30
>   (!~~) = (<~~) 32 

>   infixl 7 ~!~, ~!!~
>   ((~!~), (~!!~)) = combiner 30
	  
>   combiner step = ((~~!), (~~!!))
>         where
>		(~~!) :: Parser (b -> c) -> Parser b -> Int -> (Int,Int) ->  [c]
>		(~~!) p q l (i,j)  = [x y | k <- [1..(step-l)],
>					    x <- p (i,i+k), y <- q (i+k,j)]
>		(~~!!) :: (Int -> (Int, Int) -> [(b -> c)]) -> Parser b -> Parser c
>		(~~!!) q r (i,j)  = [y z | l<-[1..step],
>				           y <- (q l) (i,j-l), z <- r (j-l,j)]


