> module RNAshapesMfe.Energy where
> import Data.Array
> import Numeric
> import RNAshapesMfe.RnaI
> import ADPTriCombinators
> import RNAshapesMfe.Intloop
> import RNAshapesMfe.Intloop21
> import RNAshapesMfe.Intloop22

This is a slightly different version compared to the original RNAshapes version: The energy for left and right bulged is calculated in a different way, also the grammar has changed, the inner structural motif is formed via the rule "closed" instead of "initstem"

Some constants & utilities

> e :: Float
> e = 2.718281828459

0 Degrees Celsius in Kelvin.

> t :: Float
> t = 273.1

The Temperature we work with.

> temp :: Float
> temp = t + 37.0

Universal Gas Constant2

> r :: Float
> r = 8.3143

Convert Celsius  degrees to Kelvin degrees.

> kelvin :: Float -> Float
> kelvin cels = t + cels

> log_interp :: (Integral a, Floating b) => a -> b
> log_interp size = 107.856 * logBase 2.71828 ((fromIntegral size) / 30.0)

The weighting parameter for pseudoknots

> wkn :: Float
> wkn = 0.83

Not paired base in a pseudoknot

> npp :: Float 
> npp = 20

Basepair in a pseudoknot

> pbp :: Float
> pbp =  10 * wkn
> pkinit:: Float
> pkinit = 700

-------------------- Stacking Energies ----------------------------
Stabilizing energies for canonical basepairs: AU, CG, GU
Basepairing: Parameters are in 5' 3' order.
stack_dg a b c d
         ^ ^ ^ ^
         | |_| |
         |_____|

> stack_dg :: Fractional a => Ebase -> Ebase -> Ebase -> Ebase -> a

> stack_dg C G C G =  -240
> stack_dg C C G G =  -330
> stack_dg C U G G =  -210
> stack_dg C G U G =  -140
> stack_dg C U A G =  -210
> stack_dg C A U G =  -210
> stack_dg G G C C =  -330
> stack_dg G C G C =  -340
> stack_dg G U G C =  -250
> stack_dg G G U C =  -150
> stack_dg G U A C =  -220
> stack_dg G A U C =  -240
> stack_dg G G C U =  -210
> stack_dg G C G U =  -250
> stack_dg G U G U =   130
> stack_dg G G U U =   -50
> stack_dg G U A U =  -140
> stack_dg G A U U =  -130
> stack_dg U G C G =  -140
> stack_dg U C G G =  -150
> stack_dg U U G G =   -50
> stack_dg U G U G =    30
> stack_dg U U A G =   -60
> stack_dg U A U G =  -100
> stack_dg A G C U =  -210
> stack_dg A C G U =  -220
> stack_dg A U G U =  -140
> stack_dg A G U U =   -60
> stack_dg A U A U =  -110
> stack_dg A A U U =   -90
> stack_dg U G C A =  -210
> stack_dg U C G A =  -240
> stack_dg U U G A =  -130
> stack_dg U G U A =  -100
> stack_dg U U A A =   -90
> stack_dg U A U A =  -130
> stack_dg a b c d = error "stack_dg: not in table"

> sr_energy :: Fractional a => RNAInput -> Region -> a
> sr_energy seq (i,j) = stack_dg (seq!i) (seq!(i+1)) (seq!(j-1)) (seq!j)

------------------ Hairpin Loop Energies --------------------------

1. Entropic Term

DESTABILIZING ENERGIES BY SIZE OF LOOP (Hairpin)

> hl_ent :: Floating b => Int -> b

> hl_ent  3 = 570
> hl_ent  4 = 560
> hl_ent  5 = 560
> hl_ent  6 = 540
> hl_ent  7 = 590
> hl_ent  8 = 560
> hl_ent  9 = 640
> hl_ent  10 = 650
> hl_ent  11 = 660
> hl_ent  12 = 670
> hl_ent  13 = 678
> hl_ent  14 = 686
> hl_ent  15 = 694
> hl_ent  16 = 701
> hl_ent  17 = 707
> hl_ent  18 = 713
> hl_ent  19 = 719
> hl_ent  20 = 725
> hl_ent  21 = 730
> hl_ent  22 = 735
> hl_ent  23 = 740
> hl_ent  24 = 744
> hl_ent  25 = 749
> hl_ent  26 = 753
> hl_ent  27 = 757
> hl_ent  28 = 761
> hl_ent  29 = 765
> hl_ent  30 = 769

> hl_ent size = if size < 3
>		then error "hl_ent: size < 3"
>		else hl_ent 30 + log_interp size


2. Stacking Interaction

> tstackh_dg :: Fractional a => Ebase -> Ebase -> Ebase -> Ebase -> a

> tstackh_dg C A A G = -150
> tstackh_dg C A C G = -150
> tstackh_dg C A G G = -140
> tstackh_dg C A U G = -180
> tstackh_dg C C A G = -100
> tstackh_dg C C C G =  -90
> tstackh_dg C C G G = -290
> tstackh_dg C C U G =  -80
> tstackh_dg C G A G = -220
> tstackh_dg C G C G = -200
> tstackh_dg C G G G = -160
> tstackh_dg C G U G = -110
> tstackh_dg C U A G = -170
> tstackh_dg C U C G = -140
> tstackh_dg C U G G = -180
> tstackh_dg C U U G = -200
> tstackh_dg G A A C = -110
> tstackh_dg G A C C = -150
> tstackh_dg G A G C = -130
> tstackh_dg G A U C = -210
> tstackh_dg G C A C = -110
> tstackh_dg G C C C =  -70
> tstackh_dg G C G C = -240
> tstackh_dg G C U C =  -50
> tstackh_dg G G A C = -240
> tstackh_dg G G C C = -290
> tstackh_dg G G G C = -140
> tstackh_dg G G U C = -120
> tstackh_dg G U A C = -190
> tstackh_dg G U C C = -100
> tstackh_dg G U G C = -220
> tstackh_dg G U U C = -150
> tstackh_dg G A A U =   20
> tstackh_dg G A C U =  -50
> tstackh_dg G A G U =  -30
> tstackh_dg G A U U =  -30
> tstackh_dg G C A U =  -10
> tstackh_dg G C C U =  -20
> tstackh_dg G C G U = -150
> tstackh_dg G C U U =  -20
> tstackh_dg G G A U =  -90
> tstackh_dg G G C U = -110
> tstackh_dg G G G U =  -30
> tstackh_dg G G U U =    0
> tstackh_dg G U A U =  -30
> tstackh_dg G U C U =  -30
> tstackh_dg G U G U =  -40
> tstackh_dg G U U U = -110
> tstackh_dg U A A G =  -50
> tstackh_dg U A C G =  -30
> tstackh_dg U A G G =  -60
> tstackh_dg U A U G =  -50
> tstackh_dg U C A G =  -20
> tstackh_dg U C C G =  -10
> tstackh_dg U C G G = -170
> tstackh_dg U C U G =    0
> tstackh_dg U G A G =  -80
> tstackh_dg U G C G = -120
> tstackh_dg U G G G =  -30
> tstackh_dg U G U G =  -70
> tstackh_dg U U A G =  -60
> tstackh_dg U U C G =  -10
> tstackh_dg U U G G =  -60
> tstackh_dg U U U G =  -80
> tstackh_dg A A A U =  -30
> tstackh_dg A A C U =  -50
> tstackh_dg A A G U =  -30
> tstackh_dg A A U U =  -30
> tstackh_dg A C A U =  -10
> tstackh_dg A C C U =  -20
> tstackh_dg A C G U = -150
> tstackh_dg A C U U =  -20
> tstackh_dg A G A U = -110
> tstackh_dg A G C U = -120
> tstackh_dg A G G U =  -20
> tstackh_dg A G U U =   20
> tstackh_dg A U A U =  -30
> tstackh_dg A U C U =  -30
> tstackh_dg A U G U =  -60
> tstackh_dg A U U U = -110
> tstackh_dg U A A A =  -50
> tstackh_dg U A C A =  -30
> tstackh_dg U A G A =  -60
> tstackh_dg U A U A =  -50
> tstackh_dg U C A A =  -20
> tstackh_dg U C C A =  -10
> tstackh_dg U C G A = -120
> tstackh_dg U C U A =    0
> tstackh_dg U G A A = -140
> tstackh_dg U G C A = -120
> tstackh_dg U G G A =  -70
> tstackh_dg U G U A =  -20
> tstackh_dg U U A A =  -30
> tstackh_dg U U C A =  -10
> tstackh_dg U U G A =  -50
> tstackh_dg U U U A =  -80
> tstackh_dg a b c d =   20  -- the maximal value

error "tstackh_dg: not in table"

> hl_stack :: Fractional a => RNAInput -> Region -> a
> hl_stack seq (i,j) = tstackh_dg (seq!i) (seq!(i+1)) (seq!(j-1)) (seq!j)

3. Tetraloop Bonus Energies

Ultrastable tetra-loops & energy bonus at 37 °C:

> hl_tetra :: Fractional a => [Ebase] -> a

> hl_tetra [G,G,G,G,A,C] = -300
> hl_tetra [G,G,U,G,A,C] = -300
> hl_tetra [C,G,A,A,A,G] = -300
> hl_tetra [G,G,A,G,A,C] = -300
> hl_tetra [C,G,C,A,A,G] = -300
> hl_tetra [G,G,A,A,A,C] = -300
> hl_tetra [C,G,G,A,A,G] = -300
> hl_tetra [C,U,U,C,G,G] = -300
> hl_tetra [C,G,U,G,A,G] = -300
> hl_tetra [C,G,A,A,G,G] = -250
> hl_tetra [C,U,A,C,G,G] = -250
> hl_tetra [G,G,C,A,A,C] = -250
> hl_tetra [C,G,C,G,A,G] = -250
> hl_tetra [U,G,A,G,A,G] = -250
> hl_tetra [C,G,A,G,A,G] = -200
> hl_tetra [A,G,A,A,A,U] = -200
> hl_tetra [C,G,U,A,A,G] = -200
> hl_tetra [C,U,A,A,C,G] = -200
> hl_tetra [U,G,A,A,A,G] = -200
> hl_tetra [G,G,A,A,G,C] = -150
> hl_tetra [G,G,G,A,A,C] = -150
> hl_tetra [U,G,A,A,A,A] = -150
> hl_tetra [A,G,C,A,A,U] = -150
> hl_tetra [A,G,U,A,A,U] = -150
> hl_tetra [C,G,G,G,A,G] = -150
> hl_tetra [A,G,U,G,A,U] = -150
> hl_tetra [G,G,C,G,A,C] = -150
> hl_tetra [G,G,G,A,G,C] = -150
> hl_tetra [G,U,G,A,A,C] = -150
> hl_tetra [U,G,G,A,A,A] = -150
> hl_tetra _ = 0

> inpregion :: RNAInput -> Region -> [Ebase]
> inpregion seq (i,j) = [ seq!k | k <- [i+1 .. j]]

> hl_energy :: Floating a => RNAInput -> Region -> a


 Terminal AU penalty is included in hl_stack, therefor it must be added explicitely only for (size == 3)

> hl_energy seq (i,j) | size == 3 = entropy + termaupen                  
>		      | size == 4 = entropy + stack_mismatch + tetra_bonus
>		      | size  > 4 = entropy + stack_mismatch
>		      | otherwise = error "hl_energy: size < 3"
>		      where
>    size	      = j - i - 1
>    entropy	      = hl_ent size
>    stack_mismatch   = hl_stack seq (i,j)
>    tetra_bonus      = (hl_tetra . inpregion seq) (i-1,j)
>    termaupen	      = termaupenalty (seq!(i)) (seq!(j))

---------------------- Bulge Loop Energies --------------------------

> bl_ent :: Floating b => Int -> b

> bl_ent  1 = 380
> bl_ent  2 = 280
> bl_ent  3 = 320
> bl_ent  4 = 360
> bl_ent  5 = 400
> bl_ent  6 = 440
> bl_ent  7 = 459
> bl_ent  8 = 470
> bl_ent  9 = 480
> bl_ent  10 = 490
> bl_ent  11 = 500
> bl_ent  12 = 510
> bl_ent  13 = 519
> bl_ent  14 = 527
> bl_ent  15 = 534
> bl_ent  16 = 541
> bl_ent  17 = 548
> bl_ent  18 = 554
> bl_ent  19 = 560
> bl_ent  20 = 565
> bl_ent  21 = 571
> bl_ent  22 = 576
> bl_ent  23 = 580
> bl_ent  24 = 585
> bl_ent  25 = 589
> bl_ent  26 = 594
> bl_ent  27 = 598
> bl_ent  28 = 602
> bl_ent  29 = 605
> bl_ent  30 = 609

> bl_ent size = if size < 1 
>		then error "bl_ent: size < 1"
>		else bl_ent 30 + log_interp size


------------------------ Bulge Loop Left ----------------------------
      											  .        .
      											  .        .
      											  .        .
										       (bl+3) - (br-2)   
 If size == 1 the terminal aupenalty for the stem starting after the bulge (that is    (bl+2) - (br-1)) 
										    bl+1
										          bl  -   br			   

 is added possibly. This is unwanted. Since we do not have a chance to check the size of the bulge when parsing the stem
 we substract the possible penalty here! 

> bl_energy :: Floating a => RNAInput -> Int -> Region -> Int -> a
> bl_energy seq bl (i,j) br | size == 1 = entropy + stacking -- - termaupenalty (seq!(bl+2)) (seq!(br-1))
>                       | size  > 1 = entropy + termaupenalty (seq!bl)    (seq!br) + termaupenalty (seq!(j+1)) (seq!(br-1))
>                       | otherwise = error "bl_energy size < 1" 
>                       where
>              stacking = stack_dg (seq!bl) (seq!(j+1)) (seq!(br-1)) (seq!br)
>              size     = sizeof (i,j)
>              entropy  = bl_ent size

----------------------- Bulge Loop Right ----------------------------

> br_energy :: Floating a => RNAInput -> Int -> Region -> Int -> a

> br_energy seq bl (i,j) br  | size == 1 = stacking + entropy -- - termaupenalty (seq!(bl+1)) (seq!(br-2))
>                        | size  > 1 = entropy + termaupenalty (seq!bl)     (seq!br) + termaupenalty (seq!(bl+1)) (seq!i)
>                        | otherwise = error "br_energy size < 1"
>                        where
>               stacking = stack_dg (seq!bl) (seq!(bl+1)) (seq!i) (seq!br)
>               size     = sizeof (i,j)
>               entropy  = bl_ent size


-------------------- Interior Loop Energies -------------------------

1. Entropic Term

DESTABILIZING ENERGIES BY SIZE OF LOOP

il_ent 1 and 2 undefined in the tables of Mathews et al. since
special energy values exist

> il_ent :: Floating b => Int -> b
> il_ent  2 = 150 
> il_ent  3 = 160
> il_ent  4 = 170
> il_ent  5 = 180
> il_ent  6 = 200
> il_ent  7 = 220
> il_ent  8 = 230
> il_ent  9 = 240
> il_ent  10 = 250
> il_ent  11 = 260
> il_ent  12 = 270
> il_ent  13 = 278
> il_ent  14 = 286
> il_ent  15 = 294
> il_ent  16 = 301
> il_ent  17 = 307
> il_ent  18 = 313
> il_ent  19 = 319
> il_ent  20 = 325
> il_ent  21 = 330
> il_ent  22 = 335
> il_ent  23 = 340
> il_ent  24 = 345
> il_ent  25 = 349
> il_ent  26 = 353
> il_ent  27 = 357
> il_ent  28 = 361
> il_ent  29 = 365
> il_ent  30 = 369

> il_ent size = if size < 2 
>		then error "il_ent: size < 2"
>		else il_ent 30 + log_interp size

2. Stacking Interaction

STACKING ENERGIES : TERMINAL MISMATCHES AND BASE-PAIRS.

Stabilizing energies for canonical basepairs: AU, CG, GU
Basepairing: Paramers are in 5' 3' order.
tstacki_dg a b c d
           ^ ^ ^ ^
           | |_| |
           |_____|

> tstacki_dg :: Fractional a => Ebase -> Ebase -> Ebase -> Ebase -> a


> tstacki_dg C A A G =    0
> tstacki_dg C A C G =    0
> tstacki_dg C A G G = -110
> tstacki_dg C A U G =    0
> tstacki_dg C C A G =    0
> tstacki_dg C C C G =    0
> tstacki_dg C C G G =    0
> tstacki_dg C C U G =    0
> tstacki_dg C G A G = -110
> tstacki_dg C G C G =    0
> tstacki_dg C G G G =    0
> tstacki_dg C G U G =    0
> tstacki_dg C U A G =    0
> tstacki_dg C U C G =    0
> tstacki_dg C U G G =    0
> tstacki_dg C U U G =  -70
> tstacki_dg G A A C =    0
> tstacki_dg G A C C =    0
> tstacki_dg G A G C = -110
> tstacki_dg G A U C =    0
> tstacki_dg G C A C =    0
> tstacki_dg G C C C =    0
> tstacki_dg G C G C =    0
> tstacki_dg G C U C =    0
> tstacki_dg G G A C = -110
> tstacki_dg G G C C =    0
> tstacki_dg G G G C =    0
> tstacki_dg G G U C =    0
> tstacki_dg G U A C =    0
> tstacki_dg G U C C =    0
> tstacki_dg G U G C =    0
> tstacki_dg G U U C =  -70
> tstacki_dg G A A U =   70
> tstacki_dg G A C U =   70
> tstacki_dg G A G U =  -40
> tstacki_dg G A U U =   70
> tstacki_dg G C A U =   70
> tstacki_dg G C C U =   70
> tstacki_dg G C G U =   70
> tstacki_dg G C U U =   70
> tstacki_dg G G A U =  -40
> tstacki_dg G G C U =   70
> tstacki_dg G G G U =   70
> tstacki_dg G G U U =   70
> tstacki_dg G U A U =   70
> tstacki_dg G U C U =   70
> tstacki_dg G U G U =   70
> tstacki_dg G U U U =    0
> tstacki_dg U A A G =   70
> tstacki_dg U A C G =   70
> tstacki_dg U A G G =  -40
> tstacki_dg U A U G =   70
> tstacki_dg U C A G =   70
> tstacki_dg U C C G =   70
> tstacki_dg U C G G =   70
> tstacki_dg U C U G =   70
> tstacki_dg U G A G =  -40
> tstacki_dg U G C G =   70
> tstacki_dg U G G G =   70
> tstacki_dg U G U G =   70
> tstacki_dg U U A G =   70
> tstacki_dg U U C G =   70
> tstacki_dg U U G G =   70
> tstacki_dg U U U G =    0
> tstacki_dg A A A U =   70
> tstacki_dg A A C U =   70
> tstacki_dg A A G U =  -40
> tstacki_dg A A U U =   70
> tstacki_dg A C A U =   70
> tstacki_dg A C C U =   70
> tstacki_dg A C G U =   70
> tstacki_dg A C U U =   70
> tstacki_dg A G A U =  -40
> tstacki_dg A G C U =   70
> tstacki_dg A G G U =   70
> tstacki_dg A G U U =   70
> tstacki_dg A U A U =   70
> tstacki_dg A U C U =   70
> tstacki_dg A U G U =   70
> tstacki_dg A U U U =    0
> tstacki_dg U A A A =   70
> tstacki_dg U A C A =   70
> tstacki_dg U A G A =  -40
> tstacki_dg U A U A =   70
> tstacki_dg U C A A =   70
> tstacki_dg U C C A =   70
> tstacki_dg U C G A =   70
> tstacki_dg U C U A =   70
> tstacki_dg U G A A =  -40
> tstacki_dg U G C A =   70
> tstacki_dg U G G A =   70
> tstacki_dg U G U A =   70
> tstacki_dg U U A A =   70
> tstacki_dg U U C A =   70
> tstacki_dg U U G A =   70
> tstacki_dg U U U A =    0
> tstacki_dg _ _ _ _ =   70 

error "tstacki_dg: not in table" 



the time intensive n^4 version of internal loops
(used in reduced form O(n^2*c^2) where c is the maximal internal loop size)

(i,j) = left region, (k,l) = right region

        i --- l+1
 5'    /        \    3'
 |   i+1         l  / \
 |    |          |   |
\ /   |          |   |
 3'   |          |   5'
      j         k+1
       \        /
        j+1 --- k


> il_stack :: Fractional a => RNAInput -> Region -> Region -> a

> il_stack seq (i,j) (k,l) = tstacki_dg (seq!i) (seq!(i+1)) (seq!l) (seq!(l+1))
>                      + tstacki_dg (seq!(j+1)) (seq!j) (seq!(k+1)) (seq!k)

Ninio's equation

> il_asym :: (Fractional a, Ord a, Integral b, Integral c) => b -> c -> a

> il_asym sl sr = min 300 (diff * 50)
>                 where diff = abs ((fromIntegral sl) - (fromIntegral sr))

> il_energy :: (Floating a, Ord a) => RNAInput -> Region -> Region -> a

> il_energy seq (i,j) (k,l)
>	    | sl ==1 && sr ==1 = il11_energy seq i (l+1) 
>	    | sl ==1 && sr ==2 = il12_energy seq i (l+1) 
>	    | sl ==2 && sr ==1 = il21_energy seq i (l+1)
>	    | sl ==2 && sr ==2 = fromIntegral(il22_energy seq i (l+1))
>	    | otherwise	=    (il_ent (sl + sr))
>                         + (il_stack seq (i,j) (k,l))
>                         + (il_asym sl sr)
>                         where sl = sizeof (i,j)
>                               sr = sizeof (k,l)





Lyngso's decomposition

> top_stack :: Fractional a => RNAInput -> Int -> Int -> a
> top_stack seq lb rb = tstacki_dg (seq!lb) (seq!(lb+1)) (seq!(rb-1)) (seq!rb)

> bot_stack :: Fractional a => RNAInput -> Int -> Int -> a
> bot_stack seq lb rb = tstacki_dg (seq!(lb+1)) (seq!lb) (seq!rb) (seq!(rb-1))

/* Ninio-correction for asymmetric internal loops with branches n1 and n2 */
/*    ninio_energy = min{max_ninio, |n1-n2|*F_ninio[min{4.0, n1, n2}] } */
PUBLIC int         MAX_NINIO = 300;                   /* maximum correction */
PUBLIC int F_ninio37[5] = { 0, 40, 50, 20, 10 };      /* only F[2] used */

> asym :: (Ord a, Integral b, Fractional a) => b -> a
> asym a = min 300 ((fromIntegral a) * 50)

<--------------------------- Dangling Ends --------------------------------

  lb x rb

-------- dangle right --------

> dr_dangle_dg :: Fractional a => (Ebase,Ebase) -> Ebase -> a
> dr_dangle_dg (C,G) A = -110
> dr_dangle_dg (C,G) C =  -40
> dr_dangle_dg (C,G) G = -130
> dr_dangle_dg (C,G) U =  -60
> dr_dangle_dg (C,G) N =  -40

> dr_dangle_dg (G,C) A = -170
> dr_dangle_dg (G,C) C =  -80
> dr_dangle_dg (G,C) G = -170
> dr_dangle_dg (G,C) U = -120
> dr_dangle_dg (G,C) N =  -80

> dr_dangle_dg (G,U) A =  -70
> dr_dangle_dg (G,U) C =  -10
> dr_dangle_dg (G,U) G =  -70
> dr_dangle_dg (G,U) U =  -10
> dr_dangle_dg (G,U) N =  -10

> dr_dangle_dg (U,G) A =  -80
> dr_dangle_dg (U,G) C =  -50
> dr_dangle_dg (U,G) G =  -80
> dr_dangle_dg (U,G) U =  -60
> dr_dangle_dg (U,G) N =  -50

> dr_dangle_dg (A,U) A =  -70
> dr_dangle_dg (A,U) C =  -10
> dr_dangle_dg (A,U) G =  -70
> dr_dangle_dg (A,U) U =  -10
> dr_dangle_dg (A,U) N =  -10

> dr_dangle_dg (U,A) A =  -80
> dr_dangle_dg (U,A) C =  -50
> dr_dangle_dg (U,A) G =  -80
> dr_dangle_dg (U,A) U =  -60
> dr_dangle_dg (U,A) N =  -50

> dr_dangle_dg a b     =    error "dr_dangle_dg: not in table"


> dr_energy :: Fractional a => RNAInput -> Region -> a
> dr_energy seq (i,j) = dr_dangle_dg ((seq!i),(seq!j)) (seq!(j+1))

> dli_energy :: Fractional a => RNAInput -> Region -> a
> dli_energy seq (i,j) = dr_dangle_dg ((seq!j),(seq!i)) (seq!(i+1))

-------- dangle left --------

> dl_dangle_dg :: Fractional a => Ebase -> (Ebase,Ebase) -> a
> dl_dangle_dg A (C,G) =  -50
> dl_dangle_dg C (C,G) =  -30
> dl_dangle_dg G (C,G) =  -20
> dl_dangle_dg U (C,G) =  -10
> dl_dangle_dg N (C,G) =  -10

> dl_dangle_dg A (G,C) =  -20
> dl_dangle_dg C (G,C) =  -30
> dl_dangle_dg G (G,C) =    0
> dl_dangle_dg U (G,C) =    0
> dl_dangle_dg N (G,C) =    0

> dl_dangle_dg A (G,U) =  -30
> dl_dangle_dg C (G,U) =  -30
> dl_dangle_dg G (G,U) =  -40
> dl_dangle_dg U (G,U) =  -20
> dl_dangle_dg N (G,U) =  -20

> dl_dangle_dg A (U,G) =  -30
> dl_dangle_dg C (U,G) =  -10
> dl_dangle_dg G (U,G) =  -20
> dl_dangle_dg U (U,G) =  -20
> dl_dangle_dg N (U,G) =  -10

> dl_dangle_dg A (A,U) =  -30
> dl_dangle_dg C (A,U) =  -30
> dl_dangle_dg G (A,U) =  -40
> dl_dangle_dg U (A,U) =  -20
> dl_dangle_dg N (A,U) =  -20

> dl_dangle_dg A (U,A) =  -30
> dl_dangle_dg C (U,A) =  -10
> dl_dangle_dg G (U,A) =  -20
> dl_dangle_dg U (U,A) =  -20
> dl_dangle_dg N (U,A) =  -10

> dl_dangle_dg _ _ =  error "dl_dangle_dg: not in table"

> dl_energy :: Fractional a => RNAInput -> Region -> a
> dl_energy seq (i,j) = dl_dangle_dg (seq!(i-1)) ((seq!i),(seq!j))

> dri_energy :: Fractional a => RNAInput -> Region -> a
> dri_energy seq (i,j) = dl_dangle_dg (seq!(j-1)) ((seq!j),(seq!i))


------------------ Multi-branched Loop Energies -------------------------

E = a + <# single-stranded bases> * b + <# helices> * c
offset = a,  free base penalty = b,  helix penalty = c
      4.6         0.4                0.1

 ml_energy comps = sum (4.6 : energies)
                    where energies = [energy|(energy,comp) <- comps]

 en (SS _ x) = 0.4 * fromIntegral (sizeof x)

 en (SS x) = 0.4 * fromIntegral (sizeof x)
 en closed = 0.1

 struct_energy comps = sum energies
                    where energies = [energy|(energy,comp) <- comps]

> ss_energy :: Fractional a => Region -> a

> ss_energy x = 0 -- 40 * fromIntegral (sizeof x)

----------------------------
special pseudoknot energies

 This are the dangling energies for the bases bridging the stacks

> dangles inp (i,j) (i',j') (k,l) (k',l') = (dli_energy inp (j,k+1) + dri_energy inp (j',k'+1)) * wkn
 
> sspenalty:: Int-> Float
> sspenalty a = npp *  fromIntegral a


> termaupenalty :: Fractional a => Ebase -> Ebase -> a

> termaupenalty A U = 50
> termaupenalty U A = 50
> termaupenalty G U = 50
> termaupenalty U G = 50
> termaupenalty G C = 0
> termaupenalty C G = 0
> termaupenalty x y = error ((show x)++" "++(show y))
