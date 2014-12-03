> module Energy where
> import Data.Array
> import Numeric
> import RnaI

Some constants & utilities

> e :: Float
> e = 2.718281828459

0 Degrees Celsius in Kelvin.

> t :: Float
> t = 273.15

The Temperature we work with.

> temp :: Float
> temp = t + 37.0

Universal Gas Constant

> r :: Float
> r = 8.3143

Convert Celsius  degrees to Kelvin degrees.

> kelvin :: Float -> Float
> kelvin cels = t + cels

> log_interp :: (Integral a, Floating b) => a -> b
> log_interp size = 1.079 * logBase 2.71828 ((fromIntegral size) / 30.0)



-------------------------------------------------

The type of an Energy Lookup Table

 type Energytable a = Array (Ebase,Ebase,Ebase,Ebase) a

VX AC
WY BD

Terminal Mismatch Stacking Energies (Hairpin Loop)

 term_mis_ste_hl :: Energytable Float

 term_mis_ste_hl = a where
		    a = array ((A,A,A,A),(U,U,U,U))
			      [((a,b,c,d), 1.0) | -- filled with ones for the moment!!!
			      a <- enumFrom A, b <- enumFrom A, c <- enumFrom A, d <-enumFrom A]

Lookup the energy in the fourdimensional array.

Old version: maps chars to Ebase first

 look :: Energytable b -> Char -> Char -> Char -> Char -> b
 look ar v w x y = ar!(a,b,c,d) where
		      a = nuc v
		      b = nuc w					    
		      c = nuc x 
		      d = nuc y

Returns the stacking energy for i<->j (i+1)<->(j-1) stacking pair

Old version using look

 stack_energy :: Energytable a -> (Int,Int) -> a
 stack_energy ar (i,j) = look ar (inp!(i+1)) (inp!j) (inp!(i+2)) (inp!(j-1))


 stack_energy :: Energytable a -> (Int,Int) -> a
 stack_energy ar (i,j) = ar!((inp!(i+1)),(inp!j),(inp!(i+2)),(inp!(j-1)))




--------------- The Energy Tables ---------------------

Loop free energies at 37 ° :

DESTABILIZING ENERGIES BY SIZE OF LOOP (INTERPOLATE WHERE NEEDED)               
hp3 ave calc no tmm hp4 ave calc with tmm  ave all bulges                       
SIZE         INTERNAL          BULGE            HAIRPIN                         
-------------------------------------------------------                         
 1              .                3.9               .
 2             4.1               3.1               .
 3             5.1               3.5              4.1
 4             4.9               4.2              4.9
 5             5.3               4.8              4.4
 6             5.7               5.0              4.7
 7             5.9               5.2              5.0
 8             6.0               5.3              5.1
 9             6.1               5.4              5.2
10             6.3               5.5              5.3
11             6.4               5.7              5.4
12             6.4               5.7              5.5
13             6.5               5.8              5.6
14             6.6               5.9              5.7
15             6.7               6.0              5.8
16             6.8               6.1              5.8
17             6.8               6.1              5.9
18             6.9               6.2              5.9
19             6.9               6.2              6.0
20             7.0               6.3              6.1
21             7.1               6.3              6.1
22             7.1               6.4              6.2
23             7.1               6.4              6.2
24             7.2               6.5              6.3
25             7.2               6.5              6.3
26             7.3               6.5              6.3
27             7.3               6.6              6.4
28             7.4               6.7              6.4
29             7.4               6.7              6.5
30             7.4               6.7              6.5


-------------------- Stacking Energies ----------------------------

Stabilizing energies for canonical basepairs: AU, CG, GU
Basepairing: Parameters are in 5' 3' order.
stack_dg a b c d
         ^ ^ ^ ^
         | |_| |
         |_____|

> stack_dg :: Fractional a => Ebase -> Ebase -> Ebase -> Ebase -> a

> stack_dg A A U U = -0.9
> stack_dg A C G U = -2.1
> stack_dg A G C U = -1.7
> stack_dg A G U U = -0.5
> stack_dg A U A U = -0.9
> stack_dg A U G U = -1.0
> stack_dg C A U G = -1.8
> stack_dg C C G G = -2.9
> stack_dg C G C G = -2.0
> stack_dg C G U G = -1.2
> stack_dg C U A G = -1.7
> stack_dg C U G G = -1.9
> stack_dg G A U C = -2.3
> stack_dg G A U U = -1.1
> stack_dg G C G C = -3.4
> stack_dg G C G U = -2.1
> stack_dg G G C C = -2.9
> stack_dg G G U C = -1.4
> stack_dg G G C U = -1.9
> stack_dg G G U U = -0.4
> stack_dg G U A C = -2.1
> stack_dg G U G C = -2.1
> stack_dg G U A U = -1.0
> stack_dg G U G U =  1.5
> stack_dg U A U A = -1.1
> stack_dg U A U G = -0.8
> stack_dg U C G A = -2.3
> stack_dg U C G G = -1.4
> stack_dg U G C A = -1.8
> stack_dg U G U A = -0.8
> stack_dg U G C G = -1.2
> stack_dg U G U G = -0.2
> stack_dg U U A A = -0.9
> stack_dg U U G A = -1.1
> stack_dg U U A G = -0.5
> stack_dg U U G G = -0.4

> stack_dg a b c d = error ("stack_dg "++show (a,b,c,d) ++ ": not in table")

> sr_energy :: Fractional a => RNAInput -> Region -> a
> sr_energy seq (i,j) = stack_dg (seq!i) (seq!(i+1)) (seq!(j-1)) (seq!j)


------------------ Hairpin Loop Energies --------------------------

1. Entropic Term

DESTABILIZING ENERGIES BY SIZE OF LOOP (Hairpin)

> hl_ent :: Floating b => Int -> b

> hl_ent  3 = 4.1
> hl_ent  4 = 4.9
> hl_ent  5 = 4.4
> hl_ent  6 = 4.7
> hl_ent  7 = 5.0
> hl_ent  8 = 5.1
> hl_ent  9 = 5.2
> hl_ent 10 = 5.3
> hl_ent 11 = 5.4
> hl_ent 12 = 5.5
> hl_ent 13 = 5.6
> hl_ent 14 = 5.7
> hl_ent 15 = 5.8
> hl_ent 16 = 5.8
> hl_ent 17 = 5.9
> hl_ent 18 = 5.9
> hl_ent 19 = 6.0
> hl_ent 20 = 6.1
> hl_ent 21 = 6.1
> hl_ent 22 = 6.2
> hl_ent 23 = 6.2
> hl_ent 24 = 6.3
> hl_ent 25 = 6.3
> hl_ent 26 = 6.3
> hl_ent 27 = 6.4
> hl_ent 28 = 6.4
> hl_ent 29 = 6.5
> hl_ent 30 = 6.5

> hl_ent size = if size < 3 
>		then error "hl_ent: size < 3"
>		else hl_ent 30 + log_interp size

hl_e :: [Float]
hl_e = [4.1, 4.9, 4.4,  4.7,  5.0,  5.1,  5.2,  5.3,  5.4,  5.5,  5.6,  5.7,  5.8,  5.8,  5.9,  5.9,  6.0,  6.1,  6.1,  6.2,  6.2,  6.3,  6.3,  6.3,  6.4,  6.4,  6.5,  6.5]


hl_e_ar :: Array Int Float
hl_e_ar = array (3,30) (zip [3..30] hl_e)

hl_ent :: Int -> Float
hl_ent size = if  size < 3
	      then error "size < 3"
	      else ( if size <= 30
		     then hl_e_ar!size
		     else hl_e_ar!30 +
			  1.079 * logBase e ((fromIntegral size)/30.0)
		   )




2. Stacking Interaction

> tstackh_dg :: Fractional a => Ebase -> Ebase -> Ebase -> Ebase -> a

> tstackh_dg A A U A = -1.0
> tstackh_dg A A U C = -0.7
> tstackh_dg A A U G = -1.8
> tstackh_dg A A A U = -0.8
> tstackh_dg A A C U = -1.0
> tstackh_dg A A G U = -1.7
> tstackh_dg A A U U = -1.0
> tstackh_dg A C G A = -1.1
> tstackh_dg A C G C = -1.1
> tstackh_dg A C G G = -2.3
> tstackh_dg A C A U = -0.7
> tstackh_dg A C C U = -0.7
> tstackh_dg A C G U = -0.7
> tstackh_dg A C U U = -0.7
> tstackh_dg A G C A = -1.4
> tstackh_dg A G U A = -1.2
> tstackh_dg A G C C = -1.0
> tstackh_dg A G U C = -0.9
> tstackh_dg A G C G = -2.1
> tstackh_dg A G U G = -2.0
> tstackh_dg A G A U = -1.5
> tstackh_dg A G C U = -1.0
> tstackh_dg A G G U = -1.0
> tstackh_dg A G U U = -1.0
> tstackh_dg A U A A = -0.8
> tstackh_dg A U G A = -0.8
> tstackh_dg A U A C = -0.7
> tstackh_dg A U G C = -0.7
> tstackh_dg A U A G = -1.5
> tstackh_dg A U G G = -1.5
> tstackh_dg A U A U = -0.8
> tstackh_dg A U C U = -0.8
> tstackh_dg A U G U = -0.8
> tstackh_dg A U U U = -0.8
> tstackh_dg C A U A = -0.8
> tstackh_dg C A U C = -0.6
> tstackh_dg C A A G = -1.4
> tstackh_dg C A C G = -2.0
> tstackh_dg C A G G = -2.1
> tstackh_dg C A U G = -1.9
> tstackh_dg C A U U = -0.6
> tstackh_dg C C G A = -1.3
> tstackh_dg C C G C = -0.6
> tstackh_dg C C A G = -1.0
> tstackh_dg C C C G = -1.1
> tstackh_dg C C G G = -1.0
> tstackh_dg C C U G = -0.8
> tstackh_dg C C G U = -0.8
> tstackh_dg C G C A = -2.0
> tstackh_dg C G U A = -1.4
> tstackh_dg C G C C = -1.1
> tstackh_dg C G U C = -0.9
> tstackh_dg C G A G = -2.1
> tstackh_dg C G C G = -1.9
> tstackh_dg C G G G = -1.4
> tstackh_dg C G U G = -1.9
> tstackh_dg C G C U = -1.5
> tstackh_dg C G U U = -1.1
> tstackh_dg C U A A = -1.0
> tstackh_dg C U G A = -1.0
> tstackh_dg C U A C = -0.7
> tstackh_dg C U G C = -0.7
> tstackh_dg C U A G = -1.4
> tstackh_dg C U C G = -1.5
> tstackh_dg C U G G = -1.4
> tstackh_dg C U U G = -1.2
> tstackh_dg C U A U = -0.8
> tstackh_dg C U G U = -0.8
> tstackh_dg G A U A = -1.8
> tstackh_dg G A A C = -1.1
> tstackh_dg G A C C = -1.3
> tstackh_dg G A G C = -2.0
> tstackh_dg G A U C = -1.3
> tstackh_dg G A U G = -1.2
> tstackh_dg G A A U = -0.8
> tstackh_dg G A C U = -1.0
> tstackh_dg G A G U = -1.7
> tstackh_dg G A U U = -1.0
> tstackh_dg G C G A = -2.0
> tstackh_dg G C A C = -1.1
> tstackh_dg G C C C = -0.6
> tstackh_dg G C G C = -0.6
> tstackh_dg G C U C = -0.5
> tstackh_dg G C G G = -1.4
> tstackh_dg G C A U = -0.7
> tstackh_dg G C C U = -0.7
> tstackh_dg G C G U = -0.7
> tstackh_dg G C U U = -0.7
> tstackh_dg G G C A = -2.1
> tstackh_dg G G U A = -2.0
> tstackh_dg G G A C = -2.3
> tstackh_dg G G C C = -1.5
> tstackh_dg G G G C = -1.4
> tstackh_dg G G U C = -1.5
> tstackh_dg G G C G = -1.4
> tstackh_dg G G U G = -1.3
> tstackh_dg G G A U = -1.5
> tstackh_dg G G C U = -1.0
> tstackh_dg G G G U = -1.0
> tstackh_dg G G U U = -1.0
> tstackh_dg G U A A = -1.7
> tstackh_dg G U G A = -1.7
> tstackh_dg G U A C = -0.8
> tstackh_dg G U C C = -0.8
> tstackh_dg G U G C = -0.8
> tstackh_dg G U U C = -0.7
> tstackh_dg G U A G = -1.0
> tstackh_dg G U G G = -1.0
> tstackh_dg G U A U = -0.8
> tstackh_dg G U C U = -0.8
> tstackh_dg G U G U = -0.8
> tstackh_dg G U U U = -0.8
> tstackh_dg U A A A = -1.0
> tstackh_dg U A C A = -0.8
> tstackh_dg U A G A = -1.8
> tstackh_dg U A U A = -0.9
> tstackh_dg U A U C = -0.5
> tstackh_dg U A A G = -1.2
> tstackh_dg U A C G = -1.4
> tstackh_dg U A G G = -2.0
> tstackh_dg U A U G = -1.4
> tstackh_dg U A U U = -0.5
> tstackh_dg U C A A = -0.7
> tstackh_dg U C C A = -0.6
> tstackh_dg U C G A = -0.3
> tstackh_dg U C U A = -0.5
> tstackh_dg U C G C = -0.5
> tstackh_dg U C A G = -0.9
> tstackh_dg U C C G = -0.9
> tstackh_dg U C G G = -0.7
> tstackh_dg U C U G = -0.7
> tstackh_dg U C G U = -0.7
> tstackh_dg U G A A = -1.8
> tstackh_dg U G C A = -0.9
> tstackh_dg U G G A = -1.2
> tstackh_dg U G U A = -0.9
> tstackh_dg U G C C = -0.8
> tstackh_dg U G U C = -0.7
> tstackh_dg U G A G = -2.0
> tstackh_dg U G C G = -1.4
> tstackh_dg U G G G = -1.3
> tstackh_dg U G U G = -1.4
> tstackh_dg U G C U = -1.2
> tstackh_dg U G U U = -0.9
> tstackh_dg U U A A = -0.3
> tstackh_dg U U C A = -0.6
> tstackh_dg U U G A = -0.3
> tstackh_dg U U U A = -0.5
> tstackh_dg U U A C = -0.7
> tstackh_dg U U G C = -0.7
> tstackh_dg U U A G = -0.9
> tstackh_dg U U C G = -1.1
> tstackh_dg U U G G = -0.9
> tstackh_dg U U U G = -0.9
> tstackh_dg U U A U = -0.8
> tstackh_dg U U G U = -0.8

> tstackh_dg _ _ _ _ = error "tstackh_dg: not in table"

> hl_stack :: Fractional a => RNAInput -> Region -> a
> hl_stack seq (i,j) = tstackh_dg (seq!i) (seq!(i+1)) (seq!(j-1)) (seq!j)


 hl_stack :: (Int,Int) -> Float
 hl_stack (i,j) = if sizeof (i,j) > 3
		   then stack_energy term_mis_ste_hl (i,j)
		   else 0.0

3. Tetraloop Bonus Energies

Ultrastable tetra-loops & energy bonus at 37 °C:

Tetra-loops
 Seq  Energy   
 -----------   

> hl_tetra :: Fractional a => [Ebase] -> a

> hl_tetra [A,G,A,A,A,U] =  -2.0
> hl_tetra [A,G,C,A,A,U] =  -2.0
> hl_tetra [A,G,A,G,A,U] =  -2.0
> hl_tetra [A,G,U,G,A,U] =  -2.0
> hl_tetra [A,G,G,A,A,U] =  -2.0
> hl_tetra [A,U,U,C,G,U] =  -2.0
> hl_tetra [A,U,A,C,G,U] =  -2.0
> hl_tetra [A,G,C,G,A,U] =  -2.0
> hl_tetra [A,U,C,C,G,U] =  -2.0
> hl_tetra [A,G,U,A,A,U] =  -2.0
> hl_tetra [A,C,U,U,G,U] =  -2.0
> hl_tetra [A,A,U,U,U,U] =  -2.0
> hl_tetra [A,U,U,U,A,U] =  -2.0
> hl_tetra [C,G,A,A,A,G] =  -2.0
> hl_tetra [C,G,C,A,A,G] =  -2.0
> hl_tetra [C,G,A,G,A,G] =  -2.0
> hl_tetra [C,G,U,G,A,G] =  -2.0
> hl_tetra [C,G,G,A,A,G] =  -2.0
> hl_tetra [C,U,U,C,G,G] =  -2.0
> hl_tetra [C,U,A,C,G,G] =  -2.0
> hl_tetra [C,G,C,G,A,G] =  -2.0
> hl_tetra [C,U,C,C,G,G] =  -2.0
> hl_tetra [C,G,U,A,A,G] =  -2.0
> hl_tetra [C,C,U,U,G,G] =  -2.0
> hl_tetra [C,A,U,U,U,G] =  -2.0
> hl_tetra [C,U,U,U,A,G] =  -2.0
> hl_tetra [G,G,A,A,A,C] =  -2.0
> hl_tetra [G,G,C,A,A,C] =  -2.0
> hl_tetra [G,G,A,G,A,C] =  -2.0
> hl_tetra [G,G,U,G,A,C] =  -2.0
> hl_tetra [G,G,G,A,A,C] =  -2.0
> hl_tetra [G,U,U,C,G,C] =  -2.0
> hl_tetra [G,U,A,C,G,C] =  -2.0
> hl_tetra [G,G,C,G,A,C] =  -2.0
> hl_tetra [G,U,C,C,G,C] =  -2.0
> hl_tetra [G,G,U,A,A,C] =  -2.0
> hl_tetra [G,C,U,U,G,C] =  -2.0
> hl_tetra [G,A,U,U,U,C] =  -2.0
> hl_tetra [G,U,U,U,A,C] =  -2.0
> hl_tetra [U,G,A,A,A,A] =  -2.0
> hl_tetra [U,G,C,A,A,A] =  -2.0
> hl_tetra [U,G,A,G,A,A] =  -2.0
> hl_tetra [U,G,U,G,A,A] =  -2.0
> hl_tetra [U,G,G,A,A,A] =  -2.0
> hl_tetra [U,U,U,C,G,A] =  -2.0
> hl_tetra [U,U,A,C,G,A] =  -2.0
> hl_tetra [U,G,C,G,A,A] =  -2.0
> hl_tetra [U,U,C,C,G,A] =  -2.0
> hl_tetra [U,G,U,A,A,A] =  -2.0
> hl_tetra [U,C,U,U,G,A] =  -2.0
> hl_tetra [U,A,U,U,U,A] =  -2.0
> hl_tetra [U,U,U,U,A,A] =  -2.0
> hl_tetra [G,G,A,A,A,U] =  -2.0
> hl_tetra [G,G,C,A,A,U] =  -2.0
> hl_tetra [G,G,A,G,A,U] =  -2.0
> hl_tetra [G,G,U,G,A,U] =  -2.0
> hl_tetra [G,G,G,A,A,U] =  -2.0
> hl_tetra [G,U,U,C,G,U] =  -2.0
> hl_tetra [G,U,A,C,G,U] =  -2.0
> hl_tetra [G,G,C,G,A,U] =  -2.0
> hl_tetra [G,U,C,C,G,U] =  -2.0
> hl_tetra [G,G,U,A,A,U] =  -2.0
> hl_tetra [G,C,U,U,G,U] =  -2.0
> hl_tetra [G,A,U,U,U,U] =  -2.0
> hl_tetra [G,U,U,U,A,U] =  -2.0
> hl_tetra [U,G,A,A,A,G] =  -2.0
> hl_tetra [U,G,C,A,A,G] =  -2.0
> hl_tetra [U,G,A,G,A,G] =  -2.0
> hl_tetra [U,G,U,G,A,G] =  -2.0
> hl_tetra [U,G,G,A,A,G] =  -2.0
> hl_tetra [U,U,U,C,G,G] =  -2.0
> hl_tetra [U,U,A,C,G,G] =  -2.0
> hl_tetra [U,G,C,G,A,G] =  -2.0
> hl_tetra [U,U,C,C,G,G] =  -2.0
> hl_tetra [U,G,U,A,A,G] =  -2.0
> hl_tetra [U,C,U,U,G,G] =  -2.0
> hl_tetra [U,A,U,U,U,G] =  -2.0
> hl_tetra [U,U,U,U,A,G] =  -2.0

> hl_tetra _ = 0.0

 hl_t :: [([Base],Float)] 

 hl_t = [([G,A,A,A],-2.0), ([G,C,A,A],-2.0), ([G,A,G,A],-2.0),
	  ([G,U,G,A],-2.0), ([G,G,A,A],-2.0), ([U,U,C,G],-2.0),
	  ([U,A,C,G],-2.0), ([G,C,G,A],-2.0), ([U,C,C,G],-2.0),
	  ([G,U,A,A],-2.0), ([C,U,U,G],-2.0), ([A,U,U,U],-2.0),
	  ([U,U,U,A],-2.0)]


Das geht mit Sicherheit schoener!!!


 hl_tetra :: (Int,Int) -> Float
 hl_tetra reg = if l == []
		 then 0.0
		 else en
		 where
		 (tet,en):ls = l
		 l = dropWhile (notcmp reg) hl_t 

 cmp :: (Int,Int) -> ([Base],Float) -> Bool

 notcmp reg (tet,en) = not(cmp reg (tet,en))
 cmp reg (tet,en) = ((inpregion reg) == tet)

> inpregion :: RNAInput -> Region -> [Ebase]
> inpregion seq (i,j) = [ seq!k | k <- [i+1 .. j]]


The whole thing

the function below is correct!!!
 hl_energy :: Floating a => Region -> a
 hl_energy (i,j) = hl_ent (sizeof (i,j-1))
		    + (if sizeof (i,j-1) == 3 then 0.0 else hl_stack (i,j))
		    + (hl_tetra . inpregion) (i-1,j)


> hl_energy :: Floating a => RNAInput -> Region -> a

> hl_energy seq (i,j) | size == 3 = entropy
>		      | size == 4 = entropy + stack_mismatch + tetra_bonus
>		      | size  > 4 = entropy + stack_mismatch
>		      | otherwise = error "hl_energy: size < 3"
>		      where
>    size	      = j - i - 1
>    entropy	      = hl_ent size
>    stack_mismatch   = hl_stack seq (i,j)
>    tetra_bonus      = (hl_tetra . inpregion seq) (i-1,j)


---------------------- Bulge Loop Energies --------------------------

> bl_ent :: Floating b => Int -> b

> bl_ent  1 = 3.9
> bl_ent  2 = 3.1
> bl_ent  3 = 3.5
> bl_ent  4 = 4.2
> bl_ent  5 = 4.8
> bl_ent  6 = 5.0
> bl_ent  7 = 5.2
> bl_ent  8 = 5.3
> bl_ent  9 = 5.4
> bl_ent 10 = 5.5
> bl_ent 11 = 5.7
> bl_ent 12 = 5.7
> bl_ent 13 = 5.8
> bl_ent 14 = 5.9
> bl_ent 15 = 6.0
> bl_ent 16 = 6.1
> bl_ent 17 = 6.1
> bl_ent 18 = 6.2
> bl_ent 19 = 6.2
> bl_ent 20 = 6.3
> bl_ent 21 = 6.3
> bl_ent 22 = 6.4
> bl_ent 23 = 6.4
> bl_ent 24 = 6.5
> bl_ent 25 = 6.5
> bl_ent 26 = 6.5
> bl_ent 27 = 6.6
> bl_ent 28 = 6.7
> bl_ent 29 = 6.7
> bl_ent 30 = 6.7

> bl_ent size = if size < 1 
>		then error "bl_ent: size < 1"
>		else bl_ent 30 + log_interp size

------------------------ Bulge Loop Left ----------------------------

> bl_energy :: Floating a => RNAInput -> Int -> Region -> Int -> a

> bl_energy seq bl (i,j) br | size == 1 = stacking + entropy
>			| size  > 1 = entropy
>			| otherwise = error "bl_energy size < 1" 
>			where
>              stacking = stack_dg (seq!(bl)) (seq!(j+1)) (seq!(br-1)) (seq!(br)) 
>	       size	= sizeof (i,j)
>	       entropy	= bl_ent size

----------------------- Bulge Loop Right ----------------------------

> br_energy :: Floating a => RNAInput -> Int -> Region -> Int -> a

> br_energy seq bl (i,j) br  | size == 1 = stacking + entropy
>			 | size  > 1 = entropy
>			 | otherwise = error "br_energy size < 1"
>			 where
>               stacking = stack_dg (seq!bl) (seq!(bl+1)) (seq!i) (seq!br)
>	        size	 = sizeof (i,j)
>	        entropy	 = bl_ent size

-------------------- Interior Loop Energies -------------------------

1. Entropic Term

DESTABILIZING ENERGIES BY SIZE OF LOOP

> il_ent :: Floating b => Int -> b

> il_ent  2 = 4.1
> il_ent  3 = 5.1
> il_ent  4 = 4.9
> il_ent  5 = 5.3
> il_ent  6 = 5.7
> il_ent  7 = 5.9
> il_ent  8 = 6.0
> il_ent  9 = 6.1
> il_ent 10 = 6.3
> il_ent 11 = 6.4
> il_ent 12 = 6.4
> il_ent 13 = 6.5
> il_ent 14 = 6.6
> il_ent 15 = 6.7
> il_ent 16 = 6.8
> il_ent 17 = 6.8
> il_ent 18 = 6.9
> il_ent 19 = 6.9
> il_ent 20 = 7.0
> il_ent 21 = 7.1
> il_ent 22 = 7.1
> il_ent 23 = 7.1
> il_ent 24 = 7.2
> il_ent 25 = 7.2
> il_ent 26 = 7.3
> il_ent 27 = 7.3
> il_ent 28 = 7.4
> il_ent 29 = 7.4
> il_ent 30 = 7.4

> il_ent size = if size < 2 
>		then error "il_ent: size < 2"
>		else il_ent 30 + log_interp size

2. Stacking Interaction

STACKING ENERGIES : TERMINAL MISMATCHES AND BASE-PAIRS.

Stabilizing energies for canonical basepairs: AU, CG, GU
Basepairing: Parameters are in 5' 3' order.
stack_dg a b c d
         ^ ^ ^ ^
         | |_| |
         |_____|

> tstacki_dg :: Fractional a => Ebase -> Ebase -> Ebase -> Ebase -> a

> tstacki_dg A A U A = -1.0
> tstacki_dg A A U C = -1.0
> tstacki_dg A A U G = -2.2
> tstacki_dg A A A U = -1.0
> tstacki_dg A A C U = -1.0
> tstacki_dg A A G U = -2.2
> tstacki_dg A A U U = -0.5
> tstacki_dg A C G A = -1.5
> tstacki_dg A C G C = -1.5
> tstacki_dg A C G G = -2.7
> tstacki_dg A C A U = -1.0
> tstacki_dg A C C U = -1.0
> tstacki_dg A C G U = -0.2
> tstacki_dg A C U U = -1.0
> tstacki_dg A G C A = -1.5
> tstacki_dg A G U A = -1.0
> tstacki_dg A G C C = -1.5
> tstacki_dg A G U C = -1.0
> tstacki_dg A G C G = -2.7
> tstacki_dg A G U G = -2.2
> tstacki_dg A G A U = -2.2
> tstacki_dg A G C U = -0.5
> tstacki_dg A G G U = -1.0
> tstacki_dg A G U U = -0.5
> tstacki_dg A U A A = -1.0
> tstacki_dg A U G A = -1.5
> tstacki_dg A U A C = -1.0
> tstacki_dg A U G C = -1.5
> tstacki_dg A U A G = -2.2
> tstacki_dg A U G G = -2.7
> tstacki_dg A U A U = -0.3
> tstacki_dg A U C U = -1.0
> tstacki_dg A U G U = -0.3
> tstacki_dg A U U U = -2.0
> tstacki_dg C A U A = -1.0
> tstacki_dg C A U C = -1.0
> tstacki_dg C A A G = -1.5
> tstacki_dg C A C G = -1.5
> tstacki_dg C A G G = -2.7
> tstacki_dg C A U G = -1.9
> tstacki_dg C A U U = -1.0
> tstacki_dg C C G A = -1.5
> tstacki_dg C C G C = -1.5
> tstacki_dg C C A G = -1.5
> tstacki_dg C C C G = -1.5
> tstacki_dg C C G G = -1.0
> tstacki_dg C C U G = -1.5
> tstacki_dg C C G U = -1.5
> tstacki_dg C G C A = -1.5
> tstacki_dg C G U A = -1.0
> tstacki_dg C G C C = -1.5
> tstacki_dg C G U C = -1.0
> tstacki_dg C G A G = -2.7
> tstacki_dg C G C G = -1.9
> tstacki_dg C G G G = -1.5
> tstacki_dg C G U G = -1.9
> tstacki_dg C G C U = -1.5
> tstacki_dg C G U U = -1.0
> tstacki_dg C U A A = -1.0
> tstacki_dg C U G A = -1.5
> tstacki_dg C U A C = -1.0
> tstacki_dg C U G C = -1.5
> tstacki_dg C U A G = -1.4
> tstacki_dg C U C G = -1.5
> tstacki_dg C U G G = -1.4
> tstacki_dg C U U G = -2.5
> tstacki_dg C U A U = -1.0
> tstacki_dg C U G U = -1.5
> tstacki_dg G A U A = -2.2
> tstacki_dg G A A C = -1.5
> tstacki_dg G A C C = -1.5
> tstacki_dg G A G C = -2.7
> tstacki_dg G A U C = -1.3
> tstacki_dg G A U G = -1.0
> tstacki_dg G A A U = -1.5
> tstacki_dg G A C U = -1.5
> tstacki_dg G A G U = -2.7
> tstacki_dg G A U U = -1.3
> tstacki_dg G C G A = -2.7
> tstacki_dg G C A C = -1.5
> tstacki_dg G C C C = -1.5
> tstacki_dg G C G C = -0.6
> tstacki_dg G C U C = -1.5
> tstacki_dg G C G G = -1.5
> tstacki_dg G C A U = -1.5
> tstacki_dg G C C U = -1.5
> tstacki_dg G C G U = -0.6
> tstacki_dg G C U U = -1.5
> tstacki_dg G G C A = -2.7
> tstacki_dg G G U A = -2.2
> tstacki_dg G G A C = -2.7
> tstacki_dg G G C C = -1.5
> tstacki_dg G G G C = -1.5
> tstacki_dg G G U C = -1.5
> tstacki_dg G G C G = -1.5
> tstacki_dg G G U G = -1.0
> tstacki_dg G G A U = -2.7
> tstacki_dg G G C U = -1.5
> tstacki_dg G G G U = -1.5
> tstacki_dg G G U U = -1.5
> tstacki_dg G U A A = -2.2
> tstacki_dg G U G A = -2.7
> tstacki_dg G U A C = -0.8
> tstacki_dg G U C C = -1.5
> tstacki_dg G U G C = -0.8
> tstacki_dg G U U C = -2.5
> tstacki_dg G U A G = -1.0
> tstacki_dg G U G G = -1.5
> tstacki_dg G U A U = -0.8
> tstacki_dg G U C U = -1.5
> tstacki_dg G U G U = -0.8
> tstacki_dg G U U U = -2.5
> tstacki_dg U A A A = -1.0
> tstacki_dg U A C A = -1.0
> tstacki_dg U A G A = -2.2
> tstacki_dg U A U A = -0.4
> tstacki_dg U A U C = -1.0
> tstacki_dg U A A G = -1.0
> tstacki_dg U A C G = -1.0
> tstacki_dg U A G G = -2.2
> tstacki_dg U A U G = -0.4
> tstacki_dg U A U U = -2.0
> tstacki_dg U C A A = -1.0
> tstacki_dg U C C A = -1.0
> tstacki_dg U C G A = 0.2
> tstacki_dg U C U A = -1.0
> tstacki_dg U C G C = -1.5
> tstacki_dg U C A G = -1.0
> tstacki_dg U C C G = -1.0
> tstacki_dg U C G G = 0.2
> tstacki_dg U C U G = -1.0
> tstacki_dg U C G U = -2.5
> tstacki_dg U G A A = -2.2
> tstacki_dg U G C A = -0.4
> tstacki_dg U G G A = -1.0
> tstacki_dg U G U A = -0.4
> tstacki_dg U G C C = -1.5
> tstacki_dg U G U C = -1.0
> tstacki_dg U G A G = -2.2
> tstacki_dg U G C G = -0.4
> tstacki_dg U G G G = -1.0
> tstacki_dg U G U G = -0.4
> tstacki_dg U G C U = -2.5
> tstacki_dg U G U U = -2.0
> tstacki_dg U U A A = 0.2
> tstacki_dg U U C A = -1.0
> tstacki_dg U U G A = 0.2
> tstacki_dg U U U A = -2.0
> tstacki_dg U U A C = -1.0
> tstacki_dg U U G C = -1.5
> tstacki_dg U U A G = 0.2
> tstacki_dg U U C G = -1.0
> tstacki_dg U U G G = 0.2
> tstacki_dg U U U G = -2.0
> tstacki_dg U U A U = -2.0
> tstacki_dg U U G U = -2.5


> tstacki_dg a b c d = error ("tstacki_dg:" ++ show(a,b,c,d) ++ "not in table")

Table is symmetric
therefore size could be reduced by half!

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
>		       + tstacki_dg (seq!(j+1)) (seq!j) (seq!(k+1)) (seq!k)

Ninio's equation

> il_asym :: (Fractional a, Ord a, Integral b, Integral c) => b -> c -> a

> il_asym sl sr = min 3.0 (diff * 0.3)
>		  where diff = abs ((fromIntegral sl) - (fromIntegral sr))

> il_energy :: (Floating a, Ord a) => RNAInput -> Region -> Region -> a

> il_energy seq (i,j) (k,l) = (il_ent (sl + sr))
>			  + (il_stack seq (i,j) (k,l))
>			  + (il_asym sl sr)
>			  where sl = sizeof (i,j)
>				sr = sizeof (k,l)

Lyngso's decomposition

> top_stack :: Fractional a => RNAInput -> Int -> Int -> a
> top_stack seq lb rb = tstacki_dg (seq!lb) (seq!(lb+1)) (seq!(rb-1)) (seq!rb)

> bot_stack :: Fractional a => RNAInput -> Int -> Int -> a
> bot_stack seq lb rb = tstacki_dg (seq!(lb+1)) (seq!lb) (seq!rb) (seq!(rb-1))

> asym :: (Ord a, Integral b, Fractional a) => b -> a
> asym a = min 3.0 ((fromIntegral a) * 0.3)


Special cases of small loops not implemented yet.
symmetric and asymmetric internal loops of size 1x1 2x2 1x2 2x3 etc.

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

> ss_energy x = 0.4 * fromIntegral (sizeof x)

<--------------------------- Dangling Ends --------------------------------

   lb x rb

-------- dangle right --------

> dr_dangle_dg :: Fractional a => (Ebase,Ebase) -> Ebase -> a

> dr_dangle_dg (U,A) A = -0.8 
> dr_dangle_dg (U,A) C = -0.5
> dr_dangle_dg (U,A) G = -0.8
> dr_dangle_dg (U,A) U = -0.6

> dr_dangle_dg (G,C) A = -1.7
> dr_dangle_dg (G,C) C = -0.8
> dr_dangle_dg (G,C) G = -1.7
> dr_dangle_dg (G,C) U = -1.2

> dr_dangle_dg (C,G) A = -1.1
> dr_dangle_dg (C,G) C = -0.4
> dr_dangle_dg (C,G) G = -1.3
> dr_dangle_dg (C,G) U = -0.6

> dr_dangle_dg (U,G) A = -0.8
> dr_dangle_dg (U,G) C = -0.5
> dr_dangle_dg (U,G) G = -0.8
> dr_dangle_dg (U,G) U = -0.6

> dr_dangle_dg (A,U) A = -0.7
> dr_dangle_dg (A,U) C = -0.1
> dr_dangle_dg (A,U) G = -0.7
> dr_dangle_dg (A,U) U = -0.1

> dr_dangle_dg (G,U) A = -1.2
> dr_dangle_dg (G,U) C = -0.5
> dr_dangle_dg (G,U) G = -1.2
> dr_dangle_dg (G,U) U = -0.7


> dr_dangle_dg _ _ = error "dr_dangle_dg: not in table"

> dr_energy :: Fractional a => RNAInput -> Region -> a
> dr_energy seq (i,j) = dr_dangle_dg ((seq!i),(seq!j)) (seq!(j+1))

> dli_energy :: Fractional a => RNAInput -> Region -> a
> dli_energy seq (i,j) = dr_dangle_dg ((seq!j),(seq!i)) (seq!(i+1))

-------- dangle left --------

> dl_dangle_dg :: Fractional a => Ebase -> (Ebase,Ebase) -> a

> dl_dangle_dg A (U,A) = -0.3 
> dl_dangle_dg C (U,A) = -0.1
> dl_dangle_dg G (U,A) = -0.2
> dl_dangle_dg U (U,A) = -0.2

> dl_dangle_dg A (G,C) = -0.2 
> dl_dangle_dg C (G,C) = -0.3
> dl_dangle_dg G (G,C) =  0.0 -- Have to check energies! Missing entry?
> dl_dangle_dg U (G,C) =  0.0 -- Have to check energies! Missing entry?

> dl_dangle_dg A (C,G) = -0.5 
> dl_dangle_dg C (C,G) = -0.3
> dl_dangle_dg G (C,G) = -0.2
> dl_dangle_dg U (C,G) = -0.1

> dl_dangle_dg A (U,G) = -0.2 
> dl_dangle_dg C (U,G) = -0.2
> dl_dangle_dg G (U,G) = -0.2
> dl_dangle_dg U (U,G) = -0.2

> dl_dangle_dg A (A,U) = -0.3 
> dl_dangle_dg C (A,U) = -0.3
> dl_dangle_dg G (A,U) = -0.4
> dl_dangle_dg U (A,U) = -0.2

> dl_dangle_dg A (G,U) = -0.2 
> dl_dangle_dg C (G,U) = -0.2
> dl_dangle_dg G (G,U) = -0.2
> dl_dangle_dg U (G,U) = -0.2

> dl_dangle_dg _ _ = error "dl_dangle_dg: not in table"

> dl_energy :: Fractional a => RNAInput -> Region -> a
> dl_energy seq (i,j) = dl_dangle_dg (seq!(i-1)) ((seq!i),(seq!j))

> dri_energy :: Fractional a => RNAInput -> Region -> a
> dri_energy seq (i,j) = dl_dangle_dg (seq!(j-1)) ((seq!j),(seq!i))


----------------------------------- Test Area ---------------------------------

