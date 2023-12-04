
          ********************************************
          *              FoldingSpace                *
          *                                          *
          *    Sequence and structure data types,    *
          *       and basic functions thereon        *
          ********************************************

       1. Data types for sequences and structures
       2. Bases, pairing rules
       3. The Foldingspace
       4. Various string notations


> module Foldingspace where
> import Data.Array
> import RNACombinators


------ 1. Data types for sequences and structures --------------------------


Basic Types

> type Base   = Ebase

The Enum Type of nucleotides.

> data Ebase = A | C | G | U | N
>	       deriving (Bounded,Eq,Ord,Ix,Enum,Read,Show)

An indexed base

> type Ibase = Int

RNA is a string of bases

> type RNA = [Ebase]

> rna :: String -> RNA
> rna cs = [nuc t | t <- cs]

The input to a parser is an indexed subword of the input.

> type Input a = (a,Region)

conversion from simple string to parser input type

> str2inp :: String -> Input RNAInput
> str2inp str = (inp,(0,n)) where
>    inp = (toarray . rna) str
>    (_,n) = bounds inp

> nuc :: Char -> Ebase
> nuc 'A' = A
> nuc 'C' = C
> nuc 'G' = G
> nuc 'U' = U
> nuc 'T' = U  --replace DNA with RNA
> nuc 'a' = A
> nuc 'c' = C
> nuc 'g' = G
> nuc 'u' = U
> nuc 't' = U
> nuc  x  = N  --all other characters are mapped to N and will not be paired

error ("malformed nucleotide (" ++show x++")")



---------- 2. Bases, pairing rules------------

> pair :: (Base,Base) -> Bool
> pair (A,U) = True
> pair (U,A) = True
> pair (C,G) = True
> pair (G,C) = True
> pair (G,U) = True
> pair (U,G) = True
> pair   _   = False

> type RNAInput = Array Int Base

> basepair' :: Input RNAInput -> Bool
> basepair'	(inp,(i,j)) = (i+1 < j) && (pair (inp!(i+1), inp!j))

> basepair_withseq pseudobaseseq (inp,(i,j)) = basepair' (inp,(i,j)) && bracketpair (pseudobaseseq!(i+1)) (pseudobaseseq!j)
> bracketpair  '[' ']' = True
> bracketpair  '(' ')' = True
> bracketpair  '{' '}' = True	
> bracketpair    x  y  = False

> nobasepair' :: Input RNAInput -> Bool
> nobasepair' = not . basepair'

> stackpair' :: Input RNAInput -> Bool
> stackpair' (seq,(i,j)) = (i+3 < j) && (pair (seq!(i+1), seq!j)) && (pair (seq!(i+2), seq!(j-1)))

> stackpair_withseq pseudobaseseq (inp,(i,j))= stackpair' (inp,(i,j)) && bracketpair  (pseudobaseseq!(i+1)) (pseudobaseseq!j)
>						&&  bracketpair  (pseudobaseseq!(i+2)) (pseudobaseseq!(j-1))

> maxsize :: Int -> Region -> Bool
> maxsize s r = (sizeof r) <= s

> size :: Int -> Region -> Bool
> size s r  = (sizeof r) == s

> minloopsize' :: Int -> Input RNAInput -> Bool
> minloopsize' s (_,r) = (sizeof r) >= s

> nospecialintloop:: (a,Int,Int)->Bool
> nospecialintloop (_,l,asym) | (l>4 || asym>1) = True 
>			      | otherwise	= False 


--------------- 3. The Foldingspace -----------------

The Folding Space of an RNA consists of structures

> data  FS base = STRUCT [Component base ]
>            deriving (Eq,Ord,Show)


RNA structures are made up of the following components.

> data  Component base = 
>		    SS        Region		               |
>                   ES        Region		               |
>		    HL  base  Region                      base |
>		    SR  base            (Component base)         base |
>		    BL  base  Region    (Component base)         base |
>		    BR  base            (Component base)  Region base |
>		    IL  base  Region    (Component base)  Region base |
>                   ILN base         ((Component base),Int)      base |
>                   ILX base         ((Component base),Int)      base |
>                   ILL Region base     (Component base)         base |
>                   ILR base            (Component base)  base Region |
>                   ILS base            (Component base)         base |
>		    ML  base           [(Component base)]        base |
>                   DL  base            (Component base)              |
>                   DR                  (Component base)         base |
>                   DLR base            (Component base)         base |
>                   EDL base            (Component base)              |
>                   EDR                 (Component base)         base |
>                   EDLR base           (Component base)         base |
>                   MLL  base  base    [(Component base)]        base |
>                   MLR  base          [(Component base)] base   base |
>                   MLLR base base     [(Component base)] base   base |
>		    BLOCK (Component base) (Component base)			 |
>		    PK  Region    [(Component base)]  
>		        Region    [(Component base)] Region
>		                  [(Component base)] Region  
>		    deriving (Eq,Ord,Show)


The Folding Space of an RNA consists of structures

> data  EFS = ESTRUCT [EComponent]
>            deriving (Eq,Ord,Show)


RNA structures are made up of the following components.

> data  ComponentE = SSE          Region 		             |
>                    ESE          Region			     |
>		     HLE   Ibase  Region                       Ibase |
>		     SRE   Ibase            EComponent	       Ibase |
>		     BLE   Ibase  Region    EComponent	       Ibase |
>		     BRE   Ibase            EComponent  Region Ibase |
>		     ILE   Ibase  Region    EComponent  Region Ibase |
>                    ILNE  Ibase         (EComponent,Int)      Ibase |
>                    ILXE  Ibase         (EComponent,Int)      Ibase |
>                    ILLE  Region Ibase     EComponent         Ibase |
>                    ILRE  Ibase            EComponent  Ibase Region |
>                    ILSE  Ibase            EComponent         Ibase |
>		     MLE   Ibase           [EComponent]        Ibase |
>                    DLE   Ibase            EComponent		     |
>                    DRE   		    EComponent	       Ibase |
>                    DLRE  Ibase            EComponent         Ibase |
>                    EDLE  Ibase            EComponent		     |
>                    EDRE   		    EComponent	       Ibase |
>                    EDLRE Ibase            EComponent         Ibase |
>                    MLLE  Ibase  Ibase    [EComponent]        Ibase |
>                    MLRE  Ibase           [EComponent] Ibase  Ibase |
>                    MLLRE Ibase  Ibase    [EComponent] Ibase  Ibase |
>		     BLOCKE EComponent EComponent		     |      
>		     PKE  Region    [EComponent]  
>		          Region    [EComponent] Region
>				    [EComponent] Region		     	-- pseudoknot
>		     deriving (Eq,Ord,Show)

> type EComponent = (Float,ComponentE)


---------------- 4. Various string notations ---------------

Homorphism to VIENNA Notation


> viennaE :: [ComponentE] -> String
> viennaE   cs      = concat (map v cs) where
>   v (SSE r)       = dots r
>   v (ESE r)       = dots r
>   v (HLE b1 r b2)  = "(" ++ dots r ++ ")"
>   v (SRE b1 (e,s) b2)  = "(" ++ v s    ++ ")"
>   v (BLE b1 r (e,s) b2)      = "(" ++ dots r ++ v s ++ ")"
>   v (BRE b1 (e,s) r b2)      = "(" ++ v s  ++ dots r ++ ")"
>   v (ILE b1 r1 (e,s) r2 b2)  = "(" ++ dots r1 ++ v s  ++ dots r2 ++ ")"
>   v (ILNE b1 ((e,s), i) b2)  = "(" ++ v s ++ ")" 
>   v (ILXE b1 ((e,s), i) b2)  = "(" ++ v s ++ ")"
>   v (ILLE r1 b1 (e,s) b2)    = dots r1 ++ "(" ++ v s ++ ")"
>   v (ILRE b1 (e,s) b2 r1)    = "(" ++ v s ++ ")" ++ dots r1
>   v (ILSE b1 (e,s) b2)       = "(" ++ v s ++ ")"
>   v (MLE b1 cs b2)           = "(" ++ concat (map v (map snd cs))  ++ ")"
>   v (DLE b1 (e,s))	       = "." ++ v s 
>   v (DRE (e,s) b1)	       =         v s ++ "."
>   v (DLRE b1 (e,s) b2)       =  "." ++ v s ++ "."
>   v (EDLE b1 (e,s))	       = "." ++ v s 
>   v (EDRE (e,s) b1)	       =         v s ++ "."
>   v (EDLRE b1 (e,s) b2)      =  "." ++ v s ++ "."
>   v (MLLE b1 b2 cs b3 )      =  "(" ++ "." ++ concat (map v (map snd cs))         ++ ")"
>   v (MLRE b1 cs b2 b3 )      =  "(" ++        concat (map v (map snd cs)) ++ "."  ++ ")"
>   v (MLLRE b1 b2 cs b3 b4 )  =  "(" ++ "." ++ concat (map v (map snd cs)) ++ "."  ++ ")"
>   v (BLOCKE (e1,s1) (e2,s2)) =  v s1 ++ v s2
>   v (PKE a fro b' mid a' bac b) = open1 a ++ concat(map v (map snd fro)) ++ open2 b' ++
>					       concat (map v (map snd mid)) ++ 
>                                  close1 a' ++ concat (map v (map snd bac)) ++ close2 b

> dots   (i,j)      = ['.' | k<- [i+1 .. j]]
> open1  (i,j)      = ['[' | x<- [i+1 .. j]]
> close1 (i,j)      = [']' | x<- [i+1 .. j]]
> open2  (i,j)      = ['{' | x<- [i+1 .. j]]
> close2 (i,j)      = ['}' | x<- [i+1 .. j]]

> vienna :: [Component a] -> String
> vienna    cs               = concat (map v cs) where
>   v (SS r)		     = dots r
>   v (ES r)		     = dots r
>   v (HL b1 r b2)	     = "(" ++ dots r ++ ")"
>   v (SR b1 s b2)	     = "(" ++ v s    ++ ")"
>   v (BL b1 r s b2)	     = "(" ++ dots r ++ v s ++ ")"
>   v (BR b1 s r b2)	     = "(" ++ v s  ++ dots r ++ ")"
>   v (IL b1 r1 s r2 b2)     = "(" ++ dots r1 ++ v s  ++ dots r2 ++ ")"
>   v (ILN b1 (s, i) b2)     = "(" ++ v s ++ ")" 
>   v (ILX b1 (s, i) b2)     = "(" ++ v s ++ ")"
>   v (ILL r1 b1 s b2)	     = dots r1 ++ "(" ++ v s ++ ")"
>   v (ILR b1 s b2 r1)	     = "(" ++ v s ++ ")" ++ dots r1
>   v (ILS b1 s b2)	     = "(" ++ v s ++ ")"
>   v (ML b1 cs b2)	     = "(" ++ concat (map v cs) ++ ")"
>   v (DL b1 s)		     = "." ++ v s 
>   v (DR s b1)	             =        v s ++ "."
>   v (DLR b1 s b2)          = "." ++ v s ++ "."
>   v (EDL b1 s)             = "." ++ v s 
>   v (EDR s b1)	     =        v s ++ "."
>   v (EDLR b1 s b2)	     = "." ++ v s ++ "."
>   v (MLL b1 b2 cs b3 )     = "(" ++ "." ++ concat (map v  cs)         ++ ")"
>   v (MLR b1 cs b2 b3 )     = "(" ++        concat (map v cs) ++ "."  ++ ")"
>   v (MLLR b1 b2 cs b3 b4 ) = "(" ++ "." ++ concat (map v  cs) ++ "."  ++ ")"
>   v (BLOCK s1 s2)	     =  v s1 ++ v s2
>   v (PK a fro b' mid a' bac b) = open1 a   ++ concat (map v fro) ++ open2 b' ++
>					        concat (map v mid) ++ 
>                                  close1 a' ++ concat (map v bac) ++ close2 b


