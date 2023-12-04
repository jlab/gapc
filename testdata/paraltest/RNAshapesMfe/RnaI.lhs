> module RNAshapesMfe.RnaI where
> import Data.Array
> import ADPTriCombinators

Basic Types

> type Base    = Ebase
> type Region  = Subword
> type Input a = (a,Region)

The Enum Type of nucleotides.

> data Ebase = A | C | G | U | N
>	       deriving (Bounded,Eq,Ord,Ix,Enum,Read,Show)

An indexed base

> type Ibase = Int

RNA is a string of bases

> type RNA = [Ebase]

> rna :: String -> RNA
> rna cs = [nuc t | t <- cs]

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
> nuc 'a' = A
> nuc 'c' = C
> nuc 'g' = G
> nuc 'u' = U
> nuc 't' = U
> nuc 'T' = U
> nuc  x  = N --error "malformed nucleotide (nuc)"


> pair :: (Base,Base) -> Bool
> pair (A,U) = True
> pair (U,A) = True
> pair (C,G) = True
> pair (G,C) = True
> pair (G,U) = True
> pair (U,G) = True
> pair   _   = False

> type RNAInput = Array Int Base

 basepair :: Input RNAInput -> Bool
 basepair	(inp,(i,j)) = (i+1 < j) && (pair (inp!(i+1), inp!j))

 nobasepair :: Input RNAInput -> Bool
 nobasepair = not . basepair

 minloopsize :: Int -> Input RNAInput -> Bool
 minloopsize s (_,r) = (sizeof r) >= s

The Folding Space of an RNA consists of structures

> data  FS = STRUCT [Component]
>            deriving (Eq,Ord,Show)


RNA structures are made up of the following components.

> data  Component = SS         Region			         |
>                   ES         Region			         |
>		    HL  Ibase  Region                      Ibase |
>		    SR  Ibase            Component         Ibase |
>		    BL  Ibase  Region    Component         Ibase |
>		    BR  Ibase            Component  Region Ibase |
>		    IL  Ibase  Region    Component  Region Ibase |
>                   ILN Ibase         (Component,Int)      Ibase |
>                   ILX Ibase         (Component,Int)      Ibase |
>                   ILL Region Ibase     Component         Ibase |
>                   ILR Ibase            Component  Ibase Region |
>                   ILS Ibase            Component         Ibase |
>		    ML  Ibase           [Component]        Ibase |
>                   DL  Ibase            Component               |
>                   DR                   Component         Ibase |
>                   DLR Ibase            Component         Ibase |
>                   EDL Ibase            Component               |
>                   EDR                  Component         Ibase |
>                   EDLR Ibase           Component         Ibase |
>                   MLL Ibase  Ibase    [Component]        Ibase |
>                   MLR Ibase           [Component] Ibase  Ibase |
>                   MLLR Ibase Ibase    [Component] Ibase  Ibase |
>		    BLOCK Component Component
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
>		     BLOCKE EComponent EComponent
>		     deriving (Eq,Ord,Show)

> type EComponent = (Float,ComponentE)

------------ Utilities -----------------

Return the length of a region.

> sizeof :: Region -> Int
> sizeof (i,j) = j-i

Create an array and fill it with the list.

> toarray :: [b] -> Array Int b
> toarray l = array (1,length l) (zip [1..] l)

----------------------------------- Test Area ----------------------------------

