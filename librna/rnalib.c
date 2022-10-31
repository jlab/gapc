/* {{{

    This file is part of gapc (GAPC - Grammars, Algebras, Products - Compiler;
      a system to compile algebraic dynamic programming programs)

    Copyright (C) 2008-2011  Georg Sauthoff
         email: gsauthof@techfak.uni-bielefeld.de or gsauthof@sdf.lonestar.org

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

}}} */

/* RNA energy library

   library of wrapper functions to access the Vienna-Tables for the Turner1999 and Turner2004 energy values from Bellman's GAP programs
   works with the Vienna-Package 1.8.5 and Vienna-Package 2.0.0
   written by Stefan Janssen, 12.09.2011
   modified on 28.10.2011 to support gaps in sequences, which is necessary to fold alignments. Basic concept: the common structure is given by the usual grammar, but base- and/or stackpairing is only possible if x% of sequences at this positions can form pairs. Thus pairs with gaps or gap-gap-pairs are possible for single sequences; loops can contain gaps and thus e.g. an internal loop might become a bulge loop or even a stack.
   modified on 24.11.2011 to merge 2004 and 1999 versions into one file
   modified on 13.04.2012 to use the same trick as the Vienna-Package to just once rescale energie values to given temperature

   more information about the nearest neighbor energy model can be found in
    - David Mathews "Nearest Neighbor Database Homepage": http://rna.urmc.rochester.edu/NNDB/
    - Michael Zukers user manual to mfold 3.0: http://mfold.rna.albany.edu/download/mfold-3.0-manual.pdf.gz
    - fundamental information about the nearest neighbor energy model are included in the papers listed in the begin of the file "energy_par.c"
    - file H/loop_energies.h of the Vienna RNA package >= 2.0.0
    - some aspects of the Turner2004 model are not integrated in the Vienna RNA package, for details see their upcoming paper "ViennaRNA Package 2.0" by Lorenz et al.
*/

#include "rnalib.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "ViennaRNA/datastructures/basic.h"

#include "ViennaRNA/params/default.h"
#include "ViennaRNA/params/constants.h"
#include "ViennaRNA/params/basic.h"
#include "ViennaRNA/params/io.h"
#include "ViennaRNA/fold_vars.h"

static vrna_param_t  *P = 0;

void librna_read_param_file(const char *filename) {
  if (P) {
    free(P);
  }

  /* create a new model details structure to store the Model Settings */
  vrna_md_t md;

  /* ALWAYS set default model settings first! */
  vrna_md_set_default(&md);

  if (filename) {
      /* overwrite default energy parameters with those from given file,
         iff filename is not Null */
      vrna_params_load(filename, VRNA_PARAMETER_FORMAT_DEFAULT);
    }

  /* adjust temperature for energy parameters */
  md.temperature = temperature;
  /* re-scale energies to updated temperature */
  P = vrna_params(&md);
}

/* test if dangling an unpaired base from 5' onto a stack AND another from
   3' onto the stack results in the same energy as forming an exterior
   mismatch. This was the case for Turner1999, but not for Turner2004.
   Unfortunately, macrostate depends on this assumption
   minimal counter example is "CCcCCaaaGGCCaaaGGuuGG" with structure
   "((.((...))((...))..))"
*/
bool test_macrostate_mme_assumption() {
  // enum bp_t { N_BP, CG_BP, GC_BP, GU_BP, UG_BP, AU_BP, UA_BP, NO_BP };
  for (int closingBP = CG_BP; closingBP != UA_BP; closingBP++) {
    // enum base_t {N_BASE, A_BASE, C_BASE, G_BASE, U_BASE, GAP_BASE,
    // SEPARATOR_BASE }
    for (int lbase = A_BASE; lbase != U_BASE; lbase++) {
      for (int rbase = A_BASE; rbase != U_BASE; rbase++) {
        if (abs(P->mismatchExt[closingBP][lbase][rbase] -
                (P->dangle5[closingBP][lbase] +
                 P->dangle3[closingBP][rbase])) > 2) {
          fprintf(stderr, "WARNING\n"
            "The macrostate grammar has two aims:\n"
            "1) enumerating all dot bracket candidates (but no more) and\n"
            "2) compute proper energy contributions for dangling ends.\n"
            "The design was based on the assumption that dangling an unpaired "
            "base directly\nleft of a stem onto this stem (dl_energy) and - "
            "at the same time - dangling\nanother unpaired base directly right"
            " of the stem onto the very same stem\n(dr_energy) results in the "
            "same energy contribution as forming an external\nmismatch of both"
            " unpaired bases onto the stem (ext_mismatch_energy).\nThis was "
            "true for the Turner1999 energy parameters, but is violated by "
            "the\nones provided here!\n"
            "See https://github.com/jlab/fold-grammars/issues/26 for more "
            "details.\n"
            "Expect Macrostate mfe/pfunc values to be slightly off.\n");
          return false;
        }
      }
    }
  }
  return true;
}

/*
  encodes a basepair to an int. This is necessary to address the correct cells
  in the energy tables
  Codes are the following (see /usr/include/librna/rnalib.h bp_t):
    0 = no base pair
    1 = C-G
    2 = G-C
    3 = G-U
    4 = U-G
    5 = A-U
    6 = U-A
    7 = N-N, N might be a gap. Opposite to N_BP=0, NO_BP is set 0, instead of
        -INF in the Turner1999 energies.
  Input is
    x = 5' base of the basepair
    y = 3' base of the basepair
*/
int bp_index(char x, char y) {
  switch (x) {
    case A_BASE : switch (y) {
        case U_BASE : return AU_BP;
      }
      break;
    case C_BASE : switch (y) {
        case G_BASE : return CG_BP;
      }
      break;
    case G_BASE : switch (y) {
        case C_BASE : return GC_BP;
        case U_BASE : return GU_BP;
      }
      break;
    case U_BASE : switch (y) {
        case G_BASE : return UG_BP;
        case A_BASE : return UA_BP;
      }
      break;
  }
  return NO_BP;
}

/* ============== Alignment functions ================= */
/*
   counts how many GAP symbols are between i and j in input s to find the real
   length of a loop
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4,
         GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = first base of loop region
     j = last base of loop region
*/
static rsize noGaps(const char *s, rsize i, rsize j) {
  rsize noGaps = 0;
  for (rsize k = i; k <= j; ++k)
    if (s[k] == GAP_BASE)
      ++noGaps;
  return noGaps;
}

/*
   fills char array ungapped with s, but GAP_BASEs will be skipped. This is
   necessary for tetra- and hexaloop energies for hairpins.
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4,
         GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = first base of loop region
     j = last base of loop region
     ungapped = a char array of appropriate size, i.e. j-i+1, to hold the
                ungapped string between i and j
*/
static size_t ungapRegion(const char *s, rsize i, rsize j, char *ungapped) {
  rsize pos = 0;
  for (rsize y = i; y <= j; ++y) {
    if (s[y] != GAP_BASE) {
      ungapped[pos++] = s[y];
    }
  }
  return pos;
}

/*
   returns a char pointer, i.e. string, of the decode RNA sequence in letters, after a decoding of the bit encoded chars
           this is a necessary helper function to get energies for special hairpin loop cases, i.e. tri-, tetra- and hexaloops
           due to the strange way the Vienna guys store those energies.
   Input is
     *x = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     len = the length of the RNA bit encoded sequence, since with C arrays one does not know their length
*/
static void decode(char *s, const char *x, const int len) {
    unsigned int i;
    for (i = 0; i < len; ++i) {
    s[i] = BASE_CHARS[x[i]];
    }
}

/*
   approximates the destabilizing effect of unpaired loop regions longer than 30 bases, since there are no measured wet lab data
   currently (11.09.2011) lxc37 is 107.856 and MAXLOOP is 30
   this function is used for hairpins, bulges and internal loops
   Input is
     just the size, i.e. number of bases, of the unpaired loop region: l
*/
static int jacobson_stockmayer(rsize l) {
    return (int)(P->lxc*log((l)/(1.0 * MAXLOOP)));
}

/*
   returns destabilizing energy values for unpaired hairpin loops smaller then MAXLOOP bases
   for larger loops jacobson_stockmayer is used.
   currently (11.09.2011) MAXLOOP is 30
   Input is
     just the size, i.e. number of bases, of the unpaired loop region: l
*/
static int hl_ent(rsize l) {
  if (l > MAXLOOP) {
      return P->hairpin[MAXLOOP]+jacobson_stockmayer(l);
  } else {
      return P->hairpin[l];
  }
}

/*
   returns the destabilizing energy of all but GC or CG basepairs. This should be applied only on terminal basepairs. It should be better understood as a bonus for CG / GC basepairs.
       X     (X = some closed structure)
     |   |
     i - j
     |   |
     5'  3'
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the stem terminating basepair
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the stem terminating basepair
*/
int termau_energy(const char *s, rsize i, rsize j) {
  if (
    (s[i] == G_BASE && s[j] == C_BASE) ||
    (s[i] == C_BASE && s[j] == G_BASE)
  ) {
    return 0;
  } else {
    return P->TerminalAU;
  }
}

/*
  returns the destabilizing energy of two RNA molecules that form a dimere. Needed for co-folding and initial penalty.
*/
int duplex_energy(void) {
  return P->DuplexInit;
}

int il_ent(rsize l) {
  assert(l > 1);
  if (l > MAXLOOP) {
      return P->internal_loop[MAXLOOP] + jacobson_stockmayer(l);
  } else {
      return P->internal_loop[l];
  }
}

/*
   returns the destabilizing penalty for internal loops with asymmetric sized unpaired loop regions
   version 1999: currently (12.09.2011) ninio37[2] is 50 and MAX_NINIO is 300
   version 2004: currently (12.09.2011) ninio37 is 60 and MAX_NINIO is 300
   Input is
     sl = size of the 5' unpaired loop region
     sr = size of the 3' unpaired loop region
*/
int il_asym(rsize sl, rsize sr) {
  int r = abs(sl-sr) * P->ninio[2];
  if (r < MAX_NINIO) {
    return r;
  }
  return MAX_NINIO;
}

/*
   returns destabilizing energy values for an unpaired loop smaller then MAXLOOP bases in a bulge loop
   for larger loops jacobson_stockmayer is used.
   currently (12.09.2011) MAXLOOP is 30
   Input is
     just the size, i.e. number of bases, of the unpaired loop region: l
*/
int bl_ent(rsize l) {
  assert(l > 0);
  if (l > MAXLOOP) {
    return P->bulge[MAXLOOP] + jacobson_stockmayer(l);
  } else {
    return P->bulge[l];
  }
}

/*
   returns the stabilizing energy of two successive basepairs forming a stack
       X          (X = some closed structure)
     |   |
   i+1 - j-1      (i+1 and j-1 form the embedded basepair)
     |   |
     i - j        (i and j form the closing basepair)
     |   |
     5'  3'
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the closing basepair
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the closing basepair
   stack37 data-arrangement:
     1. index = closingBP = code for closing basepair i-j
     2. index = enclosedBP = code for enclosed basepair i+1 and j-1. Note, basepair is reversed to preserver 5'-3' order
*/
int sr_energy(const char *s, rsize i, rsize j) {
  int closingBP = bp_index(s[i], s[j]);
  // Note, basepair is reversed to preserver 5'-3' order
  int enclosedBP = bp_index(s[j-1], s[i+1]);
  return P->stack[closingBP][enclosedBP];
}

/*
   the same as "sr_energy" but here the input is not relative to the RNA input sequence, but can be any combination of four bases
   currently used for the coaxial stacking of both stems of a pseudoknot
*/
int sr_pk_energy(char a, char b, char c, char d) {
  int closingBP = bp_index(a, b);
  // Note, basepair is reversed to preserver 5'-3' order
  int enclosedBP = bp_index(d, c);
  return P->stack[closingBP][enclosedBP];
}

/*
   returns the energy for initiating a multiloop
   version 1999: currently (12.09.2011) this value is set to 340
   version 2004: currently (12.09.2011) this value is set to 930
*/
int ml_energy(void) {
  return P->MLclosing;
}

/*
   returns the energy for initiating a stem within a multiloop
   version 1999: currently (12.09.2011) this value is set to 40
   version 2004: currently (12.09.2011) this value is set to -90
*/
int ul_energy(void) {
  return P->MLintern[0];
}

/*
   returns the energy for one unpaired base, not included into loops (hairpin-, bulge-, internal- loops), but in single stranded stretches next to closed substructures also in multiloops
   currently (12.09.2011) this value is set to 0
*/
int sbase_energy(void) {
  return 0;
}

/*
   same es sbase_energy, but for zero to n bases
   currently (12.09.2011) this value is set to 0
*/
int ss_energy(rsize i, rsize j) {
  return 0;
}


/*
   scales the energy value x into a partition function value
*/
double mk_pf(double x) {
  // temperature is defined in ViennaRNA/params/basic.h
  return exp((-1.0 * x/100.0) / (GASCONST/1000 * (temperature + K0)));
}

/*
   returns a partition function bonus for x unpaired bases
*/
double scale(int x) {
  /* mean energy for random sequences: 184.3*length cal */
  double mean_nrg = -0.1843;
  double mean_scale = exp(-1.0 * mean_nrg / (GASCONST/1000 * (
    temperature + K0)));

  return (1.0 / pow(mean_scale, x));
}

/*
   support function for dl_energy, for the case when the energy contribution must be independent of the input RNA sequence. This is the case where MacroStates has to use a n-tupel as answer type, where n reflects several possible dangling cases.
       X          (X = some closed structure)
     |   |
     i - j        (i and j form the closing basepair)
dangle   |        (dangle = 5' dangling base)
     5'  3'
   Input is
     dangle = the code for the dangling base, note: here it is not an index of the input RNA sequence!
     i = the code for the 5' partner of the closing basepair, note: here it is not an index of the input RNA sequence!
     j = the code for the 3' partner of the closing basepair, note: here it is not an index of the input RNA sequence!
*/
int dl_dangle_dg(enum base_t dangle, enum base_t i, enum base_t j) {
  int closingBP = bp_index(i, j);
  int dd = P->dangle5[closingBP][dangle];
  return (dd > 0) ? 0 : dd;  /* must be <= 0 */
}

/*
   symmetric case to dl_dangle_dg
       X          (X = some closed structure)
     |   |
     i - j        (i and j form the closing basepair)
     |    dangle  (dangle = 3' dangling base)
     5'  3'
*/
int dr_dangle_dg(enum base_t i, enum base_t j, enum base_t dangle) {
  int closingBP = bp_index(i, j);
  return P->dangle3[closingBP][dangle];
  int dd = P->dangle3[closingBP][dangle];
  return (dd > 0) ? 0 : dd;  /* must be <= 0 */
}

// added by gsauthof, 2012

static const bool map_base_iupac[5][13] = {
  /*      {N   , A    , C    , G    , U    , _    , +    , B    , D    ,
           H    , R    , V    , Y    }, */
  /* N */ {true, true , true , true , true , true , false, true , true ,
           true , true , true , true },
  /* A */ {true, true , false, false, false, false, false, false, true ,
           true , true , true , false},
  /* C */ {true, false, true , false, false, false, false, true , false,
           true , false, true , true },
  /* G */ {true, false, false, true , false, false, false, true , true ,
           false, true , true , false},
  /* U */ {true, false, false, false, true , false, false, true , true ,
           true , false, false, true },
};

bool iupac_match(char base, char iupac_base) {
  assert(base >= 0);
  assert(base < 6);
  assert(iupac_base >= 0);
  assert(iupac_base < 13);
  return map_base_iupac[base][iupac_base];
}

#ifndef LOOKUP

/*
   Next downstream base is not necessarily the next character in s. There might be one or more GAPs between both. Thus getNext jumps over GAPs to the steps-next non GAP base in s. This is restricted by left and right borders "pos" and "rightBorder".
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     pos = startpoint of search for right neighboring non-GAP base.
     steps = sometimes we don't need the direct neighboring base, but also the second, third, ... neighboring non GAP base.
     rightBorder = last character position to look for right neighboring non-GAP bases.
*/
static rsize getNext(const char *s, rsize pos, rsize steps, rsize rightBorder) {
  assert(steps > 0);
  rsize nongaps = 0;
  rsize x = pos+1;
  if (x >= rightBorder)
    return rightBorder;

  do {
    if (s[x] != GAP_BASE)
      ++nongaps;
    if (s[x] == SEPARATOR_BASE)
      return x-1;
  } while (nongaps < steps && ++x < rightBorder);

  return x;
}

/*
   Next upstream base is not necessarily the previous character in s. There might be one or more GAPs between both. Thus getNext jumps over GAPs to the step-previous non GAP base in s. This is restricted by left and right borders "leftBorder" and "pos".
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     pos = last character position to look for left neighboring non-GAP bases.
     steps = sometimes we don't need the direct neighboring base, but also the second, third, ... neighboring non GAP base.
     leftBorder = startpoint of search for left neighboring non-GAP base.
*/
static rsize getPrev(const char *s, rsize pos, rsize steps, rsize leftBorder) {
  assert(pos >= 0);
  assert(steps > 0);
  rsize x = pos-1;

  if ((pos <= leftBorder) || (x <= leftBorder))
    return leftBorder;

  rsize nongaps = 0;

  do {
    if (s[x] != GAP_BASE)
      ++nongaps;
    if (s[x] == SEPARATOR_BASE)
      return x+1;
  } while (nongaps < steps && --x > leftBorder);

  return x;
}
/* ============== END: Alignment functions ================= */

/*
   returns stabilizing energy values for the outermost bases of a hairpin loop, which stack onto the closing base pair
   the outermost bases of the hairpin loop do not form a basepair, thus they are a mismatch
          . . .
        .       .
        .       .
       i+1     j-1
          i - j        (i and j form the closing basepair)
          |   |
          5'  3'
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the hairpin closing basepair
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the hairpin closing basepair
   mismatchH37 data-arrangement:
     1. index = bp = code for closing basepair i-j
     2. index = lbase = code for 5' base i+1 stacking on closing basepair i-j
     3. index = rbase = code for 3' base j-1 stacking on closing basepair i-j
*/

static int hl_stack(const char *s, rsize i, rsize j) {
  int bp = bp_index(s[i], s[j]);
  char lbase = s[getNext(s, i, 1, j-1)];
  char rbase = s[getPrev(s, j, 1, i+1)];
  return P->mismatchH[bp][lbase][rbase];
}

/*
   returns the energy contribution for hairpin loops, i.e one closing basepair with an embedded unpaired loop region (of 3 bases, at least).
          . . .
        .       .
        .       .
       i+1     j-1
          i - j        (i and j form the closing basepair)
          |   |
          5'  3'
   The energy contribution is composed of:
    a) the stabilizing effect of stacking the outermost bases of the loop onto the enclosing basepair, i.e. "hl_stack": i+1 and j-1 stack onto the pair i-j
    b) the destabilizing effect of the loop region "hl_ent"
    c) the destabilizing effect of the closing basepair i-j if it is AU, UA, GU or UG "termau_energy" (partially included in "hl_stack")
   Prior to these three effects, there exist special energy values for some ultrastable tetra-loops (tri-, tetra- and hexa-loops in 2004 version) (referring to the size of the loop region) "hl_tetra"
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the hairpin closing basepair
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the hairpin closing basepair
*/
int hl_energy(const char *s, rsize i, rsize j) {
  assert(j-i > 1);

  // if we are in an alignment, it might be true, that 5' partner of closing
  // basepair is a gap from the start up to this position --> end gap
  char lbase = s[getPrev(s, i+1, 1, 0)];
  if ((lbase == GAP_BASE) || (lbase == SEPARATOR_BASE)) {
    return 0;
  }

  rsize size = j-i-1 - noGaps(s, i+1, j-1);

  // destabilizing energy for the unpaired region in correlation to its length
  int entropy = hl_ent(size);

  // stabilizing energy for stacking bases
  int stack_mismatch = hl_stack(s, i, j);

  // handling for hairpin loops in alignments, where the sequence is
  // completely missing
  if (size < 3) {
    return 600;
  }

  // test for special loop cases, i.e. Tri-, Tetra- and Hexa-loops. Wired
  // comparison stems from the Vienna Package: H/loop_energies.h method
  // "E_Hairpin()"
  if (size == 3 || size == 4 || size == 6) {
    // loop type depends on ungapped loop sequence
    char ungapped[j-i+1];
    int sizeUngapped = ungapRegion(s, i, j, ungapped);
        char loop[sizeUngapped+1];
        loop[sizeUngapped] = 0;
    decode(loop, ungapped, sizeUngapped);
    if (sizeUngapped == 3+2) {
      // special triloop cases
      char tl[6] = {0, 0, 0, 0, 0, 0}, *ts;
      strncpy(tl, loop, 5);
      if ((ts=strstr(P->Triloops, tl))) {
        return (P->Triloop_E[(ts - P->Triloops)/6]);
      }
    } else if (sizeUngapped == 4+2) {
      // special tetraloop cases
      char tl[7]={0}, *ts;
      strncpy(tl, loop, 6);
      if ((ts=strstr(P->Tetraloops, tl))) {
        return (P->Tetraloop_E[(ts - P->Tetraloops)/7]);
      }
    } else if (sizeUngapped == 6+2) {
      // special hexaloop cases
      char tl[9]={0}, *ts;
      strncpy(tl, loop, 8);
      if ((ts=strstr(P->Hexaloops, tl))) {
        return (P->Hexaloop_E[(ts - P->Hexaloops)/9]);
      }
    }
  }

  if (size == 3) {
    // normal hairpins of loop size 3
    return entropy + termau_energy(s, i, j);
  } else {
    // normal hairpins of loop sizes larger than three
    return entropy + stack_mismatch;
  }

  // throw a warning to Bellman's GAP user, if they forget to restrict the
  // loop region to sizes larger than two
  fprintf(stderr, "hairpin loop < 3 found. Please use production\n");
  fprintf(stderr, "hl(BASE, REGION with minsize(3), BASE)\n");
  fprintf(stderr, "in your grammar.\n");
  assert(0);
  abort();
}

/* like hl_energy, just no penalty for size > 4 structures */
int hl_energy_stem(const char *s, rsize i, rsize j) {
  int r = hl_energy(s, i, j);
  rsize size = j-i-1 - noGaps(s, i+1, j-1);
  if (size >= 4) {
    int stack_mismatch = hl_stack(s, i, j);
    return r - stack_mismatch;
  }
  return r;
}

/*
   returns the energy of an internal loop with exactly one unpaired base on 5' and 3' side.
       X     (X = some closed structure)
     |   |
   i+2 - j-2      (i+2 and j-2 form the embedded basepair)
 i+1       j-1    (i+1 and j-1 are the unpaired bases in the internal loop)
     i - j        (i and j form the closing basepair)
     |   |
     5'  3'
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the internal loop closing basepair
     k = non-GAP-case: == i+2, GAP-case: 5' partner of basal X basepair. We need this to not look to much downstream for the enclosedBP, because sometimes it might be missing due to gaps, sometimes just the loop ist extended by a few gaps.
     l = non-GAP-case: == j-2, GAP-case: 3' partner of basal X basepair. We need this to not look to much upstream for the enclosedBP, because sometimes it might be missing due to gaps, sometimes just the loop ist extended by a few gaps.
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the internal loop closing basepair
   int11_37 data-arrangement:
     1. index = closingBP = code for closing basepair i-j
     2. index = enclosedBP = code for enclosed basepair, i.e. the fist basepair of the embedded substructure, i+2 - j-2.
     3. index = lbase = code for 5' unpaired base of the internal loop
     4. index = rbase = code for 3' unpaired base of the internal loop
*/
int il11_energy(const char *s, rsize i, rsize k, rsize l, rsize j) {
  int closingBP = bp_index(s[i], s[j]);
  // we know that the enclosed base pair is at exactly this position, since
  // both unpaired regions have size 1.  Note, basepair is reversed to
  // preserver 5'-3' order.
  int enclosedBP = bp_index(s[getPrev(s, j, 2, l)], s[getNext(s, i, 2, k)]);
  char lbase = s[getNext(s, i, 1, k)];
  char rbase = s[getPrev(s, j, 1, l)];
  return P->int11[closingBP][enclosedBP][lbase][rbase];
}

/*
   returns the energy of an internal loop with exactly one unpaired base on 5' and two unpaired bases on 3' side.
       X     (X = some closed structure)
     |   |
   i+2 - j-3      (i+2 and j-3 form the embedded basepair)
 i+1       j-2    (i+1, j-1 and j-2 are the unpaired bases in the internal loop)
           j-1
     i - j        (i and j form the closing basepair)
     |   |
     5'  3'
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the internal loop closing basepair
     k = non-GAP-case: == i+2, GAP-case: 5' partner of basal X basepair. We need this to not look to much downstream for the enclosedBP, because sometimes it might be missing due to gaps, sometimes just the loop ist extended by a few gaps.
     l = non-GAP-case: == j-3, GAP-case: 3' partner of basal X basepair. We need this to not look to much upstream for the enclosedBP, because sometimes it might be missing due to gaps, sometimes just the loop ist extended by a few gaps.
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the internal loop closing basepair
   int21_37 data-arrangement:
     1. index = closingBP = code for closing basepair i-j
     2. index = enclosedBP = code for enclosed basepair, i.e. the fist basepair of the embedded substructure, i+2 - j-3.
     3. index = lbase = code for single 5' unpaired base of the internal loop, i+1
     4. index = rbase = code for first (= close to the embedded substructure) 3' unpaired base of the internal loop, j-2
     5. index = rrbase = code for second (= close to the i-j basepair) 3' unpaired base of the internal loop, j-1
*/
int il12_energy(const char *s, rsize i, rsize k, rsize l, rsize j) {
  int closingBP = bp_index(s[i], s[j]);
  // Note, basepair is reversed to preserver 5'-3' order
  int enclosedBP = bp_index(s[getPrev(s, j, 3, l)], s[getNext(s, i, 2, k)]);
  char lbase = s[getNext(s, i, 1, k)];
  char rbase = s[getPrev(s, j, 2, l)];
  char rrbase = s[getPrev(s, j, 1, l)];
  return P->int21[closingBP][enclosedBP][lbase][rbase][rrbase];
}

/*
   symmetric case to il12_energy
*/
int il21_energy(const char *s, rsize i, rsize k, rsize l, rsize j) {
  // Note, basepair is reversed to preserver 5'-3' order
  int closingBP = bp_index(s[getPrev(s, j, 2, l)], s[getNext(s, i, 3, k)]);
  int enclosedBP = bp_index(s[i], s[j]);
  char lbase = s[getPrev(s, j, 1, l)];
  char rbase = s[getNext(s, i, 1, k)];
  char rrbase = s[getNext(s, i, 2, k)];
  return P->int21[closingBP][enclosedBP][lbase][rbase][rrbase];
}

/*
   returns the energy of an internal loop with exactly two unpaired base on 5' and 3' side.
       X          (X = some closed structure)
     |   |
   i+3 - j-3      (i+3 and j-3 form the embedded basepair)
 i+2       j-2    (i+2 and j-2 are the unpaired bases in the internal loop)
 i+1       j-1    (i+1 and j-1 are the unpaired bases in the internal loop)
     i - j        (i and j form the closing basepair)
     |   |
     5'  3'
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the internal loop closing basepair
     k = non-GAP-case: == i+3, GAP-case: 5' partner of basal X basepair. We need this to not look to much downstream for the enclosedBP, because sometimes it might be missing due to gaps, sometimes just the loop ist extended by a few gaps.
     l = non-GAP-case: == j-3, GAP-case: 3' partner of basal X basepair. We need this to not look to much upstream for the enclosedBP, because sometimes it might be missing due to gaps, sometimes just the loop ist extended by a few gaps.
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the internal loop closing basepair
   int22_37 data-arrangement:
     1 = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     2. index = closingBP = code for closing basepair i-j
     3. index = enclosedBP = code for enclosed basepair, i.e. the fist basepair of the embedded substructure, i+2 - j-2.
     4. index = lbase = code for first (= closer to the closing basepair) 5' unpaired base of the internal loop, i+1
     5. index = llbase = code for second (= closer to the embedded substructure) 5' unpaired base of the internal loop, i+2
     6. index = rbase = code for first (= closer to the embedded substructure) 3' unpaired base of the internal loop, j-2
     7. index = rrbase = code for second (= closer to the closing basepair) 3' unpaired base of the internal loop, j-1
*/
int il22_energy(const char *s, rsize i, rsize k, rsize l, rsize j) {
  int closingBP = bp_index(s[i], s[j]);
  // Note, basepair is reversed to preserver 5'-3' order
  int enclosedBP = bp_index(s[getPrev(s, j, 3, l)], s[getNext(s, i, 3, k)]);
  char lbase = s[getNext(s, i, 1, k)];
  char llbase = s[getNext(s, i, 2, k)];
  char rbase = s[getPrev(s, j, 2, l)];
  char rrbase = s[getPrev(s, j, 1, l)];
  return P->int22[closingBP][enclosedBP][lbase][llbase][rbase][rrbase];
}

/*
   returns destabilizing energy values for unpaired loops smaller then MAXLOOP bases in internal loops
   for larger loops jacobson_stockmayer is used.
   currently (11.09.2011) MAXLOOP is 30
   Input is just the size, i.e. number of bases, of the unpaired loop region: l
*/


/*
   returns the stabilizing energy for the outermost bases of the unpaired regions stacking on the closing and the embedded basepair of the internal loop
   the outermost bases of the internal loops do not form a basepair, thus they are mismatches
       X          (X = some closed structure)
     |   |
     k - l        (k and l form the embedded basepair)
 k-1       l+1    (k-1 and l+1 are the unpaired bases in the internal loop)
  .         .
 i+1       j-1    (i+1 and j-1 are the unpaired bases in the internal loop)
     i - j        (i and j form the closing basepair)
     |   |
     5'  3'
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the internal loop closing basepair
     k = the index (first base of s is 0, second 1, ...) of the 5' partner of the internal loop embedded basepair.
     l = the index (first base of s is 0, second 1, ...) of the 3' partner of the internal loop embedded basepair.
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the internal loop closing basepair
   mismatchI37 data-arrangement:
     1. index = out_closingBP = in_closingBP = code for closing basepair i-j
     2. index = out_lbase = in_lbase = code for first (= closer to the closing basepair) 5' unpaired base of the internal loop, i+1
     3. index = out_rbase = in_rbase = code for second (= closer to the closing basepair) 3' unpaired base of the internal loop, j-1
*/
int il_stack(const char *s, rsize i, rsize k, rsize l, rsize j) {
  int out_closingBP = bp_index(s[i], s[j]);
  char out_lbase = s[getNext(s, i, 1, j-1)];
  char out_rbase = s[getPrev(s, j, 1, i+1)];
  // Note, basepair and stacking bases are reversed to preserver 5'-3' order
  int in_closingBP = bp_index(s[l], s[k]);
  char in_lbase = s[getNext(s, l, 1, j-1)];
  char in_rbase = s[getPrev(s, k, 1, i+1)];
  return P->mismatchI[out_closingBP][out_lbase][out_rbase] +
         P->mismatchI[in_closingBP][in_lbase][in_rbase];
}

/*
   returns the energy contribution of an internal loop, i.e. two basepairs forming a stem which has bulged out bases on 5' and 3' side.
       X          (X = some closed structure)
     |   |
     k - l        (k and l form the embedded basepair)
 k-1       l+1    (k-1 and l+1 are the unpaired bases in the internal loop)
  .         .
 i+1       j-1    (i+1 and j-1 are the unpaired bases in the internal loop)
     i - j        (i and j form the closing basepair)
     |   |
     5'  3'
   The energy contribution is composed of:
    a) the stabilizing effect of stacking the outermost bases of the loop regions onto the closing or embedded basepair, i.e. "il_stack" i+1 and j-1 stack onto the pair i-j AND k-1 and l+1 stack onto the pair k-l
    b) the destabilizing effect of the loop regions "il_ent"
    c) the destabilizing effect of asymmetric loop sizes "il_asym"
   Prior to these three effects, there exist energy values for some special cases
    1x1 internal loops, i.e. internal loop with exactly one bulged base on both sides: "il11_energy"
    1x2 internal loops: "il12_energy"
    1xn internal loops with n > 2: "mismatch1nI37" and others (only for T2004)
    2x1 internal loops: "il21_energy"
    2x2 internal loops: "il22_energy"
    2x3 internal loops: "mismatch23I37" and others (only for T2004)
    3x2 internal loops: "mismatch23I37" and others (only for T2004)
    nx1 internal loops with n > 2: "mismatch1nI37" and others (only for T2004)
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the internal loop closing basepair
     k = the index (first base of s is 0, second 1, ...) of the 5' partner of the internal loop embedded basepair.
     l = the index (first base of s is 0, second 1, ...) of the 3' partner of the internal loop embedded basepair.
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the internal loop closing basepair
*/
int il_energy(const char *s, rsize i, rsize k, rsize l, rsize j) {
  rsize sl = k-i-1 - noGaps(s, i+1, k-1);
  rsize sr = j-l-1 - noGaps(s, l+1, j-1);

  int out_closingBP = bp_index(s[i], s[j]);
  int out_lbase = s[getNext(s, i, 1, j-1)];
  int out_rbase = s[getPrev(s, j, 1, i+1)];
  // Note, basepair and stacking bases are reversed to preserver 5'-3' order
  int in_closingBP = bp_index(s[l], s[k]);
  int in_lbase = s[getNext(s, l, 1, j-1)];
  int in_rbase = s[getPrev(s, k, 1, i+1)];

  // internal loop really is an right bulge, because left unpaired region is
  // just a gap
  if (sl == 0) return br_energy(s, i, l+1, j-1, j, k);
  // internal loop really is an left bulge, because right unpaired region is
  // just a gap
  if (sr == 0) return bl_energy(s, i, i+1, k-1, j, l);


  if (sl == 1) {
    if (sr == 1) {
      return il11_energy(s, i, k, l, j);
    } else if (sr == 2) {
      return il12_energy(s, i, k, l, j);
    } else {
      return il_ent(sl+sr) + il_asym(sl, sr) +
        P->mismatch1nI[out_closingBP][out_lbase][out_rbase] +
        P->mismatch1nI[in_closingBP][in_lbase][in_rbase];
    }
  } else if (sl == 2) {
    if (sr == 1) {
      return il21_energy(s, i, k, l, j);
    } else if (sr == 2) {
      return il22_energy(s, i, k, l, j);
    } else if (sr == 3) {
      return P->internal_loop[5]+P->ninio[2] +
        P->mismatch23I[out_closingBP][out_lbase][out_rbase] +
        P->mismatch23I[in_closingBP][in_lbase][in_rbase];
    }
  } else if ((sl == 3) && (sr == 2)) {
    return P->internal_loop[5]+P->ninio[2] +
      P->mismatch23I[out_closingBP][out_lbase][out_rbase] +
      P->mismatch23I[in_closingBP][in_lbase][in_rbase];
  } else {
    if (sr == 1) {
      return il_ent(sl+sr) + il_asym(sl, sr) +
        P->mismatch1nI[out_closingBP][out_lbase][out_rbase] +
        P->mismatch1nI[in_closingBP][in_lbase][in_rbase];
    }
  }

  return il_ent(sl+sr) + il_stack(s, i, k, l, j) + il_asym(sl, sr);
}

/*
   returns the energy contribution of a left bulge loop, i.e. two basepairs forming a stem which has bulged out bases on 5' side.
       X          (X = some closed structure)
     |   |
   l+1 - j-1      (l+1 and j-1 form the embedded basepair)
  l      |        (l is the last unpaired bases in the bulge loop)
  .      |
  k      |        (k is the first the unpaired bases in the bulge loop)
     i - j        (i and j form the closing basepair)
     |   |
     5'  3'
   The energy contribution is composed of:
    a) the destabilizing effect of the loop region "bl_ent"
    b) the destabilizing effect of the closing basepair i-j if it is AU, UA, GU or UG "termau_energy"
    c) the destabilizing effect of the embedded basepair l+1 - j-1 if it is AU, UA, GU or UG "termau_energy"
   Prior to these three effects, there exist energy values for the special case if exactly one base is bulged, then b) and c) are replaced by the energy of stacking closing- and embedded basepair
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the internal loop closing basepair
     k = the index (first base of s is 0, second 1, ...) of the first unpaired base of the 5' bulge region
     l = the index (first base of s is 0, second 1, ...) of the last unpaired base of the 5' bulge region
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the internal loop closing basepair
     Xright = for GAP case: the 3' partner of the enclosed basepair might not be at j-1 if we consider gaps. We have two possibilities: either it is seperated from j by one or more GAPs, or it might even be missing at all. Thus we have to know where the closed substructure X has its right border.
*/
int bl_energy(const char *s, rsize i, rsize k, rsize l, rsize j, rsize Xright) {
  // this is of no biological relevance, just to avoid an underflow
  assert(j >= 2);

  rsize size = l-k+1 - noGaps(s, k, l);

  if (size == 0) {
    int closingBP = bp_index(s[i], s[j]);
    // Note, basepair is reversed to preserver 5'-3' order
    int enclosedBP = bp_index(s[getPrev(s, j, 1, Xright)], s[l+1]);
    return P->stack[closingBP][enclosedBP];
  }
  if (size == 1) {
    int closingBP = bp_index(s[i], s[j]);
    // Note, basepair is reversed to preserver 5'-3' order
    int enclosedBP = bp_index(s[getPrev(s, j, 1, Xright)], s[l+1]);
    return bl_ent(size) + P->stack[closingBP][enclosedBP];
  }
  if (size > 1) {
    return bl_ent(size) + termau_energy(s, i, j) +
      termau_energy(s, getPrev(s, j, 1, Xright), l+1);
  }

  fprintf(stderr, "bl_energy size < 1\n");
  assert(0);
}

/*
   symmetric case to "bl_energy"
       X          (X = some closed structure)
     |   |
   i+1 - k-1      (i+1 and k-1 form the embedded basepair)
     |      k     (k is the first unpaired bases in the bulge loop)
     |      .
     |      l     (l is the last the unpaired bases in the bulge loop)
     i - j        (i and j form the closing basepair)
     |   |
     5'  3'
*/
int br_energy(const char *s, rsize i, rsize k, rsize l, rsize j, rsize Xleft) {
  // this is of no biological relevance, just to avoid an underflow
  assert(j >= 1);

  rsize size = l-k+1 - noGaps(s, k, l);

  if (size == 0) {
    int closingBP = bp_index(s[i], s[j]);
    // Note, basepair is reversed to preserver 5'-3' order
    int enclosedBP = bp_index(s[k-1], s[getNext(s, i, 1, Xleft)]);
    return P->stack[closingBP][enclosedBP];
  }
  if (size == 1) {
    int closingBP = bp_index(s[i], s[j]);
    // Note, basepair is reversed to preserver 5'-3' order
    int enclosedBP = bp_index(s[k-1], s[getNext(s, i, 1, Xleft)]);
    return bl_ent(size) + P->stack[closingBP][enclosedBP];
  }
  if (size > 1) {
    return bl_ent(size) + termau_energy(s, i, j) +
      termau_energy(s, k-1, getNext(s, i, 1, Xleft));
  }

  fprintf(stderr, "br_energy size < 1\n");
  assert(0);
}

/*
   returns the energy contribution of a single base left to a stem which dangles on this stem from the outside
       X          (X = some closed structure)
     |   |
     i - j        (i and j form the closing basepair)
  i-1    |        (5' dangling base)
     5'  3'
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the closing basepair
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the closing basepair
   dangle5_37 data-arrangement:
     1. index = closingBP = code for closing basepair i-j
     2. index = dbase = code for dangling base
*/
int dl_energy(const char *s, rsize i, rsize j) {
  if (i == 0) return 0;
  int closingBP = bp_index(s[i], s[j]);
  char dbase = s[getPrev(s, i, 1, 0)];
  // RNAfold treats a gap like a N-base that is dangling on the pair.
  // Strange but true.
  if (dbase == GAP_BASE) dbase = N_BASE;
  int dd = P->dangle5[closingBP][dbase];
  return (dd > 0) ? 0 : dd;  /* must be <= 0 */
}

/*
   symmetric case to dl_energy, but here the dangling base is on the 3' side of the stem
       X          (X = some closed structure)
     |   |
     i - j        (i and j form the closing basepair)
     |    j+1     (3' dangling base)
     5'  3'
   Input is
     in addition: n = length of the input RNA sequence (necessary for OverDangle, should there be no more base on the right side of a stem)
*/
int dr_energy(const char *s, rsize i, rsize j, rsize n) {
  if ((j+1) >= n) return 0;
  int closingBP = bp_index(s[i], s[j]);
  char dbase = s[getNext(s, j, 1, n-1)];
  // RNAfold treats a gap like a N-base that is dangling on the pair.
  // Strange but true.
  if (dbase == GAP_BASE) dbase = N_BASE;
  int dd = P->dangle3[closingBP][dbase];
  return (dd > 0) ? 0 : dd;  /* must be <= 0 */
}

/*
   similar to dl_energy, but here the base dangles from the inside onto the stem. This happens only in multiloops.
      X Y         (X and Y = some closed structures)
  i+1    |        (5' base, dangling onto the inner terminal basepair of a multiloop-closing stem)
     i - j        (i and j form the closing basepair)
     |   |
     5'  3'
*/
int dli_energy(const char *s, rsize i, rsize j) {
  // Note, basepair is reversed to preserver 5'-3' order
  int closingBP = bp_index(s[j], s[i]);
  char dbase = s[getNext(s, i, 1, j-1)];
  int dd = P->dangle3[closingBP][dbase];
  return (dd > 0) ? 0 : dd;  /* must be <= 0 */
}

/*
   symmetric case to dli_energy, but here the base which dangles onto a multiloop closing stem from inside is on 3' side
      X Y         (X and Y = some closed structures)
     |    j-1     (3' base, dangling onto the inner terminal basepair of a multiloop-closing stem)
     i - j        (i and j form the closing basepair)
     |   |
     5'  3'
*/
int dri_energy(const char *s, rsize i, rsize j) {
  // Note, basepair is reversed to preserver 5'-3' order
  int closingBP = bp_index(s[j], s[i]);
  char dbase = s[getPrev(s, j, 1, i+1)];
  int dd = P->dangle5[closingBP][dbase];
  return (dd > 0) ? 0 : dd;  /* must be <= 0 */
}

/*
   returns the energy contribution of two bases dangling from the outside onto a stem, but do not form a basepair
       X          (X = some closed structure)
     |   |
     i - j        (i and j form the closing basepair)
  i-1     j+1     (5' and 3' dangling bases)
     5'  3'
   Input is
     s = the input RNA sequence in bit encoding, i.e. N=0, A=1, C=2, G=3, U=4, GAP=5 (see /usr/include/librna/rnalib.h base_t)
     i = the index (first base of s is 0, second 1, ...) of the 5' partner of the closing basepair
     j = the index (first base of s is 0, second 1, ...) of the 3' partner of the closing basepair
     n = length of the input RNA sequence (necessary for OverDangle, should there be no more base on the right side of a stem)
   mismatchExt37 data-arrangement:
     1. index = closingBP = code for closing basepair i-j
     2. index = lbase = code for dangling 5' base
     3. index = rbase = code for dangling 3' base
*/
int ext_mismatch_energy(const char *s, rsize i, rsize j, rsize n) {
  int rbasePos = getNext(s, j, 1, n-1);
  // we try to dangle left and right neighboring base if available. They are
  // not if we already are at first or last base ...
  if ((i > 0) && ((j+1) < n) &&
      (s[i-1] != SEPARATOR_BASE) && ((j+1) <= rbasePos)) {
    // or in outside mode, if left / right neighbor is the separator base
    int closingBP = bp_index(s[i], s[j]);
    int lbase = s[getPrev(s, i, 1, 0)];
    int rbase = s[rbasePos];
    if (lbase == GAP_BASE) lbase = N_BASE;
    if (rbase == GAP_BASE) rbase = N_BASE;
    if ((lbase <  5) && (rbase <  5)) {
      // left and right dangling positions are real bases
      return P->mismatchExt[closingBP][lbase][rbase];
    }
    if ((lbase <  5) && (rbase >= 5)) {
      // if right position is a gap and left position is a real base, we resort
      // to a left dangle
      return dl_energy(s, i, j);
    }
    if ((lbase >= 5) && (rbase <  5)) {
      // if left position is a gap and right position is a real base, we resort
      // to a right dangle
      return dr_energy(s, i, j, n);
    }
    if ((lbase >= 5) && (rbase >= 5)) {
      // if left and right positions are gaps, we just return 0;
      return 0;
    }
  } else {
    if ((i > 0) && (s[i-1] != SEPARATOR_BASE)) {
      return dl_energy(s, i, j);
    }
    if (((j+1) < n) && ((j+1) <= rbasePos)) {
      return dr_energy(s, i, j, n);
    }
    return 0;
  }
}

/*
   same as ext_mismatch_energy but for two bases dangling on the inside of a multiloop stem
      X Y         (X and Y = some closed structures)
  i+1     j-1     (5' and 3' base, dangling onto the inner terminal basepair of a multiloop-closing stem)
     i - j        (i and j form the closing basepair)
     |   |
     5'  3'
*/
int ml_mismatch_energy(const char *s, rsize i, rsize j) {
  // Note, basepairs and stacking bases are reversed to preserver 5'-3' order
  int closingBP = bp_index(s[j], s[i]);
  int lbase = s[getPrev(s, j, 1, i+1)];
  int rbase = s[getNext(s, i, 1, j-1)];
  if ((lbase <  5) && (rbase <  5)) {
    // left and right dangling positions are real bases
    return P->mismatchM[closingBP][lbase][rbase];
  }
  if ((lbase <  5) && (rbase >= 5)) {
    // if right position is a gap and left position is a real base, we resort
    // to a left dangle
    return dli_energy(s, i, j);
  }
  if ((lbase >= 5) && (rbase <  5)) {
    // if left position is a gap and right position is a real base, we resort
    // to a right dangle
    return dri_energy(s, i, j);
  }
  if ((lbase >= 5) && (rbase >= 5)) {
    // if left and right positions are gaps, we just return 0;
    return 0;
  }
}

#else

#include <stdbool.h>

static rsize getNext(const char *s, rsize pos, rsize steps,
                     rsize rightBorder, unsigned currRow, unsigned maxRows) {
  assert(steps > 0);

  rsize x = pos + 1;
  if (x >= rightBorder)
    return rightBorder;

  rsize nongaps = 0;

  static bool init = false;
  static rsize *lookup;
  static unsigned seqLen;
  static unsigned triu;

  if (!init) {
    seqLen = strlen(s);
    triu = (seqLen * (seqLen + 1)) / 2 + 1;
    unsigned long size = maxRows * triu * 3;
    lookup = calloc(size, sizeof(rsize));
    init = true;
  }

  unsigned long index = (steps - 1) * triu * maxRows + triu * currRow +
                        pos * seqLen + rightBorder -
                        ((pos * (pos + 1)) / 2);

  if (lookup[index]) {
    return lookup[index];
  }

  do {
    if (s[x] != GAP_BASE)
      ++nongaps;
    if (s[x] == SEPARATOR_BASE) {
      lookup[index] = x - 1;
      return x - 1;
    }
  } while (nongaps < steps && ++x < rightBorder);

  lookup[index] = x;

  return x;
}

static rsize getPrev(const char *s, rsize pos, rsize steps, rsize leftBorder,
                     unsigned currRow, unsigned maxRows) {
  assert(pos >= 0);
  assert(steps > 0);
  rsize x = pos - 1;

  if ((pos <= leftBorder) || (x <= leftBorder))
    return leftBorder;

  rsize nongaps = 0;

  static bool init = false;
  static rsize *lookup;
  static unsigned seqLen;
  static unsigned triu;

  if (!init) {
    seqLen = strlen(s);
    triu = (seqLen * (seqLen + 1)) / 2 + 1;
    unsigned long size = maxRows * triu * 3;
    lookup = calloc(size, sizeof(rsize));
    init = true;
  }

  unsigned long index = triu * (steps - 1) * maxRows + currRow * triu +
                        leftBorder * seqLen + pos -
                        ((leftBorder * (leftBorder + 1)) / 2);

  if (lookup[index]) {
    return lookup[index];
  }

  do {
    if (s[x] != GAP_BASE)
      ++nongaps;
    if (s[x] == SEPARATOR_BASE) {
      lookup[index] = x + 1;
      return x + 1;
    }
  } while (nongaps < steps && --x > leftBorder);

  lookup[index] = x;

  return x;
}

static int hl_stack(const char *s, rsize i, rsize j,
                    unsigned currRow, unsigned maxRows) {
  int bp = bp_index(s[i], s[j]);
  char lbase = s[getNext(s, i, 1, j-1, currRow, maxRows)];
  char rbase = s[getPrev(s, j, 1, i+1, currRow, maxRows)];
  return P->mismatchH[bp][lbase][rbase];
}

int hl_energy(const char *s, rsize i, rsize j,
              unsigned currRow, unsigned maxRows) {
  assert(j-i > 1);

  // if we are in an alignment, it might be true, that 5' partner of closing
  // basepair is a gap from the start up to this position --> end gap
  char lbase = s[getPrev(s, i+1, 1, 0, currRow, maxRows)];
  if ((lbase == GAP_BASE) || (lbase == SEPARATOR_BASE)) {
    return 0;
  }

  rsize size = j-i-1 - noGaps(s, i+1, j-1);

  // destabilizing energy for the unpaired region in correlation to its length
  int entropy = hl_ent(size);

  // stabilizing energy for stacking bases
  int stack_mismatch = hl_stack(s, i, j, currRow, maxRows);

  // handling for hairpin loops in alignments, where the sequence is
  // completely missing
  if (size < 3) {
    return 600;
  }

  // test for special loop cases, i.e. Tri-, Tetra- and Hexa-loops. Wired
  // comparison stems from the Vienna Package: H/loop_energies.h method
  // "E_Hairpin()"
  if (size == 3 || size == 4 || size == 6) {
    // loop type depends on ungapped loop sequence
    char ungapped[j-i+1];
    int sizeUngapped = ungapRegion(s, i, j, ungapped);
        char loop[sizeUngapped+1];
        loop[sizeUngapped] = 0;
    decode(loop, ungapped, sizeUngapped);
    if (sizeUngapped == 3+2) {
      // special triloop cases
      char tl[6] = {0, 0, 0, 0, 0, 0}, *ts;
      strncpy(tl, loop, 5);
      if ((ts=strstr(P->Triloops, tl))) {
        return (P->Triloop_E[(ts - P->Triloops)/6]);
      }
    } else if (sizeUngapped == 4+2) {
      // special tetraloop cases
      char tl[7]={0}, *ts;
      strncpy(tl, loop, 6);
      if ((ts=strstr(P->Tetraloops, tl))) {
        return (P->Tetraloop_E[(ts - P->Tetraloops)/7]);
      }
    } else if (sizeUngapped == 6+2) {
      // special hexaloop cases
      char tl[9]={0}, *ts;
      strncpy(tl, loop, 8);
      if ((ts=strstr(P->Hexaloops, tl))) {
        return (P->Hexaloop_E[(ts - P->Hexaloops)/9]);
      }
    }
  }

  if (size == 3) {
    // normal hairpins of loop size 3
    return entropy + termau_energy(s, i, j);
  } else {
    // normal hairpins of loop sizes larger than three
    return entropy + stack_mismatch;
  }

  // throw a warning to Bellman's GAP user, if they forget to restrict the
  // loop region to sizes larger than two
  fprintf(stderr, "hairpin loop < 3 found. Please use production\n");
  fprintf(stderr, "hl(BASE, REGION with minsize(3), BASE)\n");
  fprintf(stderr, "in your grammar.\n");
  assert(0);
  abort();
}

int il11_energy(const char *s, rsize i, rsize k, rsize l, rsize j,
                unsigned currRow, unsigned maxRows) {
  int closingBP = bp_index(s[i], s[j]);
  // we know that the enclosed base pair is at exactly this position, since
  // both unpaired regions have size 1.  Note, basepair is reversed to
  // preserver 5'-3' order.
  int enclosedBP = bp_index(s[getPrev(s, j, 2, l, currRow, maxRows)],
                            s[getNext(s, i, 2, k, currRow, maxRows)]);
  char lbase = s[getNext(s, i, 1, k, currRow, maxRows)];
  char rbase = s[getPrev(s, j, 1, l, currRow, maxRows)];
  return P->int11[closingBP][enclosedBP][lbase][rbase];
}

int il12_energy(const char *s, rsize i, rsize k, rsize l, rsize j,
                unsigned currRow, unsigned maxRows) {
  int closingBP = bp_index(s[i], s[j]);
  // Note, basepair is reversed to preserver 5'-3' order
  int enclosedBP = bp_index(s[getPrev(s, j, 3, l, currRow, maxRows)],
                            s[getNext(s, i, 2, k, currRow, maxRows)]);
  char lbase = s[getNext(s, i, 1, k, currRow, maxRows)];
  char rbase = s[getPrev(s, j, 2, l, currRow, maxRows)];
  char rrbase = s[getPrev(s, j, 1, l, currRow, maxRows)];
  return P->int21[closingBP][enclosedBP][lbase][rbase][rrbase];
}

int il21_energy(const char *s, rsize i, rsize k, rsize l, rsize j,
               unsigned currRow, unsigned maxRows) {
  // Note, basepair is reversed to preserver 5'-3' order
  int closingBP = bp_index(s[getPrev(s, j, 2, l, currRow, maxRows)],
                           s[getNext(s, i, 3, k, currRow, maxRows)]);
  int enclosedBP = bp_index(s[i], s[j]);
  char lbase = s[getPrev(s, j, 1, l, currRow, maxRows)];
  char rbase = s[getNext(s, i, 1, k, currRow, maxRows)];
  char rrbase = s[getNext(s, i, 2, k, currRow, maxRows)];
  return P->int21[closingBP][enclosedBP][lbase][rbase][rrbase];
}

int il22_energy(const char *s, rsize i, rsize k, rsize l, rsize j,
                unsigned currRow, unsigned maxRows) {
  int closingBP = bp_index(s[i], s[j]);
  // Note, basepair is reversed to preserver 5'-3' order
  int enclosedBP = bp_index(s[getPrev(s, j, 3, l, currRow, maxRows)],
                            s[getNext(s, i, 3, k, currRow, maxRows)]);
  char lbase = s[getNext(s, i, 1, k, currRow, maxRows)];
  char llbase = s[getNext(s, i, 2, k, currRow, maxRows)];
  char rbase = s[getPrev(s, j, 2, l, currRow, maxRows)];
  char rrbase = s[getPrev(s, j, 1, l, currRow, maxRows)];
  return P->int22[closingBP][enclosedBP][lbase][llbase][rbase][rrbase];
}

int il_stack(const char *s, rsize i, rsize k, rsize l, rsize j,
             unsigned currRow, unsigned maxRows) {
  int out_closingBP = bp_index(s[i], s[j]);
  char out_lbase = s[getNext(s, i, 1, j-1, currRow, maxRows)];
  char out_rbase = s[getPrev(s, j, 1, i+1, currRow, maxRows)];
  // Note, basepair and stacking bases are reversed to preserver 5'-3' order
  int in_closingBP = bp_index(s[l], s[k]);
  char in_lbase = s[getNext(s, l, 1, j-1, currRow, maxRows)];
  char in_rbase = s[getPrev(s, k, 1, i+1, currRow, maxRows)];
  return P->mismatchI[out_closingBP][out_lbase][out_rbase] +
         P->mismatchI[in_closingBP][in_lbase][in_rbase];
}

int il_energy(const char *s, rsize i, rsize k, rsize l, rsize j,
              unsigned currRow, unsigned maxRows) {
  rsize sl = k-i-1 - noGaps(s, i+1, k-1);
  rsize sr = j-l-1 - noGaps(s, l+1, j-1);

  int out_closingBP = bp_index(s[i], s[j]);
  int out_lbase = s[getNext(s, i, 1, j-1, currRow, maxRows)];
  int out_rbase = s[getPrev(s, j, 1, i+1, currRow, maxRows)];
  // Note, basepair and stacking bases are reversed to preserver 5'-3' order
  int in_closingBP = bp_index(s[l], s[k]);
  int in_lbase = s[getNext(s, l, 1, j-1, currRow, maxRows)];
  int in_rbase = s[getPrev(s, k, 1, i+1, currRow, maxRows)];

  // internal loop really is an right bulge, because left unpaired region is
  // just a gap
  if (sl == 0) return br_energy(s, i, l+1, j-1, j, k, currRow, maxRows);
  // internal loop really is an left bulge, because right unpaired region is
  // just a gap
  if (sr == 0) return bl_energy(s, i, i+1, k-1, j, l, currRow, maxRows);


  if (sl == 1) {
    if (sr == 1) {
      return il11_energy(s, i, k, l, j, currRow, maxRows);
    } else if (sr == 2) {
      return il12_energy(s, i, k, l, j, currRow, maxRows);
    } else {
      return il_ent(sl+sr) + il_asym(sl, sr) +
        P->mismatch1nI[out_closingBP][out_lbase][out_rbase] +
        P->mismatch1nI[in_closingBP][in_lbase][in_rbase];
    }
  } else if (sl == 2) {
    if (sr == 1) {
      return il21_energy(s, i, k, l, j, currRow, maxRows);
    } else if (sr == 2) {
      return il22_energy(s, i, k, l, j, currRow, maxRows);
    } else if (sr == 3) {
      return P->internal_loop[5]+P->ninio[2] +
        P->mismatch23I[out_closingBP][out_lbase][out_rbase] +
        P->mismatch23I[in_closingBP][in_lbase][in_rbase];
    }
  } else if ((sl == 3) && (sr == 2)) {
    return P->internal_loop[5]+P->ninio[2] +
      P->mismatch23I[out_closingBP][out_lbase][out_rbase] +
      P->mismatch23I[in_closingBP][in_lbase][in_rbase];
  } else {
    if (sr == 1) {
      return il_ent(sl+sr) + il_asym(sl, sr) +
        P->mismatch1nI[out_closingBP][out_lbase][out_rbase] +
        P->mismatch1nI[in_closingBP][in_lbase][in_rbase];
    }
  }

  return il_ent(sl+sr) + il_stack(s, i, k, l, j, currRow, maxRows) +
         il_asym(sl, sr);
}

int bl_energy(const char *s, rsize i, rsize k, rsize l, rsize j,
              rsize Xright, unsigned currRow, unsigned maxRows) {
  // this is of no biological relevance, just to avoid an underflow
  assert(j >= 2);

  rsize size = l-k+1 - noGaps(s, k, l);

  if (size == 0) {
    int closingBP = bp_index(s[i], s[j]);
    // Note, basepair is reversed to preserver 5'-3' order
    int enclosedBP = bp_index(s[getPrev(s, j, 1, Xright, currRow, maxRows)],
                              s[l+1]);
    return P->stack[closingBP][enclosedBP];
  }
  if (size == 1) {
    int closingBP = bp_index(s[i], s[j]);
    // Note, basepair is reversed to preserver 5'-3' order
    int enclosedBP = bp_index(s[getPrev(s, j, 1, Xright, currRow, maxRows)],
                              s[l+1]);
    return bl_ent(size) + P->stack[closingBP][enclosedBP];
  }
  if (size > 1) {
    return bl_ent(size) + termau_energy(s, i, j) +
      termau_energy(s, getPrev(s, j, 1, Xright, currRow, maxRows), l+1);
  }

  fprintf(stderr, "bl_energy size < 1\n");
  assert(0);
}

int br_energy(const char *s, rsize i, rsize k, rsize l, rsize j, rsize Xleft,
              unsigned currRow, unsigned maxRows) {
  // this is of no biological relevance, just to avoid an underflow
  assert(j >= 1);

  rsize size = l-k+1 - noGaps(s, k, l);

  if (size == 0) {
    int closingBP = bp_index(s[i], s[j]);
    // Note, basepair is reversed to preserver 5'-3' order
    int enclosedBP = bp_index(s[k-1],
                              s[getNext(s, i, 1, Xleft, currRow, maxRows)]);
    return P->stack[closingBP][enclosedBP];
  }
  if (size == 1) {
    int closingBP = bp_index(s[i], s[j]);
    // Note, basepair is reversed to preserver 5'-3' order
    int enclosedBP = bp_index(s[k-1],
                              s[getNext(s, i, 1, Xleft, currRow, maxRows)]);
    return bl_ent(size) + P->stack[closingBP][enclosedBP];
  }
  if (size > 1) {
    return bl_ent(size) + termau_energy(s, i, j) +
      termau_energy(s, k-1, getNext(s, i, 1, Xleft, currRow, maxRows));
  }

  fprintf(stderr, "br_energy size < 1\n");
  assert(0);
}

int dl_energy(const char *s, rsize i, rsize j,
              unsigned currRow, unsigned maxRows) {
  if (i == 0) return 0;
  int closingBP = bp_index(s[i], s[j]);
  char dbase = s[getPrev(s, i, 1, 0, currRow, maxRows)];
  // RNAfold treats a gap like a N-base that is dangling on the pair.
  // Strange but true.
  if (dbase == GAP_BASE) dbase = N_BASE;
  int dd = P->dangle5[closingBP][dbase];
  return (dd > 0) ? 0 : dd;  /* must be <= 0 */
}

int dr_energy(const char *s, rsize i, rsize j, rsize n,
              unsigned currRow, unsigned maxRows) {
  if ((j+1) >= n) return 0;
  int closingBP = bp_index(s[i], s[j]);
  char dbase = s[getNext(s, j, 1, n-1, currRow, maxRows)];
  // RNAfold treats a gap like a N-base that is dangling on the pair.
  // Strange but true.
  if (dbase == GAP_BASE) dbase = N_BASE;
  int dd = P->dangle3[closingBP][dbase];
  return (dd > 0) ? 0 : dd;  /* must be <= 0 */
}

int dli_energy(const char *s, rsize i, rsize j,
               unsigned currRow, unsigned maxRows) {
  // Note, basepair is reversed to preserver 5'-3' order
  int closingBP = bp_index(s[j], s[i]);
  char dbase = s[getNext(s, i, 1, j-1, currRow, maxRows)];
  int dd = P->dangle3[closingBP][dbase];
  return (dd > 0) ? 0 : dd;  /* must be <= 0 */
}

int dri_energy(const char *s, rsize i, rsize j,
               unsigned currRow, unsigned maxRows) {
  // Note, basepair is reversed to preserver 5'-3' order
  int closingBP = bp_index(s[j], s[i]);
  char dbase = s[getPrev(s, j, 1, i+1, currRow, maxRows)];
  int dd = P->dangle5[closingBP][dbase];
  return (dd > 0) ? 0 : dd;  /* must be <= 0 */
}

int ext_mismatch_energy(const char *s, rsize i, rsize j, rsize n,
                        unsigned currRow, unsigned maxRows) {
  int rbasePos = getNext(s, j, 1, n-1, currRow, maxRows);
  // we try to dangle left and right neighboring base if available. They are
  // not if we already are at first or last base ...
  if ((i > 0) && ((j+1) < n) &&
      (s[i-1] != SEPARATOR_BASE) && ((j+1) <= rbasePos)) {
    // or in outside mode, if left / right neighbor is the separator base
    int closingBP = bp_index(s[i], s[j]);
    int lbase = s[getPrev(s, i, 1, 0, currRow, maxRows)];
    int rbase = s[rbasePos];
    if (lbase == GAP_BASE) lbase = N_BASE;
    if (rbase == GAP_BASE) rbase = N_BASE;
    if ((lbase <  5) && (rbase <  5)) {
      // left and right dangling positions are real bases
      return P->mismatchExt[closingBP][lbase][rbase];
    }
    if ((lbase <  5) && (rbase >= 5)) {
      // if right position is a gap and left position is a real base, we resort
      // to a left dangle
      return dl_energy(s, i, j, currRow, maxRows);
    }
    if ((lbase >= 5) && (rbase <  5)) {
      // if left position is a gap and right position is a real base, we resort
      // to a right dangle
      return dr_energy(s, i, j, n, currRow, maxRows);
    }
    if ((lbase >= 5) && (rbase >= 5)) {
      // if left and right positions are gaps, we just return 0;
      return 0;
    }
  } else {
    if ((i > 0) && (s[i-1] != SEPARATOR_BASE)) {
      return dl_energy(s, i, j, currRow, maxRows);
    }
    if (((j+1) < n) && ((j+1) <= rbasePos)) {
      return dr_energy(s, i, j, n, currRow, maxRows);
    }
    return 0;
  }
}

int ml_mismatch_energy(const char *s, rsize i, rsize j,
                       unsigned currRow, unsigned maxRows) {
  // Note, basepairs and stacking bases are reversed to preserver 5'-3' order
  int closingBP = bp_index(s[j], s[i]);
  int lbase = s[getPrev(s, j, 1, i+1, currRow, maxRows)];
  int rbase = s[getNext(s, i, 1, j-1, currRow, maxRows)];
  if ((lbase <  5) && (rbase <  5)) {
    // left and right dangling positions are real bases
    return P->mismatchM[closingBP][lbase][rbase];
  }
  if ((lbase <  5) && (rbase >= 5)) {
    // if right position is a gap and left position is a real base, we resort
    // to a left dangle
    return dli_energy(s, i, j, currRow, maxRows);
  }
  if ((lbase >= 5) && (rbase <  5)) {
    // if left position is a gap and right position is a real base, we resort
    // to a right dangle
    return dri_energy(s, i, j, currRow, maxRows);
  }
  if ((lbase >= 5) && (rbase >= 5)) {
    // if left and right positions are gaps, we just return 0;
    return 0;
  }
}

/* like hl_energy, just no penalty for size > 4 structures */
int hl_energy_stem(const char *s, rsize i, rsize j,
                   unsigned currRow, unsigned maxRows) {
  int r = hl_energy(s, i, j, currRow, maxRows);
  rsize size = j-i-1 - noGaps(s, i+1, j-1);
  if (size >= 4) {
    int stack_mismatch = hl_stack(s, i, j, currRow, maxRows);
    return r - stack_mismatch;
  }
  return r;
}

#endif
