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

#ifndef LIBRNA_RNALIB_H_
#define LIBRNA_RNALIB_H_

// Sun CC (C++ compiler) really makes an effort to educate the user that parts
// of C99 are not standarized in the last C++ specification
#if !defined(__SUNPRO_CC)
#include <stdbool.h>
#endif

enum base_t {
  N_BASE, A_BASE, C_BASE, G_BASE, U_BASE, GAP_BASE, SEPARATOR_BASE };
static char BASE_CHARS[SEPARATOR_BASE+1] = {
  'N', 'A', 'C', 'G', 'U', '_', '+'};

enum iupac_t { N_IUPAC = 0,
  B_IUPAC = 7,
  D_IUPAC = 8,
  H_IUPAC = 9,
  R_IUPAC = 10,
  V_IUPAC = 11,
  Y_IUPAC = 12
};
enum bp_t { N_BP, CG_BP, GC_BP, GU_BP, UG_BP, AU_BP, UA_BP, NO_BP };

typedef unsigned int rsize;

// temperature is defined in vienna/fold_vars.c
extern double temperature;

void librna_read_param_file(const char *filename);
bool test_macrostate_mme_assumption();

int termau_energy(const char *s, rsize i, rsize j);
int duplex_energy(void);
int hl_energy(const char *s, rsize i, rsize j);
int hl_energy_stem(const char *s, rsize i, rsize j);
int il_energy(const char *s, rsize i, rsize j, rsize k, rsize l);
int bl_energy(const char *s, rsize bl, rsize i, rsize j, rsize br,
              rsize Xright);
int br_energy(const char *s, rsize bl, rsize i, rsize j, rsize br, rsize Xleft);
int sr_energy(const char *s, rsize i, rsize j);
int sr_pk_energy(char a, char b, char c, char d);
int dl_energy(const char *s, rsize i, rsize j);
int dr_energy(const char *s, rsize i, rsize j, rsize n);
int dli_energy(const char *s, rsize i, rsize j);
int dri_energy(const char *s, rsize i, rsize j);
int ext_mismatch_energy(const char *s, rsize i, rsize j, rsize n);
int ml_mismatch_energy(const char *s, rsize i, rsize j);
int ml_energy(void);
int ul_energy(void);
int sbase_energy(void);
int ss_energy(rsize i, rsize j);

// for MacroState partition function
int dl_dangle_dg(enum base_t dangle, enum base_t i, enum base_t j);
// for MacroState partition function
int dr_dangle_dg(enum base_t i, enum base_t j, enum base_t dangle);

// not in rna.hh
double mk_pf(double x);
// not in rna.hh
double scale(int x);


bool iupac_match(char base, char iupac_base);
int bp_index(char x, char y);

/* the below 8 functions are exposed to recreate energy computation of the
   original RNAhybrid, i.e. Haskell and ADPc */
int bl_ent(rsize l);
int il11_energy(const char *s, rsize i, rsize k, rsize l, rsize j);
int il12_energy(const char *s, rsize i, rsize k, rsize l, rsize j);
int il21_energy(const char *s, rsize i, rsize k, rsize l, rsize j);
int il22_energy(const char *s, rsize i, rsize k, rsize l, rsize j);
int il_ent(rsize l);
int il_stack(const char *s, rsize i, rsize k, rsize l, rsize j);
int il_asym(rsize sl, rsize sr);

#endif  // LIBRNA_RNALIB_H_
