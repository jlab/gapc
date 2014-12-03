#ifndef RNALIB_H
#define RNALIB_H

// Sun CC (C++ compiler) really makes an effort to educate the user that parts
// of C99 are not standarized in the last C++ specification
#if !defined(__SUNPRO_CC)
#include <stdbool.h>
#endif

enum base_t { N_BASE, A_BASE, C_BASE, G_BASE, U_BASE, GAP_BASE, SEPARATOR_BASE };
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

int termau_energy(const char *s, rsize i, rsize j);
int hl_energy(const char *s, rsize i, rsize j);
int hl_energy_stem(const char *s, rsize i, rsize j);
int il_energy(const char *s, rsize i, rsize j, rsize k, rsize l);
int bl_energy(const char *s, rsize bl, rsize i, rsize j, rsize br, rsize Xright);
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

//for MacroState partition function
int dl_dangle_dg(enum base_t dangle, enum base_t i, enum base_t j);
//for MacroState partition function
int dr_dangle_dg(enum base_t i, enum base_t j, enum base_t dangle);

// not in rna.hh
double mk_pf(double x);
// not in rna.hh
double scale(int x);


bool iupac_match(char base, char iupac_base);

#endif
