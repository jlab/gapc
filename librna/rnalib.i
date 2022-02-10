%module gapcrna
%{
typedef unsigned int rsize;
extern void librna_read_param_file(const char *filename);
extern int hl_energy(const char *s, rsize i, rsize j);
extern int sr_energy(const char *s, rsize i, rsize j);
extern int il_energy(const char *s, rsize i, rsize j, rsize k, rsize l);
extern int bl_energy(const char *s, rsize bl, rsize i, rsize j, rsize br, rsize Xright);
extern int br_energy(const char *s, rsize bl, rsize i, rsize j, rsize br, rsize Xleft);
extern int ml_energy(void);
extern int ul_energy(void);
extern int ss_energy(rsize i, rsize j);
extern int termau_energy(const char *s, rsize i, rsize j);
extern int sbase_energy(void);
%}

extern void librna_read_param_file(const char *filename);
extern int hl_energy(const char *s, int i, int j);
extern int sr_energy(const char *s, int i, int j);
extern int il_energy(const char *s, int i, int j, int k, int l);
extern int bl_energy(const char *s, int bl, int i, int j, int br, int Xright);
extern int br_energy(const char *s, int bl, int i, int j, int br, int Xleft);
extern int ml_energy(void);
extern int ul_energy(void);
extern int ss_energy(int i, int j);
extern int termau_energy(const char *s, int i, int j);
extern int sbase_energy(void);
