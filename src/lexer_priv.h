
#ifndef LEXER_PRIV_H
#define LEXER_PRIV_H

#include <cstdio>

#define YY_USE_CONST
#ifdef YY_USE_CONST
#define yyconst const
#else
#define yyconst
#endif
#ifndef YY_TYPEDEF_YY_BUFFER_STATE
#define YY_TYPEDEF_YY_BUFFER_STATE
typedef struct yy_buffer_state *YY_BUFFER_STATE;
#endif
YY_BUFFER_STATE yy_scan_string (yyconst char *yy_str  );

//YY_BUFFER_STATE yy_new_buffer ( FILE *file, int size );
YY_BUFFER_STATE yy_create_buffer ( std::FILE *file, int size );

void yy_delete_buffer ( YY_BUFFER_STATE buffer );

/* Size of default input buffer. */
#ifndef YY_BUF_SIZE
#ifdef __ia64__
/* On IA-64, the buffer size is 16k, not 8k.
 * Moreover, YY_BUF_SIZE is 2*YY_READ_BUF_SIZE in the general case.
 * Ditto for the __ia64__ case accordingly.
 */
#define YY_BUF_SIZE 32768
#else
#define YY_BUF_SIZE 16384
#endif /* __ia64__ */
#endif

void yypush_buffer_state ( YY_BUFFER_STATE buffer );

class Driver;

namespace scanner {


void init(Driver *d);

}

#endif
