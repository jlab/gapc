#ifndef LEXER_H
#define LEXER_H

#include <cstdio>

/* YY_DECL is used by flex */
#define YY_DECL yy::Parser::token_type yylex(yy::Parser::semantic_type *yylval,\
                                             yy::Parser::location_type *yylloc, \
                                             yy::Parser::token_type start_symbol)

extern int yy_flex_debug;
extern std::FILE *yyin;


#endif
