%{

#include <string>
#include <map>
#include "expr_parser.hh"

#include <boost/lexical_cast.hpp>

#define YY_DECL yy::Expr_Parser::token_type yylex(yy::Expr_Parser::semantic_type *yylval)

/* By default yylex returns int, we use token_type.
   Unfortunately yyterminate by default returns 0, which is
   not of token_type.  -> see info bison */
#define yyterminate() return token::END

typedef yy::Expr_Parser::token token;


%}

%option noyywrap nounput batch

/* outfile=""  see -o */

%%


[0-9]+       { yylval->ival = boost::lexical_cast<int>(yytext);
                return token::NUMBER; }
[0-9_a-z]+   { yylval->sval = new std::string(yytext); return token::ID; }
[+*/()]          { return yy::Expr_Parser::token_type(yytext[0]); }

\n ;
. ;


%%
