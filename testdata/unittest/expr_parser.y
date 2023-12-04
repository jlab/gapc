
%{
// is pasted into parser.cc since bison 2.4

#include <string>
#include <map>
#include <cassert>
#include <cstdlib>


%}


%skeleton "lalr1.cc"
%defines  /* yacc -d */
%define "parser_class_name" "Expr_Parser"
%debug    /* yacc -t */ /* FIXME */
%verbose  /* yacc -v */
%error-verbose /* FIXME needed? */

%parse-param { std::map<std::string, int> &look }
%parse-param { int &result }

/* %define "location_type" "Loc"
%locations */

%union {
  std::string *sval;
  int ival;

}

%{
// is pasted only into parser.cc
yy::Expr_Parser::token_type yylex(yy::Expr_Parser::semantic_type *yylval);
%}

%type <ival> expr

%token <ival> NUMBER
%token <sval> ID
%token END 0 "end of file"


%left '-' '+'
%left '*' '/'

%%

state: expr { result = $1; }

expr: ID { std::map<std::string, int>::iterator i = look.find(*$1);
           if (i == look.end()) {
             std::cerr << "Could not find: " << *$1 << '\n';
             $$ = 3242;
           } else
             $$ = i->second; } |
      NUMBER { $$ = $1; } |
      expr '+' expr { $$ = $1 + $3; } |
      expr '*' expr { $$ = $1 * $3; } |
      expr '/' expr { $$ = $1 / $3; } |
      '(' expr ')' { $$ = $2; } ;
%%

#ifdef BISONNEW
void yy::Expr_Parser::error(const std::string& m)
{
  std::cerr << m << '\n';
  std::exit(1);
}
#else
void yy::Expr_Parser::error(const yy::Expr_Parser::location_type &l, const std::string& m)
{
  std::cerr << l << ' ' << m << '\n';
  std::exit(1);
}
#endif
