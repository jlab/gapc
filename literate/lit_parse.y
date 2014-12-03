%{

#include <string>
#include <iostream>

int yylex(void);
extern int yylineno;
int yyerror (char const *s) { std::cerr << yylineno << ' ' << s << '\n'; return 0; }

%}

%debug    /* yacc -t */ /* FIXME */
%verbose  /* yacc -v */
%error-verbose /* FIXME needed? */

%union {
  std::string *sval;
} 


/* %type <sval> import */

/*
%token END 0 "end of file"

%token ALGEBRA
%token <sval>MODE
*/

%token <sval>TOKEN
%token <sval>TERMINAL
%token LINE_BREAK



%%


axiom: nts ;

nts: nt |
     nts { std::cout << '\n'; } nt ;

nt:  token ':' { std::cout << ":"; } rules ';'
  { std::cout << "\n        ;\n"; } ;

rules: rule |
       rules '|' { std::cout << " |"; } rule ;

rule: { std::cout << "\n        "; } tokens | ;

tokens: token |
        tokens { std::cout << ' '; } token ;

token: TOKEN { std::cout << *$1 ; delete $1; } |
       TERMINAL { std::cout << "@" << *$1 << "@"; delete $1; } |
       LINE_BREAK { std::cout << "\n       "; } ;


%%

