%{

#include <iostream>

template<typename A, typename B>
inline
int yylex(A a, B b)
{
  return 0;
}


%}


%skeleton "lalr1.cc"
%defines  /* yacc -d */
%locations
%debug    /* yacc -t */ /* FIXME */
%verbose  /* yacc -v */
%error-verbose



%{

// 

%}

%%

axiom: ;

%%

#include <string>

void yy::parser::error(const yy::location& loc, const std::string& msg)
{
}


int main()
{
  return 0;
}
