#include "../driver.hh"

#include "../parser.hh"

#include "../lexer.h"
YY_DECL;

#include <iostream>

int main(int argc, char **argv)
{
  yy::Parser::semantic_type val;
  yy::Parser::location_type loc;
  Driver driver;
  while (yylex(&val, &loc, driver) != yy::Parser::token::END)
    std::cout << loc << ": " << std::endl;
  return 0;
}
