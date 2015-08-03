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

%{


#include "parser.hh"

#include "lexer.h"

typedef yy::Parser::token token;

#include <stack>

#include "driver.hh"

namespace scanner {


static Driver *driver;

struct state_t {

state_t()
: offset(0), last_nl(0), filename(0) {}

long offset;
long last_nl;
std::string *filename;

};

state_t state;

static bool first_call = true;

void init(Driver *d)
{
  driver = d;
  state.offset = 0;
  state.last_nl = 0;
  state.filename = driver->filename();
  first_call = true;
}

std::stack<state_t> states;

}


#define YY_USER_ACTION do { \
  yylloc->step(); \
  yylloc->columns(yyleng); \
  scanner::state.offset+=yyleng; \
} while(0);

/* By default yylex returns int, we use token_type.
   Unfortunately yyterminate by default returns 0, which is
   not of token_type.  -> see info bison */
#define yyterminate() return token::END

static void new_line(yy::Parser::location_type *yylloc)
{
  yylloc->lines();
  yylloc->step();
  scanner::state.last_nl = scanner::state.offset;
  yylloc->setOffset(scanner::state.last_nl);
  yylloc->set_file(yyin);
}

%}

%option noyywrap nounput batch
/* debug */

%x SKIP_LINE
%x SKIP_COMMENT
%x STRING_QUOTE
%x CHAR_QUOTE

%x INCLUDE

%%

 /* gets pasted in yylex at the beginning */
%{
  yylloc->step();
  yylloc->setOffset(scanner::state.last_nl);
  yylloc->set_file(yyin);

  if (scanner::first_call) {
    scanner::first_call = false;
    return start_symbol;
  }
%}

 /* ------------------------------------------------------------------------ */
 /* keywords */
 /* ------------------------------------------------------------------------ */

import { return token::IMPORT; }

input { return token::INPUT; }

type { return token::TYPE; }

extern { return token::EXTERN; }

signature { return token::SIGNATURE; }

choice { return token::CHOICE; }

algebra { return token::ALGEBRA; }

implements { return token::IMPLEMENTS; }

extends { return token::EXTENDS; }

parameters { return token::PARAMETERS; }

alphabet { return token::ALPHABET; }

 /* answer { return token::ANSWER; } -> just the return type of axiom alg fn */

grammar { return token::GRAMMAR; }

uses { return token::USES; }

axiom { return token::AXIOM; }

instance { return token::INSTANCE; }

tabulated { return token::TABULATED; }

auto { return token::AUTOMATIC; }

with_overlay { return token::WITH_OVERLAY; }

with { return token::WITH; }

suchthat_overlay { return token::SUCHTHAT_OVERLAY; }

suchthat { return token::SUCHTHAT; }


void { return token::VOID; }

 /* ------------------------------------------------------------------------ */
 /* algebra types */
 /* ------------------------------------------------------------------------ */

synoptic|pretty|classify|scoring|kscoring {
     yylval->sval = new std::string(yytext);
     return token::MODE; }

 /* ------------------------------------------------------------------------ */
 /* statements */
 /* ------------------------------------------------------------------------ */

if { return token::IF; }

else { return token::ELSE; }

for { return token::FOR; }

while { return token::WHILE; }

return { return token::RETURN; }

continue { return token::CONTINUE; }

break { return token::BREAK; }

 /* ------------------------------------------------------------------------ */
 /* include files */
 /* ------------------------------------------------------------------------ */

^[ \t]*include { BEGIN INCLUDE; }

<INCLUDE>{


\"[^"]+\" {
  std::string s(yytext);
  std::string t(s.substr(1, s.size()-2));

  scanner::states.push(scanner::state);
  scanner::state = scanner::state_t();
  scanner::state.filename = yylloc->begin.filename = yylloc->end.filename = new std::string(t);

  scanner::driver->push_buffer(t);

  yylloc->set_file(yyin);

  BEGIN INITIAL;
}

[ \t] ;

. {  return token::UNEXPECTED_CHARACTER; }

}


 /* ------------------------------------------------------------------------ */
 /* comments */
 /* ------------------------------------------------------------------------ */

"//" { BEGIN SKIP_LINE; }

<SKIP_LINE>{
\n { new_line(yylloc); BEGIN INITIAL; }
[^\n]+ ;
}

"/*" { BEGIN SKIP_COMMENT; }

<SKIP_COMMENT>{
"*/" { BEGIN INITIAL; }
[^*\n]+ ;
"*" ;
\n { new_line(yylloc); }
}

 /* ------------------------------------------------------------------------ */
 /* operators */
 /* ------------------------------------------------------------------------ */
== { return token::EQ; }

!= { return token::NEQ; }

"<=" { return token::LT; }

>= { return token::GT; }

&& { return token::AND; }

"||" { return token::OR; }

! { return token::NOT; }

"++" { return token::INC; }

"+=" { return token::IEQ; }

-- { return token::DEC; }

-= { return token::DEQ; }

 /* ------------------------------------------------------------------------ */
 /* chars, strings */
 /* ------------------------------------------------------------------------ */

\" { BEGIN STRING_QUOTE; return yy::Parser::token_type(yytext[0]); }

<STRING_QUOTE>{
[^\"]+ {  yylval->sval = new std::string(yytext); return token::STRING; }
\" { BEGIN INITIAL; return yy::Parser::token_type(yytext[0]); }
}

' { BEGIN CHAR_QUOTE; return yy::Parser::token_type(yytext[0]); }

<CHAR_QUOTE>{
[^']+ { yylval->sval = new std::string(yytext); return token::STRING; }
' { BEGIN INITIAL; return yy::Parser::token_type(yytext[0]); }
}

 /* ------------------------------------------------------------------------ */
 /* special characters */
 /* ------------------------------------------------------------------------ */

[%$.<>'\"#:|=(),{};\[\]*/+^-] { return yy::Parser::token_type(yytext[0]); }

\n+ { yylloc->lines(yyleng-1); new_line(yylloc); }

[ ]+ ;

[\t]+ { yylloc->columns(yyleng*8-yyleng); }

 /* ------------------------------------------------------------------------ */
 /* sval aka string , number ... */
 /* ------------------------------------------------------------------------ */
[0-9]+ { yylval->sval = new std::string(yytext); return token::NUMBER; }

-?[0-9]+\.[0-9]+(e[+-]?[0-9]+)? {
   yylval->sval = new std::string(yytext); return token::FLOAT;
}

[A-Za-z0-9_]+ { yylval->sval = new std::string(yytext); return token::STRING; }

 /* ignore carriage return - stupid windows users ... */
\15 ;

<<EOF>> {
  yypop_buffer_state();
  if (!YY_CURRENT_BUFFER)
    yyterminate();
  else {
    assert(!scanner::states.empty());
    scanner::state = scanner::states.top();
    scanner::states.pop();
    yylloc->begin.filename = yylloc->end.filename = scanner::state.filename;
  }
}

. { return token::UNEXPECTED_CHARACTER; }

%%


