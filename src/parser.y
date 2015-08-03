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

// is pasted into parser.hh && parser.cc
// is pasted into parser.cc since bison 2.4

class Driver;

#ifdef BISONNEW
# define YYLLOC_DEFAULT(Current, Rhs, N)        \
do {                            \
  if (N)                        \
    {                            \
      (Current).begin = YYRHSLOC(Rhs, 1).begin;            \
      (Current).end   = YYRHSLOC(Rhs, N).end;            \
      (Current).setOffset(YYRHSLOC(Rhs, 1).offset());           \
      (Current).set_file(YYRHSLOC(Rhs, 1).file_ptr());           \
    }                            \
  else                            \
    {                            \
      (Current).begin = (Current).end = YYRHSLOC(Rhs, 0).end;    \
      (Current).setOffset(YYRHSLOC(Rhs, 0).offset());           \
      (Current).set_file(YYRHSLOC(Rhs, 0).file_ptr());           \
   }                            \
} while (false)
#else
# define YYLLOC_DEFAULT(Current, Rhs, N)		\
do {							\
  if (N)						\
    {							\
      (Current).begin = (Rhs)[1].begin;			\
      (Current).end   = (Rhs)[N].end;			\
      (Current).setOffset((Rhs)[1].offset());           \
      (Current).set_file((Rhs)[1].file_ptr());           \
    }							\
  else							\
    {							\
      (Current).begin = (Current).end = (Rhs)[0].end;	\
      (Current).setOffset((Rhs)[0].offset());           \
      (Current).set_file((Rhs)[0].file_ptr());           \
   }							\
} while (false)
#endif

%}

%skeleton "lalr1.cc"
%defines  /* yacc -d */
%define "parser_class_name" "Parser"
%define "location_type" "Loc"
%parse-param { Driver& driver }
%parse-param { yy::Parser::token_type start_symbol }
%lex-param   { yy::Parser::token_type start_symbol }
%locations
%debug    /* yacc -t */ /* FIXME */
%verbose  /* yacc -v */
%error-verbose /* FIXME needed? */

%initial-action
{
@$.begin.filename = @$.end.filename = driver.filename();
}

%code requires {
// is pasted into parser.hh only 
// (&& parser.cc says the documentation)

#include "type_fwd.hh"
#include <list>
#include <utility>
#include <string>
#include "hashtable.hh"
#include "statement_fwd.hh"
#include "product_fwd.hh"
#include "alt_fwd.hh"
#include "fn_arg_fwd.hh"
#include "const_fwd.hh"
#include "var_acc_fwd.hh"
#include "symbol_fwd.hh"

class Grammar;
class Algebra;
class Fn_Decl;
class Signature;
class Fn_Def;
class Instance;
class Arg;
class Driver;

namespace Para_Decl { class Base; }

#include "filter.hh"
}

%union {
  std::string *sval;
  int ival;
  Algebra *algebra;
  Type::Base *datatype;
  Fn_Decl *fn_decl;
  Signature *signature;
  Filter::Type filter_kw;
  std::list<Expr::Base*> *exprs;
  Filter *filter;
  std::list<Alt::Base*> *alts;
  Alt::Base *alt;
  std::list<Fn_Arg::Base*> *rhs_args;
  Fn_Arg::Base *rhs_arg;
  Expr::Base *expr;
  Const::Base *cons;
  Var_Acc::Base *var_acc;
  Tuple_Pair *named_datatype;
  Tuple_List *named_datatypes;
  Grammar *grammar;
  std::list<Grammar*> *grammars;
  std::pair< hashtable<std::string, Arg*>*,
             std::list<Symbol::NT*> *> *grammar_body;
  Symbol::NT *production;
  std::list<Symbol::NT*> *productions;
  std::list<Type::Base*> *datatypes;
  hashtable<std::string, Type::Base*> *eqs;
  std::pair<std::string*, Type::Base*> *eq;
  hashtable<std::string, Arg*> *args;
  Arg *arg;
  hashtable<std::string, Fn_Decl*> *sig_decls;
  std::pair<std::list<Alt::Base*>*, std::string*>* rhs;
  Fn_Def *fn_def;
  hashtable<std::string, Fn_Def*> *fn_defs;
  Para_Decl::Base *para_decl;
  std::list<Para_Decl::Base*> *para_decls;
  hashtable<std::string, Instance*> *instances;
  Instance* instance;
  std::pair<std::string*,hashtable<std::string, Arg*>*> *i_lhs;
  Product::Base *product;
  Statement::Base *statement;
  std::list<Statement::Base*>* statements;
  std::list<std::string*> *inputs;
  std::list<Filter*> *filters;
} 

%{
// is pasted only into parser.cc

#include <string>
// FIXME
#include <iostream>


#include "loc.hh"



#include "ast.hh"
#include "fn_arg.hh"
#include "type.hh"
#include "type/multi.hh"
#include "para_decl.hh"
#include "signature.hh"
#include "fn_def.hh"
#include "product.hh"
#include "instance.hh"
#include "var_acc.hh"
#include "expr.hh"
#include "const.hh"
#include "statement.hh"
#include "statement/fn_call.hh"
#include "statement/while.hh"
#include "filter.hh"
#include "arg.hh"


#include "driver.hh"
#include "lexer.h"
YY_DECL;
%}

%type <sval> import
%type <algebra> algebra_head
%type <sval> sig_var
%type <datatype> datatype
%type <fn_decl> decl
%type <datatype> qual_datatype
%type <signature> signature
%type <filter_kw> filter_kw
%type <exprs> exprs
%type <exprs> exprs_empty
%type <exprs> ntparas
%type <filter> filter_fn
%type <alts> alts
%type <alt> alt
%type <alts> tracks
%type <alt> track
%type <rhs> rhs 
%type <rhs_args> rhs_args;
%type <rhs_arg> rhs_arg;
%type <expr> expr;
%type <cons> const;
%type <cons> number;
%type <var_acc> var_access;
%type <named_datatype> named_datatype;
%type <named_datatypes> named_datatypes;
%type <grammar> grammar;
%type <grammars> grammars;
%type <args> tabulated;
%type <grammar_body> grammar_body;
%type <production> production;
%type <productions> productions;
%type <datatypes> datatypes;
%type <datatypes> signtparas;
%type <eqs> eqs;
%type <eq> eq;
%type <args> args;
%type <arg> arg;
%type <args> sig_args;
%type <sig_decls> sig_decls;
%type <fn_def> fn_def;
%type <fn_defs> fn_defs;
%type <para_decl> para_decl;
%type <para_decls> para_decls;
%type <para_decls> ntargs;
%type <para_decls> fnntparas;
%type <instances> instances_;
%type <instance> instance;
%type <i_lhs> i_lhs;
%type <product> product;
%type <statement> return;
%type <statement> continue;
%type <statement> break;
%type <statement> statement;
%type <statement> inc_stmt;
%type <statement> if;
%type <statement> assign;
%type <statement> for;
%type <statement> while;
%type <statement> var_decl;
%type <statement> var_decl_init;
%type <statement> var_decl_init_p;
%type <statement> var_decl_init_k;
%type <statements> statements;
%type <statement> fn_call;
%type <sval> mode_opt;
%type <sval> ident;

%type <sval> mode;


%type <sval> input_specifier;
%type <sval> type_specifier;
%type <sval> automatic_specifier;

%type <sval> character_constant;
%type <sval> string_constant;

%type <sval> algebra_ident;
%type <sval> signature_ident;
%type <sval> nt_ident;
%type <sval> choice_fn_ident;
%type <sval> sig_fn_or_term_ident;
%type <sval> symbol_ident;
%type <sval> module_ident;
%type <sval> name_ident;
%type <sval> sort_ident;

%type <sval> input
%type <inputs> inputs

%type <datatypes> multi_datatypes
%type <datatype> multi_datatype

%type <filters> filters

%token START_PROGRAM
%token START_PRODUCT

%token END 0 "end of file"

%token ALGEBRA
%token <sval>MODE

%token ALPHABET "alphabet keyword"

%token AND
%token AXIOM
%token CHOICE
%token DEC
%token DEQ
%token ELSE
%token EXTENDS
%token EQ
%token FOR
%token WHILE
%token GRAMMAR
%token GT
%token <sval>STRING
%token IEQ
%token IF
%token IMPLEMENTS
%token IMPORT
%token INPUT
%token INC
%token INSTANCE
%token LT
%token NEQ
%token NOT
%token <sval> NUMBER
%token <sval> FLOAT 
%token OR
// FIXME not needed anymore
%token PARAMETERS
%token RETURN
%token CONTINUE
%token BREAK
%token SIGNATURE
%token TABULATED
%token TYPE
%token EXTERN
%token USES
%token VOID
%token WITH
%token SUCHTHAT
%token WITH_OVERLAY
%token SUCHTHAT_OVERLAY
%token AUTOMATIC

%token LEB
%token REB

%token UNEXPECTED_CHARACTER

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%left OR
%left AND
%left '<' '>' LT GT EQ NEQ
%left '-' '+'
%left '*' '/' '|' '%' '.' '^'
%right NOT WITH SUCHTHAT
//%right '(' /* because of shift reduce in product (defaults rule) */
%right '{' /* because of shift reduce in product (defaults rule) */




%%


start: START_PROGRAM program |
       START_PRODUCT { driver.ast.algebra_seen.clear(); } product { driver.ast.set_product($3); } ;

// FIXME allow more signatures in one file to
//       be more flexible

/**{
A \gap{} program is structured into several sections. Some sections
are optional, but the order of the sections is fixed. The
non-terminal \lstinline!program! is the start symbol of the syntax description.
Some optional parts are mandatory to generate target code, but
are not needed for semantic analyses by \gapc.
**/
program: imports_opt input_opt types_opt /**/
       signature algebras_opt grammars 
       {
         driver.ast.set_grammars($6);
       }
       instances_opt
       ;
/**}
**/

/**{
\subsection{Imports}
**/
imports_opt: imports | ;

imports: import |
         imports import ;

import: IMPORT module_ident { driver.ast.imports.push_back(new Import($2, @$)); } |
		IMPORT '"' STRING '"' { driver.ast.imports.push_back(new Import($3, true, @$)); } ;
/**}

\wuerglst

The \lstinline!module_ident! can be a module from \gapm{}; otherwise module names
are treated as names of a user defined module. The module {\tt rna} is an example
for a module from \gapm{} (see \ref{sec:modrna}). It defines several functions for
computing free energy contributions of bases in different RNA secondary
structure elements.
**/

module_ident: STRING { $$ = $1; } ;

/**{
\subsection{Input}
\label{gr:input}
**/
input_opt: INPUT input_specifier
         {
           driver.ast.input.set(*$2, @2);
           delete($2);
         }
         |
         INPUT '<' inputs '>'
         {
           driver.ast.input.set(*$3, @3);
           delete($3);
         }
         | ;

inputs: input
      {
        std::list<std::string*> *l = new std::list<std::string*>();
        l->push_back($1);
        $$ = l;
      }
      |
      inputs ',' input
      {
        $1->push_back($3);
        $$ = $1;
      } ;

input: input_specifier { $$ = $1; } ;

/**}

\wuerglst

The input declaration specifies a special input string conversion. By default
the input is read as is (\texttt{raw}). The input specifier \texttt{rna}
signals an input conversion from ASCII encoded nucleotide strings to strings,
which are encoded in the interval $[0..4]$:
\\\\
\begin{tabular}{l|l}
value & nucleotide \\
\hline
0 & an unspecified base \\
1 & A \\
2 & C \\
3 & G \\
4 & U
\end{tabular}
\\\\
The alphabet of the input string is specified in the algebra definition.

The input declaration also specifies the number of input tracks in a
multi track \gap{} program. For example, \verb$input <raw, raw>$
means two-track input and both tracks are read as is.
In \gapl{} the default is single-track processing.

Multi-track dynamic programming algorithms work on more than one
input sequence. For example, the Needleman-Wunsch pairwise
sequence alignment algorithm \cite{needleman} or the Sankoff fold and align
algorithm \cite{sankoff} work on two input sequences (two-track). RNA folding,
like the Zuker minimum free energy (MFE) algorithm \cite{zuker}, work on one
input sequence (single-track).

**/

input_specifier: STRING { $$ = $1; } ;

/**{
\subsection{Types}
**/
types_opt: types | ;

types: type | types type ;

type: TYPE ident '=' datatype
        { driver.ast.add_type($2, @2, new Type::Def($2, @2, $4)); }
      |
      TYPE ident '=' EXTERN
        { driver.ast.add_type($2, @2, new Type::External($2, @2)); }
      ;
/**}
Type declarations at the global level are either type synonyms or
declarations of datatypes in imported external modules.
**/

/**{
**/
datatype: type_specifier
            { $$ = new Type::Usage(driver.ast.get_type(*$1, @1), @1); } |
          ALPHABET
            { $$ = new Type::Usage(
               driver.ast.get_type(std::string("alphabet"), @1), @1); } |
          VOID
            { $$ = new Type::Usage(
                driver.ast.get_type(std::string("void"), @1), @1); } |
          '[' type_specifier ']'
            { $$ = new Type::List(driver.ast.get_type(*$2, @2), @$); } |
/* named adressing? */
          '(' named_datatypes ')'
            { Type::Tuple *t = new Type::TupleDef(@$);
              t->init($2);
              delete $2;
              $$ = t; } ;
/**}
A datatype is either an elementary datatype, the alphabet type,
{\tt void}, a list or a (named) tuple.

The {\tt alphabet} type can only be used in the signature declaration. It is a
placeholder for an actual datatype of an alphabet. An algebra implementing the
signature declares which datatype is the alphabet datatype.

Elementary datatypes are:

{\tt
int, integer, float, string, char, bool,
rational,
bigint,
subsequence,
shape,
void.
}

{\tt float} is in double precision, {\tt rational} and {\tt  bigint} are of
unlimited precision and {\tt subsequence} saves only the
begin-/end-index of a substring. {\tt int} is at least $32$ bit long and {\tt
integer} is at least $64$ bit long.
**/

type_specifier: STRING { $$ = $1; } ;

/**{
**/
named_datatypes: named_datatype 
                 { Tuple_List *l = new Tuple_List();
                   l->push_back($1);
                   $$ = l; }
                 |
                 named_datatypes ',' named_datatype
                 { $1->push_back($3); } ;

named_datatype: datatype name_ident
              { $$ = new Tuple_Pair(new Type::Name($1, @2), $2); }
              ;
/**}

\wuerglst

Note that this syntax forces the programmer to name the components used in
tuples.
**/

name_ident : STRING { $$ = $1; } ;

/**{
\subsection{Signature}
**/
signature: SIGNATURE ident '(' sig_args ')'
         { Signature *s = new Signature($2, @1 + @2);
           driver.ast.add_sig_types(*$4, s);
           $<signature>$ = s; } '{' sig_decls '}'
         { Signature *s = $<signature>6;
           s->set_args(*$4);
           delete $4;
           s->setDecls(*$8);
           delete $8;
           driver.ast.signature = s;
           hashtable<std::string, Type::Base*>::iterator i =
             driver.ast.types.find("alphabet");
           assert(i != driver.ast.types.end());
           dynamic_cast<Type::Alphabet*>(i->second)->signature = s;
           $$ = s; }
         ;

sig_args: ALPHABET ',' args signtparas
        { std::string *s = new std::string("alphabet");
          Arg::add_arg(*$3, new Arg(s, @1));
          $$ = $3;
        }
        ; /* one answer datatype or more */
/**}
The parameters of the signature declaration are the alphabet
keyword and one or more sorts. A sort is a name for a data type which will be
substituted in an algebra that implements the signature. (In Haskell
terminology, a \gap{} sort is a type parameter.)
**/

/**{
**/
args: arg
      { hashtable<std::string, Arg*> *h =
          new hashtable<std::string, Arg*>();
        Arg::add_arg(*h, $1);
        $$ = h; }
      |
      args ',' arg
      { Arg::add_arg(*$1, $3);
        $$ = $1; } ;

arg: ident { $$ =  new Arg($1, @1); }
   ;


signtparas: ';' datatypes
          { $$ = $2; }
          |
          { $$ = 0; }
          ;


sig_decls: decl ';'
             { hashtable<std::string, Fn_Decl*> *h =
                 new hashtable<std::string, Fn_Decl*>();
               Fn_Decl::add_fn_decl(*h, $1);
               $$ = h; }
           |
           sig_decls decl ';'
             { Fn_Decl::add_fn_decl(*$1, $2);
               $$ = $1; }
           ;

decl: qual_datatype ident '(' multi_datatypes  signtparas ')'
    { Fn_Decl *f = new Fn_Decl($1, $2, @2);
      f->set_types($4);
      delete $4;
      f->set_nttypes($5);
      delete $5;
      $$ = f; }
    ;
/**}
The signature contains one or more signature function declarations. The
\lstinline!qual_datatype! indicates the result type of the function.
**/

/**{
**/
qual_datatype: datatype { $$ = $1; } |
               CHOICE datatype { $$ = new Type::Choice($2, @1+@2); } ;
/**}
The qualifier {\tt choice} marks a signature function name as
objective function. The declaration of several objective functions is
allowed. For the objective function, argument and return types must be list types.
**/

/**{
**/

datatypes: datatype
           {
             std::list<Type::Base*> *l = new std::list<Type::Base*>();
             l->push_back($1);
             $$ = l;
           }
           |
           datatypes ',' datatype
           {
             $1->push_back($3);
             $$ = $1;
           } ;

multi_datatype: '<' datatypes '>'
                {
                  Type::Multi *m = new Type::Multi(*$2, @$);
                  delete $2;
                  $$ = m;
                }
                |
                datatype
                {
                  $$ = $1;
                }
                ;
/**}
A \verb$multi_datatype$ is a tuple of datatypes. In an algebra
function the $i$-th component of this type comes from the $i$-th
input track. In a single track context, \verb$datatype$ is equal
to \verb$< datatype >$.
**/
/**{
**/

multi_datatypes: multi_datatype
           {
             std::list<Type::Base*> *l = new std::list<Type::Base*>();
             l->push_back($1);
             $$ = l;
           }
           |
           multi_datatypes ',' multi_datatype
           {
             $1->push_back($3);
             $$ = $1;
           }
           ;
/**}
**/

/**{

\wuerglst

\subsection{Algebras}

\label{sec:syn:algebras}

An algebra implements a signature. The algebra declaration specifies which data
type is used for the alphabet and which data type is used for each sort. The
body of the algebra contains a
compatible function definition for each signature function declaration, where alphabet and sort types are substituted
according to the head of the algebra declaration.
**/
algebras_opt: algebras | ;

algebras: algebra | algebras algebra ;
        
algebra: algebra_head '{' fn_defs '}'
       { if (driver.ast.algebras.find(*$1->name) != driver.ast.algebras.end()){
           error($1->location, "Algebra " + *$1->name
                 + " already defined ");
           error(driver.ast.algebras[*$1->name]->location, "here.");
         } else {
           $1->set_fns(*$3);
           delete $3;
           $1->check_params(*driver.ast.signature);
           driver.ast.algebras[*$1->name] = $1;
         }
       }
                                                      |
         ALGEBRA ident AUTOMATIC automatic_specifier ';'
           {
             if (driver.ast.algebras.find(*$2)
                 != driver.ast.algebras.end()) {
               error(@2, "Algebra " + *$2
                     + " already defined ");
               error(driver.ast.algebras[*$2]->location, "here.");
             } else {
               Algebra *algebra = driver.ast.signature->generate($2, $4);
               if (algebra)
                 driver.ast.algebras[*$2] = algebra;
               else
                 error(@4, "Unknown automatic modifier " + *$4 +
                       ". Use something like count ...");
             }
           }
       ;
/**}
\label{sec:syn:autoalg}
\begin{lstlisting}
automatic_specifier:
        @enum@ |
        @count@
        ;
\end{lstlisting}
The {\tt automatic} keyword specifies the auto generation of the
specified algebra. The \gap{} compiler supports the
auto generation of an enumeration ({\tt enum}) algebra and a counting
({\tt count}) algebra under an arbitrary signature. An enumeration algebra prints
each candidate term
as a human readable string and keeps all candidate strings in the
objective
function, i.e.\ running the enumeration algebra alone prints the
whole candidate
search space. A counting algebra counts how many candidates there are in the search
space.

**/

automatic_specifier: STRING { $$ = $1; } ;



mode: MODE { $$ = $1; } ; 

         // mode_opt introduces shift/reduce conflict with algebras_opt

/**{
\label{gr:extend}
\label{gr:algebra}
**/
algebra_head:
         mode ALGEBRA ident parameters IMPLEMENTS signature_ident '(' eqs  ')'
           {
             if (*$6 != *driver.ast.signature->name)
               error(@6, "Unknown signature " + *$6 + ".");
             Algebra *a = new Algebra($3, $6, @2+@3);
             a->set_default_choice_fn_mode($1);
             a->set_params($8);
             delete $8;
             $$ = a; }
         |
         ALGEBRA ident parameters IMPLEMENTS signature_ident '(' eqs  ')'
           {
             if (*$5 != *driver.ast.signature->name)
               error(@5, "Unknown signature " + *$5 + ".");
             Algebra *a = new Algebra($2, $5, @1+@2);
             a->set_params($7);
             delete $7;
             $$ = a; }
         |
         ALGEBRA ident parameters EXTENDS algebra_ident
           { 
             Algebra *a = new Algebra($2, @1+@2);
             if (driver.ast.algebras.find(*$5) == driver.ast.algebras.end())
               error(@4+@5, *$5 + " unknown!");
             else
               *a = *(driver.ast.algebras[*$5]->copy());
             $$ = a; }
         ;
/**}
An algebra is declared as an implementation of a signature or as
an extension of a previously defined algebra. If a signature is
directly implemented, the mapping between signature parameters
(alphabet and sorts) and concrete datatypes is specified. In the
case of an extension, every already declared algebra function can
be overwritten.

The mode of an algebra is optional and either:
{\tt
synoptic
stringrep %pretty
classify
scoring
kscoring
}

\texttt{kscoring} is the default mode for every objective function of the algebra
and can be overwritten by a declaration of an objective function.

In case of no mode specification, the compiler tries to derive the
mode automatically. If an objective function uses the
generic list minimization function, the objective function mode is
autodetected as \texttt{scoring}.

**/

/**{
**/
parameters: parameter_block | ;

parameter_block: '(' var_decl_init_p
               | '(' var_decl_inits var_decl_init_p  ;

var_decl_inits: var_decl_init_k | var_decl_inits var_decl_init_k ;

var_decl_init_p: datatype ident '=' expr ')'
             { $$ = new Statement::Var_Decl($1, $2, $4, @1); }
             ;

var_decl_init_k: datatype ident '=' expr ','
             { $$ = new Statement::Var_Decl($1, $2, $4, @1); }
             ;

/**}
Parameters of the algebra are optional. If present, they are
supplied or overwritten at runtime of the resulting \gap{} program,
e.g.\ via command line switches and
are intended to be
supplied or overwritten by the user of a generated \gap{} program.
**/

var_decl_init: datatype ident '=' expr ';'
             { $$ = new Statement::Var_Decl($1, $2, $4, @1); }
             ;

var_decl: datatype ident ';'
        { $$ = new Statement::Var_Decl($1, $2, @1); }
        |
        var_decl_init
        { $$ = $1; }
        ;
       

algebra_ident: STRING { $$ = $1; } | MODE { $$ = $1; } ;

ident: STRING { $$ = $1; } | MODE { $$ = $1; } ; 

/**{
**/
eqs: eq
      { hashtable<std::string, Type::Base*> *h =
         new hashtable<std::string, Type::Base*>();
        Algebra::add_sig_var(*h, *$1, @1);
        delete $1->first;
        delete $1;
        $$ = h; } |
     eqs ',' eq
      { Algebra::add_sig_var(*$1, *$3, @3);
        delete $3->first;
        delete $3;
        $$ = $1; } ;

eq: sig_var '=' datatype 
  { @$= @1;
    $$ = new std::pair<std::string*, Type::Base*>($1, $3); } ;

sig_var: sort_ident { driver.ast.get_type(*$1, @1); $$ = $1; } |
         ALPHABET { $$ = new std::string("alphabet"); } ;
/**}
**/

sort_ident: STRING { $$ = $1; } ;

/**{
\subsubsection{Algebra Functions}
\label{sec:syn:algfns}
**/
fn_defs: /* empty */
         {
           hashtable<std::string, Fn_Def *> *h =
               new hashtable<std::string, Fn_Def*>();
           $$ = h;
         }
         |
         fn_defs fn_def
         { hashtable<std::string, Fn_Def*>::iterator i = $1->find(*$2->name);
           if (i != $1->end()) {
             error($2->location, "Algebra function " + *$2->name+ " redefined");
             error(i->second->location, "here.");
           } else {
            (*$1)[*$2->name] = $2;
           }
           $$ = $1;
         } ;

fn_def:
     mode_opt qual_datatype ident '(' para_decls fnntparas ')' '{' statements '}'
     { Fn_Def *f = new Fn_Def($2, $3, @3);
       f->set_paras(*$5);
       delete $5;
       f->set_ntparas($6);
       delete $6;
       f->set_statements(*$9);
       delete($9);
       f->set_mode($1);
       delete $1;
       $$ = f;
     } ;

fnntparas: ';' para_decls
         { $$ = $2; }
         |
         { $$ = 0; }
         ;

mode_opt: mode { $$ = $1; } | { $$ = 0; } ;

para_decls: { 
              $$ = new std::list<Para_Decl::Base*>();
            }
            |
            para_decl {
              std::list<Para_Decl::Base*> *l =
                new std::list<Para_Decl::Base*>();
              l->push_back($1);
              $$ = l;
            } |
            para_decls ',' para_decl {
              $1->push_back($3);
              $$ = $1;
            } ;

para_decl: datatype ident
           { $$ = new Para_Decl::Simple($1, $2, @$); }
           |
           '<' para_decls '>'
           {
             Para_Decl::Multi *m = new Para_Decl::Multi(*$2, @$);
             delete $2;
             $$ = m;
           }
           |
           VOID {
             static unsigned x = 0;
             std::ostringstream o;
             o << "VOID_INTERNAL" << x++;
             $$ = new Para_Decl::Simple
               (new Type::Void(@1), new std::string(o.str()));
           }
           ;
/**}
An algebra contains normal functions and one or more objective
functions. A function is marked as objective function by
using 
the keyword \texttt{choice} (see definition of {\tt
qual\_datatype}).
In each declaration of the objective function it is possible to
overwrite the default algebra mode. It is possible to
declare an algebra with two objective functions, where the first one
is of \texttt{scoring} mode and the second one is of
\texttt{kscoring} mode.

A \verb$para_decl$ is either a single-track, a multi-track or a
\verb$VOID$ parameter declaration. A multi-track parameter
declaration is the implementation of a multi-track tuple type of
the corresponding signature function parameter. If a
non-terminal parser evaluates a branching element, it feeds each
branch result into the corresponding declared parameter of a
multi-track parameter declaration.

\label{gr:multipara}

An example of the multi-track parameter declaration syntax is
the following algebra function \lstinline[style=gapc]$match$:
\begin{lstlisting}[style=gapc]
int match( <char a, char b>, int rest)
{
  if (a == b)
    return 1 + rest;
  else
    return rest;
}
\end{lstlisting}

The corresponding signature function is:
\begin{lstlisting}[style=gapc]
answer match( <char, char>, answer);
\end{lstlisting}

The signature function symbol match may be used in a grammar
rule, e.g.:
\begin{lstlisting}[style=gapc]
ali = match( < CHAR, CHAR>, ali)
\end{lstlisting}

**/

/**{
\subsection{Statements}
**/
statements: statement
          { std::list<Statement::Base*> *l =
              new std::list<Statement::Base*>();
            l->push_back($1);
            $$ = l; }
          |
          statements statement
          { $1->push_back($2);
            $$ = $1; }
          ;

statement: 
            continue |
            break |
            return |
            if | 
            for |
            while |
            assign |
            var_decl |
            fn_call |
            '{' statements '}' { $$ = new Statement::Block(*$2, @1); }
            ;

continue: CONTINUE ';' { $$ = new Statement::Continue(@1); } ;

break: BREAK ';' { $$ = new Statement::Break(@1); } ;

fn_call: ident '(' exprs ')' ';'
       { $$ = new Statement::Fn_Call($1, $3, @1);
         delete $3;
       }
       ;

return: RETURN expr ';'
      { $$ = new Statement::Return($2, @1); }
      ;


if: 
    IF '(' expr ')' statement %prec LOWER_THAN_ELSE
    { $$ = new Statement::If($3, $5, @1); }
    |
    IF '(' expr ')' statement ELSE statement
    { $$ = new Statement::If($3, $5, $7, @1); }
    ; 

/**}
The \lstinline!%prec LOWER_THAN_ELSE! grammar description annotation specifies
that the else part of an if statement belongs to
the last started if statement (like in C/Java) while parsing nested conditionals.
**/

/**{
**/

for: FOR '(' var_decl_init expr ';' inc_stmt ')' statement
   {
     Statement::For *f = new Statement::For(dynamic_cast<Statement::Var_Decl*>($3), $4, $6, @1);
     if ($8->is(Statement::BLOCK)) {
       Statement::Block *b = dynamic_cast<Statement::Block*>($8);
       f->statements = b->statements;
       delete b;
     } else
       f->push_back($8);
     $$ = f;
   }
   ;

while: WHILE '(' expr ')' statement
     {
       Statement::While *w = new Statement::While($3, @1);
       if ($5->is(Statement::BLOCK)) {
         Statement::Block *b = dynamic_cast<Statement::Block*>($5);
         w->statements = b->statements;
         delete b;
       } else
         w->push_back($5);
       $$ = w;
     }

assign: var_access '=' expr ';'
      { $$ = new Statement::Var_Assign($1, $3, @2); }
      ;
/**}
**/


inc_stmt: var_access '=' expr
      { $$ = new Statement::Var_Assign($1, $3, @2); }
      ;

/*
inc_stmt: STRING inc_eq_op expr |
          STRING inc_op  ;

inc_op: INC | DEC ;

inc_eq_op: IEQ | DEQ ;
*/

/**{

\wuerglst

\subsection{Variable Access}
**/
var_access: ident { $$ = new Var_Acc::Plain($1, @1); } |
            var_access '.' name_ident { $$ = new Var_Acc::Comp($1, $3, @$); } |
            var_access '[' expr ']' { $$ = new Var_Acc::Array($1, $3, @$); } ;
/**}

\wuerglst

A variable access is either an access to a simple variable, an access to a
component of a named tuple or an access to an array.
**/



expr: expr '<' expr { $$ = new Expr::Less($1, $3, @2); } | 
           expr '>' expr { $$ = new Expr::Greater($1, $3, @2); } |
           expr LT expr { $$ = new Expr::Less_Eq($1, $3, @2); } |
           expr GT expr { $$ = new Expr::Greater_Eq($1, $3, @2); } |
           expr EQ expr { $$ = new Expr::Eq($1, $3, @2); } |
           expr NEQ expr { $$ = new Expr::Not_Eq($1, $3, @2); } |
           expr AND expr { $$ = new Expr::And($1, $3, @2); } |
           expr OR expr { $$ = new Expr::Or($1, $3, @2); }|
           '(' expr ')' { $$ = new Expr::Comp($2, @$); } |
           NOT expr { $$ = new Expr::Not($2, @1); } |

      expr '+' expr { $$ = new Expr::Plus($1, $3, @$); } |
      expr '-' expr { $$ = new Expr::Minus($1, $3, @$); } |
      expr '*' expr { $$ = new Expr::Times($1, $3, @$); } |
      expr '/' expr { $$ = new Expr::Div($1, $3, @$); } |
      STRING '(' exprs_empty ')' { Expr::Fn_Call *f = new Expr::Fn_Call($1, @$);
                             if (!f->name)
                               delete $1;
                             f->exprs = *$3;
                             delete $3;
                             $$ = f; } |
      var_access { $$ = new Expr::Vacc($1, @1); } |
      const { $$ = new Expr::Const($1, @1); } ;

number: NUMBER { $$ = new Const::Int(*$1, @1); delete $1; } |
        NUMBER '.' NUMBER
          { $$ = new Const::Float(*$1 + "." + *$3, @$);
            delete $1; delete $3; } |
        FLOAT { $$ = new Const::Float(*$1, @1);
                delete $1;
              } |
        NUMBER '$' NUMBER
          {
            $$ = new Const::Rational($1, $3, @$);
          } |
        '-' number { Const::Number *n = dynamic_cast<Const::Number*>($2);
                     assert(n);
                     n->setNegative();
                     $$ = n; } ;

exprs: expr            { std::list<Expr::Base*> *l =
                           new std::list<Expr::Base*>();
                         l->push_back($1);
                         $$ = l; } |
       exprs ',' expr  { $1->push_back($3);
                         $$ = $1; } ;

exprs_empty: exprs
           { $$ = $1; }
           |
           { $$ = new std::list<Expr::Base*>(); }
           ;

/**{
\subsection{Grammar}

\subsubsection{Grammar rules}

**/

grammars: grammar 
          {
            std::list<Grammar*> *l = new std::list<Grammar*>();
            l->push_back($1);
            $$ = l;
          }
          |
          grammars grammar
          {
            $1->push_back($2);
            $$ = $1;
          } ;

grammar: GRAMMAR ident USES signature_ident '(' AXIOM '=' nt_ident ')'
           '{' grammar_body '}'
         {
           if (*$4 != *driver.ast.signature->name)
             error(@4, "Unknown signature " + *$4 + ".");
           Grammar *g = new Grammar(driver.ast, $2, $4, $8, @1 + @2);
           g->axiom_loc = @8;
          if ($11->first) {
            g->tab_names = *$11->first;
            delete $11->first;
          }
          assert($11);
          assert($11->second);
          for (std::list<Symbol::NT*>::iterator i = $11->second->begin();
               i != $11->second->end(); ++i)
            g->add_nt(*i);
          $$ = g;
         }
         ;

grammar_body: tabulated productions
              {
                assert($1);
                assert($2);
                $$ = new std::pair< hashtable<std::string, Arg*>*,
                                    std::list<Symbol::NT*>*>($1, $2);
              }
              |
              productions
              {
                assert($1);
                $$ = new std::pair< hashtable<std::string, Arg*>*,
                                    std::list<Symbol::NT*>*>(0, $1);
              }
              ;
/**}
The definition of a grammar specifies the name of the grammar,
the used signature and the name of the start symbol. The grammar
is a regular tree grammar. The right hand side contains
function symbols from the signature as tree nodes. The \gapc{}
checks whether the grammar is valid under the specified signature.
**/
/**{
\label{gr:tabulated}
**/

tabulated: TABULATED '{' args '}'
         {
           $$ = $3;
         } ;
/**}
With the optional {\tt tabulated} declaration it is possible to
request the tabulation of a list of non-terminals. In case of an
increased optimization level or a non-present {\tt tabulated}
declaration the compiler automatically computes a good table
configuration (see Section \ref{sec:tdesign}).
**/

signature_ident: STRING { $$ = $1; }

nt_ident: STRING { $$ = $1; }

/**{
**/
productions: production
             {
               std::list<Symbol::NT*> *l = new std::list<Symbol::NT*>();
               l->push_back($1);
               $$ = l;
             }
             |
             productions production
             {
               $1->push_back($2);
               $$ = $1;
             }
             ;

production: ident ntargs '=' rhs  ';'
          {
            Symbol::NT *nt = new Symbol::NT($1, @1);
            nt->set_alts(*$4->first);
            nt->set_eval_fn($4->second);
            nt->set_ntargs($2);
            delete $4->first;
            delete $4;
            delete $2;
            $$ = nt;
          }
          ;

ntargs: '(' para_decls ')'
      { $$ = $2; }
      |
      { $$ = 0; }
      ;

/**}

\label{sec:syn:paramnt}

A non-terminal symbol can be defined with arguments
(i.e.\ a parameterized non-terminal). The arguments, or expressions
including the arguments, can be used on
the right hand side as extra arguments of a function symbol, a
filter function or another parametrized non-terminal call.
A parametrized non-terminal cannot be tabulated, because for
every combination of parameter values a separate table would be
needed.

An example for the use of parametrized non-terminals is
the design of RNA pattern matching algorithms in ADP
\cite{MEY:GIE:2002}, where a non-terminal models e.g.\ a stack of base
pairings and the argument of the non-terminal is the stack
length. The argument is then decremented, if greater than zero,
and applied to a recursive
non-terminal call. Another example is \pknots{}
\cite{REE:GIE:2004}, where canonicalization information is
supplied via non-terminal parameters (Section \ref{sec:indexhack}).
**/
/**{
**/

rhs: alts { std::pair<std::list<Alt::Base*>*, std::string*> *p =
              new std::pair<std::list<Alt::Base*>*, std::string*>($1, NULL);
             $$ = p; } |
     alts '#' choice_fn_ident
       { std::pair<std::list<Alt::Base*>*, std::string*> *p =
           new std::pair<std::list<Alt::Base*>*, std::string*>($1, $3);
         $$ = p; } ;
/**}
The right hand side of a production is a set of alternatives with
an optional application of an objective function which was declared
in the signature.
**/

choice_fn_ident: STRING { $$ = $1; } ;

/**{
\label{gr:filter}
\label{gr:multitrack}
**/
ntparas: ';' exprs
       { $$ = $2; }
       |
       { $$ = 0; }
       ;

filters: filters ',' filter_fn
       {
         $1->push_back($3);
         $$ = $1;
       }
       |
       filter_fn
       {
         std::list<Filter*> *l = new std::list<Filter*>();
         l->push_back($1);
         $$ = l;
       }
       ;

tracks: track
      {
        std::list<Alt::Base*> *l = new std::list<Alt::Base*>();
        l->push_back($1);
        $$ = l;
      }
      | tracks ',' track
      {
        $1->push_back($3);
        $$ = $1;
      }
      ;

track: alt { $$ = $1; } ;

alts: alt { std::list<Alt::Base*> *l = new std::list<Alt::Base*>();
            l->push_back($1);
            $$ = l; } |
      alts '|' alt 
      { $1->push_back($3);
        $$ = $1; }
      ;

alt: '{' alts '}'
     { $$ = new Alt::Block(*$2, @$); delete $2; }
     |
     sig_fn_or_term_ident '(' rhs_args  ntparas ')'
     { Alt::Simple *a = new Alt::Simple($1, @1);
       a->args = *$3;
       delete $3;
       a->set_ntparas($4);
       delete $4;
       $$ = a; }
     | 
     symbol_ident
     { $$ = new Alt::Link($1, @1); } 
     |
     alt filter_kw filter_fn
     {
       if (($2 == Filter::WITH_OVERLAY || $2 == Filter::SUCHTHAT_OVERLAY)
           && !$1->is(Alt::SIMPLE))
         error(@2, "Overlay filtering makes only sense with a function symbol"
                   " on the lhs.");

       $3->type = $2;
       $1->filters.push_back($3);
       $$ = $1;
     }
     |
/**}

\wuerglst

An alternative is a block of enclosed alternatives, a
function symbol from the signature plus its arguments, a
non-terminal/terminal parser call or a conditional alternative.

**/
/**{
\subsubsection{Multi-Track Rules}
\label{sec:syn:multitrack}
**/
     '<' tracks '>'
     {
       Alt::Multi *r = new Alt::Multi(*$2, @$);
       delete $2;
       $$ = r;
     }
     |
     alt filter_kw '<' filters '>' 
     {
       $1->add_multitrack_filter(*$4, $2, @4);
       delete $4;
       $$ = $1;
     }
     |
/**}

For multi-track \dynp{} an alternative can also be a
branching from a multi-track context into several single-track
contexts or a conditional alternative guarded by different
single-track filters for each track.

A multi-track context of $n$ tracks may contain an $n$-fold
branching \verb$< a_1, ..., a_n >$. Each $a_i$ is then in a
single-track context for each track $i$, where $a_i$ is a
terminal- or non-terminal parser call.

For example, \verb$match ( < CHAR, CHAR >, ali )$ is a grammar
rule that calls two character reading terminal parsers, which read
a character from the first or the second input track,
respectively.

To apply a filter on different tracks in a multi-track context, a
list of filters has to be included in \verb$<>$ parentheses.

In multi-track mode the grammar may contain combinations of
single-track and multi-track productions. The following example
contains two-track and single-track productions:

\begin{lstlisting}[style=gapc]
foo = del ( < CHAR, EMPTY > , foo ) |
      ins ( < EMPTY, CHAR > , foo ) |
      x   ( < fold, REGION >, foo ) # h ;

fold = hl ( BASE, REGION, BASE ) # h' ;
\end{lstlisting}

**/
/**{

\wuerglst

\subsubsection{Non-terminal parameters}
**/
     alt '.' '(' exprs ')' '.'
     {
       $1->set_ntparas(@1+@3, $4);
       delete $4;
       $$ = $1;
     }
     |
/**}
\wuerglst

This alternative specifies the syntax for calling non-terminals
that have parameters. In case \verb$alt$ is not a link to
another non-terminal, an error should be signaled.
**/
/**{
\subsubsection{Index Hacking}
\label{sec:syn:index-hacking}
**/
     symbol_ident '[' exprs ']'
     {
       Alt::Link *l = new Alt::Link($1, @1);
       l->set_indices(*$3);
       delete $3;
       $$ =  l;
     }
     |
     alt '.' '{' alt '}'  '.'
     {
       $4->set_index_overlay($1);
       $$ = $4;
     }
     |
     '.' '[' statements ']' '.' '{' alt '}' 
     {
       $7->set_index_stmts(*$3);
       delete $3;
       $$ = $7;
     }
     ;

/**}

\wuerglst

These index hacking related alternatives specify a non-terminal
call with explicit indices, an overlay of two alternatives and
verbatim index manipulation code before an alternative. The tree
grammar search-space specification mechanism from the ADP
framework eliminates the need of using explicit indices for most
\dynp{} algorithms over sequences. However, some algorithms, like for
example \pknots{} \cite{REE:GIE:2004}, need to perform their own index
computations at selected non-terminal locations for efficiency
reasons. In the example of \pknots{},
canonicalization rules are applied to reduce the number of moving
index boundaries. In \gapl{}, these rules are implemented as verbatim
index manipulation code in the grammar. The overlaying of
alternatives is used in the semantic analyses. The left alternative
is a 
fake rule that approximates the resulting index boundaries, such
that
the runtime analysis computes more realistic results. The right
alternative is then used for code generation.

See Section \ref{sec:indexhack} for more details on index hacking in the use case of
\pknots.

\subsubsection{Grammar Filters}

\label{sec:syn:grfilter}

**/

sig_fn_or_term_ident: STRING { $$ = $1; } ;

symbol_ident: STRING { $$ = $1; } ;

/**{
**/
filter_kw: WITH { $$ = Filter::WITH; } |
           SUCHTHAT { $$ = Filter::SUCHTHAT; } |
           WITH_OVERLAY { $$ = Filter::WITH_OVERLAY; } |
           SUCHTHAT_OVERLAY { $$ = Filter::SUCHTHAT_OVERLAY; }
           ;
/**}

With \lstinline!P filter_kw f! in case of the {\tt with} keyword,
the filter function $f$ is called before $P$ is parsed, with
the sub-word that should be parsed by $P$, as an (additional)
argument. With the {\tt suchthat} keyword the filter function is
called after $P$ is evaluated for each parse of $P$. {\tt
with\_overlay} and {\tt suchthat\_overlay} are variations of {\tt
with} and {\tt suchthat} and are only defined if $P$ uses a
signature function $g$. In the case of {\tt with\_overlay} the
filter function is called with a list of sub-words which
correspond to the unparsed arguments of $g$, before $P$ is
parsed. With {\tt suchthat\_overlay} the filter function is
called after the arguments of $g$ are parsed and before the
evaluation of $g$ for each combination of argument values.

Filtering through {\tt with} and {\tt with\_overlay} clauses is
called syntactic filtering, since the filter function depends
only on the input word. Filtering with {\tt suchthat} and
{\tt suchthat\_overlay} is called semantic filtering, since the
filter does not depend on the input word, but on the used algebra.


**/

/**{
**/
filter_fn: ident
           { Filter *f = new Filter($1, @1);
             $$ = f;
           } |
           ident '(' exprs ')'
           { Filter *f = new Filter($1, @1);
             f->args = *$3;
             f->init_builtin();
             delete $3;
             $$ = f; }
           ;
/**}
The filter function can be part of the signature and algebra
definition or can be included in a module. The filter function
must return a boolean value. In addition to the default
arguments, it is possible to supply user defined arguments.

If the return value is false, the left hand side of the filter keyword is not
used during parsing. In the case of syntactic filtering this
means that the left hand side is neither parsed nor evaluated. With
semantic filtering, the left hand side is parsed and evaluated, but the
result is discarded.

The filters are used to reduce the search space which is
described by the grammar.

**/

/**{
**/
rhs_args: rhs_arg
          { std::list<Fn_Arg::Base*> *l = new std::list<Fn_Arg::Base*>();
            l->push_back($1);
            $$ = l; }
          |
          rhs_args ',' rhs_arg
          { $1->push_back($3);
            $$ = $1; } ;

/* FIXME too broad: alt ? */
rhs_arg: alt /* NT or block ... */
         { $$ = new Fn_Arg::Alt($1, @1); } |
         const
         { $$ = new Fn_Arg::Const($1, @1); } /* term parse arg */ ;

/* for consts as arguments in terminal parsers */
const: number { $$ = $1;} |
       '\'' character_constant '\''
       {
         $$ = new Const::Char(*$2, @$);
         delete $2;
       } |
       '"' string_constant '"' { $$ = new Const::String($2, @$); } ;
/**}


\subsubsection{Terminal Symbols}

The \gap{} language supports several terminal parsers or symbols.
For a terminal parser it is possible to have one or more
arguments.

The yield size of a terminal parser is the number of characters it
parses from the input word. The {\tt STRING} terminal parser
parses some non-empty string, i.e.\ its minimum yieldsize is $1$ and its
maximum yieldsize is $n$, where $n$ is the length of the input
word.

The terminal symbols without arguments (including their return
type) are listed as follows:
\\\\
\begin{tabular}{llll}
\toprule
& & \multicolumn{2}{c}{yield size} \\
\cmidrule(l){3-4}
Return type & Parser & min & max \\
\midrule
\lstinline$[void]$ & \lstinline$EMPTY$ & $0$ & $0$ \\
\lstinline$[subsequence]$ & \lstinline$LOC$ & $0$ & $0$ \\
\lstinline$[char]$ & \lstinline$CHAR$ & $1$ & $1$ \\
\lstinline$[subsequence]$ & \lstinline$BASE$ & $1$ & $1$ \\
\lstinline$[string]$ & \lstinline$STRING0$ & $0$ & $n$ \\
\lstinline$[string]$ & \lstinline$STRING$ & $1$ & $n$ \\
\lstinline$[subsequence]$ & \lstinline$REGION0$ & $0$ & $n$ \\
\lstinline$[subsequence]$ & \lstinline$REGION$ & $1$ & $n$ \\
\lstinline$[float]$ & \lstinline$FLOAT$ & $1$ & $n$ \\
\lstinline$[int]$ & \lstinline$INT$ & $1$ & $n$ \\
\lstinline$[int]$ & \lstinline$SEQ$ & $1$ & $n$ \\
\bottomrule
\end{tabular}
\\\\
If a terminal parser cannot parse successfully, an empty list is
returned. The parser \lstinline$LOC$ is used to access the
position in the input string, where the empty word was parsed.
\lstinline$INT$ reads an integer number and returns its value.
\lstinline$SEQ$ parses a sub-word from the input string and
returns its length.

The list of terminal symbols with arguments is:
\begin{lstlisting}[style=gapc]
[alphabet] CHAR(alphabet)
[int] INT(int)
[int] CONST_INT(int)
[subsequence] STRING(string)
[float] CONST_FLOAT(float)
\end{lstlisting}

The {\tt CONST\_*} terminal parsers have a maximum yieldsize of
0, i.e.\ they don't consume any sub-word of the input. Those
terminal parsers can be used in a grammar context to supply a
constant argument to an algebra function.
**/

string_constant: STRING { $$ = $1; } | { $$ = new std::string(); } ;

character_constant: STRING { $$ = $1; } ;


/**{
\subsection{Instances}

\label{sec:syn:instance}

An instance declaration specifies under which algebra (or
product) a grammar
is evaluated.
**/
instances_opt: instances | ;

instances: instances_ { driver.ast.instances = *$1; delete $1; } ;

instances_: instance { driver.ast.first_instance = $1;
                       hashtable<std::string, Instance*> *h =
                       new hashtable<std::string, Instance*>();
                       (*h)[*$1->name()] = $1;
                       $$ = h; } |
            instances_ instance { 
              hashtable<std::string, Instance*>::iterator i = 
                $1->find(*$2->name());
              if (i != $1->end()) {
                error($2->location, "Instance " + *$2->name() +
                  " already defined");
                error(i->second->location, "here.");
              } else {
                (*$1)[*$2->name()] = $2;
              }
              $$ = $1;
            } ;

instance: INSTANCE i_lhs '=' ident { driver.ast.algebra_seen.clear(); } '(' product ')' ';'
          {
            Instance *i = new Instance($2->first, $7, @2);
            if (!driver.ast.grammar_defined(*$4))
              error(@4, "Grammar " + *$4 + " is not defined.");
            else
              i->set_grammar(driver.ast.grammar(*$4));
            delete $2;
            $$ = i;
          }  ;
/**}
An instance is named. On the right hand side of the equal sign, the grammar
and the product is specified. See Section \ref{sec:prodalgebra}
for the semantics of the products.
**/

i_lhs: ident
         { $$ = new std::pair<std::string*,hashtable<std::string, Arg*>*>
                  ($1, NULL);
         } |
       ident '('args ')'
         { @$ = @1 ;
           $$ = new std::pair<std::string*,hashtable<std::string, Arg*>*>
                  ($1, $3);
         } ;
/**{
**/
product: product '*' product { $$ = new Product::Times($1, $3, @2); } |
/**}
The lexicographic product.
**/
/**{
**/
         product '/' product { $$ = new Product::Klass($1, $3, @2); } |
/**}
The interleaved product.
**/
/**{
**/
         product '%' product { $$ = new Product::Cartesian($1, $3, @2); } |
/**}
The cartesian product.
**/
/**{
**/
         product '^' product { $$ = new Product::Pareto($1, $3, @2); } |
/**}
The cartesian product.
**/
/**{
**/
         product '.' product { $$ = new Product::Takeone($1, $3, @3); } |
/**}
The take-one product. The difference to the lexicographic product is that
only one co-optimal result is chosen in the case of co-optimal results.
**/
/**{
**/
         product '|' product { $$ = new Product::Overlay($1, $3, @2); } |
/**}
The overlay product. With \lstinline!A | B!, $A$ is used in the
forward computation and $B$
is used during backtracing. An use case for this is
stochastic backtracing (Section \ref{sec:stochasticbt}),
i.e.\ the sampling of shape
strings under a partition function:

\begin{lstlisting}[style=gapc]
( (p_func | p_func_id ) * shape5 ) suchthat sample_filter )
\end{lstlisting}

The objective function of the {\tt p\_func} algebra is summation and the
objective function of the {\tt p\_func\_id} algebra is identity. During the
forward computation only {\tt p\_func} is evaluated. In the backtracing
phase the intermediate {\tt p\_func} values are evaluated by the
{\tt p\_func\_id} algebra and value lists are filtered by the
{\tt sample\_filter}. The {\tt sample\_filter} interprets the value lists as
discrete probability distributions and randomly takes one element
from the list under this distribution. During the backtracing the
shape representation is randomly built according to the computed
probability distribution, i.e.\ the repeated stochastic
backtracing samples shape strings according to their shape
probability (see Section \ref{sec:stochasticbt}).

**/
         product '{' defaults '}'
         {
           /* $1->set_defaults(); */
           $$ = $1;
         }
         | 
/**{
**/
         '(' product ')' { $$ = $2; } |
         algebra_ident
         { Product::Single *p = new Product::Single($1, @1);
           hashtable<std::string, Algebra*>::iterator i =
             driver.ast.algebras.find(*$1);
           if (i == driver.ast.algebras.end())
             error(@1, "Algebra " + *$1 + " not defined.");
           else {
             std::set<std::string>::iterator j = driver.ast.algebra_seen.find(i->first);
             if (j == driver.ast.algebra_seen.end()) {
               p->set_algebra(i->second);
               driver.ast.algebra_seen.insert(i->first);
             } else {
               static unsigned counter;
               // copy for bpmax*bpmax products ...
               Algebra *a = i->second->copy();
               p->set_algebra(a);
               std::ostringstream o;
               o << i->first << counter++;
               driver.ast.algebras[o.str()] = a;
             }
           }
           $$ = p;
         }
         |
/**}
Singleton product.
**/
/**{
\label{gr:instfilt}
**/
         product SUCHTHAT filter_fn
         {
           $1->set_filter($3);
           $$ = $1;
         }
         ;
/**}
Before evaluating the answers list with the product's objective 
function, the {\tt filter\_fn} is applied to each intermediate
(candidate) answer list.  The result of the filter\_fn is the
input for the products objective function.

A usecase for this feature is the probability mode in RNAshapes
\cite{STE:VOSS:REH:REE:GIE:2006}, where
in the computation of {\tt shape * pf} every (sub-)candidate is
removed during the computation, if the left hand side is $< 0.000001$. This
filter significantly reduces the exponential number of classes,
such that the computation of this product for longer sequences is
feasible (Section \ref{sec:synfilt}).
**/

defaults: default | defaults ',' default ;

default: var_access '=' expr ;

%%

void yy::Parser::error(const yy::Parser::location_type &l, const std::string& m)
{
  driver.error(l, m);
}

