// A Bison parser, made by GNU Bison 3.0.4.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.


// First part of user declarations.
#line 24 "../src/parser.y" // lalr1.cc:404


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


#line 81 "parser.cc" // lalr1.cc:404

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

#include "parser.hh"

// User implementation prologue.
#line 163 "../src/parser.y" // lalr1.cc:412

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

#line 130 "parser.cc" // lalr1.cc:412


#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K].location)
/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

# ifndef YYLLOC_DEFAULT
#  define YYLLOC_DEFAULT(Current, Rhs, N)                               \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).begin  = YYRHSLOC (Rhs, 1).begin;                   \
          (Current).end    = YYRHSLOC (Rhs, N).end;                     \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).begin = (Current).end = YYRHSLOC (Rhs, 0).end;      \
        }                                                               \
    while (/*CONSTCOND*/ false)
# endif


// Suppress unused-variable warnings by "using" E.
#define YYUSE(E) ((void) (E))

// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << std::endl;                  \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yystack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YYUSE(Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void>(0)
# define YY_STACK_PRINT()                static_cast<void>(0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)


namespace yy {
#line 216 "parser.cc" // lalr1.cc:479

  /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
  std::string
  Parser::yytnamerr_ (const char *yystr)
  {
    if (*yystr == '"')
      {
        std::string yyr = "";
        char const *yyp = yystr;

        for (;;)
          switch (*++yyp)
            {
            case '\'':
            case ',':
              goto do_not_strip_quotes;

            case '\\':
              if (*++yyp != '\\')
                goto do_not_strip_quotes;
              // Fall through.
            default:
              yyr += *yyp;
              break;

            case '"':
              return yyr;
            }
      do_not_strip_quotes: ;
      }

    return yystr;
  }


  /// Build a parser object.
  Parser::Parser (Driver& driver_yyarg, yy::Parser::token_type start_symbol_yyarg)
    :
#if YYDEBUG
      yydebug_ (false),
      yycdebug_ (&std::cerr),
#endif
      driver (driver_yyarg),
      start_symbol (start_symbol_yyarg)
  {}

  Parser::~Parser ()
  {}


  /*---------------.
  | Symbol types.  |
  `---------------*/

  inline
  Parser::syntax_error::syntax_error (const location_type& l, const std::string& m)
    : std::runtime_error (m)
    , location (l)
  {}

  // basic_symbol.
  template <typename Base>
  inline
  Parser::basic_symbol<Base>::basic_symbol ()
    : value ()
  {}

  template <typename Base>
  inline
  Parser::basic_symbol<Base>::basic_symbol (const basic_symbol& other)
    : Base (other)
    , value ()
    , location (other.location)
  {
    value = other.value;
  }


  template <typename Base>
  inline
  Parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const semantic_type& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}


  /// Constructor for valueless symbols.
  template <typename Base>
  inline
  Parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const location_type& l)
    : Base (t)
    , value ()
    , location (l)
  {}

  template <typename Base>
  inline
  Parser::basic_symbol<Base>::~basic_symbol ()
  {
    clear ();
  }

  template <typename Base>
  inline
  void
  Parser::basic_symbol<Base>::clear ()
  {
    Base::clear ();
  }

  template <typename Base>
  inline
  bool
  Parser::basic_symbol<Base>::empty () const
  {
    return Base::type_get () == empty_symbol;
  }

  template <typename Base>
  inline
  void
  Parser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move(s);
    value = s.value;
    location = s.location;
  }

  // by_type.
  inline
  Parser::by_type::by_type ()
    : type (empty_symbol)
  {}

  inline
  Parser::by_type::by_type (const by_type& other)
    : type (other.type)
  {}

  inline
  Parser::by_type::by_type (token_type t)
    : type (yytranslate_ (t))
  {}

  inline
  void
  Parser::by_type::clear ()
  {
    type = empty_symbol;
  }

  inline
  void
  Parser::by_type::move (by_type& that)
  {
    type = that.type;
    that.clear ();
  }

  inline
  int
  Parser::by_type::type_get () const
  {
    return type;
  }


  // by_state.
  inline
  Parser::by_state::by_state ()
    : state (empty_state)
  {}

  inline
  Parser::by_state::by_state (const by_state& other)
    : state (other.state)
  {}

  inline
  void
  Parser::by_state::clear ()
  {
    state = empty_state;
  }

  inline
  void
  Parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  inline
  Parser::by_state::by_state (state_type s)
    : state (s)
  {}

  inline
  Parser::symbol_number_type
  Parser::by_state::type_get () const
  {
    if (state == empty_state)
      return empty_symbol;
    else
      return yystos_[state];
  }

  inline
  Parser::stack_symbol_type::stack_symbol_type ()
  {}


  inline
  Parser::stack_symbol_type::stack_symbol_type (state_type s, symbol_type& that)
    : super_type (s, that.location)
  {
    value = that.value;
    // that is emptied.
    that.type = empty_symbol;
  }

  inline
  Parser::stack_symbol_type&
  Parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    value = that.value;
    location = that.location;
    return *this;
  }


  template <typename Base>
  inline
  void
  Parser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);

    // User destructor.
    YYUSE (yysym.type_get ());
  }

#if YYDEBUG
  template <typename Base>
  void
  Parser::yy_print_ (std::ostream& yyo,
                                     const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    symbol_number_type yytype = yysym.type_get ();
    // Avoid a (spurious) G++ 4.8 warning about "array subscript is
    // below array bounds".
    if (yysym.empty ())
      std::abort ();
    yyo << (yytype < yyntokens_ ? "token" : "nterm")
        << ' ' << yytname_[yytype] << " ("
        << yysym.location << ": ";
    YYUSE (yytype);
    yyo << ')';
  }
#endif

  inline
  void
  Parser::yypush_ (const char* m, state_type s, symbol_type& sym)
  {
    stack_symbol_type t (s, sym);
    yypush_ (m, t);
  }

  inline
  void
  Parser::yypush_ (const char* m, stack_symbol_type& s)
  {
    if (m)
      YY_SYMBOL_PRINT (m, s);
    yystack_.push (s);
  }

  inline
  void
  Parser::yypop_ (unsigned int n)
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  Parser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  Parser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  Parser::debug_level_type
  Parser::debug_level () const
  {
    return yydebug_;
  }

  void
  Parser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  inline Parser::state_type
  Parser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
  }

  inline bool
  Parser::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  inline bool
  Parser::yy_table_value_is_error_ (int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  int
  Parser::parse ()
  {
    // State.
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The locations where the error started and ended.
    stack_symbol_type yyerror_range[3];

    /// The return value of parse ().
    int yyresult;

    // FIXME: This shoud be completely indented.  It is not yet to
    // avoid gratuitous conflicts when merging into the master branch.
    try
      {
    YYCDEBUG << "Starting parse" << std::endl;


    // User initialization code.
    #line 82 "../src/parser.y" // lalr1.cc:741
{
yyla.location.begin.filename = yyla.location.end.filename = driver.filename();
}

#line 595 "parser.cc" // lalr1.cc:741

    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, yyla);

    // A new symbol was pushed on the stack.
  yynewstate:
    YYCDEBUG << "Entering state " << yystack_[0].state << std::endl;

    // Accept?
    if (yystack_[0].state == yyfinal_)
      goto yyacceptlab;

    goto yybackup;

    // Backup.
  yybackup:

    // Try to take a decision without lookahead.
    yyn = yypact_[yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token: ";
        try
          {
            yyla.type = yytranslate_ (yylex (&yyla.value, &yyla.location, start_symbol));
          }
        catch (const syntax_error& yyexc)
          {
            error (yyexc);
            goto yyerrlab1;
          }
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.type_get ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.type_get ())
      goto yydefault;

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", yyn, yyla);
    goto yynewstate;

  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;

  /*-----------------------------.
  | yyreduce -- Do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_(yystack_[yylen].state, yyr1_[yyn]);
      /* If YYLEN is nonzero, implement the default value of the
         action: '$$ = $1'.  Otherwise, use the top of the stack.

         Otherwise, the following line sets YYLHS.VALUE to garbage.
         This behavior is undocumented and Bison users should not rely
         upon it.  */
      if (yylen)
        yylhs.value = yystack_[yylen - 1].value;
      else
        yylhs.value = yystack_[0].value;

      // Compute the default @$.
      {
        slice<stack_symbol_type, stack_type> slice (yystack_, yylen);
        YYLLOC_DEFAULT (yylhs.location, slice, yylen);
      }

      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
      try
        {
          switch (yyn)
            {
  case 3:
#line 371 "../src/parser.y" // lalr1.cc:859
    { driver.ast.algebra_seen.clear(); }
#line 705 "parser.cc" // lalr1.cc:859
    break;

  case 4:
#line 371 "../src/parser.y" // lalr1.cc:859
    { driver.ast.set_product((yystack_[0].value.product)); }
#line 711 "parser.cc" // lalr1.cc:859
    break;

  case 5:
#line 385 "../src/parser.y" // lalr1.cc:859
    {
         driver.ast.set_grammars((yystack_[0].value.grammars));
       }
#line 719 "parser.cc" // lalr1.cc:859
    break;

  case 11:
#line 401 "../src/parser.y" // lalr1.cc:859
    { driver.ast.imports.push_back(new Import((yystack_[0].value.sval), yylhs.location)); }
#line 725 "parser.cc" // lalr1.cc:859
    break;

  case 12:
#line 402 "../src/parser.y" // lalr1.cc:859
    { driver.ast.imports.push_back(new Import((yystack_[1].value.sval), true, yylhs.location)); }
#line 731 "parser.cc" // lalr1.cc:859
    break;

  case 13:
#line 414 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 737 "parser.cc" // lalr1.cc:859
    break;

  case 14:
#line 421 "../src/parser.y" // lalr1.cc:859
    {
           driver.ast.input.set(*(yystack_[0].value.sval), yystack_[0].location);
           delete((yystack_[0].value.sval));
         }
#line 746 "parser.cc" // lalr1.cc:859
    break;

  case 15:
#line 427 "../src/parser.y" // lalr1.cc:859
    {
           driver.ast.input.set(*(yystack_[1].value.inputs), yystack_[1].location);
           delete((yystack_[1].value.inputs));
         }
#line 755 "parser.cc" // lalr1.cc:859
    break;

  case 17:
#line 434 "../src/parser.y" // lalr1.cc:859
    {
        std::list<std::string*> *l = new std::list<std::string*>();
        l->push_back((yystack_[0].value.sval));
        (yylhs.value.inputs) = l;
      }
#line 765 "parser.cc" // lalr1.cc:859
    break;

  case 18:
#line 441 "../src/parser.y" // lalr1.cc:859
    {
        (yystack_[2].value.inputs)->push_back((yystack_[0].value.sval));
        (yylhs.value.inputs) = (yystack_[2].value.inputs);
      }
#line 774 "parser.cc" // lalr1.cc:859
    break;

  case 19:
#line 446 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 780 "parser.cc" // lalr1.cc:859
    break;

  case 20:
#line 483 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 786 "parser.cc" // lalr1.cc:859
    break;

  case 25:
#line 493 "../src/parser.y" // lalr1.cc:859
    { driver.ast.add_type((yystack_[2].value.sval), yystack_[2].location, new Type::Def((yystack_[2].value.sval), yystack_[2].location, (yystack_[0].value.datatype))); }
#line 792 "parser.cc" // lalr1.cc:859
    break;

  case 26:
#line 496 "../src/parser.y" // lalr1.cc:859
    { driver.ast.add_type((yystack_[2].value.sval), yystack_[2].location, new Type::External((yystack_[2].value.sval), yystack_[2].location)); }
#line 798 "parser.cc" // lalr1.cc:859
    break;

  case 27:
#line 506 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.datatype) = new Type::Usage(driver.ast.get_type(*(yystack_[0].value.sval), yystack_[0].location), yystack_[0].location); }
#line 804 "parser.cc" // lalr1.cc:859
    break;

  case 28:
#line 508 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.datatype) = new Type::Usage(
               driver.ast.get_type(std::string("alphabet"), yystack_[0].location), yystack_[0].location); }
#line 811 "parser.cc" // lalr1.cc:859
    break;

  case 29:
#line 511 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.datatype) = new Type::Usage(
                driver.ast.get_type(std::string("void"), yystack_[0].location), yystack_[0].location); }
#line 818 "parser.cc" // lalr1.cc:859
    break;

  case 30:
#line 514 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.datatype) = new Type::List(driver.ast.get_type(*(yystack_[1].value.sval), yystack_[1].location), yylhs.location); }
#line 824 "parser.cc" // lalr1.cc:859
    break;

  case 31:
#line 517 "../src/parser.y" // lalr1.cc:859
    { Type::Tuple *t = new Type::TupleDef(yylhs.location);
              t->init((yystack_[1].value.named_datatypes));
              delete (yystack_[1].value.named_datatypes);
              (yylhs.value.datatype) = t; }
#line 833 "parser.cc" // lalr1.cc:859
    break;

  case 32:
#line 546 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 839 "parser.cc" // lalr1.cc:859
    break;

  case 33:
#line 551 "../src/parser.y" // lalr1.cc:859
    { Tuple_List *l = new Tuple_List();
                   l->push_back((yystack_[0].value.named_datatype));
                   (yylhs.value.named_datatypes) = l; }
#line 847 "parser.cc" // lalr1.cc:859
    break;

  case 34:
#line 556 "../src/parser.y" // lalr1.cc:859
    { (yystack_[2].value.named_datatypes)->push_back((yystack_[0].value.named_datatype)); }
#line 853 "parser.cc" // lalr1.cc:859
    break;

  case 35:
#line 559 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.named_datatype) = new Tuple_Pair(new Type::Name((yystack_[1].value.datatype), yystack_[0].location), (yystack_[0].value.sval)); }
#line 859 "parser.cc" // lalr1.cc:859
    break;

  case 36:
#line 569 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 865 "parser.cc" // lalr1.cc:859
    break;

  case 37:
#line 575 "../src/parser.y" // lalr1.cc:859
    { Signature *s = new Signature((yystack_[3].value.sval), yystack_[4].location + yystack_[3].location);
           driver.ast.add_sig_types(*(yystack_[1].value.args), s);
           (yylhs.value.signature) = s; }
#line 873 "parser.cc" // lalr1.cc:859
    break;

  case 38:
#line 578 "../src/parser.y" // lalr1.cc:859
    { Signature *s = (yystack_[3].value.signature);
           s->set_args(*(yystack_[5].value.args));
           delete (yystack_[5].value.args);
           s->setDecls(*(yystack_[1].value.sig_decls));
           delete (yystack_[1].value.sig_decls);
           driver.ast.signature = s;
           hashtable<std::string, Type::Base*>::iterator i =
             driver.ast.types.find("alphabet");
           assert(i != driver.ast.types.end());
           dynamic_cast<Type::Alphabet*>(i->second)->signature = s;
           (yylhs.value.signature) = s; }
#line 889 "parser.cc" // lalr1.cc:859
    break;

  case 39:
#line 592 "../src/parser.y" // lalr1.cc:859
    { std::string *s = new std::string("alphabet");
          Arg::add_arg(*(yystack_[1].value.args), new Arg(s, yystack_[3].location));
          (yylhs.value.args) = (yystack_[1].value.args);
        }
#line 898 "parser.cc" // lalr1.cc:859
    break;

  case 40:
#line 607 "../src/parser.y" // lalr1.cc:859
    { hashtable<std::string, Arg*> *h =
          new hashtable<std::string, Arg*>();
        Arg::add_arg(*h, (yystack_[0].value.arg));
        (yylhs.value.args) = h; }
#line 907 "parser.cc" // lalr1.cc:859
    break;

  case 41:
#line 613 "../src/parser.y" // lalr1.cc:859
    { Arg::add_arg(*(yystack_[2].value.args), (yystack_[0].value.arg));
        (yylhs.value.args) = (yystack_[2].value.args); }
#line 914 "parser.cc" // lalr1.cc:859
    break;

  case 42:
#line 616 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.arg) =  new Arg((yystack_[0].value.sval), yystack_[0].location); }
#line 920 "parser.cc" // lalr1.cc:859
    break;

  case 43:
#line 621 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.datatypes) = (yystack_[0].value.datatypes); }
#line 926 "parser.cc" // lalr1.cc:859
    break;

  case 44:
#line 623 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.datatypes) = 0; }
#line 932 "parser.cc" // lalr1.cc:859
    break;

  case 45:
#line 628 "../src/parser.y" // lalr1.cc:859
    { hashtable<std::string, Fn_Decl*> *h =
                 new hashtable<std::string, Fn_Decl*>();
               Fn_Decl::add_fn_decl(*h, (yystack_[1].value.fn_decl));
               (yylhs.value.sig_decls) = h; }
#line 941 "parser.cc" // lalr1.cc:859
    break;

  case 46:
#line 634 "../src/parser.y" // lalr1.cc:859
    { Fn_Decl::add_fn_decl(*(yystack_[2].value.sig_decls), (yystack_[1].value.fn_decl));
               (yylhs.value.sig_decls) = (yystack_[2].value.sig_decls); }
#line 948 "parser.cc" // lalr1.cc:859
    break;

  case 47:
#line 639 "../src/parser.y" // lalr1.cc:859
    { Fn_Decl *f = new Fn_Decl((yystack_[5].value.datatype), (yystack_[4].value.sval), yystack_[4].location);
      f->set_types((yystack_[2].value.datatypes));
      delete (yystack_[2].value.datatypes);
      f->set_nttypes((yystack_[1].value.datatypes));
      delete (yystack_[1].value.datatypes);
      (yylhs.value.fn_decl) = f; }
#line 959 "parser.cc" // lalr1.cc:859
    break;

  case 48:
#line 653 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.datatype) = (yystack_[0].value.datatype); }
#line 965 "parser.cc" // lalr1.cc:859
    break;

  case 49:
#line 654 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.datatype) = new Type::Choice((yystack_[0].value.datatype), yystack_[1].location+yystack_[0].location); }
#line 971 "parser.cc" // lalr1.cc:859
    break;

  case 50:
#line 665 "../src/parser.y" // lalr1.cc:859
    {
             std::list<Type::Base*> *l = new std::list<Type::Base*>();
             l->push_back((yystack_[0].value.datatype));
             (yylhs.value.datatypes) = l;
           }
#line 981 "parser.cc" // lalr1.cc:859
    break;

  case 51:
#line 672 "../src/parser.y" // lalr1.cc:859
    {
             (yystack_[2].value.datatypes)->push_back((yystack_[0].value.datatype));
             (yylhs.value.datatypes) = (yystack_[2].value.datatypes);
           }
#line 990 "parser.cc" // lalr1.cc:859
    break;

  case 52:
#line 678 "../src/parser.y" // lalr1.cc:859
    {
                  Type::Multi *m = new Type::Multi(*(yystack_[1].value.datatypes), yylhs.location);
                  delete (yystack_[1].value.datatypes);
                  (yylhs.value.datatype) = m;
                }
#line 1000 "parser.cc" // lalr1.cc:859
    break;

  case 53:
#line 685 "../src/parser.y" // lalr1.cc:859
    {
                  (yylhs.value.datatype) = (yystack_[0].value.datatype);
                }
#line 1008 "parser.cc" // lalr1.cc:859
    break;

  case 54:
#line 699 "../src/parser.y" // lalr1.cc:859
    {
             std::list<Type::Base*> *l = new std::list<Type::Base*>();
             l->push_back((yystack_[0].value.datatype));
             (yylhs.value.datatypes) = l;
           }
#line 1018 "parser.cc" // lalr1.cc:859
    break;

  case 55:
#line 706 "../src/parser.y" // lalr1.cc:859
    {
             (yystack_[2].value.datatypes)->push_back((yystack_[0].value.datatype));
             (yylhs.value.datatypes) = (yystack_[2].value.datatypes);
           }
#line 1027 "parser.cc" // lalr1.cc:859
    break;

  case 60:
#line 733 "../src/parser.y" // lalr1.cc:859
    { if (driver.ast.algebras.find(*(yystack_[3].value.algebra)->name) != driver.ast.algebras.end()){
           error((yystack_[3].value.algebra)->location, "Algebra " + *(yystack_[3].value.algebra)->name
                 + " already defined ");
           error(driver.ast.algebras[*(yystack_[3].value.algebra)->name]->location, "here.");
         } else {
           (yystack_[3].value.algebra)->set_fns(*(yystack_[1].value.fn_defs));
           delete (yystack_[1].value.fn_defs);
           (yystack_[3].value.algebra)->check_params(*driver.ast.signature);
           driver.ast.algebras[*(yystack_[3].value.algebra)->name] = (yystack_[3].value.algebra);
         }
       }
#line 1043 "parser.cc" // lalr1.cc:859
    break;

  case 61:
#line 746 "../src/parser.y" // lalr1.cc:859
    {
             if (driver.ast.algebras.find(*(yystack_[3].value.sval))
                 != driver.ast.algebras.end()) {
               error(yystack_[3].location, "Algebra " + *(yystack_[3].value.sval)
                     + " already defined ");
               error(driver.ast.algebras[*(yystack_[3].value.sval)]->location, "here.");
             } else {
               Algebra *algebra = driver.ast.signature->generate((yystack_[3].value.sval), (yystack_[1].value.sval));
               if (algebra)
                 driver.ast.algebras[*(yystack_[3].value.sval)] = algebra;
               else
                 error(yystack_[1].location, "Unknown automatic modifier " + *(yystack_[1].value.sval) +
                       ". Use something like count ...");
             }
           }
#line 1063 "parser.cc" // lalr1.cc:859
    break;

  case 62:
#line 784 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1069 "parser.cc" // lalr1.cc:859
    break;

  case 63:
#line 788 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1075 "parser.cc" // lalr1.cc:859
    break;

  case 64:
#line 798 "../src/parser.y" // lalr1.cc:859
    {
             if (*(yystack_[3].value.sval) != *driver.ast.signature->name)
               error(yystack_[3].location, "Unknown signature " + *(yystack_[3].value.sval) + ".");
             Algebra *a = new Algebra((yystack_[6].value.sval), (yystack_[3].value.sval), yystack_[7].location+yystack_[6].location);
             a->set_default_choice_fn_mode((yystack_[8].value.sval));
             a->set_params((yystack_[1].value.eqs));
             delete (yystack_[1].value.eqs);
             (yylhs.value.algebra) = a; }
#line 1088 "parser.cc" // lalr1.cc:859
    break;

  case 65:
#line 808 "../src/parser.y" // lalr1.cc:859
    {
             if (*(yystack_[3].value.sval) != *driver.ast.signature->name)
               error(yystack_[3].location, "Unknown signature " + *(yystack_[3].value.sval) + ".");
             Algebra *a = new Algebra((yystack_[6].value.sval), (yystack_[3].value.sval), yystack_[7].location+yystack_[6].location);
             a->set_params((yystack_[1].value.eqs));
             delete (yystack_[1].value.eqs);
             (yylhs.value.algebra) = a; }
#line 1100 "parser.cc" // lalr1.cc:859
    break;

  case 66:
#line 817 "../src/parser.y" // lalr1.cc:859
    { 
             Algebra *a = new Algebra((yystack_[3].value.sval), yystack_[4].location+yystack_[3].location);
             if (driver.ast.algebras.find(*(yystack_[0].value.sval)) == driver.ast.algebras.end())
               error(yystack_[1].location+yystack_[0].location, *(yystack_[0].value.sval) + " unknown!");
             else
               *a = *(driver.ast.algebras[*(yystack_[0].value.sval)]->copy());
             (yylhs.value.algebra) = a; }
#line 1112 "parser.cc" // lalr1.cc:859
    break;

  case 73:
#line 862 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::Var_Decl((yystack_[4].value.datatype), (yystack_[3].value.sval), (yystack_[1].value.expr), yystack_[4].location); }
#line 1118 "parser.cc" // lalr1.cc:859
    break;

  case 74:
#line 866 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::Var_Decl((yystack_[4].value.datatype), (yystack_[3].value.sval), (yystack_[1].value.expr), yystack_[4].location); }
#line 1124 "parser.cc" // lalr1.cc:859
    break;

  case 75:
#line 878 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::Var_Decl((yystack_[4].value.datatype), (yystack_[3].value.sval), (yystack_[1].value.expr), yystack_[4].location); }
#line 1130 "parser.cc" // lalr1.cc:859
    break;

  case 76:
#line 882 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::Var_Decl((yystack_[2].value.datatype), (yystack_[1].value.sval), yystack_[2].location); }
#line 1136 "parser.cc" // lalr1.cc:859
    break;

  case 77:
#line 885 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = (yystack_[0].value.statement); }
#line 1142 "parser.cc" // lalr1.cc:859
    break;

  case 78:
#line 889 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1148 "parser.cc" // lalr1.cc:859
    break;

  case 79:
#line 889 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1154 "parser.cc" // lalr1.cc:859
    break;

  case 80:
#line 891 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1160 "parser.cc" // lalr1.cc:859
    break;

  case 81:
#line 891 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1166 "parser.cc" // lalr1.cc:859
    break;

  case 82:
#line 896 "../src/parser.y" // lalr1.cc:859
    { hashtable<std::string, Type::Base*> *h =
         new hashtable<std::string, Type::Base*>();
        Algebra::add_sig_var(*h, *(yystack_[0].value.eq), yystack_[0].location);
        delete (yystack_[0].value.eq)->first;
        delete (yystack_[0].value.eq);
        (yylhs.value.eqs) = h; }
#line 1177 "parser.cc" // lalr1.cc:859
    break;

  case 83:
#line 903 "../src/parser.y" // lalr1.cc:859
    { Algebra::add_sig_var(*(yystack_[2].value.eqs), *(yystack_[0].value.eq), yystack_[0].location);
        delete (yystack_[0].value.eq)->first;
        delete (yystack_[0].value.eq);
        (yylhs.value.eqs) = (yystack_[2].value.eqs); }
#line 1186 "parser.cc" // lalr1.cc:859
    break;

  case 84:
#line 909 "../src/parser.y" // lalr1.cc:859
    { yylhs.location= yystack_[2].location;
    (yylhs.value.eq) = new std::pair<std::string*, Type::Base*>((yystack_[2].value.sval), (yystack_[0].value.datatype)); }
#line 1193 "parser.cc" // lalr1.cc:859
    break;

  case 85:
#line 912 "../src/parser.y" // lalr1.cc:859
    { driver.ast.get_type(*(yystack_[0].value.sval), yystack_[0].location); (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1199 "parser.cc" // lalr1.cc:859
    break;

  case 86:
#line 913 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = new std::string("alphabet"); }
#line 1205 "parser.cc" // lalr1.cc:859
    break;

  case 87:
#line 917 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1211 "parser.cc" // lalr1.cc:859
    break;

  case 88:
#line 924 "../src/parser.y" // lalr1.cc:859
    {
           hashtable<std::string, Fn_Def *> *h =
               new hashtable<std::string, Fn_Def*>();
           (yylhs.value.fn_defs) = h;
         }
#line 1221 "parser.cc" // lalr1.cc:859
    break;

  case 89:
#line 931 "../src/parser.y" // lalr1.cc:859
    { hashtable<std::string, Fn_Def*>::iterator i = (yystack_[1].value.fn_defs)->find(*(yystack_[0].value.fn_def)->name);
           if (i != (yystack_[1].value.fn_defs)->end()) {
             error((yystack_[0].value.fn_def)->location, "Algebra function " + *(yystack_[0].value.fn_def)->name+ " redefined");
             error(i->second->location, "here.");
           } else {
            (*(yystack_[1].value.fn_defs))[*(yystack_[0].value.fn_def)->name] = (yystack_[0].value.fn_def);
           }
           (yylhs.value.fn_defs) = (yystack_[1].value.fn_defs);
         }
#line 1235 "parser.cc" // lalr1.cc:859
    break;

  case 90:
#line 943 "../src/parser.y" // lalr1.cc:859
    { Fn_Def *f = new Fn_Def((yystack_[8].value.datatype), (yystack_[7].value.sval), yystack_[7].location);
       f->set_paras(*(yystack_[5].value.para_decls));
       delete (yystack_[5].value.para_decls);
       f->set_ntparas((yystack_[4].value.para_decls));
       delete (yystack_[4].value.para_decls);
       f->set_statements(*(yystack_[1].value.statements));
       delete((yystack_[1].value.statements));
       f->set_mode((yystack_[9].value.sval));
       delete (yystack_[9].value.sval);
       (yylhs.value.fn_def) = f;
     }
#line 1251 "parser.cc" // lalr1.cc:859
    break;

  case 91:
#line 956 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.para_decls) = (yystack_[0].value.para_decls); }
#line 1257 "parser.cc" // lalr1.cc:859
    break;

  case 92:
#line 958 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.para_decls) = 0; }
#line 1263 "parser.cc" // lalr1.cc:859
    break;

  case 93:
#line 961 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1269 "parser.cc" // lalr1.cc:859
    break;

  case 94:
#line 961 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = 0; }
#line 1275 "parser.cc" // lalr1.cc:859
    break;

  case 95:
#line 963 "../src/parser.y" // lalr1.cc:859
    { 
              (yylhs.value.para_decls) = new std::list<Para_Decl::Base*>();
            }
#line 1283 "parser.cc" // lalr1.cc:859
    break;

  case 96:
#line 967 "../src/parser.y" // lalr1.cc:859
    {
              std::list<Para_Decl::Base*> *l =
                new std::list<Para_Decl::Base*>();
              l->push_back((yystack_[0].value.para_decl));
              (yylhs.value.para_decls) = l;
            }
#line 1294 "parser.cc" // lalr1.cc:859
    break;

  case 97:
#line 973 "../src/parser.y" // lalr1.cc:859
    {
              (yystack_[2].value.para_decls)->push_back((yystack_[0].value.para_decl));
              (yylhs.value.para_decls) = (yystack_[2].value.para_decls);
            }
#line 1303 "parser.cc" // lalr1.cc:859
    break;

  case 98:
#line 979 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.para_decl) = new Para_Decl::Simple((yystack_[1].value.datatype), (yystack_[0].value.sval), yylhs.location); }
#line 1309 "parser.cc" // lalr1.cc:859
    break;

  case 99:
#line 982 "../src/parser.y" // lalr1.cc:859
    {
             Para_Decl::Multi *m = new Para_Decl::Multi(*(yystack_[1].value.para_decls), yylhs.location);
             delete (yystack_[1].value.para_decls);
             (yylhs.value.para_decl) = m;
           }
#line 1319 "parser.cc" // lalr1.cc:859
    break;

  case 100:
#line 988 "../src/parser.y" // lalr1.cc:859
    {
             static unsigned x = 0;
             std::ostringstream o;
             o << "VOID_INTERNAL" << x++;
             (yylhs.value.para_decl) = new Para_Decl::Simple
               (new Type::Void(yystack_[0].location), new std::string(o.str()));
           }
#line 1331 "parser.cc" // lalr1.cc:859
    break;

  case 101:
#line 1047 "../src/parser.y" // lalr1.cc:859
    { std::list<Statement::Base*> *l =
              new std::list<Statement::Base*>();
            l->push_back((yystack_[0].value.statement));
            (yylhs.value.statements) = l; }
#line 1340 "parser.cc" // lalr1.cc:859
    break;

  case 102:
#line 1053 "../src/parser.y" // lalr1.cc:859
    { (yystack_[1].value.statements)->push_back((yystack_[0].value.statement));
            (yylhs.value.statements) = (yystack_[1].value.statements); }
#line 1347 "parser.cc" // lalr1.cc:859
    break;

  case 112:
#line 1067 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::Block(*(yystack_[1].value.statements), yystack_[2].location); }
#line 1353 "parser.cc" // lalr1.cc:859
    break;

  case 113:
#line 1070 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::Continue(yystack_[1].location); }
#line 1359 "parser.cc" // lalr1.cc:859
    break;

  case 114:
#line 1072 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::Break(yystack_[1].location); }
#line 1365 "parser.cc" // lalr1.cc:859
    break;

  case 115:
#line 1075 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::Fn_Call((yystack_[4].value.sval), (yystack_[2].value.exprs), yystack_[4].location);
         delete (yystack_[2].value.exprs);
       }
#line 1373 "parser.cc" // lalr1.cc:859
    break;

  case 116:
#line 1081 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::Return((yystack_[1].value.expr), yystack_[2].location); }
#line 1379 "parser.cc" // lalr1.cc:859
    break;

  case 117:
#line 1087 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::If((yystack_[2].value.expr), (yystack_[0].value.statement), yystack_[4].location); }
#line 1385 "parser.cc" // lalr1.cc:859
    break;

  case 118:
#line 1090 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::If((yystack_[4].value.expr), (yystack_[2].value.statement), (yystack_[0].value.statement), yystack_[6].location); }
#line 1391 "parser.cc" // lalr1.cc:859
    break;

  case 119:
#line 1103 "../src/parser.y" // lalr1.cc:859
    {
     Statement::For *f = new Statement::For(dynamic_cast<Statement::Var_Decl*>((yystack_[5].value.statement)), (yystack_[4].value.expr), (yystack_[2].value.statement), yystack_[7].location);
     if ((yystack_[0].value.statement)->is(Statement::BLOCK)) {
       Statement::Block *b = dynamic_cast<Statement::Block*>((yystack_[0].value.statement));
       f->statements = b->statements;
       delete b;
     } else
       f->push_back((yystack_[0].value.statement));
     (yylhs.value.statement) = f;
   }
#line 1406 "parser.cc" // lalr1.cc:859
    break;

  case 120:
#line 1116 "../src/parser.y" // lalr1.cc:859
    {
       Statement::While *w = new Statement::While((yystack_[2].value.expr), yystack_[4].location);
       if ((yystack_[0].value.statement)->is(Statement::BLOCK)) {
         Statement::Block *b = dynamic_cast<Statement::Block*>((yystack_[0].value.statement));
         w->statements = b->statements;
         delete b;
       } else
         w->push_back((yystack_[0].value.statement));
       (yylhs.value.statement) = w;
     }
#line 1421 "parser.cc" // lalr1.cc:859
    break;

  case 121:
#line 1128 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::Var_Assign((yystack_[3].value.var_acc), (yystack_[1].value.expr), yystack_[2].location); }
#line 1427 "parser.cc" // lalr1.cc:859
    break;

  case 122:
#line 1135 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.statement) = new Statement::Var_Assign((yystack_[2].value.var_acc), (yystack_[0].value.expr), yystack_[1].location); }
#line 1433 "parser.cc" // lalr1.cc:859
    break;

  case 123:
#line 1153 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.var_acc) = new Var_Acc::Plain((yystack_[0].value.sval), yystack_[0].location); }
#line 1439 "parser.cc" // lalr1.cc:859
    break;

  case 124:
#line 1154 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.var_acc) = new Var_Acc::Comp((yystack_[2].value.var_acc), (yystack_[0].value.sval), yylhs.location); }
#line 1445 "parser.cc" // lalr1.cc:859
    break;

  case 125:
#line 1155 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.var_acc) = new Var_Acc::Array((yystack_[3].value.var_acc), (yystack_[1].value.expr), yylhs.location); }
#line 1451 "parser.cc" // lalr1.cc:859
    break;

  case 126:
#line 1166 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Less((yystack_[2].value.expr), (yystack_[0].value.expr), yystack_[1].location); }
#line 1457 "parser.cc" // lalr1.cc:859
    break;

  case 127:
#line 1167 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Greater((yystack_[2].value.expr), (yystack_[0].value.expr), yystack_[1].location); }
#line 1463 "parser.cc" // lalr1.cc:859
    break;

  case 128:
#line 1168 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Less_Eq((yystack_[2].value.expr), (yystack_[0].value.expr), yystack_[1].location); }
#line 1469 "parser.cc" // lalr1.cc:859
    break;

  case 129:
#line 1169 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Greater_Eq((yystack_[2].value.expr), (yystack_[0].value.expr), yystack_[1].location); }
#line 1475 "parser.cc" // lalr1.cc:859
    break;

  case 130:
#line 1170 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Eq((yystack_[2].value.expr), (yystack_[0].value.expr), yystack_[1].location); }
#line 1481 "parser.cc" // lalr1.cc:859
    break;

  case 131:
#line 1171 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Not_Eq((yystack_[2].value.expr), (yystack_[0].value.expr), yystack_[1].location); }
#line 1487 "parser.cc" // lalr1.cc:859
    break;

  case 132:
#line 1172 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::And((yystack_[2].value.expr), (yystack_[0].value.expr), yystack_[1].location); }
#line 1493 "parser.cc" // lalr1.cc:859
    break;

  case 133:
#line 1173 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Or((yystack_[2].value.expr), (yystack_[0].value.expr), yystack_[1].location); }
#line 1499 "parser.cc" // lalr1.cc:859
    break;

  case 134:
#line 1174 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Comp((yystack_[1].value.expr), yylhs.location); }
#line 1505 "parser.cc" // lalr1.cc:859
    break;

  case 135:
#line 1175 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Not((yystack_[0].value.expr), yystack_[1].location); }
#line 1511 "parser.cc" // lalr1.cc:859
    break;

  case 136:
#line 1177 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Plus((yystack_[2].value.expr), (yystack_[0].value.expr), yylhs.location); }
#line 1517 "parser.cc" // lalr1.cc:859
    break;

  case 137:
#line 1178 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Minus((yystack_[2].value.expr), (yystack_[0].value.expr), yylhs.location); }
#line 1523 "parser.cc" // lalr1.cc:859
    break;

  case 138:
#line 1179 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Times((yystack_[2].value.expr), (yystack_[0].value.expr), yylhs.location); }
#line 1529 "parser.cc" // lalr1.cc:859
    break;

  case 139:
#line 1180 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Div((yystack_[2].value.expr), (yystack_[0].value.expr), yylhs.location); }
#line 1535 "parser.cc" // lalr1.cc:859
    break;

  case 140:
#line 1181 "../src/parser.y" // lalr1.cc:859
    { Expr::Fn_Call *f = new Expr::Fn_Call((yystack_[3].value.sval), yylhs.location);
                             if (!f->name)
                               delete (yystack_[3].value.sval);
                             f->exprs = *(yystack_[1].value.exprs);
                             delete (yystack_[1].value.exprs);
                             (yylhs.value.expr) = f; }
#line 1546 "parser.cc" // lalr1.cc:859
    break;

  case 141:
#line 1187 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Vacc((yystack_[0].value.var_acc), yystack_[0].location); }
#line 1552 "parser.cc" // lalr1.cc:859
    break;

  case 142:
#line 1188 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.expr) = new Expr::Const((yystack_[0].value.cons), yystack_[0].location); }
#line 1558 "parser.cc" // lalr1.cc:859
    break;

  case 143:
#line 1190 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.cons) = new Const::Int(*(yystack_[0].value.sval), yystack_[0].location); delete (yystack_[0].value.sval); }
#line 1564 "parser.cc" // lalr1.cc:859
    break;

  case 144:
#line 1192 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.cons) = new Const::Float(*(yystack_[2].value.sval) + "." + *(yystack_[0].value.sval), yylhs.location);
            delete (yystack_[2].value.sval); delete (yystack_[0].value.sval); }
#line 1571 "parser.cc" // lalr1.cc:859
    break;

  case 145:
#line 1194 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.cons) = new Const::Float(*(yystack_[0].value.sval), yystack_[0].location);
                delete (yystack_[0].value.sval);
              }
#line 1579 "parser.cc" // lalr1.cc:859
    break;

  case 146:
#line 1198 "../src/parser.y" // lalr1.cc:859
    {
            (yylhs.value.cons) = new Const::Rational((yystack_[2].value.sval), (yystack_[0].value.sval), yylhs.location);
          }
#line 1587 "parser.cc" // lalr1.cc:859
    break;

  case 147:
#line 1201 "../src/parser.y" // lalr1.cc:859
    { Const::Number *n = dynamic_cast<Const::Number*>((yystack_[0].value.cons));
                     assert(n);
                     n->setNegative();
                     (yylhs.value.cons) = n; }
#line 1596 "parser.cc" // lalr1.cc:859
    break;

  case 148:
#line 1206 "../src/parser.y" // lalr1.cc:859
    { std::list<Expr::Base*> *l =
                           new std::list<Expr::Base*>();
                         l->push_back((yystack_[0].value.expr));
                         (yylhs.value.exprs) = l; }
#line 1605 "parser.cc" // lalr1.cc:859
    break;

  case 149:
#line 1210 "../src/parser.y" // lalr1.cc:859
    { (yystack_[2].value.exprs)->push_back((yystack_[0].value.expr));
                         (yylhs.value.exprs) = (yystack_[2].value.exprs); }
#line 1612 "parser.cc" // lalr1.cc:859
    break;

  case 150:
#line 1214 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.exprs) = (yystack_[0].value.exprs); }
#line 1618 "parser.cc" // lalr1.cc:859
    break;

  case 151:
#line 1216 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.exprs) = new std::list<Expr::Base*>(); }
#line 1624 "parser.cc" // lalr1.cc:859
    break;

  case 152:
#line 1227 "../src/parser.y" // lalr1.cc:859
    {
            std::list<Grammar*> *l = new std::list<Grammar*>();
            l->push_back((yystack_[0].value.grammar));
            (yylhs.value.grammars) = l;
          }
#line 1634 "parser.cc" // lalr1.cc:859
    break;

  case 153:
#line 1234 "../src/parser.y" // lalr1.cc:859
    {
            (yystack_[1].value.grammars)->push_back((yystack_[0].value.grammar));
            (yylhs.value.grammars) = (yystack_[1].value.grammars);
          }
#line 1643 "parser.cc" // lalr1.cc:859
    break;

  case 154:
#line 1241 "../src/parser.y" // lalr1.cc:859
    {
           if (*(yystack_[8].value.sval) != *driver.ast.signature->name)
             error(yystack_[8].location, "Unknown signature " + *(yystack_[8].value.sval) + ".");
           Grammar *g = new Grammar(driver.ast, (yystack_[10].value.sval), (yystack_[8].value.sval), (yystack_[4].value.sval), yystack_[11].location + yystack_[10].location);
           g->axiom_loc = yystack_[4].location;
          if ((yystack_[1].value.grammar_body)->first) {
            g->tab_names = *(yystack_[1].value.grammar_body)->first;
            delete (yystack_[1].value.grammar_body)->first;
          }
          assert((yystack_[1].value.grammar_body));
          assert((yystack_[1].value.grammar_body)->second);
          for (std::list<Symbol::NT*>::iterator i = (yystack_[1].value.grammar_body)->second->begin();
               i != (yystack_[1].value.grammar_body)->second->end(); ++i)
            g->add_nt(*i);
          (yylhs.value.grammar) = g;
         }
#line 1664 "parser.cc" // lalr1.cc:859
    break;

  case 155:
#line 1260 "../src/parser.y" // lalr1.cc:859
    {
                assert((yystack_[1].value.args));
                assert((yystack_[0].value.productions));
                (yylhs.value.grammar_body) = new std::pair< hashtable<std::string, Arg*>*,
                                    std::list<Symbol::NT*>*>((yystack_[1].value.args), (yystack_[0].value.productions));
              }
#line 1675 "parser.cc" // lalr1.cc:859
    break;

  case 156:
#line 1268 "../src/parser.y" // lalr1.cc:859
    {
                assert((yystack_[0].value.productions));
                (yylhs.value.grammar_body) = new std::pair< hashtable<std::string, Arg*>*,
                                    std::list<Symbol::NT*>*>(0, (yystack_[0].value.productions));
              }
#line 1685 "parser.cc" // lalr1.cc:859
    break;

  case 157:
#line 1286 "../src/parser.y" // lalr1.cc:859
    {
           (yylhs.value.args) = (yystack_[1].value.args);
         }
#line 1693 "parser.cc" // lalr1.cc:859
    break;

  case 158:
#line 1297 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1699 "parser.cc" // lalr1.cc:859
    break;

  case 159:
#line 1299 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1705 "parser.cc" // lalr1.cc:859
    break;

  case 160:
#line 1304 "../src/parser.y" // lalr1.cc:859
    {
               std::list<Symbol::NT*> *l = new std::list<Symbol::NT*>();
               l->push_back((yystack_[0].value.production));
               (yylhs.value.productions) = l;
             }
#line 1715 "parser.cc" // lalr1.cc:859
    break;

  case 161:
#line 1311 "../src/parser.y" // lalr1.cc:859
    {
               (yystack_[1].value.productions)->push_back((yystack_[0].value.production));
               (yylhs.value.productions) = (yystack_[1].value.productions);
             }
#line 1724 "parser.cc" // lalr1.cc:859
    break;

  case 162:
#line 1318 "../src/parser.y" // lalr1.cc:859
    {
            Symbol::NT *nt = new Symbol::NT((yystack_[4].value.sval), yystack_[4].location);
            nt->set_alts(*(yystack_[1].value.rhs)->first);
            nt->set_eval_fn((yystack_[1].value.rhs)->second);
            nt->set_ntargs((yystack_[3].value.para_decls));
            delete (yystack_[1].value.rhs)->first;
            delete (yystack_[1].value.rhs);
            delete (yystack_[3].value.para_decls);
            (yylhs.value.production) = nt;
          }
#line 1739 "parser.cc" // lalr1.cc:859
    break;

  case 163:
#line 1331 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.para_decls) = (yystack_[1].value.para_decls); }
#line 1745 "parser.cc" // lalr1.cc:859
    break;

  case 164:
#line 1333 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.para_decls) = 0; }
#line 1751 "parser.cc" // lalr1.cc:859
    break;

  case 165:
#line 1362 "../src/parser.y" // lalr1.cc:859
    { std::pair<std::list<Alt::Base*>*, std::string*> *p =
              new std::pair<std::list<Alt::Base*>*, std::string*>((yystack_[0].value.alts), NULL);
             (yylhs.value.rhs) = p; }
#line 1759 "parser.cc" // lalr1.cc:859
    break;

  case 166:
#line 1366 "../src/parser.y" // lalr1.cc:859
    { std::pair<std::list<Alt::Base*>*, std::string*> *p =
           new std::pair<std::list<Alt::Base*>*, std::string*>((yystack_[2].value.alts), (yystack_[0].value.sval));
         (yylhs.value.rhs) = p; }
#line 1767 "parser.cc" // lalr1.cc:859
    break;

  case 167:
#line 1375 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1773 "parser.cc" // lalr1.cc:859
    break;

  case 168:
#line 1382 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.exprs) = (yystack_[0].value.exprs); }
#line 1779 "parser.cc" // lalr1.cc:859
    break;

  case 169:
#line 1384 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.exprs) = 0; }
#line 1785 "parser.cc" // lalr1.cc:859
    break;

  case 170:
#line 1388 "../src/parser.y" // lalr1.cc:859
    {
         (yystack_[2].value.filters)->push_back((yystack_[0].value.filter));
         (yylhs.value.filters) = (yystack_[2].value.filters);
       }
#line 1794 "parser.cc" // lalr1.cc:859
    break;

  case 171:
#line 1394 "../src/parser.y" // lalr1.cc:859
    {
         std::list<Filter*> *l = new std::list<Filter*>();
         l->push_back((yystack_[0].value.filter));
         (yylhs.value.filters) = l;
       }
#line 1804 "parser.cc" // lalr1.cc:859
    break;

  case 172:
#line 1402 "../src/parser.y" // lalr1.cc:859
    {
        std::list<Alt::Base*> *l = new std::list<Alt::Base*>();
        l->push_back((yystack_[0].value.alt));
        (yylhs.value.alts) = l;
      }
#line 1814 "parser.cc" // lalr1.cc:859
    break;

  case 173:
#line 1408 "../src/parser.y" // lalr1.cc:859
    {
        (yystack_[2].value.alts)->push_back((yystack_[0].value.alt));
        (yylhs.value.alts) = (yystack_[2].value.alts);
      }
#line 1823 "parser.cc" // lalr1.cc:859
    break;

  case 174:
#line 1414 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.alt) = (yystack_[0].value.alt); }
#line 1829 "parser.cc" // lalr1.cc:859
    break;

  case 175:
#line 1416 "../src/parser.y" // lalr1.cc:859
    { std::list<Alt::Base*> *l = new std::list<Alt::Base*>();
            l->push_back((yystack_[0].value.alt));
            (yylhs.value.alts) = l; }
#line 1837 "parser.cc" // lalr1.cc:859
    break;

  case 176:
#line 1420 "../src/parser.y" // lalr1.cc:859
    { (yystack_[2].value.alts)->push_back((yystack_[0].value.alt));
        (yylhs.value.alts) = (yystack_[2].value.alts); }
#line 1844 "parser.cc" // lalr1.cc:859
    break;

  case 177:
#line 1425 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.alt) = new Alt::Block(*(yystack_[1].value.alts), yylhs.location); delete (yystack_[1].value.alts); }
#line 1850 "parser.cc" // lalr1.cc:859
    break;

  case 178:
#line 1428 "../src/parser.y" // lalr1.cc:859
    { Alt::Simple *a = new Alt::Simple((yystack_[4].value.sval), yystack_[4].location);
       a->args = *(yystack_[2].value.rhs_args);
       delete (yystack_[2].value.rhs_args);
       a->set_ntparas((yystack_[1].value.exprs));
       delete (yystack_[1].value.exprs);
       (yylhs.value.alt) = a; }
#line 1861 "parser.cc" // lalr1.cc:859
    break;

  case 179:
#line 1436 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.alt) = new Alt::Link((yystack_[0].value.sval), yystack_[0].location); }
#line 1867 "parser.cc" // lalr1.cc:859
    break;

  case 180:
#line 1439 "../src/parser.y" // lalr1.cc:859
    {
       if (((yystack_[1].value.filter_kw) == Filter::WITH_OVERLAY || (yystack_[1].value.filter_kw) == Filter::SUCHTHAT_OVERLAY)
           && !(yystack_[2].value.alt)->is(Alt::SIMPLE))
         error(yystack_[1].location, "Overlay filtering makes only sense with a function symbol"
                   " on the lhs.");

       (yystack_[0].value.filter)->type = (yystack_[1].value.filter_kw);
       (yystack_[2].value.alt)->filters.push_back((yystack_[0].value.filter));
       (yylhs.value.alt) = (yystack_[2].value.alt);
     }
#line 1882 "parser.cc" // lalr1.cc:859
    break;

  case 181:
#line 1464 "../src/parser.y" // lalr1.cc:859
    {
       Alt::Multi *r = new Alt::Multi(*(yystack_[1].value.alts), yylhs.location);
       delete (yystack_[1].value.alts);
       (yylhs.value.alt) = r;
     }
#line 1892 "parser.cc" // lalr1.cc:859
    break;

  case 182:
#line 1471 "../src/parser.y" // lalr1.cc:859
    {
       (yystack_[4].value.alt)->add_multitrack_filter(*(yystack_[1].value.filters), (yystack_[3].value.filter_kw), yystack_[1].location);
       delete (yystack_[1].value.filters);
       (yylhs.value.alt) = (yystack_[4].value.alt);
     }
#line 1902 "parser.cc" // lalr1.cc:859
    break;

  case 183:
#line 1517 "../src/parser.y" // lalr1.cc:859
    {
       (yystack_[5].value.alt)->set_ntparas(yystack_[5].location+yystack_[3].location, (yystack_[2].value.exprs));
       delete (yystack_[2].value.exprs);
       (yylhs.value.alt) = (yystack_[5].value.alt);
     }
#line 1912 "parser.cc" // lalr1.cc:859
    break;

  case 184:
#line 1535 "../src/parser.y" // lalr1.cc:859
    {
       Alt::Link *l = new Alt::Link((yystack_[3].value.sval), yystack_[3].location);
       l->set_indices(*(yystack_[1].value.exprs));
       delete (yystack_[1].value.exprs);
       (yylhs.value.alt) =  l;
     }
#line 1923 "parser.cc" // lalr1.cc:859
    break;

  case 185:
#line 1543 "../src/parser.y" // lalr1.cc:859
    {
       (yystack_[2].value.alt)->set_index_overlay((yystack_[5].value.alt));
       (yylhs.value.alt) = (yystack_[2].value.alt);
     }
#line 1932 "parser.cc" // lalr1.cc:859
    break;

  case 186:
#line 1549 "../src/parser.y" // lalr1.cc:859
    {
       (yystack_[1].value.alt)->set_index_stmts(*(yystack_[5].value.statements));
       delete (yystack_[5].value.statements);
       (yylhs.value.alt) = (yystack_[1].value.alt);
     }
#line 1942 "parser.cc" // lalr1.cc:859
    break;

  case 187:
#line 1588 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1948 "parser.cc" // lalr1.cc:859
    break;

  case 188:
#line 1590 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 1954 "parser.cc" // lalr1.cc:859
    break;

  case 189:
#line 1594 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.filter_kw) = Filter::WITH; }
#line 1960 "parser.cc" // lalr1.cc:859
    break;

  case 190:
#line 1595 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.filter_kw) = Filter::SUCHTHAT; }
#line 1966 "parser.cc" // lalr1.cc:859
    break;

  case 191:
#line 1596 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.filter_kw) = Filter::WITH_OVERLAY; }
#line 1972 "parser.cc" // lalr1.cc:859
    break;

  case 192:
#line 1597 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.filter_kw) = Filter::SUCHTHAT_OVERLAY; }
#line 1978 "parser.cc" // lalr1.cc:859
    break;

  case 193:
#line 1627 "../src/parser.y" // lalr1.cc:859
    { Filter *f = new Filter((yystack_[0].value.sval), yystack_[0].location);
             (yylhs.value.filter) = f;
           }
#line 1986 "parser.cc" // lalr1.cc:859
    break;

  case 194:
#line 1631 "../src/parser.y" // lalr1.cc:859
    { Filter *f = new Filter((yystack_[3].value.sval), yystack_[3].location);
             f->args = *(yystack_[1].value.exprs);
             f->init_builtin();
             delete (yystack_[1].value.exprs);
             (yylhs.value.filter) = f; }
#line 1996 "parser.cc" // lalr1.cc:859
    break;

  case 195:
#line 1657 "../src/parser.y" // lalr1.cc:859
    { std::list<Fn_Arg::Base*> *l = new std::list<Fn_Arg::Base*>();
            l->push_back((yystack_[0].value.rhs_arg));
            (yylhs.value.rhs_args) = l; }
#line 2004 "parser.cc" // lalr1.cc:859
    break;

  case 196:
#line 1662 "../src/parser.y" // lalr1.cc:859
    { (yystack_[2].value.rhs_args)->push_back((yystack_[0].value.rhs_arg));
            (yylhs.value.rhs_args) = (yystack_[2].value.rhs_args); }
#line 2011 "parser.cc" // lalr1.cc:859
    break;

  case 197:
#line 1667 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.rhs_arg) = new Fn_Arg::Alt((yystack_[0].value.alt), yystack_[0].location); }
#line 2017 "parser.cc" // lalr1.cc:859
    break;

  case 198:
#line 1669 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.rhs_arg) = new Fn_Arg::Const((yystack_[0].value.cons), yystack_[0].location); }
#line 2023 "parser.cc" // lalr1.cc:859
    break;

  case 199:
#line 1672 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.cons) = (yystack_[0].value.cons);}
#line 2029 "parser.cc" // lalr1.cc:859
    break;

  case 200:
#line 1674 "../src/parser.y" // lalr1.cc:859
    {
         (yylhs.value.cons) = new Const::Char(*(yystack_[1].value.sval), yylhs.location);
         delete (yystack_[1].value.sval);
       }
#line 2038 "parser.cc" // lalr1.cc:859
    break;

  case 201:
#line 1678 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.cons) = new Const::String((yystack_[1].value.sval), yylhs.location); }
#line 2044 "parser.cc" // lalr1.cc:859
    break;

  case 202:
#line 1739 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 2050 "parser.cc" // lalr1.cc:859
    break;

  case 203:
#line 1739 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = new std::string(); }
#line 2056 "parser.cc" // lalr1.cc:859
    break;

  case 204:
#line 1741 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.sval) = (yystack_[0].value.sval); }
#line 2062 "parser.cc" // lalr1.cc:859
    break;

  case 207:
#line 1755 "../src/parser.y" // lalr1.cc:859
    { driver.ast.instances = *(yystack_[0].value.instances); delete (yystack_[0].value.instances); }
#line 2068 "parser.cc" // lalr1.cc:859
    break;

  case 208:
#line 1757 "../src/parser.y" // lalr1.cc:859
    { driver.ast.first_instance = (yystack_[0].value.instance);
                       hashtable<std::string, Instance*> *h =
                       new hashtable<std::string, Instance*>();
                       (*h)[*(yystack_[0].value.instance)->name()] = (yystack_[0].value.instance);
                       (yylhs.value.instances) = h; }
#line 2078 "parser.cc" // lalr1.cc:859
    break;

  case 209:
#line 1762 "../src/parser.y" // lalr1.cc:859
    { 
              hashtable<std::string, Instance*>::iterator i = 
                (yystack_[1].value.instances)->find(*(yystack_[0].value.instance)->name());
              if (i != (yystack_[1].value.instances)->end()) {
                error((yystack_[0].value.instance)->location, "Instance " + *(yystack_[0].value.instance)->name() +
                  " already defined");
                error(i->second->location, "here.");
              } else {
                (*(yystack_[1].value.instances))[*(yystack_[0].value.instance)->name()] = (yystack_[0].value.instance);
              }
              (yylhs.value.instances) = (yystack_[1].value.instances);
            }
#line 2095 "parser.cc" // lalr1.cc:859
    break;

  case 210:
#line 1775 "../src/parser.y" // lalr1.cc:859
    { driver.ast.algebra_seen.clear(); }
#line 2101 "parser.cc" // lalr1.cc:859
    break;

  case 211:
#line 1776 "../src/parser.y" // lalr1.cc:859
    {
            Instance *i = new Instance((yystack_[7].value.i_lhs)->first, (yystack_[2].value.product), yystack_[7].location);
            if (!driver.ast.grammar_defined(*(yystack_[5].value.sval)))
              error(yystack_[5].location, "Grammar " + *(yystack_[5].value.sval) + " is not defined.");
            else
              i->set_grammar(driver.ast.grammar(*(yystack_[5].value.sval)));
            delete (yystack_[7].value.i_lhs);
            (yylhs.value.instance) = i;
          }
#line 2115 "parser.cc" // lalr1.cc:859
    break;

  case 212:
#line 1792 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.i_lhs) = new std::pair<std::string*,hashtable<std::string, Arg*>*>
                  ((yystack_[0].value.sval), NULL);
         }
#line 2123 "parser.cc" // lalr1.cc:859
    break;

  case 213:
#line 1796 "../src/parser.y" // lalr1.cc:859
    { yylhs.location = yystack_[3].location ;
           (yylhs.value.i_lhs) = new std::pair<std::string*,hashtable<std::string, Arg*>*>
                  ((yystack_[3].value.sval), (yystack_[1].value.args));
         }
#line 2132 "parser.cc" // lalr1.cc:859
    break;

  case 214:
#line 1802 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.product) = new Product::Times((yystack_[2].value.product), (yystack_[0].value.product), yystack_[1].location); }
#line 2138 "parser.cc" // lalr1.cc:859
    break;

  case 215:
#line 1808 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.product) = new Product::Klass((yystack_[2].value.product), (yystack_[0].value.product), yystack_[1].location); }
#line 2144 "parser.cc" // lalr1.cc:859
    break;

  case 216:
#line 1814 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.product) = new Product::Cartesian((yystack_[2].value.product), (yystack_[0].value.product), yystack_[1].location); }
#line 2150 "parser.cc" // lalr1.cc:859
    break;

  case 217:
#line 1820 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.product) = new Product::Pareto((yystack_[2].value.product), (yystack_[0].value.product), yystack_[1].location); }
#line 2156 "parser.cc" // lalr1.cc:859
    break;

  case 218:
#line 1826 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.product) = new Product::Takeone((yystack_[2].value.product), (yystack_[0].value.product), yystack_[0].location); }
#line 2162 "parser.cc" // lalr1.cc:859
    break;

  case 219:
#line 1833 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.product) = new Product::Overlay((yystack_[2].value.product), (yystack_[0].value.product), yystack_[1].location); }
#line 2168 "parser.cc" // lalr1.cc:859
    break;

  case 220:
#line 1861 "../src/parser.y" // lalr1.cc:859
    {
           /* $1->set_defaults(); */
           (yylhs.value.product) = (yystack_[3].value.product);
         }
#line 2177 "parser.cc" // lalr1.cc:859
    break;

  case 221:
#line 1868 "../src/parser.y" // lalr1.cc:859
    { (yylhs.value.product) = (yystack_[1].value.product); }
#line 2183 "parser.cc" // lalr1.cc:859
    break;

  case 222:
#line 1870 "../src/parser.y" // lalr1.cc:859
    { Product::Single *p = new Product::Single((yystack_[0].value.sval), yystack_[0].location);
           hashtable<std::string, Algebra*>::iterator i =
             driver.ast.algebras.find(*(yystack_[0].value.sval));
           if (i == driver.ast.algebras.end())
             error(yystack_[0].location, "Algebra " + *(yystack_[0].value.sval) + " not defined.");
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
           (yylhs.value.product) = p;
         }
#line 2210 "parser.cc" // lalr1.cc:859
    break;

  case 223:
#line 1900 "../src/parser.y" // lalr1.cc:859
    {
           (yystack_[2].value.product)->set_filter((yystack_[0].value.filter));
           (yylhs.value.product) = (yystack_[2].value.product);
         }
#line 2219 "parser.cc" // lalr1.cc:859
    break;


#line 2223 "parser.cc" // lalr1.cc:859
            default:
              break;
            }
        }
      catch (const syntax_error& yyexc)
        {
          error (yyexc);
          YYERROR;
        }
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;
      YY_STACK_PRINT ();

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, yylhs);
    }
    goto yynewstate;

  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        error (yyla.location, yysyntax_error_ (yystack_[0].state, yyla));
      }


    yyerror_range[1].location = yyla.location;
    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.type_get () == yyeof_)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:

    /* Pacify compilers like GCC when the user code never invokes
       YYERROR and the label yyerrorlab therefore never appears in user
       code.  */
    if (false)
      goto yyerrorlab;
    yyerror_range[1].location = yystack_[yylen - 1].location;
    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    goto yyerrlab1;

  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    {
      stack_symbol_type error_token;
      for (;;)
        {
          yyn = yypact_[yystack_[0].state];
          if (!yy_pact_value_is_default_ (yyn))
            {
              yyn += yyterror_;
              if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == yyterror_)
                {
                  yyn = yytable_[yyn];
                  if (0 < yyn)
                    break;
                }
            }

          // Pop the current state because it cannot handle the error token.
          if (yystack_.size () == 1)
            YYABORT;

          yyerror_range[1].location = yystack_[0].location;
          yy_destroy_ ("Error: popping", yystack_[0]);
          yypop_ ();
          YY_STACK_PRINT ();
        }

      yyerror_range[2].location = yyla.location;
      YYLLOC_DEFAULT (error_token.location, yyerror_range, 2);

      // Shift the error token.
      error_token.state = yyn;
      yypush_ ("Shifting", error_token);
    }
    goto yynewstate;

    // Accept.
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;

    // Abort.
  yyabortlab:
    yyresult = 1;
    goto yyreturn;

  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack"
                 << std::endl;
        // Do not try to display the values of the reclaimed symbols,
        // as their printer might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
  }

  void
  Parser::error (const syntax_error& yyexc)
  {
    error (yyexc.location, yyexc.what());
  }

  // Generate an error message.
  std::string
  Parser::yysyntax_error_ (state_type yystate, const symbol_type& yyla) const
  {
    // Number of reported tokens (one for the "unexpected", one per
    // "expected").
    size_t yycount = 0;
    // Its maximum.
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    // Arguments of yyformat.
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];

    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yyla) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is
         a consistent state with a default action.  There might have
         been a previous inconsistent state, consistent state with a
         non-default action, or user semantic action that manipulated
         yyla.  (However, yyla is currently not documented for users.)
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state
         merging (from LALR or IELR) and default reductions corrupt the
         expected token list.  However, the list is correct for
         canonical LR with one exception: it will still contain any
         token that will not be accepted due to an error action in a
         later state.
    */
    if (!yyla.empty ())
      {
        int yytoken = yyla.type_get ();
        yyarg[yycount++] = yytname_[yytoken];
        int yyn = yypact_[yystate];
        if (!yy_pact_value_is_default_ (yyn))
          {
            /* Start YYX at -YYN if negative to avoid negative indexes in
               YYCHECK.  In other words, skip the first -YYN actions for
               this state because they are default actions.  */
            int yyxbegin = yyn < 0 ? -yyn : 0;
            // Stay within bounds of both yycheck and yytname.
            int yychecklim = yylast_ - yyn + 1;
            int yyxend = yychecklim < yyntokens_ ? yychecklim : yyntokens_;
            for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
              if (yycheck_[yyx + yyn] == yyx && yyx != yyterror_
                  && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
                {
                  if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                    {
                      yycount = 1;
                      break;
                    }
                  else
                    yyarg[yycount++] = yytname_[yyx];
                }
          }
      }

    char const* yyformat = YY_NULLPTR;
    switch (yycount)
      {
#define YYCASE_(N, S)                         \
        case N:                               \
          yyformat = S;                       \
        break
        YYCASE_(0, YY_("syntax error"));
        YYCASE_(1, YY_("syntax error, unexpected %s"));
        YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
        YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
        YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
        YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
      }

    std::string yyres;
    // Argument number.
    size_t yyi = 0;
    for (char const* yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount)
        {
          yyres += yytnamerr_ (yyarg[yyi++]);
          ++yyp;
        }
      else
        yyres += *yyp;
    return yyres;
  }


  const short int Parser::yypact_ninf_ = -381;

  const short int Parser::yytable_ninf_ = -188;

  const short int
  Parser::yypact_[] =
  {
     141,    -5,  -381,   108,    -9,  -381,    93,    -5,  -381,    -3,
    -381,  -381,   127,  -381,    -7,   144,  -381,  -381,  -381,    -3,
    -381,   575,   117,  -381,   176,  -381,    36,   170,   144,  -381,
     525,    36,    -3,    -3,    -3,    -3,    -3,    -3,    36,  -381,
      83,  -381,  -381,  -381,  -381,   180,    36,   331,  -381,  -381,
     152,  -381,    -2,    -2,    -2,    -2,    -2,    -2,  -381,   138,
      96,  -381,  -381,   176,   148,   175,    36,  -381,   238,   331,
    -381,   255,   204,    66,   274,    66,    66,    36,  -381,  -381,
    -381,  -381,  -381,  -381,   276,   242,  -381,  -381,   271,    19,
      36,   238,  -381,  -381,    36,  -381,   233,    66,    51,  -381,
     102,   293,    66,   304,    55,   596,  -381,   130,  -381,  -381,
    -381,   596,   306,  -381,   236,   274,   178,  -381,   251,   256,
     327,   242,    27,  -381,   313,   340,  -381,   300,    23,    66,
    -381,   342,   344,  -381,  -381,   307,   536,  -381,   309,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,  -381,  -381,  -381,  -381,   242,  -381,    36,  -381,
    -381,   314,    36,   242,  -381,  -381,   107,   372,   372,    36,
    -381,  -381,   340,  -381,   375,  -381,  -381,  -381,   183,   335,
     334,  -381,  -381,  -381,  -381,  -381,   422,   353,   353,   353,
     353,   611,   353,   353,   322,   322,  -381,  -381,   596,  -381,
     110,  -381,  -381,   343,  -381,   347,  -381,  -381,  -381,  -381,
     345,   364,   366,   373,  -381,   372,   242,  -381,    36,  -381,
      36,   242,  -381,   183,    66,    32,   429,    36,    36,   376,
    -381,   379,  -381,  -381,   377,    85,   380,    36,   483,  -381,
    -381,   250,  -381,   378,  -381,   383,   252,  -381,    32,   202,
     242,  -381,   381,  -381,   385,  -381,  -381,    32,  -381,   242,
     435,  -381,   391,   258,   109,   202,    36,   151,  -381,  -381,
    -381,   234,  -381,  -381,  -381,   392,    -3,  -381,    88,  -381,
     202,   202,   393,   242,  -381,  -381,   155,   401,   614,  -381,
    -381,   400,   403,    97,   234,   397,    31,   398,   487,  -381,
    -381,  -381,   408,   404,   419,    36,    36,  -381,  -381,   423,
     427,   111,   430,    66,   425,   433,   487,    36,  -381,  -381,
     432,   103,  -381,  -381,  -381,  -381,  -381,  -381,  -381,  -381,
    -381,   172,    36,   202,   440,  -381,    36,  -381,   242,    66,
      66,   362,  -381,  -381,   143,   158,    66,  -381,  -381,    66,
     227,   268,    38,    36,    66,   544,   588,  -381,  -381,    66,
    -381,   283,   368,  -381,  -381,   439,    38,   443,    38,   441,
     -21,   296,   445,   450,   459,   374,   487,   487,   428,   446,
    -381,   122,  -381,   296,   487,   114,  -381,    38,   506,  -381,
    -381,  -381,  -381,   194,    10,   244,    66,    36,  -381,   514,
    -381,  -381,  -381,    38,   452,  -381,   296,  -381,  -381,    38,
      66,    36,  -381,   296,   160,  -381,  -381,    25,   458,   192,
     487,  -381,   468,   190,   284,   123,  -381,   244,    66,   461,
    -381,   487,    66,  -381,   469,   472,   473,  -381,    36,  -381,
     335,  -381,  -381,   596,    38,  -381,  -381,  -381,   285,  -381
  };

  const unsigned char
  Parser::yydefact_[] =
  {
       0,     8,     3,     0,     0,     2,    16,     7,     9,     0,
       1,    13,     0,    11,     0,    22,    10,    79,    78,     0,
     222,     4,     0,    20,     0,    14,     0,     0,    21,    23,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    12,
       0,    17,    19,    81,    80,     0,     0,    57,    24,   221,
     193,   223,   214,   215,   219,   216,   218,   217,   123,     0,
       0,   224,    15,     0,     0,     0,     0,    63,     0,    56,
      58,     0,     0,     0,     0,     0,     0,     0,   220,    18,
      28,    32,    26,    29,     0,     0,    25,    27,     0,    68,
       0,     5,   152,    59,     0,    88,    80,     0,   143,   145,
       0,   203,     0,     0,   141,   148,   199,     0,   142,    36,
     124,   226,     0,   225,     0,     0,     0,    33,     0,     0,
       0,     0,     0,    67,     0,   206,   153,    68,    94,   151,
     135,     0,     0,   147,   202,     0,     0,   204,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   194,   125,    30,    35,     0,    31,     0,    37,
      62,     0,     0,     0,    69,    71,     0,     0,     0,     0,
       6,   205,   207,   208,     0,    60,    93,    89,     0,   150,
       0,   144,   146,   201,   134,   200,   132,   130,   129,   128,
     131,   133,   126,   127,   137,   136,   138,   139,   149,    34,
      44,    40,    42,     0,    61,     0,    70,    72,    66,   158,
       0,     0,   212,     0,   209,     0,     0,    48,     0,   140,
       0,     0,    39,     0,     0,     0,     0,     0,     0,     0,
      49,     0,    41,    50,    43,     0,     0,     0,     0,    86,
      87,     0,    82,     0,    85,     0,     0,   210,     0,    95,
       0,    38,     0,    45,     0,    74,    73,     0,    65,     0,
       0,   213,     0,     0,   100,    95,     0,    92,    96,    51,
      46,     0,    83,    84,   159,     0,     0,    64,     0,    98,
       0,    95,     0,     0,    53,    54,    44,     0,     0,    99,
      97,    91,     0,     0,     0,     0,     0,     0,     0,    52,
      55,    47,     0,   164,     0,     0,   156,   160,   211,     0,
       0,    80,     0,     0,     0,     0,     0,     0,    77,   110,
     123,     0,   101,   103,   104,   111,   105,   106,   107,   108,
     109,     0,     0,    95,     0,   154,   155,   161,     0,     0,
       0,     0,   113,   114,     0,     0,     0,    90,   102,     0,
       0,     0,     0,     0,     0,     0,     0,   116,   112,     0,
      76,     0,     0,   157,   163,   188,     0,     0,     0,     0,
     165,   175,     0,   179,     0,     0,     0,     0,     0,     0,
     121,     0,   172,   174,     0,     0,   162,     0,     0,   189,
     190,   191,   192,     0,     0,     0,     0,     0,   120,   117,
      75,   115,   181,     0,     0,   177,   176,   167,   166,     0,
       0,     0,   180,   197,   169,   195,   198,     0,     0,     0,
       0,   173,     0,     0,     0,     0,   171,     0,     0,     0,
     184,     0,     0,   118,     0,     0,     0,   182,     0,   196,
     168,   178,   119,   122,     0,   185,   183,   170,     0,   186
  };

  const short int
  Parser::yypgoto_[] =
  {
    -381,  -381,  -381,  -381,  -381,  -381,  -381,   528,  -381,  -381,
    -381,   479,   529,  -381,  -381,   517,   -52,   462,  -381,   402,
     434,  -381,  -381,  -381,  -225,   337,   261,  -381,   325,   384,
     278,   272,  -381,  -381,  -381,   498,  -381,   447,  -381,   444,
    -381,  -381,   405,   411,   240,  -381,   410,   -26,   332,   324,
    -381,  -381,  -381,  -381,  -381,  -381,  -233,   299,  -282,  -317,
    -381,  -381,  -381,  -381,  -381,  -381,  -381,  -381,  -381,   -32,
     -66,   505,  -128,  -381,  -381,   518,  -381,  -381,  -161,  -381,
     303,  -292,  -381,  -381,  -381,  -381,  -381,  -381,   207,   245,
    -338,  -381,  -381,  -381,  -376,  -381,   185,  -380,  -381,  -381,
    -381,  -381,  -381,   451,  -381,  -381,   -11,  -381,   541
  };

  const short int
  Parser::yydefgoto_[] =
  {
      -1,     3,     9,     5,   125,     6,     7,     8,    13,    15,
      40,    41,    42,    27,    28,    29,   317,    87,   116,   117,
     110,    47,   203,   119,   200,   201,   222,   235,   236,   237,
     234,   285,   286,    68,    69,    70,   161,    71,    72,   122,
     123,   163,   164,   165,   318,   319,    20,    58,   241,   242,
     243,   244,   128,   177,   282,   178,   267,   268,   321,   322,
     323,   324,   325,   326,   327,   328,   329,   330,   418,   104,
     105,   106,   107,   180,    91,    92,   304,   305,   210,   275,
     306,   307,   334,   369,   408,   429,   425,   381,   382,   370,
     371,   372,   373,   394,    51,   414,   415,   108,   135,   138,
     170,   171,   172,   173,   262,   213,    21,    60,    61
  };

  const short int
  Parser::yytable_[] =
  {
      45,   179,   246,    17,   348,    50,    59,   211,    30,   111,
     112,    11,    86,    23,   337,   416,    43,    18,   412,     4,
      65,    52,    53,    54,    55,    56,    57,   348,   383,    67,
      44,   130,   278,   115,   344,   426,   136,    43,   387,   239,
      89,   166,    43,    31,   337,    59,    24,   416,   291,   406,
     167,    44,   240,   388,   229,    12,    44,   413,   365,   398,
     399,    38,   447,   411,   124,   383,    19,   120,   127,   162,
     302,   423,    43,   186,   187,   188,   189,   190,   191,   192,
     193,   194,   195,   196,   197,   198,    96,   348,   121,   413,
     151,   366,    80,   430,   175,   216,    97,    98,    99,   367,
     351,   368,   404,   433,   115,    81,   448,   350,    10,    43,
      80,   162,   131,    17,   442,   -29,    74,   -32,    14,   309,
     310,   100,    76,   311,   132,   312,   217,    18,    83,   -29,
     101,   -32,   202,    98,    99,   102,   205,    62,   313,   314,
     315,   103,   289,   212,     1,     2,    83,    22,    63,    43,
      80,   299,    84,   280,    85,    80,   251,   100,   238,   309,
     310,    77,   250,   311,   230,   312,   316,    78,    81,   233,
      84,   217,    85,   387,   347,   220,   402,   437,   313,   314,
     315,    39,   221,   217,    26,   405,    83,   403,   438,    82,
      80,    83,   231,   216,   202,   151,    23,   266,   269,    74,
     152,   202,   247,    81,    75,    76,   316,   273,    46,    80,
      84,   254,    85,   266,   358,    84,   280,    85,   361,   284,
     294,    73,    81,   281,   359,   427,    83,   221,   266,   266,
     360,   233,   428,    74,   389,   390,   391,   392,   349,    76,
     279,    80,   284,   156,    88,   264,    64,   341,   157,    80,
      84,   393,    85,    74,    81,   265,    90,   409,   432,    76,
      94,   435,    81,   410,   365,   288,   331,    95,   417,    84,
     303,    85,   320,   355,   356,    98,    99,    83,   118,   303,
     303,   266,   424,   362,   331,    83,   353,   283,   375,   331,
     320,   345,   220,   378,   109,   320,    81,   366,   363,   100,
     440,    84,   129,    85,   154,   367,   202,   368,   101,    84,
     303,    85,   331,   134,   139,   257,   158,   220,   320,   103,
     258,   140,   261,   257,   137,   141,   159,   374,   277,   389,
     390,   391,   392,   280,   142,   143,    66,    67,   364,   144,
     389,   390,   391,   392,   331,   331,   393,   160,   151,   151,
     320,   320,   331,   379,   436,   168,   449,   393,   320,   145,
     146,   147,   148,   149,   150,   419,   443,   169,    50,   121,
     139,   183,   331,   181,   153,   182,   139,   140,   320,   149,
     150,   141,   139,   140,   185,    50,   204,   141,   331,   140,
     142,   143,   209,   141,   320,   144,   142,   143,   215,   331,
     151,   144,   142,   143,   219,   320,   223,   144,   147,   148,
     149,   150,    50,   224,   225,   145,   146,   147,   148,   149,
     150,   145,   146,   147,   148,   149,   150,   145,   146,   147,
     148,   149,   150,   226,   357,   227,   139,   140,   245,   228,
     380,   141,   250,   140,   259,   248,   397,   141,   249,   260,
     142,   143,   253,   270,   271,   274,   142,   143,    43,    80,
     276,   144,   287,   292,   296,   280,   298,   301,   309,   310,
     308,   332,   311,   333,   312,   145,   146,   147,   148,   149,
     150,   145,   146,   147,   148,   149,   150,   313,   314,   315,
     335,   139,   338,    43,    80,    83,   339,   342,   140,   340,
     400,   346,   141,   309,   310,   343,   352,   311,  -187,   312,
     384,   142,   143,   386,   395,   316,   144,   396,   401,    84,
     422,    85,   313,   314,   315,   359,   407,   420,   431,   434,
      83,   441,   444,   445,   446,    16,   145,   146,   147,   148,
     149,   150,    79,    25,   139,    48,   114,   295,   255,   155,
     316,   140,   139,   256,    84,   141,    85,   232,   199,   140,
     252,   293,   218,   141,   142,   143,   300,    93,   206,   144,
      31,   174,   142,   143,   207,   176,   208,   144,   354,   290,
     263,   272,    32,    33,    34,    35,    36,    37,    38,   145,
     146,   147,   148,   149,   150,    49,   139,   145,   146,   147,
     148,   149,   150,   140,   139,   133,   184,   141,   336,   126,
     421,   140,   439,   385,   376,   141,   142,   143,   113,   139,
      31,   144,     0,   214,   142,   143,   140,     0,     0,   144,
     141,     0,    32,    33,    34,    35,    36,    37,    38,   142,
     143,   145,   146,   147,   148,   149,   150,     0,     0,   145,
     146,   147,   148,   149,   150,     0,     0,     0,   377,    31,
       0,     0,     0,     0,   145,   146,   147,   148,   149,   150,
       0,    32,    33,    34,    35,    36,    37,    38,     0,     0,
       0,     0,     0,     0,   297
  };

  const short int
  Parser::yycheck_[] =
  {
      26,   129,   227,     6,   321,    31,    38,   168,    19,    75,
      76,    20,    64,    20,   306,   395,     6,    20,   394,    24,
      46,    32,    33,    34,    35,    36,    37,   344,   366,     6,
      20,    97,   265,    85,   316,   411,   102,     6,    59,     7,
      66,    14,     6,    45,   336,    77,    53,   427,   281,   387,
      23,    20,    20,    74,   215,    64,    20,   395,    20,   376,
     377,    63,   438,    53,    90,   403,    69,    48,    94,   121,
      39,   409,     6,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,    20,   404,    69,   427,
      65,    53,     7,    68,    71,    10,    30,    31,    32,    61,
     333,    63,   384,   420,   156,    20,   444,   332,     0,     6,
       7,   163,    61,     6,   431,     6,    61,     6,    25,    16,
      17,    55,    67,    20,    73,    22,   178,    20,    43,    20,
      64,    20,   158,    31,    32,    69,   162,    54,    35,    36,
      37,    75,    54,   169,     3,     4,    43,    20,    65,     6,
       7,    54,    67,    65,    69,     7,    71,    55,   224,    16,
      17,    65,    65,    20,   216,    22,    63,    71,    20,   221,
      67,   223,    69,    59,    71,    65,    54,    54,    35,    36,
      37,    64,    72,   235,    40,    71,    43,    65,    65,    41,
       7,    43,   218,    10,   220,    65,    20,   249,   250,    61,
      70,   227,   228,    20,    66,    67,    63,   259,    38,     7,
      67,   237,    69,   265,    71,    67,    65,    69,   346,   271,
      65,    69,    20,    72,    66,    65,    43,    72,   280,   281,
      72,   283,    72,    61,    44,    45,    46,    47,    66,    67,
     266,     7,   294,    65,    69,    43,    66,   313,    70,     7,
      67,    61,    69,    61,    20,    53,    18,    63,    66,    67,
       5,    71,    20,    69,    20,   276,   298,    63,   396,    67,
     296,    69,   298,   339,   340,    31,    32,    43,     7,   305,
     306,   333,   410,   349,   316,    43,   338,    53,   354,   321,
     316,   317,    65,   359,    20,   321,    20,    53,    71,    55,
     428,    67,    69,    69,    68,    61,   332,    63,    64,    67,
     336,    69,   344,    20,     8,    65,    65,    65,   344,    75,
      70,    15,    70,    65,    20,    19,    70,   353,    70,    44,
      45,    46,    47,    65,    28,    29,     5,     6,    70,    33,
      44,    45,    46,    47,   376,   377,    61,    20,    65,    65,
     376,   377,   384,    70,    70,    42,    71,    61,   384,    53,
      54,    55,    56,    57,    58,   397,   432,    27,   394,    69,
       8,    64,   404,    31,    68,    31,     8,    15,   404,    57,
      58,    19,     8,    15,    75,   411,    72,    19,   420,    15,
      28,    29,    20,    19,   420,    33,    28,    29,    23,   431,
      65,    33,    28,    29,    70,   431,    63,    33,    55,    56,
      57,    58,   438,    66,    69,    53,    54,    55,    56,    57,
      58,    53,    54,    55,    56,    57,    58,    53,    54,    55,
      56,    57,    58,    69,    72,    69,     8,    15,     9,    66,
      72,    19,    65,    15,    66,    69,    72,    19,    69,    66,
      28,    29,    72,    72,    69,    20,    28,    29,     6,     7,
      69,    33,    70,    70,    63,    65,    63,    70,    16,    17,
      72,    63,    20,    69,    22,    53,    54,    55,    56,    57,
      58,    53,    54,    55,    56,    57,    58,    35,    36,    37,
      71,     8,    69,     6,     7,    43,    69,    72,    15,    69,
      72,    69,    19,    16,    17,    72,    66,    20,    69,    22,
      67,    28,    29,    72,    69,    63,    33,    67,    72,    67,
      68,    69,    35,    36,    37,    66,    20,    13,    70,    61,
      43,    70,    63,    61,    61,     7,    53,    54,    55,    56,
      57,    58,    63,    14,     8,    28,    84,   286,    65,   115,
      63,    15,     8,    70,    67,    19,    69,   220,   156,    15,
     235,   283,   178,    19,    28,    29,   294,    69,   163,    33,
      45,   127,    28,    29,   163,   128,   166,    33,   338,   280,
     248,   257,    57,    58,    59,    60,    61,    62,    63,    53,
      54,    55,    56,    57,    58,    70,     8,    53,    54,    55,
      56,    57,    58,    15,     8,   100,    70,    19,   305,    91,
     403,    15,   427,   368,    70,    19,    28,    29,    77,     8,
      45,    33,    -1,   172,    28,    29,    15,    -1,    -1,    33,
      19,    -1,    57,    58,    59,    60,    61,    62,    63,    28,
      29,    53,    54,    55,    56,    57,    58,    -1,    -1,    53,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    70,    45,
      -1,    -1,    -1,    -1,    53,    54,    55,    56,    57,    58,
      -1,    57,    58,    59,    60,    61,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    70
  };

  const unsigned char
  Parser::yystos_[] =
  {
       0,     3,     4,    77,    24,    79,    81,    82,    83,    78,
       0,    20,    64,    84,    25,    85,    83,     6,    20,    69,
     122,   182,    20,    20,    53,    88,    40,    89,    90,    91,
     182,    45,    57,    58,    59,    60,    61,    62,    63,    64,
      86,    87,    88,     6,    20,   123,    38,    97,    91,    70,
     123,   170,   182,   182,   182,   182,   182,   182,   123,   145,
     183,   184,    54,    65,    66,   123,     5,     6,   109,   110,
     111,   113,   114,    69,    61,    66,    67,    65,    71,    87,
       7,    20,    41,    43,    67,    69,    92,    93,    69,   123,
      18,   150,   151,   111,     5,    63,    20,    30,    31,    32,
      55,    64,    69,    75,   145,   146,   147,   148,   173,    20,
      96,   146,   146,   184,    93,    92,    94,    95,     7,    99,
      48,    69,   115,   116,   123,    80,   151,   123,   128,    69,
     146,    61,    73,   147,    20,   174,   146,    20,   175,     8,
      15,    19,    28,    29,    33,    53,    54,    55,    56,    57,
      58,    65,    70,    68,    68,    96,    65,    70,    65,    70,
      20,   112,    92,   117,   118,   119,    14,    23,    42,    27,
     176,   177,   178,   179,   115,    71,   113,   129,   131,   148,
     149,    31,    31,    64,    70,    75,   146,   146,   146,   146,
     146,   146,   146,   146,   146,   146,   146,   146,   146,    95,
     100,   101,   123,    98,    72,   123,   118,   119,   122,    20,
     154,   154,   123,   181,   179,    23,    10,    92,   105,    70,
      65,    72,   102,    63,    66,    69,    69,    69,    66,   154,
      92,   123,   101,    92,   106,   103,   104,   105,   146,     7,
      20,   124,   125,   126,   127,     9,   100,   123,    69,    69,
      65,    71,   104,    72,   123,    65,    70,    65,    70,    66,
      66,    70,   180,   124,    43,    53,    92,   132,   133,    92,
      72,    69,   125,    92,    20,   155,    69,    70,   132,   123,
      65,    72,   130,    53,    92,   107,   108,    70,   182,    54,
     133,   132,    70,   106,    65,   102,    63,    70,    63,    54,
     107,    70,    39,   123,   152,   153,   156,   157,    72,    16,
      17,    20,    22,    35,    36,    37,    63,    92,   120,   121,
     123,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   145,    63,    69,   158,    71,   156,   157,    69,    69,
      69,   146,    72,    72,   134,   123,    69,    71,   135,    66,
     100,   132,    66,    92,   120,   146,   146,    72,    71,    66,
      72,   148,   146,    71,    70,    20,    53,    61,    63,   159,
     165,   166,   167,   168,   123,   146,    70,    70,   146,    70,
      72,   163,   164,   166,    67,   165,    72,    59,    74,    44,
      45,    46,    47,    61,   169,    69,    67,    72,   135,   135,
      72,    72,    54,    65,   134,    71,   166,    20,   160,    63,
      69,    53,   170,   166,   171,   172,   173,   148,   144,   145,
      13,   164,    68,   166,   148,   162,   170,    65,    72,   161,
      68,    70,    66,   135,    61,    71,    70,    54,    65,   172,
     148,    70,   135,   146,    63,    61,    61,   170,   166,    71
  };

  const unsigned char
  Parser::yyr1_[] =
  {
       0,    76,    77,    78,    77,    80,    79,    81,    81,    82,
      82,    83,    83,    84,    85,    85,    85,    86,    86,    87,
      88,    89,    89,    90,    90,    91,    91,    92,    92,    92,
      92,    92,    93,    94,    94,    95,    96,    98,    97,    99,
     100,   100,   101,   102,   102,   103,   103,   104,   105,   105,
     106,   106,   107,   107,   108,   108,   109,   109,   110,   110,
     111,   111,   112,   113,   114,   114,   114,   115,   115,   116,
     116,   117,   117,   118,   119,   120,   121,   121,   122,   122,
     123,   123,   124,   124,   125,   126,   126,   127,   128,   128,
     129,   130,   130,   131,   131,   132,   132,   132,   133,   133,
     133,   134,   134,   135,   135,   135,   135,   135,   135,   135,
     135,   135,   135,   136,   137,   138,   139,   140,   140,   141,
     142,   143,   144,   145,   145,   145,   146,   146,   146,   146,
     146,   146,   146,   146,   146,   146,   146,   146,   146,   146,
     146,   146,   146,   147,   147,   147,   147,   147,   148,   148,
     149,   149,   150,   150,   151,   152,   152,   153,   154,   155,
     156,   156,   157,   158,   158,   159,   159,   160,   161,   161,
     162,   162,   163,   163,   164,   165,   165,   166,   166,   166,
     166,   166,   166,   166,   166,   166,   166,   167,   168,   169,
     169,   169,   169,   170,   170,   171,   171,   172,   172,   173,
     173,   173,   174,   174,   175,   176,   176,   177,   178,   178,
     180,   179,   181,   181,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   183,   183,   184
  };

  const unsigned char
  Parser::yyr2_[] =
  {
       0,     2,     2,     0,     3,     0,     8,     1,     0,     1,
       2,     2,     4,     1,     2,     4,     0,     1,     3,     1,
       1,     1,     0,     1,     2,     4,     4,     1,     1,     1,
       3,     3,     1,     1,     3,     2,     1,     0,     9,     4,
       1,     3,     1,     2,     0,     2,     3,     6,     1,     2,
       1,     3,     3,     1,     1,     3,     1,     0,     1,     2,
       4,     5,     1,     1,     9,     8,     5,     1,     0,     2,
       3,     1,     2,     5,     5,     5,     3,     1,     1,     1,
       1,     1,     1,     3,     3,     1,     1,     1,     0,     2,
      10,     2,     0,     1,     0,     0,     1,     3,     2,     3,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     2,     2,     5,     3,     5,     7,     8,
       5,     4,     3,     1,     3,     4,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     2,     3,     3,     3,     3,
       4,     1,     1,     1,     3,     1,     3,     2,     1,     3,
       1,     0,     1,     2,    12,     2,     1,     4,     1,     1,
       1,     2,     5,     3,     0,     1,     3,     1,     2,     0,
       3,     1,     1,     3,     1,     1,     3,     3,     5,     1,
       3,     3,     5,     6,     4,     6,     8,     1,     1,     1,
       1,     1,     1,     1,     4,     1,     3,     1,     1,     1,
       3,     3,     1,     0,     1,     1,     0,     1,     1,     2,
       0,     9,     1,     4,     3,     3,     3,     3,     3,     3,
       4,     3,     1,     3,     1,     3,     3
  };



  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a yyntokens_, nonterminals.
  const char*
  const Parser::yytname_[] =
  {
  "\"end of file\"", "error", "$undefined", "START_PROGRAM",
  "START_PRODUCT", "ALGEBRA", "MODE", "\"alphabet keyword\"", "AND",
  "AXIOM", "CHOICE", "DEC", "DEQ", "ELSE", "EXTENDS", "EQ", "FOR", "WHILE",
  "GRAMMAR", "GT", "STRING", "IEQ", "IF", "IMPLEMENTS", "IMPORT", "INPUT",
  "INC", "INSTANCE", "LT", "NEQ", "NOT", "NUMBER", "FLOAT", "OR",
  "PARAMETERS", "RETURN", "CONTINUE", "BREAK", "SIGNATURE", "TABULATED",
  "TYPE", "EXTERN", "USES", "VOID", "WITH", "SUCHTHAT", "WITH_OVERLAY",
  "SUCHTHAT_OVERLAY", "AUTOMATIC", "LEB", "REB", "UNEXPECTED_CHARACTER",
  "LOWER_THAN_ELSE", "'<'", "'>'", "'-'", "'+'", "'*'", "'/'", "'|'",
  "'%'", "'.'", "'^'", "'{'", "'\"'", "','", "'='", "'['", "']'", "'('",
  "')'", "'}'", "';'", "'$'", "'#'", "'\\''", "$accept", "start", "$@1",
  "program", "$@2", "imports_opt", "imports", "import", "module_ident",
  "input_opt", "inputs", "input", "input_specifier", "types_opt", "types",
  "type", "datatype", "type_specifier", "named_datatypes",
  "named_datatype", "name_ident", "signature", "@3", "sig_args", "args",
  "arg", "signtparas", "sig_decls", "decl", "qual_datatype", "datatypes",
  "multi_datatype", "multi_datatypes", "algebras_opt", "algebras",
  "algebra", "automatic_specifier", "mode", "algebra_head", "parameters",
  "parameter_block", "var_decl_inits", "var_decl_init_p",
  "var_decl_init_k", "var_decl_init", "var_decl", "algebra_ident", "ident",
  "eqs", "eq", "sig_var", "sort_ident", "fn_defs", "fn_def", "fnntparas",
  "mode_opt", "para_decls", "para_decl", "statements", "statement",
  "continue", "break", "fn_call", "return", "if", "for", "while", "assign",
  "inc_stmt", "var_access", "expr", "number", "exprs", "exprs_empty",
  "grammars", "grammar", "grammar_body", "tabulated", "signature_ident",
  "nt_ident", "productions", "production", "ntargs", "rhs",
  "choice_fn_ident", "ntparas", "filters", "tracks", "track", "alts",
  "alt", "sig_fn_or_term_ident", "symbol_ident", "filter_kw", "filter_fn",
  "rhs_args", "rhs_arg", "const", "string_constant", "character_constant",
  "instances_opt", "instances", "instances_", "instance", "$@4", "i_lhs",
  "product", "defaults", "default", YY_NULLPTR
  };

#if YYDEBUG
  const unsigned short int
  Parser::yyrline_[] =
  {
       0,   370,   370,   371,   371,   385,   383,   396,   396,   398,
     399,   401,   402,   414,   420,   426,   431,   433,   440,   446,
     483,   488,   488,   490,   490,   492,   495,   505,   507,   510,
     513,   516,   546,   550,   555,   558,   569,   575,   574,   591,
     606,   612,   616,   620,   623,   627,   633,   638,   653,   654,
     664,   671,   677,   684,   698,   705,   728,   728,   730,   730,
     732,   745,   784,   788,   797,   807,   816,   854,   854,   856,
     857,   859,   859,   861,   865,   877,   881,   884,   889,   889,
     891,   891,   895,   902,   908,   912,   913,   917,   924,   930,
     942,   955,   958,   961,   961,   963,   967,   973,   978,   981,
     988,  1046,  1052,  1058,  1059,  1060,  1061,  1062,  1063,  1064,
    1065,  1066,  1067,  1070,  1072,  1074,  1080,  1086,  1089,  1102,
    1115,  1127,  1134,  1153,  1154,  1155,  1166,  1167,  1168,  1169,
    1170,  1171,  1172,  1173,  1174,  1175,  1177,  1178,  1179,  1180,
    1181,  1187,  1188,  1190,  1191,  1194,  1197,  1201,  1206,  1210,
    1213,  1216,  1226,  1233,  1239,  1259,  1267,  1285,  1297,  1299,
    1303,  1310,  1317,  1330,  1333,  1362,  1365,  1375,  1381,  1384,
    1387,  1393,  1401,  1407,  1414,  1416,  1419,  1424,  1427,  1435,
    1438,  1463,  1470,  1516,  1534,  1542,  1548,  1588,  1590,  1594,
    1595,  1596,  1597,  1626,  1630,  1656,  1661,  1666,  1668,  1672,
    1673,  1678,  1739,  1739,  1741,  1753,  1753,  1755,  1757,  1762,
    1775,  1775,  1791,  1795,  1802,  1808,  1814,  1820,  1826,  1833,
    1860,  1868,  1869,  1899,  1920,  1920,  1922
  };

  // Print the state stack on the debug stream.
  void
  Parser::yystack_print_ ()
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << i->state;
    *yycdebug_ << std::endl;
  }

  // Report on the debug stream that the rule \a yyrule is going to be reduced.
  void
  Parser::yy_reduce_print_ (int yyrule)
  {
    unsigned int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):" << std::endl;
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG

  // Symbol number corresponding to token number t.
  inline
  Parser::token_number_type
  Parser::yytranslate_ (int t)
  {
    static
    const token_number_type
    translate_table[] =
    {
     0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    64,    74,    73,    60,     2,    75,
      69,    70,    57,    56,    65,    55,    61,    58,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    72,
      53,    66,    54,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    67,     2,    68,    62,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    63,    59,    71,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52
    };
    const unsigned int user_token_number_max_ = 307;
    const token_number_type undef_token_ = 2;

    if (static_cast<int>(t) <= yyeof_)
      return yyeof_;
    else if (static_cast<unsigned int> (t) <= user_token_number_max_)
      return translate_table[t];
    else
      return undef_token_;
  }


} // yy
#line 3020 "parser.cc" // lalr1.cc:1167
#line 1924 "../src/parser.y" // lalr1.cc:1168


void yy::Parser::error(const yy::Parser::location_type &l, const std::string& m)
{
  driver.error(l, m);
}

