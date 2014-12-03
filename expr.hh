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


#ifndef EXPR_HH
#define EXPR_HH


#include "loc.hh"

#include <string>
#include <list>
#include <ostream>
#include <cassert>
#include <map>

#include "statement_fwd.hh"
#include "type_fwd.hh"
#include "const_fwd.hh"
#include "var_acc_fwd.hh"
#include "yield_fwd.hh"
#include "expr_fwd.hh"

class Fn_Decl;
class Fn_Def;
class Filter;

#include "expr/base.hh"

#include "expr/vacc.hh"

namespace Expr {


  class Plus : public Two {
    public:
      Plus(Base *l, Base *r, const Loc &lo) : Two(PLUS, l, r, lo) {}
      Plus(Base *l, Base *r) : Two(PLUS, l, r) {}

      void put(std::ostream &s) const;

      Base *copy() const;
  };

  class Minus : public Two {
    public:
      Minus(Base *l, Base *r, const Loc &lo) : Two(MINUS, l, r, lo) {}
      Minus(Base *l, Base *r) : Two(MINUS, l, r) {}

      void put(std::ostream &s) const;

      Base *copy() const;
  };

  class Times: public Two {
    public:
      Times(Base *l, Base *r, const Loc &lo) : Two(TIMES, l, r, lo)
      {
        set_pretty_op("*");
      }

      Times(Base *l, Base *r) : Two(TIMES, l, r)
      {
        set_pretty_op("*");
      }

      Base *copy() const;
  };

  class Div: public Two {
    public:
      Div(Base *l, Base *r)
        : Two(DIV, l, r)
      {
        set_pretty_op("/");
      }
      Div(Base *l, Base *r, const Loc &lo) : Two(DIV, l, r, lo)
      {
        set_pretty_op("/");
      }

      Base *copy() const;
  };

  class Comp : public Base {
    public:
      Base *expr;
      Comp(Base *e, const Loc &l) : Base(COMP, l), expr(e) {}
      void put(std::ostream &s) const;

      Base *copy() const;

  };

  class Const : public Base {
    public:
    ::Const::Base *base;
    Const (::Const::Base* b, const Loc &l) : Base(CONST, l), base(b) {}
    Const (::Const::Base* b) : Base(CONST), base(b) {}
    Const (const Yield::Poly &p);
    Const(const Yield::Size &ys);
    Const (int p);
    Const(double d);
    Const(const std::string &s);
    Const(char c);

      void put(std::ostream &s) const;

      Base *copy() const;
  };


  class Less_Eq : public Base {
    public:
      Base *lhs;
      Base *rhs;
      Less_Eq(Expr::Base *l, Expr::Base *r)
        : Base(LESS_EQ), lhs(l), rhs(r) {}
      Less_Eq(Expr::Base *l, Expr::Base *r, const Loc &loc)
        : Base(LESS_EQ, loc), lhs(l), rhs(r) {}
      Less_Eq(Base *l, const Yield::Poly &p);


      void put(std::ostream &s) const;

      Base *copy() const;
  };

  class Less : public Two {
    public:
      Less(Base *l, Base *r)
        : Two(LESS, l, r) {}
      Less(Base *l, Base *r, const Loc &loc)
        : Two(LESS, l, r, loc) {}
      Less(Base *l, const Yield::Poly &p);
      void put(std::ostream &s) const;

      Base *copy() const;
  };

  class Greater : public Two {
    public:
      Greater(Base *l, Base *r)
        : Two(GREATER, l, r) {}
      Greater(Base *l, Base *r, const Loc &loc)
        : Two(GREATER, l, r, loc) {}
      Greater(Base *l, const Yield::Poly &p);
      void put(std::ostream &s) const;

      Base *copy() const;
  };

  class Greater_Eq : public Two {
    public:
      Greater_Eq(Base *l, Base *r)
        : Two(GREATER_EQ, l, r) { set_pretty_op(">="); }
      Greater_Eq(Base *l, Base *r, const Loc &loc)
        : Two(GREATER_EQ, l, r, loc) { set_pretty_op(">="); }
      Greater_Eq(Base *l, const Yield::Poly &p);

      Base *copy() const;
  };

  class Eq : public Two {
    public:
      Eq(Base *l, Base *r)
        : Two(EQ, l, r) { set_pretty_op("=="); }
      Eq(Base *l, Base *r, const Loc &loc)
        : Two(EQ, l, r, loc) { set_pretty_op("=="); }

      Eq(Var_Acc::Base *vacc, Statement::Var_Decl *v);

      Base *copy() const;
  };

  class Not_Eq : public Two {
    public:
      Not_Eq(Base *l, Base *r)
        : Two(NOT_EQ, l, r) { set_pretty_op("!="); }
      Not_Eq(Base *l, Base *r, const Loc &loc)
        : Two(NOT_EQ, l, r, loc) { set_pretty_op("!="); }

      Base *copy() const;
  };


  class And : public Two {
    public:
      And(Base *l, Base *r)
        : Two(AND, l, r) {}
      And(Base *l, Base *r, const Loc &loc)
        : Two(AND, l, r, loc) {}
      void put(std::ostream &s) const;

      Base *copy() const;
  };

  class Or : public Two {
    public:
      Or(Base *l, Base *r)
        : Two(OR, l, r) { set_pretty_op("||"); }
      Or(Base *l, Base *r, const Loc &loc)
        : Two(OR, l, r, loc) { set_pretty_op("||"); }

      Base *copy() const;
  };

  class Max : public Base {
    public:
      Base *left, *right;
      Max(Base *l, Base *r)
        : Base(MAX), left(l), right(r) {}
      void put(std::ostream &s) const;
      Base *copy() const;
  };

  class Cond : public Base {
    public:
      Base *cond, *then, *els;
      Cond(Base *c, Base *t, Base *e)
        : Base(COND), cond(c), then(t), els(e) {}
      void put(std::ostream &s) const;
      Base *copy() const;
  };

  class Not : public Base {
    public:
      Base *base;
      Not(Base *b, const Loc &l) : Base(NOT, l), base(b) { }
      Not(Base *b) : Base(NOT), base(b) { }
      void put(std::ostream &s) const;
      Base *copy() const;
  };
 

}


#endif

