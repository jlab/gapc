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


#ifndef SRC_EXPR_BASE_HH_
#define SRC_EXPR_BASE_HH_

#include <ostream>
#include <cassert>
#include <string>
#include <algorithm>

#include "../expr_fwd.hh"

#include "../loc.hh"
#include "../tree_iterator.hh"
#include "../statement_fwd.hh"
#include "../yield_fwd.hh"


namespace Expr {

// This is the base class of all expressions.
class Base {
 private:
    Type type;

 protected:
    Base(Type t, const Loc&l) : type(t), location(l) {}
    explicit Base(Type t) : type(t) {}

 public:
    virtual ~Base();
    Loc location;


    bool is(Type t) const {
      return type == t;
    }


    Base *plus(Base *b);
    Base *plus(const Yield::Poly &p);
    Base *minus(Base *b);
    Base *minus(const Yield::Poly &p);

    virtual void put(std::ostream &s) const;

    virtual Statement::Var_Decl *var_decl();

    virtual Vacc *vacc();
    virtual Fn_Call *fn_call();

    virtual Base *copy() const;
};


// This is the base class for binary expressions, hence the name "two"
class Two : public Base {
 private:
    std::string op;

 protected:
    Two(Type t, Base *l, Base *r, const Loc &lo)
    : Base(t, lo), left_(l), right_(r) {}
    Two(Type t, Base *l, Base *r) : Base(t), left_(l), right_(r) {}
    explicit Two(Type t) : Base(t) {}
    void set_pretty_op(const std::string &s) { op = s; }
    Base *left_, *right_;

 public:
    void put(std::ostream &s) const;

    Base *left() { return left_; }
    Base *right() { return right_; }

    void copy(Two &o) const;
};


inline std::ostream &operator<<(std::ostream &s, const Base &b) {
  b.put(s);
  return s;
}


template <class ret, class expr, class Iterator> ret *seq_to_tree(
Iterator begin, Iterator end) {
  Iterator i = begin;
  if (i == end) {
    assert(false);
  }
  typename std::iterator_traits<Iterator>::value_type a = *i;
  ++i;
  if (i == end) {
    return a;
  }
  typename std::iterator_traits<Iterator>::value_type b = *i;
  ++i;
  expr *root = new expr(a, b);
  for (; i != end; ++i) {
    root = new expr(root, *i);
  }
  return root;
}


typedef Tree::Iterator<Base, Two> iterator;


inline iterator begin(Base *b) {
  return iterator(b);
}


inline iterator end() {
  return iterator();
}


}  // namespace Expr


bool operator==(Expr::Base &expr, const Statement::Var_Decl &decl);


inline bool operator==(const Statement::Var_Decl &decl, Expr::Base &expr) {
  return expr == decl;
}


#endif  // SRC_EXPR_BASE_HH_
