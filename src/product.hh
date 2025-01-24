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


#ifndef SRC_PRODUCT_HH_
#define SRC_PRODUCT_HH_

#include <string>
#include <list>

#include "loc.hh"
#include "algebra.hh"
#include "tree_iterator.hh"
#include "mode.hh"

#include "hashtable.hh"
#include "const_fwd.hh"
#include "printer_fwd.hh"

#include "bool.hh"

#include "expr/fn_call.hh"
#include "adp_mode.hh"


class Default;
class Filter;


namespace Product {
enum Type { SINGLE, TIMES, KLASS, CARTESIAN, NOP, OVERLAY, TAKEONE, PARETO};

// NONE: normal ADP
// STANDARD, MULTI: normal ADP with sorted Pareto products
// COMPERATOR, SORTER: create a comperator or sorter for generalized ADP
enum Sort_Type { NONE, STANDARD, MULTI, COMPERATOR, SORTER, COMPERATOR_SORTER,
NULLARY_COMPERATOR, NULLARY_SORTER, NULLARY_COMPERATOR_SORTER};

class Base {
 private:
    Type type_;
    ADP_Mode::Adp_Specialization adp_specialization;

 protected:
  Loc location;
  Algebra *algebra_;
  Algebra *bt_score_algebra_;

  // set to value when sorting needs to be generated for the
  // choice funtion
  Sort_Type sorted_choice;

  // number of digits used for pareto or sorting
  int float_accuracy;

 public:
  Algebra *algebra() { return algebra_; }
  Algebra *bt_score_algebra();
                    Base *bt_score_product();

 protected:
  std::string fn_suffix;

  Filter *filter_;

  Bool eliminate_lists_computed;

  Base(Type t, const Loc &l);
  explicit Base(Type t);

 public:
  virtual ~Base();

  bool is(Type t) { return type_ == t; }

  Type type() const { return type_; }

  std::list<Default*> defaults;

  bool check_defaults();

  void set_algebra(Algebra *a) { algebra_ = a; }

  virtual bool init() = 0;

  void reduce_return_type();
  virtual void eliminate_lists() = 0;

  virtual void init_fn_suffix(const std::string &p) = 0;

  virtual void codegen() = 0;

  virtual void print_code(Printer::Base &s) = 0;


  virtual Algebra *nth_algebra(unsigned int &n) = 0;
  // virtual Var_Acc:Base *nth_access(unsigned int n) = 0;
  virtual unsigned int width() = 0;
  virtual bool contains_only_times();

  virtual Base *left();
  virtual Base *right();

  virtual Base * left_most();
  virtual Base * right_most();

  virtual Base * optimize_shuffle_products();

  virtual void collect_fns(std::list<Fn_Def*> &l, const std::string &name);

  void set_filter(Filter *f) { assert(!filter_); filter_ = f; }
  Filter *filter() { return filter_; }

                    void set_sorted_choice(Sort_Type st);

                    Sort_Type get_sorted_choice();

                    bool is_sorted_choice();

    // FIXME protected:
 public:
  void install_choice_filter();

  virtual bool contains(Type t);

  virtual void set_in_use(const Fn_Decl &);

 protected:
  static Bool no_coopt_;
  static Bool no_coopt_class_;

  Var_Acc::Base *src_vacc;
  Var_Acc::Base *dst_vacc;
  void generate_filter_decl(
  std::list<Statement::Base*> &hash_code,
  std::list<Statement::Var_Decl*> &filters) const;

 public:
  virtual void init_vacc(Var_Acc::Base *src, Var_Acc::Base *dst);
  virtual void generate_hash_decl(const Fn_Def &fn,
    std::list<Statement::Base*> &hash_code,
    std::list<Statement::Var_Decl*> &filters,
    std::list<Statement::Base*> &finalize_code,
    std::list<Statement::Base*> &init_code,
    std::list<Statement::Base*> &equal_score_code,
    std::list<Statement::Base*> &compare_code) const;

  bool left_is_classify();
  bool one_per_class();

  static void set_no_coopt() {
    no_coopt_ = Bool(true);
  }
  static bool no_coopt() { return no_coopt_; }

  static void set_no_coopt_class() {
    no_coopt_class_ = Bool(true);
  }
  static bool no_coopt_class() { return no_coopt_class_; }

  virtual Base *replace_classified(bool &x) = 0;

  void set_adp_specialization(ADP_Mode::Adp_Specialization a) {
      adp_specialization = a;
  }
  ADP_Mode::Adp_Specialization get_adp_specialization() {
      return adp_specialization;
  }

  void set_float_accuracy(int a) {
      float_accuracy = a;
  }
  int get_float_accuracy() {
      return float_accuracy;
  }

// extension for sorting with kbacktrack!
 public:
  Base* sort_product;

  void set_sort_product(Base *sp) {
      sort_product = sp;
  }

  /* A product can be a tuple of tuples of ... and one specific algebra
   * is then nested within this structure. To test if the singular algebra is
   * either contained in the first or second component, we need to down traverse
   * and check the sub-structure for presence of this algebra.
   */
  virtual bool contains_algebra(Algebra &a) = 0;
  /* A product can be a tuple of tuples of ...
   * If we to generate code that accesses a specific singular algebra, it must
   * be a concatenation of .first / .second accessors. We construct this string
   * with the call of this function.
   */
  virtual std::string *get_component_accessor(Algebra &a) = 0;

  /* tests if an algebra product contains somewhere the auto generated tikZ
   * algebra. This needs to be known to influence candidate output presentation.
   */
  virtual bool uses_tikz() = 0;
};


class Single : public Base {
 private:
  std::string *name_;

 public:
  Single(std::string *n, const Loc &l) : Base(SINGLE, l), name_(n) { }
  explicit Single(Algebra *a);

  bool init();
  void eliminate_lists();

  void init_fn_suffix(const std::string &p);
  void codegen();

  void print_code(Printer::Base &s);

  unsigned int width();
  Algebra *nth_algebra(unsigned int &n);
  bool contains_only_times();

  Base * left_most();
  Base * right_most();

  const std::string &name() const { assert(name_); return *name_; }

  void init_vacc(Var_Acc::Base *src, Var_Acc::Base *dst);
  void generate_hash_decl(const Fn_Def &fn,
    std::list<Statement::Base*> &hash_code,
    std::list<Statement::Var_Decl*> &filters,
    std::list<Statement::Base*> &finalize_code,
    std::list<Statement::Base*> &init_code,
    std::list<Statement::Base*> &equal_score_code,
    std::list<Statement::Base*> &compare_code) const;

  Base *replace_classified(bool &x);

  bool contains_algebra(Algebra &a);
  std::string *get_component_accessor(Algebra &a);

  bool uses_tikz() {
    return algebra_->get_auto_role() == Algebra::TIKZ;
  }
};


class Two : public Base {
 protected:
  Base *l;
  Base *r;

 public:
  Two(Type t, const Loc &lo, Base *a, Base *b) : Base(t, lo), l(a), r(b) {}
  Two(Type t, Base *a, Base *b) : Base(t), l(a), r(b) {}
  Two(Type t, const Two &x);
  bool init();
  void eliminate_lists();

  void init_fn_suffix(const std::string &p);
  void codegen();

  void print_code(Printer::Base &s);

  Base *left() { return l; }
  Base *right() { return r; }

  unsigned int width();
  Algebra *nth_algebra(unsigned int &n);

  Base * left_most();
  Base * right_most();
  Base *optimize_shuffle_products();

  Mode & left_mode(const std::string &s);
  Mode & right_mode(const std::string &s);

  Expr::Fn_Call::Builtin right_choice_fn_type(const std::string &s) const;
  Expr::Fn_Call::Builtin left_choice_fn_type(const std::string &s) const;

  Fn_Def* left_choice_function(const std::string &s);
  Fn_Def* right_choice_function(const std::string &s);

  void collect_fns(std::list<Fn_Def*> &l, const std::string &name);

  bool contains(Type t);

  void set_in_use(const Fn_Decl &);

  void init_vacc(Var_Acc::Base *src, Var_Acc::Base *dst);

  Base *replace_classified(bool &x);

  bool contains_algebra(Algebra &a);
  std::string *get_component_accessor(Algebra &a);
  bool uses_tikz() {
    return l->uses_tikz() || r->uses_tikz();
  }
};


class Times : public Two {
 private:
 public:
  Times(Base *a, Base *b, const Loc &lo);
  Times(Base *a, Base *b) : Two(TIMES, a, b) { }
  explicit Times(const Two &t);
  bool init();
  bool contains_only_times();

  Base *optimize_shuffle_products();

  void generate_hash_decl(const Fn_Def &fn,
    std::list<Statement::Base*> &hash_code,
    std::list<Statement::Var_Decl*> &filters,
    std::list<Statement::Base*> &finalize_code,
    std::list<Statement::Base*> &init_code,
    std::list<Statement::Base*> &equal_score_code,
    std::list<Statement::Base*> &compare_code) const;
};


class Klass : public Two {
 private:
  Const::Number *parameter;

 public:
  Klass(Base *a, Base *b, const Loc &l) : Two(KLASS, l, a, b),
  parameter(NULL) { }
  bool init();

  Base *replace_classified(bool &x);
};


class Cartesian : public Two {
 public:
  Cartesian(Base *a, Base *b, const Loc &l) : Two(CARTESIAN, l, a, b) {}
  bool init();

  void generate_hash_decl(const Fn_Def &fn,
    std::list<Statement::Base*> &hash_code,
    std::list<Statement::Var_Decl*> &filters,
    std::list<Statement::Base*> &finalize_code,
    std::list<Statement::Base*> &init_code,
    std::list<Statement::Base*> &equal_score_code,
    std::list<Statement::Base*> &compare_code) const;
};


class Nop : public Two {
 public:
  explicit Nop(Times &times);
  bool contains_only_times();
};


class Overlay : public Two {
 public:
  Overlay(Base *a, Base *b, const Loc &l) : Two(OVERLAY, l, a, b) {}
  bool init();
  bool contains_only_times();

  void eliminate_lists();
  void codegen();
  void print_code(Printer::Base &s);
  void init_fn_suffix(const std::string &p);
                    Base *bt_score_product();
};


class Takeone : public Two {
 public:
  Takeone(Base *a, Base *b, const Loc &l) : Two(TAKEONE, l, a, b) {}
  bool init();
};


class Pareto : public Two {
 public:
  enum ParetoType {NoSort, Sort, ISort, MultiDimOpt, NoSortDomOpt};

 private:
  ParetoType pareto_type;
  bool multi_dim;
  int cutoff;

 public:
  Pareto(Base *a, Base *b, const Loc &l) : Two(PARETO, l, a, b),
    pareto_type(NoSort), multi_dim(false), cutoff(65) {}

  bool init();

  void set_pareto_type(int i);

  void generate_hash_decl(const Fn_Def &fn,
    std::list<Statement::Base*> &hash_code,
    std::list<Statement::Var_Decl*> &filters,
    std::list<Statement::Base*> &finalize_code,
    std::list<Statement::Base*> &init_code,
    std::list<Statement::Base*> &equal_score_code,
    std::list<Statement::Base*> &compare_code) const;

  ParetoType get_pareto_type() {
      return pareto_type;
  }

  void set_multi_dim(bool b) {
      multi_dim = b;
  }

  bool get_multi_dim() {
      return multi_dim;
  }

  void set_cutoff(int c) {
      cutoff = c;
  }

  int get_cutoff() {
      return cutoff;
  }
};

typedef Tree::Iterator<Base, Two> iterator;


inline iterator begin(Base *b) { return iterator(b); }
inline iterator end() { return iterator(); }


}  // namespace Product

#endif  // SRC_PRODUCT_HH_
