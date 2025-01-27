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


#ifndef SRC_ALT_HH_
#define SRC_ALT_HH_

#include <list>
#include <vector>
#include <string>

#include "grammar.hh"
#include "runtime.hh"
#include "yieldsize.hh"
#include "loc.hh"

#include "type.hh"

#include "fn_arg_fwd.hh"
#include "symbol_fwd.hh"
#include "expr_fwd.hh"
// FIXME only for Expr::Fn_Call::Builtin ...
#include "expr/fn_call.hh"
#include "statement_fwd.hh"

#include "bool.hh"

// for Filter::Type
#include "filter.hh"
// class Filter;
#include  "adp_mode.hh"

#include "outside/middle_end_fwd.hh"

class Grammar;
class Signature_Base;
class Visitor;
class Fn_Decl;


namespace Alt {
/*
 * The Type enumeration is used as used in the Alt::Base class
 * for marking each subclass with its appropriate type. It is
 * set by the constructor Base::Base, and used by the method
 * Base::is (Type t), which simply compares the type of the current
 * instance with the parameter value t.
 */
enum Type { SIMPLE, LINK, BLOCK, MULTI };

Expr::Base *next_index_var(
unsigned &k, size_t track,
Expr::Base *next_var, Expr::Base *last_var, Expr::Base *right,
const Yield::Size &ys, const Yield::Size &lhs, const Yield::Size &rhs,
std::list<Statement::For*> &loops,
bool for_outside_generation, bool outmost, bool is_left_not_right);

class Base {
 private:
  /*
   * Stores an enum value that states the type of this class.
   * The value is set by the constructor, and as far as I can
   * see only used by the inlined method Base::is(Type t).
   */
  Type type;

  /* Flag this alternative as being part of an outside grammar component */
  bool _is_partof_outside = false;

 protected:
  ADP_Mode::Adp_Specialization adp_specialization;
  ADP_Mode::Adp_Join adp_join;

  std::string *eval_nullary_fn;
  std::string *specialised_comparator_fn;
  std::string *specialised_sorter_fn;

  Statement::Var_Decl* marker;

  bool disabled_spec;
  bool keep_coopts;

 public:
  void set_comparator(std::string *s1, std::string *s2) {
      specialised_comparator_fn = s1;
      specialised_sorter_fn = s2;
  }

  void set_nullary(std::string *s) {
      eval_nullary_fn = s;
  }

  void set_marker(Statement::Var_Decl* m) {
      marker = m;
  }

  void set_disable_specialisation(bool b) {
      disabled_spec = b;
  }

  void set_keep_coopts(bool b) {
      keep_coopts = b;
  }

 protected:
  bool productive;

  ::Type::Base *datatype;

  bool eliminated;


 public:
  bool terminal_type;
  Bool top_level;

 protected:
  Yield::Poly list_size_;
  Base(Type t, const Loc &l);

 public:
  virtual ~Base();
  Loc location;

  virtual Base *clone() = 0;

 protected:
  std::vector<Expr::Base*> left_indices;
  std::vector<Expr::Base*> right_indices;

 public:
  Expr::Base *get_left_index(size_t track) {
    assert(left_indices.size() > track);
    return left_indices[track];
  }
  Expr::Base *get_right_index(size_t track) {
    assert(right_indices.size() > track);
    return right_indices[track];
  }
  Statement::Var_Decl *ret_decl;

  inline bool is(Type t) {
    return type == t;
  }

  void add_specialised_arguments(Statement::Fn_Call *fn);

  void set_adp_specialization(ADP_Mode::Adp_Specialization a) {
      adp_specialization = a;
  }
  ADP_Mode::Adp_Specialization get_adp_specialization() {
      return adp_specialization;
  }

  void set_adp_join(ADP_Mode::Adp_Join a) {
      adp_join = a;
  }
  ADP_Mode::Adp_Join get_adp_join() {
      return adp_join;
  }

  std::list<Filter*> filters;

  virtual bool init_links(Grammar &grammar);

  virtual bool init_productive() = 0;
  bool is_productive() {
    return productive;
  }

  virtual void collect_lr_deps(
    std::list<Symbol::NT*> &list, const Yield::Multi &left,
    const Yield::Multi &right) = 0;

  virtual size_t width() = 0;

  virtual void init_table_dim(
    const Yield::Size &a, const Yield::Size &b,
    std::vector<Yield::Size> &temp_ls,
    std::vector<Yield::Size> &temp_rs, size_t track) = 0;

  virtual void print_link(std::ostream &s) = 0;

  virtual Runtime::Poly runtime(
    std::list<Symbol::NT*> &active_list,
    const Runtime::Poly &accum_rt) = 0;


  virtual Runtime::Poly init_in_out() = 0;

  virtual void init_self_rec() = 0;

  ::Type::Base * data_type() {
    return datatype;
  }
  bool set_data_type(::Type::Base *t, const Loc &l);
  virtual bool insert_types(Signature_Base &s) = 0;
  virtual ::Type::Status infer_missing_types() = 0;

  virtual void print_type(std::ostream &s) = 0;

  virtual bool eliminate_lists() = 0;
  bool is_eliminated() {
    return eliminated;
  }

  void reset_types();

  const Yield::Poly &list_size() const {
    return list_size_;
  }
  void set_list_size(const Yield::Poly &p) {
    list_size_ = p;
  }
  virtual bool init_list_sizes() = 0;

  virtual void traverse(Visitor &v) = 0;

  virtual void init_indices(
    Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track);

  virtual void init_ret_decl(unsigned int i, const std::string &prefix);

 protected:
  Statement::If *filter_guards;
  void push_back_ret_decl();

  Expr::Base *suchthat_code(Statement::Var_Decl &decl) const;


 public:
  std::list<Statement::Base*> statements;
  virtual void codegen(AST &ast) = 0;
  void init_filter_guards(AST &ast);

  virtual void print_dot_edge(std::ostream &out, Symbol::NT &nt) = 0;

  void optimize_choice(::Type::List::Push_Type push);
  void optimize_choice(
    ::Type::List::Push_Type push, Statement::Hash_Decl *h);

  virtual void print(std::ostream &s) = 0;

  bool is_filtered() const {
    return !filters.empty();
  }
  virtual bool calls_terminal_parser() const {
    return false;
  }


 protected:
  Expr::Fn_Call::Builtin choice_fn_type_;

 public:
  void set_choice_fn_type(Expr::Fn_Call::Builtin b) {
    choice_fn_type_ = b;
  }


 protected:
  size_t tracks_;
  size_t track_pos_;

 public:
  void set_tracks(size_t t, size_t p);

  // set to public, to allow transmission to outside pendants
 public:
  Yield::Multi m_ys;

 public:
  virtual void init_multi_ys();
  const Yield::Multi &multi_ys() const {
    return m_ys;
  }

 public:
  // analogous to filters, multi_filter should be public
  std::vector<std::list<Filter*> > multi_filter;
  void add_multitrack_filter(
    const std::list<Filter*> &l, Filter::Type t, const Loc &loc);
  virtual bool multi_detect_loop(
    const Yield::Multi &left, const Yield::Multi &right,
    Symbol::NT *n) const = 0;
  size_t get_multi_filter_size() {
    return multi_filter.size();
  }

 private:
  Yield::Multi multi_ys_max_temp;

 public:
  void multi_set_max_size();
  virtual void multi_propagate_max_filter(
    std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size);

  virtual void multi_collect_factors(Runtime::Poly &p) = 0;
  virtual void multi_init_calls(
    const Runtime::Poly &p, size_t base_tracks) = 0;

 protected:
  void add_seqs(Expr::Fn_Call *fn_call, const AST &ast) const;

 public:
  virtual void set_index_stmts(const std::list<Statement::Base*> &l);
  virtual void set_index_overlay(Alt::Base *alt);


  virtual void set_ntparas(const Loc &loc, std::list<Expr::Base*> *l);

  // generates graphviz code to represent NT-parameters
  virtual void ntparas_to_dot(std::ostream &out);

  bool choice_set();
  unsigned int to_dot_semanticfilters(unsigned int *nodeID, unsigned int thisID,
    std::ostream &out, std::vector<unsigned int> *childIDs = NULL);
  virtual unsigned int* to_dot(unsigned int *nodeID, std::ostream &out,
          int plot_level);

  bool is_partof_outside() const {
    return _is_partof_outside;
  }
  void set_partof_outside() {
    _is_partof_outside = true;
  }

  virtual void outside_collect_parsers(
      std::vector<Parser*> &left_parsers,
      std::vector<Parser*> &right_parsers,
      unsigned int &num_outside_nts,
      size_t track,
      std::list<Statement::For*> &simple_loops);
  virtual void outside_uppropagate_indices(
      Expr::Vacc *left, Expr::Vacc *right, size_t track);
};


/*
 * Represents an application of an algebra function to
 * other terminal and non-terminal parsers.
 */
class Simple : public Base {
 private:
  // Stores the yield size of this perser.
  Yield::Size terminal_ys;
  // stores a flag as shorthand to determine if this
  // Alt::Simple is just a terminal symbol. This flag
  // is set at initialization time in the constructor
  // Alt::Simple::Simple where the static hashtable
  // of build in declarations is consulted via Fn_Decl::buildins.
  bool is_terminal_;

 public:
  std::string *name;
  std::list<Fn_Arg::Base*> args;
  Fn_Decl *decl;
  Simple(std::string *n, const Loc &l);

  Base *clone();

  void set_terminal_ys(const Yield::Size &a) {
    assert(is_terminal_);
    terminal_ys = a;
  }

  bool is_terminal() const {
    return is_terminal_;
  }

  bool init_links(Grammar &grammar);
  bool init_productive();

  void collect_lr_deps(
    std::list<Symbol::NT*> &list, const Yield::Multi &left,
    const Yield::Multi &right);

  size_t width();

  void init_table_dim(
    const Yield::Size &a, const Yield::Size &b,
    std::vector<Yield::Size> &temp_ls, std::vector<Yield::Size> &temp_rs,
    size_t track);

  void print_link(std::ostream &s);

  Runtime::Poly runtime(
    std::list<Symbol::NT*> &active_list, const Runtime::Poly &accum_rt);

  Runtime::Poly init_in_out();
  void init_self_rec();

  bool insert_types(Signature_Base &s);
  ::Type::Status infer_missing_types();
  void print_type(std::ostream &s);

  bool has_moving_k();
                    bool is_nullary();
  bool eliminate_lists();
  bool init_list_sizes();

  void traverse(Visitor &v);

 private:
  // FIXME convert callers
  Yield::Poly rhs_ys_min_rest(
    const std::list<Fn_Arg::Base*>::iterator &i,
    const std::list<Fn_Arg::Base*>::iterator &end) const;

 public:
  std::list<Statement::For *> loops;

 private:
  std::list<Statement::Foreach *> foreach_loops;
  std::list<Statement::Base*> body_stmts;

  Statement::If *guards;
  Statement::If *guards_outside;
  void ret_decl_empty_block(Statement::If *stmt);
  void deep_erase_if_backtrace(
    Statement::If *stmt, std::vector<Fn_Arg::Base*>::iterator start,
    std::vector<Fn_Arg::Base*>::iterator end);
  Statement::If *add_empty_check(
    std::list<Statement::Base*> &stmts, const Fn_Arg::Base &b);
  void add_clear_code(
    std::list<Statement::Base*> &stmts, const Fn_Arg::Base &b);
  std::list<Statement::Base*> *reorder_args_cg(
    AST &ast, std::list<Statement::Base*> &l);


  void add_overlay_code(
    std::list<Statement::Base*> *& stmts, AST &ast,
    std::list<Expr::Fn_Call*> &exprs, Filter::Type t) const;
  void add_with_overlay_code(
    std::list<Statement::Base*> *&stmts, AST &ast) const;
  void add_suchthat_overlay_code(
    std::list<Statement::Base*> *&stmts, AST &ast) const;

  void add_subopt_guards(std::list<Statement::Base*> *&stmts, AST &ast);
  std::list<Statement::Base*> *add_arg_code(
    AST &ast, std::list<Statement::Base*> &x);
  std::list<Statement::Base*> pre_stmts;
  std::list<std::list<Statement::Base*>*> pre_cond;
  std::list<Statement::Var_Decl*> pre_decl;

 public:
  void init_indices(
    Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track);
  void put_indices(std::ostream &s);

  void reset();

  void init_foreach();
  bool has_arg_list();
  void init_body(AST &ast);
  void init_guards();
  void init_outside_guards();
  std::list<Statement::Base*> *add_guards(
      std::list<Statement::Base*> *stmts, bool add_outside_guards);
  void codegen(AST &ast);

  void print_dot_edge(std::ostream &out, Symbol::NT &nt);

  void print(std::ostream &s);

  bool calls_terminal_parser() const;

  void init_multi_ys();

 private:
  std::list<Statement::Base*> *add_filter_guards(
    std::list<Statement::Base*> *stmts,
    Statement::If *filter_guards);
  std::list<Statement::Base*> *add_for_loops(
    std::list<Statement::Base*> *stmts,
    std::list<Statement::For *> loops,
    bool has_index_overlay);
  void sum_rhs(
    Yield::Multi &y, std::list<Fn_Arg::Base*>::const_iterator i,
    const std::list<Fn_Arg::Base*>::const_iterator &end) const;
  void sum_rhs(
    Yield::Size &y, std::list<Fn_Arg::Base*>::const_iterator i,
    const std::list<Fn_Arg::Base*>::const_iterator &end,
    size_t track) const;

 public:
  bool multi_detect_loop(
    const Yield::Multi &left, const Yield::Multi &right,
    Symbol::NT *n) const;

  void multi_propagate_max_filter(
    std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size);

  void multi_collect_factors(Runtime::Poly &p);
  void multi_init_calls(const Runtime::Poly &p, size_t base_tracks);

 private:
  std::list<Statement::Base*> index_stmts;
  Bool is_index_overlay_;

 public:
  std::list<Simple*> index_overlay;
  void set_index_stmts(const std::list<Statement::Base*> &l);

  bool has_index_overlay() const {
    return !index_stmts.empty();
  }

  void set_index_overlay(Base *alt);

 private:
  std::list<Expr::Base*> ntparas;

 public:
  void set_ntparas(std::list<Expr::Base*> *l);
  std::list<Expr::Base*> get_ntparas() const {
    return ntparas;
  }
  void remove_ntparas() {
    ntparas.clear();
  }
  void ntparas_to_dot(std::ostream &out);
  unsigned int* to_dot(unsigned int *nodeID, std::ostream &out,
          int plot_level);

  /* depth first traversal of a grammar subtree to collect all "Parser" (see
   * outside/middle_end.hh comment) components left and right of the one
   * outside NT link, to later set left/right indices */
  void outside_collect_parsers(
      std::vector<Parser*> &left_parsers,
      std::vector<Parser*> &right_parsers,
      unsigned int &num_outside_nts,
      size_t track,
      std::list<Statement::For*> &simple_loops);
  void outside_uppropagate_indices(
      Expr::Vacc *left, Expr::Vacc *right, size_t track);

 private:
  std::list<Statement::Base*> *insert_index_stmts(
    std::list<Statement::Base*> *stmts);
  std::list<Statement::Base*> *inner_code;

  // a copy of the original inside yield sizes, without re-execution of yield
  // size analysis with the outside compontents added to the grammar.
  Yield::Multi m_ys_inside;

 public:
  // a reference to the left hand side non terminal from which this alternative
  // get's "called". Necessary to construct correct outside guards, i.e.
  // we need to know the table dimension of the lhs NT.
  Symbol::NT *outside_lhsNT;
};


/*
 * A Alt::Link is a wrapper for non-terminal parsers embedding them
 * in the hirarchy of all Alt::Base subclasses.
 */
class Link : public Base {
 private:
  Runtime::Poly calls;

  /* flags the one and only situation where outside grammar
   * transitions into inside rules, i.e. user defined axiom.
   * we need to ensure that according NT call asks for the
   * empy word. */
  bool _is_outside_inside_transition = false;

 public:
  // The name of the non-terminal this instance wrappes. This
  // name will be resolved when the method Link::init_links(Grammar*)
  // is called, and the pointer 'nt' is set to point to the
  // instance of the corresponding non-terminal.
  std::string *name;
  // this field gets set when the method
  // Link::init_links(Grammar*) is called.
  Symbol::Base *nt;


  // Inits the insatnce and sets the local fields according to
  // the parameter values of the non-terminal name. It also sets
  // the pointer to the non-terminal grammar-node explicitely
  // to NULL.
  Link(std::string *n, const Loc&l)
    : Base(LINK, l), name(n), nt(NULL) {
  }

  // Creates a deep copy of this instance.
  Base *clone();

  // Inits the graph link structure of the grammar for this
  // instance.
  bool init_links(Grammar &grammar);
  // Inits the protected field Alt::Base.productive according to
  // whether this non-terminal can produce any parse results
  // at all.
  bool init_productive();

  void collect_lr_deps(
    std::list<Symbol::NT*> &list, const Yield::Multi &left,
    const Yield::Multi &right);

  size_t width();

  void init_table_dim(
    const Yield::Size &a, const Yield::Size &b,
    std::vector<Yield::Size> &temp_ls,
    std::vector<Yield::Size> &temp_rs, size_t track);

  void print_link(std::ostream &s);

  Runtime::Poly runtime(
    std::list<Symbol::NT*> &active_list, const Runtime::Poly &accum_rt);

  Runtime::Poly init_in_out();
  void init_self_rec();

  bool insert_types(Signature_Base &s);
  ::Type::Status infer_missing_types();
  void print_type(std::ostream &s);

  bool eliminate_lists();
  bool init_list_sizes();

  void traverse(Visitor &v);

  void init_indices(
    Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track);

  // void init_ret_decl(unsigned int i);

  void codegen(AST &ast);

  void print_dot_edge(std::ostream &out, Symbol::NT &nt);

  void print(std::ostream &s);

  bool calls_terminal_parser() const;

  void init_multi_ys();

  bool multi_detect_loop(
    const Yield::Multi &left,
    const Yield::Multi &right, Symbol::NT *n) const;

  void multi_propagate_max_filter(
    std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size);

  void multi_collect_factors(Runtime::Poly &p);
  void multi_init_calls(const Runtime::Poly &p, size_t base_tracks);

 private:
  void add_args(Expr::Fn_Call *fn);

 private:
  std::list<Expr::Base*> indices;

 public:
  void set_indices(const std::list<Expr::Base*> &l) {
    indices = l;
  }
  bool is_explicit() const {
    return !indices.empty();
  }
  void to_dot_overlayindices(std::ostream &out, bool is_left_index);

 private:
  std::list<Expr::Base*> ntparas;

 public:
  void set_ntparas(const Loc &loc, std::list<Expr::Base*> *l);
  std::list<Expr::Base*> get_ntparas() const {
    return ntparas;
  }
  void remove_ntparas() {
    ntparas.clear();
  }
  void ntparas_to_dot(std::ostream &out);
  bool check_ntparas();

  void optimize_choice();
  unsigned int* to_dot(unsigned int *nodeID, std::ostream &out,
          int plot_level);

  const bool is_outside_inside_transition() {
    return _is_outside_inside_transition;
  }
  void set_outside_inside_transition() {
    _is_outside_inside_transition = true;
  }

  void outside_collect_parsers(
      std::vector<Parser*> &left_parsers,
      std::vector<Parser*> &right_parsers,
      unsigned int &num_outside_nts,
      size_t track,
      std::list<Statement::For*> &simple_loops);
  void outside_uppropagate_indices(
      Expr::Vacc *left, Expr::Vacc *right, size_t track);
};


/*
 * A Alt::Block is a list of alternatives grouped together. The
 * list of alternative rules is stored in a std::list<Base*> with
 * name 'alts'.
 */
class Block : public Base {
 public:
  // Stores the list of alternatives
  std::list<Base*> alts;

  Block(std::list<Base*> &a, const Loc &l) : Base(BLOCK, l), alts(a) {
  }

  Base *clone();

  bool init_links(Grammar &grammar);
  bool init_productive();

  void collect_lr_deps(
    std::list<Symbol::NT*> &list, const Yield::Multi &left,
    const Yield::Multi &right);

  size_t width();

  void init_table_dim(
    const Yield::Size &a, const Yield::Size &b,
    std::vector<Yield::Size> &temp_ls,
    std::vector<Yield::Size> &temp_rs, size_t track);

  void print_link(std::ostream &s);

  Runtime::Poly runtime(
    std::list<Symbol::NT*> &active_list, const Runtime::Poly &accum_rt);

  Runtime::Poly init_in_out();
  void init_self_rec();

  bool insert_types(Signature_Base &s);
  ::Type::Status infer_missing_types();
  void print_type(std::ostream &s);

  bool eliminate_lists();
  bool init_list_sizes();

  void traverse(Visitor &v);
  void init_indices(
    Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track);

  void codegen(AST &ast);

  void print_dot_edge(std::ostream &out, Symbol::NT &nt);

  void print(std::ostream &s);

  void init_multi_ys();

  bool multi_detect_loop(const Yield::Multi &left,
  const Yield::Multi &right, Symbol::NT *n) const;

  void multi_propagate_max_filter(
    std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size);

  void multi_collect_factors(Runtime::Poly &p);
  void multi_init_calls(const Runtime::Poly &p, size_t base_tracks);
  unsigned int* to_dot(unsigned int *nodeID, std::ostream &out,
          int plot_level);
  void outside_collect_parsers(
      std::vector<Parser*> &left_parsers,
      std::vector<Parser*> &right_parsers,
      unsigned int &num_outside_nts,
      size_t track,
      std::list<Statement::For*> &simple_loops);
  void outside_uppropagate_indices(
      Expr::Vacc *left, Expr::Vacc *right, size_t track);
};


class Multi : public Base {
 private:
  std::list<Base*> list;
  std::list<Statement::Var_Decl*> ret_decls_;

 public:
  Multi(const std::list<Alt::Base*> &t, const Loc &l);
  Base *clone();

  size_t tracks() const {
    return list.size();
  }


  bool init_links(Grammar &grammar);

  bool init_productive();
  void collect_lr_deps(
    std::list<Symbol::NT*> &list,
    const Yield::Multi &left, const Yield::Multi &right);

  size_t width();

  void init_table_dim(
    const Yield::Size &a, const Yield::Size &b,
    std::vector<Yield::Size> &temp_ls,
    std::vector<Yield::Size> &temp_rs, size_t track);

  void print_link(std::ostream &s);

  Runtime::Poly runtime(
    std::list<Symbol::NT*> &active_list, const Runtime::Poly &accum_rt);

  Runtime::Poly init_in_out();

  void init_self_rec();
  bool insert_types(Signature_Base &s);
  ::Type::Status infer_missing_types();

  void print_type(std::ostream &s);

  bool eliminate_lists();
  bool init_list_sizes();

  void traverse(Visitor &v);

  void init_indices(
    Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track);
  void codegen(AST &ast);
  void print_dot_edge(std::ostream &out, Symbol::NT &nt);
  void print(std::ostream &s);

  void init_multi_ys();

  bool multi_detect_loop(
    const Yield::Multi &left,
    const Yield::Multi &right, Symbol::NT *n) const;

  void multi_propagate_max_filter(
    std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size);

  void multi_collect_factors(Runtime::Poly &p);
  void multi_init_calls(const Runtime::Poly &p, size_t base_tracks);

  void types(std::list< ::Type::Base*> &) const;
  const std::list<Statement::Var_Decl*> &ret_decls() const;
  void init_ret_decl(unsigned int i, const std::string &prefix);
  unsigned int* to_dot(unsigned int *nodeID, std::ostream &out,
          int plot_level);
  void outside_collect_parsers(
      std::vector<Parser*> &left,
      std::vector<Parser*> &right,
      unsigned int &num_outside_nts,
      size_t track,
      std::list<Statement::For*> &simple_loops);
  void outside_uppropagate_indices(
      Expr::Vacc *left, Expr::Vacc *right, size_t track);
};

}  // namespace Alt

#endif  // SRC_ALT_HH_
