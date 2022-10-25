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
#include <utility>

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

class Base {
 private:
  /*
   * Stores an enum value that states the type of this class.
   * The value is set by the constructor, and as far as I can
   * see only used by the inlined method Base::is(Type t).
   */
  Type type;

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
  // for outside non-terminals: store original inner left/right indices for
  // guards construction
  std::vector<Expr::Base*> left_inside_indices;
  std::vector<Expr::Base*> right_inside_indices;

 public:
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
  // recurses into alternatives, finds the single outside NT and updates
  // only its left and right indices used after init_indices_outside
  // initializes all other incides
  // e.g. foo(k1_REGION_i, i_bar_j, j_REGION_k2) --> foo(k1_REGION_i,
  //                       ^     ^
  //          k1_bar_k2, j_REGION_k2)
  //          ^^     ^^
  virtual void expand_outside_nt_indices(Expr::Base *left, Expr::Base *right,
    size_t track);
  virtual std::pair<Yield::Size*, Yield::Size*> *get_outside_accum_yieldsizes(
    size_t track, bool is_right_of_outside_nt = false);
  // sets indices for outside rules, respecting yield sizes
  virtual void init_indices_outside(
    Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track,
    Expr::Base *center_left, Expr::Base *center_right,
    bool is_right_of_outside_nt = false);

  virtual void init_ret_decl(unsigned int i, const std::string &prefix);

 protected:
  Statement::If *filter_guards;
  void push_back_ret_decl();

  Expr::Base *suchthat_code(Statement::Var_Decl &decl) const;


 public:
  std::list<Statement::Base*> statements;
  virtual void codegen(AST &ast, Symbol::NT &calling_nt) = 0;
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

 protected:
  Yield::Multi m_ys;


 public:
  virtual void init_multi_ys();
  const Yield::Multi &multi_ys() const {
    return m_ys;
  }
  Yield::Multi m_ys_inside;

 public:
  std::vector<std::list<Filter*> > multi_filter;

 public:
  void add_multitrack_filter(
    const std::list<Filter*> &l, Filter::Type t, const Loc &loc);
  virtual bool multi_detect_loop(
    const Yield::Multi &left, const Yield::Multi &right,
    Symbol::NT *n) const = 0;

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

  bool choice_set();
  unsigned int to_dot_semanticfilters(unsigned int *nodeID, unsigned int thisID,
    std::ostream &out, std::vector<unsigned int> *childIDs = NULL);

  virtual unsigned int* to_dot(unsigned int *nodeID, std::ostream &out,
    int plot_level);

  // traverses the alternative (=rhs of a production) and collects pointers
  // to all referenced non-terminals e.g.
  // ml_comps = cadd(incl(dangle), ml_comps1)
  // should return [*dangle, *ml_comps1]
  virtual void get_nonterminals(std::list<Symbol::NT*> *nt_list);

  // flag alternative as being part of a generated outside non-terminal
  // such that code generation can discriminate between inside (default)
  // and outside.
  virtual void set_partof_outside(bool is_outside);
  // traverses the alternative (=rhs of a production), iff non-terminal find
  // is contained, replace with first occurence of non-terminal replace
  virtual bool replace_nonterminal(Symbol::NT *find, Symbol::NT *replace,
    hashtable<std::string, unsigned int> &skip_occurences);

  // returns either the single outside non-terminal or NULL
  Symbol::NT *get_outside_nt(Alt::Base *alt);

 protected:
  // private flag to indicate if alternative is for inside (the default) or
  // outside production rules. See set_partof_outside()
  bool is_partof_outside;

 public:
  bool get_is_partof_outside() {
    return this->is_partof_outside;
  }
  // traverses production rule and returns pointer to topmost Alt::Block IFF
  // production rule contains an Alt::Block, otherwise NULL
  virtual Alt::Base* find_block();
  // given a Alt::Block reference, returns the Alt::Base reference of the
  // parent with child Alt::Block (necessary to change pointer when resolving
  // blocks)
  virtual Alt::Base *find_block_parent(const Alt::Base &block);
  bool inside_end = false;
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
  Expr::Base *next_index_var(
    unsigned &k, size_t track, Expr::Base *next_var,
    Expr::Base *last_var, Expr::Base *right, const Yield::Size &ys,
    const Yield::Size &lhs, const Yield::Size &rhs);

  // FIXME convert callers
  Yield::Poly rhs_ys_min_rest(
    const std::list<Fn_Arg::Base*>::iterator &i,
    const std::list<Fn_Arg::Base*>::iterator &end) const;

  std::list<Statement::For *> loops;
  std::list<Statement::Foreach *> foreach_loops;
  std::list<Statement::Base*> body_stmts;

  Statement::If *guards;
  Statement::If *guards_inside;
  void ret_decl_empty_block(Statement::If *stmt);
  void deep_erase_if_backtrace(
    Statement::If *stmt, std::vector<Fn_Arg::Base*>::iterator start,
    std::vector<Fn_Arg::Base*>::iterator end);
  Statement::If *add_empty_check(
    std::list<Statement::Base*> &stmts, const Fn_Arg::Base &b);
  void add_clear_code(
    std::list<Statement::Base*> &stmts, const Fn_Arg::Base &b);
  std::list<Statement::Base*> *reorder_args_cg(
    AST &ast, std::list<Statement::Base*> &l, Symbol::NT &calling_nt);


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
  Expr::Base *get_next_var_right2left(Expr::Base *left_index,
    Expr::Base *innermost_left_index, unsigned &k, size_t track,
    Yield::Size ys_this, Yield::Size *ys_lefts);
  Expr::Base *get_next_var_left2right(Expr::Base *right_index,
    Expr::Base *innermost_right_index, unsigned &k, size_t track,
    Yield::Size ys_this, Yield::Size *ys_rights);
  void expand_outside_nt_indices(Expr::Base *left, Expr::Base *right,
    size_t track);
  std::pair<Yield::Size*, Yield::Size*> *get_outside_accum_yieldsizes(
    size_t track, bool is_right_of_outside_nt = false);
  void init_indices_outside(
    Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track,
    Expr::Base *center_left, Expr::Base *center_right,
    bool is_right_of_outside_nt = false);
  void put_indices(std::ostream &s);

  void reset();

  void init_foreach();
  bool has_arg_list();
  void init_body(AST &ast, Symbol::NT &calling_nt);
  void init_guards();
  void codegen(AST &ast, Symbol::NT &calling_nt);

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
  std::list<Statement::Base*> *add_guards(
    std::list<Statement::Base*> *stmts,
    Statement::If *guards);
  void sum_rhs(
    Yield::Multi &y, std::list<Fn_Arg::Base*>::const_iterator i,
    const std::list<Fn_Arg::Base*>::const_iterator &end) const;
  void sum_rhs(
    Yield::Size &y, std::list<Fn_Arg::Base*>::const_iterator i,
    const std::list<Fn_Arg::Base*>::const_iterator &end,
    size_t track) const;
  Yield::Size *sum_ys_lefts(
    Yield::Size *y, std::list<Fn_Arg::Base*>::const_reverse_iterator i,
    const std::list<Fn_Arg::Base*>::const_reverse_iterator &end, size_t track)
    const;
  Yield::Size *sum_ys_rights(
    Yield::Size *y, std::list<Fn_Arg::Base*>::const_iterator i,
    const std::list<Fn_Arg::Base*>::const_iterator &end, size_t track) const;


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

  void get_nonterminals(std::list<Symbol::NT*> *nt_list);
  void set_partof_outside(bool is_outside);
  bool replace_nonterminal(Symbol::NT *find, Symbol::NT *replace,
    hashtable<std::string, unsigned int> &skip_occurences);
  unsigned int* to_dot(unsigned int *nodeID, std::ostream &out,
          int plot_level);
  Alt::Base* find_block();
  Alt::Base *find_block_parent(const Alt::Base &block);

 private:
  std::list<Statement::Base*> *insert_index_stmts(
    std::list<Statement::Base*> *stmts);
  std::list<Statement::Base*> *inner_code;
};


/*
 * A Alt::Link is a wrapper for non-terminal parsers embedding them
 * in the hirarchy of all Alt::Base subclasses.
 */
class Link : public Base {
 private:
  Runtime::Poly calls;

 public:
  // The name of the non-terminal this instance wrappes. This
  // name will be resolved when the method Link::init_links(Grammar*)
  // is called, and the pointer 'nt' is set to point to the
  // instance of the corresponding non-terminal.
  std::string *name;
  // this field gets set when the method
  // Link::init_links(Grammar*) is called.
  Symbol::Base *nt;


  // Inits the instance and sets the local fields according to
  // the parameter values of the non-terminal name. It also sets
  // the pointer to the non-terminal grammar-node explicitly
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
  std::pair<Yield::Size*, Yield::Size*> *get_outside_accum_yieldsizes(
    size_t track, bool is_right_of_outside_nt = false);
  void expand_outside_nt_indices(Expr::Base *left, Expr::Base *right,
    size_t track);
  void init_indices_outside(
    Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track,
    Expr::Base *center_left, Expr::Base *center_right,
    bool is_right_of_outside_nt = false);

  // void init_ret_decl(unsigned int i);

  void codegen(AST &ast, Symbol::NT &calling_nt);

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

 public:
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
  Statement::If *guards;

 public:
  void set_ntparas(const Loc &loc, std::list<Expr::Base*> *l);
  bool check_ntparas();

  void optimize_choice();

  void get_nonterminals(std::list<Symbol::NT*> *nt_list);
  void set_partof_outside(bool is_outside);
  bool replace_nonterminal(Symbol::NT *find, Symbol::NT *replace,
    hashtable<std::string, unsigned int> &skip_occurences);
  unsigned int* to_dot(unsigned int *nodeID, std::ostream &out,
          int plot_level);
  void init_outside_guards();
  Alt::Base* find_block();
  Alt::Base *find_block_parent(const Alt::Base &block);
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
  void expand_outside_nt_indices(Expr::Base *left, Expr::Base *right,
    size_t track);
  std::pair<Yield::Size*, Yield::Size*> *get_outside_accum_yieldsizes(
    size_t track, bool is_right_of_outside_nt = false);
  void init_indices_outside(
    Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track,
    Expr::Base *center_left, Expr::Base *center_right,
    bool is_right_of_outside_nt = false);

  void codegen(AST &ast, Symbol::NT &calling_nt);

  void print_dot_edge(std::ostream &out, Symbol::NT &nt);

  void print(std::ostream &s);

  void init_multi_ys();

  bool multi_detect_loop(const Yield::Multi &left,
  const Yield::Multi &right, Symbol::NT *n) const;

  void multi_propagate_max_filter(
    std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size);

  void multi_collect_factors(Runtime::Poly &p);
  void multi_init_calls(const Runtime::Poly &p, size_t base_tracks);

  void get_nonterminals(std::list<Symbol::NT*> *nt_list);
  void set_partof_outside(bool is_outside);
  bool replace_nonterminal(Symbol::NT *find, Symbol::NT *replace,
    hashtable<std::string, unsigned int> &skip_occurences);
  unsigned int* to_dot(unsigned int *nodeID, std::ostream &out,
          int plot_level);
  Alt::Base* find_block();
  Alt::Base *find_block_parent(const Alt::Base &block);
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
  void expand_outside_nt_indices(Expr::Base *left, Expr::Base *right,
    size_t track);
  std::pair<Yield::Size*, Yield::Size*> *get_outside_accum_yieldsizes(
    size_t track, bool is_right_of_outside_nt = false);
  void init_indices_outside(
    Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track,
    Expr::Base *center_left, Expr::Base *center_right,
    bool is_right_of_outside_nt = false);

  void codegen(AST &ast, Symbol::NT &calling_nt);
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

  void get_nonterminals(std::list<Symbol::NT*> *nt_list);
  void set_partof_outside(bool is_outside);
  bool replace_nonterminal(Symbol::NT *find, Symbol::NT *replace,
    hashtable<std::string, unsigned int> &skip_occurences);
  unsigned int* to_dot(unsigned int *nodeID, std::ostream &out,
          int plot_level);
  Alt::Base* find_block();
  Alt::Base *find_block_parent(const Alt::Base &block);
};

}  // namespace Alt

// prints left or right indices of a parser to out stream.
// used as a helper for to_dot functions
void to_dot_indices(std::vector<Expr::Base*> indices, std::ostream &out);

#endif  // SRC_ALT_HH_
