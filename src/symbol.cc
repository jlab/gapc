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


#include <algorithm>
#include <functional>
#include <list>
#include <vector>
#include <iostream>
#include <string>

#include "symbol.hh"
#include "signature.hh"
#include "log.hh"

#include "fn_decl.hh"
#include "visitor.hh"

#include "const.hh"

#include "expr.hh"
#include "statement.hh"
#include "yieldsize.hh"

#include "cc.hh"

#include "fn_def.hh"

#include "ast.hh"

#include "type/backtrace.hh"
#include "alt.hh"
#include "outside/middle_end.hh"


Symbol::Base::Base(std::string *n, Type t, const Loc &l)
  :  type(t), adp_specialization(ADP_Mode::STANDARD), adp_join(ADP_Mode::EMPTY),
                tabulated(false), reachable(false), productive(false),
    self_rec_count(0), active(false), self_rec_started(false),
    datatype(NULL), eliminated(false),
    terminal_type(false), rt_computed(false),
    name(n), orig_name(n), location(l),
    tracks_(0),
    track_pos_(0) {
  assert(name);
}


Symbol::Base::~Base() {}


Symbol::Terminal::Terminal(std::string *n, const Loc &l)
  : Base(n, TERMINAL, l) {
  productive = true;
  terminal_type = true;
  list_size_ = 1;
  tracks_ = 1;
  predefinedTerminalParser = false;
}


Symbol::NT::NT(std::string *n, const Loc &l)
  :  Base(n, NONTERMINAL, l), grammar_index_(0),
    recompute(false), tab_dim_ready(false),
    eval_fn(NULL), eval_decl(NULL),
                eval_nullary_fn(NULL), specialised_comparator_fn(NULL),
                specialised_sorter_fn(NULL), marker(NULL),
    ret_decl(NULL), table_decl(NULL),
    zero_decl(0) {
}


/*
 * Marks this terminal as a reachable symbol. For more
 * information please see the comment for the method
 * Symbol::NT::init_links (Grammar &grammar).
 */
bool Symbol::Terminal::init_links(Grammar &grammar) {
  reachable = true;
  return true;
}


/*
 * Marks this non-terminal as a reachable symbol. By definition
 * a symbol is reachable if it receives this method call. The
 * algorithm starts with the axiom, which is reachable per se,
 * and works through the list of alternatives, stored as a field
 * of type std::list<Alt::Base*>, and calls thier init_links()
 * methods one by one.
 */
bool Symbol::NT::init_links(Grammar &grammar) {
  bool r = true;
  reachable = true;
  for (std::list<Alt::Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    bool a = (*i)->init_links(grammar);
    r = r && a;
  }
  return r;
}


bool Symbol::Terminal::init_productive() {
  return false;
}


bool Symbol::NT::init_productive() {
  bool r = false;
  bool t = productive;

  for (std::list<Alt::Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    bool a = (*i)->init_productive();
    r = r || a;
    a = (*i)->is_productive();
    productive = productive || a;
  }
  if (t != productive) {
    return true;
  }
  return r;
}

void Symbol::NT::collect_lr_deps(std::list<NT*> &list) {
  if (Log::instance()->is_debug()) {
    Log::o() << "\n Start collecting from: " << *name;
  }

  Yield::Multi p(tracks_);
  for (std::list<Alt::Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    (*i)->collect_lr_deps(list, p, p);
  }
}


size_t Symbol::Terminal::width() {
  if (ys.high() < Yield::UP)
    return 0;
  else
    return 1;
}


size_t Symbol::NT::width() {
  size_t r = 0;
  for (std::list<Alt::Base*>::iterator i = alts.begin();
      i != alts.end(); ++i) {
    size_t t = (*i)->width();
    if (t > r)
      r = t;
  }
  return r;
}


void Symbol::Base::init_table_dim(const Yield::Size &l, const Yield::Size &r,
                                  std::vector<Yield::Size> &temp_ls,
                                  std::vector<Yield::Size> &temp_rs,
                                  size_t track) {
}


void Symbol::NT::init_table_dim(const Yield::Size &l, const Yield::Size &r,
                                std::vector<Yield::Size> &temp_ls,
                                std::vector<Yield::Size> &temp_rs,
                                size_t track) {
  assert(track < table_dims.size());
  Table &table_dim = table_dims[track];
  assert(grammar_index_ < temp_ls.size());
  Yield::Size &temp_l = temp_ls[grammar_index_];
  assert(grammar_index_ < temp_rs.size());
  Yield::Size &temp_r = temp_rs[grammar_index_];

  if (active) {
    if (l != temp_l && r != temp_r) {
      table_dim |= Table::QUADRATIC;
      if (temp_l.high() != Yield::Poly(Yield::UP)) {
        recompute = true;
        temp_l.set(temp_l.low(), Yield::Poly(Yield::UP));
      }
      if (temp_r.high() != Yield::Poly(Yield::UP)) {
        recompute = true;
        temp_r.set(temp_r.low(), Yield::Poly(Yield::UP));
      }
    } else if (l != temp_l) {
      if (temp_r.high() == Yield::Poly(Yield::UP)) {
        table_dim |= Table::QUADRATIC;
      } else {
        table_dim |= Table::LINEAR;
        table_dim.set_sticky(Table::RIGHT);
        table_dim.set_right_rest(r);
      }
      if (temp_l.high() != Yield::Poly(Yield::UP)) {
        recompute = true;
        temp_l.set(temp_l.low(), Yield::Poly(Yield::UP));
      }
    } else if (r != temp_r) {
      if (temp_l.high() == Yield::UP) {
        table_dim |= Table::QUADRATIC;
      } else {
        table_dim |= Table::LINEAR;
        table_dim.set_sticky(Table::LEFT);
        table_dim.set_left_rest(l);
      }
      if (temp_r.high() != Yield::Poly(Yield::UP)) {
        recompute = true;
        temp_r.set(temp_r.low(), Yield::Poly(Yield::UP));
      }
    }
    return;
  }
  active = true;
  if (m_ys(track).high() != Yield::Poly(Yield::UP))
    table_dim.set_bounded(m_ys(track).high());
  if (!temp_l.initialized())
    temp_l = l;
  if (!temp_r.initialized())
    temp_r = r;
  do {
    recompute = false;

    Table dim_t;
    dim_t = table_dim;

    if (l.high() == Yield::Poly(Yield::UP) &&
        r.high() == Yield::Poly(Yield::UP)) {
      table_dim |= Table::QUADRATIC;
    } else if (l.high() == Yield::UP || r.high() == Yield::UP) {
      table_dim |= Table::LINEAR;
      if (l.high() == Yield::UP) {
        table_dim.set_sticky(Table::RIGHT);
        table_dim.set_right_rest(r);
      } else {
        table_dim.set_sticky(Table::LEFT);
        table_dim.set_left_rest(l);
      }
    } else {
      table_dim |= Table::CONSTANT;
      table_dim.set_left_rest(l);
      table_dim.set_right_rest(r);
    }

    Yield::Size a;
    Yield::Size b;

    a = temp_l;
    b = temp_r;

    temp_l /= l;
    temp_r /= r;

    if (!(a == temp_l && b == temp_r && dim_t.type() == table_dim.type()))
      tab_dim_ready = false;

    if (tab_dim_ready) {
      if (recompute) {
        continue;
      } else {
        break;
      }
    }

    for (std::list<Alt::Base*>::iterator i = alts.begin();
        i != alts.end(); ++i) {
      (*i)->init_table_dim(temp_l, temp_r, temp_ls, temp_rs, track);
    }

    if ((a == temp_l && b == temp_r && dim_t.type() == table_dim.type()))
      if (!recompute)
        tab_dim_ready = true;
  } while (recompute);
  active = false;
  recompute = false;
}


void Symbol::Terminal::print_link(std::ostream &s) {
}

void Symbol::NT::print_link(std::ostream &s) {
  s << "NT " << *name << ':';
  for (std::list<Alt::Base*>::iterator i = alts.begin();
      i != alts.end(); ++i) {
    (*i)->print_link(s);
    s << std::endl;
  }
  s << std::endl << std::endl;
}

std::ostream & Symbol::NT::put(std::ostream &s) const {
  s << (*name) << " ";

  for (Yield::Multi::const_iterator i = m_ys.begin(); i != m_ys.end(); ++i)
    s << *i << ' ';


  for (std::vector<Table>::const_iterator i = table_dims.begin();
      i != table_dims.end(); ++i)
  s << *i << ' ';

  s  << " tabulated(" << tabulated << ')'
    << " in_out(" << in_calls << ", " << out_calls << ')';
  if (in_calls > 0)
    s << " score(" << score() << ')';
  s << " selfrec(" << self_rec_count << ')';
  return s;
}

std::ostream & Symbol::Terminal::put(std::ostream &s) const {
  s << "#" << (*name) << "# " << ys;
  return s;
}

void Symbol::Terminal::clear_runtime() {
}

void Symbol::NT::clear_runtime() {
  rt_computed = false;
}

Runtime::Poly Symbol::Terminal::runtime(std::list<NT*> &active_list,
                                        const Runtime::Poly &accum_rt) {
  Runtime::Poly rt(1);
  return rt;
}

// FIXME accumulated calls
void Symbol::NT::set_rec(const Runtime::Poly &c) {
  assert(active);
  if (c == 1) {
    if (rec > 1)
      rec.set_exp();
    else
      rec.set(1, 1);
  } else {
    rec.set_exp();
  }
}

void Symbol::NT::set_recs(const Runtime::Poly &c,
                          std::list<NT*> &active_list) {
  /*
  std::cerr << "YYY active list: ";
  for (std::list<NT*>::iterator i = active_list.begin();
       i != active_list.end(); ++i)
    std::cerr << *(*i)->name << ", ";
  std::cerr << std::endl;
  */

  std::list<NT*>::iterator f = std::find(active_list.begin(),
      active_list.end(), this);
  assert(f != active_list.end());
  for (; f != active_list.end(); ++f) {
    // std::cerr << "XXX from " << *name << " set rec of "
    // << *(*f)->name << std::endl;
    (*f)->set_rec(c);
  }
}

Runtime::Poly Symbol::NT::runtime(std::list<NT*> &active_list,
                                  const Runtime::Poly &accum_rt) {
  if (rt_computed)
    return runtime_;

  if (active) {
    set_recs(accum_rt, active_list);
    return Runtime::Poly(1);
  }
  active = true;
  active_list.push_back(this);
  rec.set(1, 0);
  Runtime::Poly rt;
  for (std::list<Alt::Base*>::iterator i = alts.begin();
      i != alts.end(); ++i)
    rt += (*i)->runtime(active_list, accum_rt);
  rt *= rec;
  active = false;
  assert(active_list.back() == this);
  active_list.pop_back();
  runtime_ = rt;
  rt_computed = true;
  return rt;
}

void Symbol::Terminal::init_in_out(const Runtime::Poly &p) {
  return;
}

void Symbol::Terminal::init_in_out() {
  return;
}

void Symbol::NT::init_in_out(const Runtime::Poly &p) {
  in_calls += p;
}

void Symbol::NT::init_in_out() {
  for (std::list<Alt::Base*>::iterator i = alts.begin();
      i != alts.end(); ++i)
    out_calls += (*i)->init_in_out();
}


void Symbol::Base::put_table_conf(std::ostream &s) {
  assert(false);
}

void Symbol::NT::put_table_conf(std::ostream &s) {
  assert(tabulated);
  if (tracks_ == 1) {
    table_dims[0].print_short(s, *name);
  } else {
    s << " < ";
    std::vector<Table>::iterator i = table_dims.begin();
    (*i).print_short(s, *name);
    ++i;
    for (; i != table_dims.end(); ++i) {
      s << ", ";
      (*i).print_short(s, *name);
    }
    s << " > ";
  }
}

void Symbol::Base::init_self_rec() {
}

void Symbol::NT::init_self_rec() {
  if (active) {
    if (self_rec_started)
      self_rec_count++;
    return;
  }
  active = true;
  self_rec_started = true;
  for_each(alts.begin(), alts.end(), std::mem_fun(&Alt::Base::init_self_rec));
  self_rec_started = false;
}

bool Symbol::Base::set_data_type(::Type::Base *t, const Loc &l) {
  if (!t)
    return true;
  bool b = ::Type::set_if_compatible(datatype, t, location, l);
  return b;
}

bool Symbol::Base::set_data_type(::Type::Base *t) {
  if (!t)
    return true;
  Loc l;
  bool b = ::Type::set_if_compatible(datatype, t, location, l);
  return b;
}

bool Symbol::Terminal::insert_types(Signature_Base &s) {
  return true;
}

bool Symbol::NT::insert_types(Signature_Base &s) {
  if (eval_fn) {
    if (!set_eval_decl(s))
      return false;
    set_data_type(eval_decl->return_type, eval_decl->location);
  }
  bool r = true;
  for (std::list<Alt::Base*>::iterator i = alts.begin();
      i != alts.end(); ++i) {
    bool b = (*i)->insert_types(s);
    r = r && b;
  }
  return r;
}

Type::Status Symbol::Terminal::infer_missing_types() {
  return ::Type::READY;
}

Type::Status Symbol::NT::infer_missing_types() {
  ::Type::Status r = ::Type::READY;
  bool terminal_types = true;
  for (std::list<Alt::Base*>::iterator i = alts.begin();
      i != alts.end(); ++i) {
    ::Type::Status b = (*i)->infer_missing_types();
    r = std::max(r, b);
    if ((*i)->data_type()) {
      bool x = set_data_type((*i)->data_type(), (*i)->location);
      if (!x)
        r = ::Type::ERROR;
    }
    terminal_types = terminal_types && (*i)->terminal_type;
  }
  if (!terminal_type && terminal_types) {
    terminal_type = true;
    r = ::Type::RUNNING;
  }
  return r;
}

void Symbol::Terminal::print_type(std::ostream &s) {
  s << "#" << *name << " (";
  if (datatype)
    s << *datatype;
  else
    s << "NULL";
  s << ")";
}

void Symbol::NT::print_type(std::ostream &s) {
  s << *name << " (";
  if (datatype)
    s << *datatype;
  else
    s << "NULL";
  s << ") = ";

  for (std::list<Alt::Base*>::iterator i = alts.begin();
      i != alts.end(); ++i) {
    (*i)->print_type(s);
    s << " |" << std::endl << "   ";
  }
  if (eval_decl)
    s << "\t# " << *eval_decl->name << ' ' << *eval_decl->return_type
      << std::endl;
  s << std::endl;
}


struct SetADPSpecializations : public Visitor {
  std::string* eval_nullary_fn;
  std::string* specialised_comparator_fn;
  std::string* specialised_sorter_fn;

  Statement::Var_Decl* marker;

  SetADPSpecializations(std::string* d, std::string* cs, std::string* cds,
                        Statement::Var_Decl* m) :
    eval_nullary_fn(d), specialised_comparator_fn(cs),
    specialised_sorter_fn(cds), marker(m) {
  }

  void visit(Alt::Base &b) {
            b.set_nullary(eval_nullary_fn);
            b.set_comparator(specialised_comparator_fn, specialised_sorter_fn);

            if (marker) {
                b.set_marker(marker);
            }
  }
};

void Symbol::NT::set_adp_specialization(ADP_Mode::Adp_Specialization a,
                                        std::string s_null, std::string s_comp,
                                        std::string s_sort) {
    // set the specialization value
    Base::set_adp_specialization(a);

    if (!eval_fn) {
      if (datatype->simple()->is(::Type::LIST)) {
        Log::instance()->error(location, "No Choice function"
          " used at non-terminal " + *name + ", but ADP specialization set!");
      }
      return;
    }
    // string of nullary
    eval_nullary_fn = new std::string(*eval_fn + s_null);
    specialised_comparator_fn = new std::string(*eval_fn + s_comp);
    specialised_sorter_fn = new std::string(*eval_fn + s_sort);

    if (adp_specialization != ADP_Mode::STANDARD &&
        !ADP_Mode::is_step(adp_specialization) ) {
        marker =  new Statement::Var_Decl(
          new ::Type::List (new ::Type::Int()), "markers");
    }

    SetADPSpecializations v = SetADPSpecializations(
      eval_nullary_fn, specialised_comparator_fn, specialised_sorter_fn,
      marker);

    for (std::list<Alt::Base*>::iterator i = alts.begin();
         i != alts.end(); ++i) {
        (*i)->traverse(v);
    }
}


// this is called at parse time
void Symbol::NT::set_eval_fn(std::string *n) {
  eval_fn = n;
}

// function reference is extracted from signature by name set at parsetime
bool Symbol::NT::set_eval_decl(Signature_Base &s) {
  if (!eval_fn)
    return false;
  eval_decl = s.decl(*eval_fn);
  if (!eval_decl) {
    Log::instance()->error(location, "Choice function " + *eval_fn +
        " used at non-terminal " + *name + " not defined in ");
    Log::instance()->error(s.location, "signature " + *s.name + ".");
    return false;
  }
  return true;
}


bool Symbol::Terminal::eliminate_lists() {
  return false;
}

bool Symbol::NT::eliminate_lists() {
  bool r = false;
  bool x = true;
  for (std::list<Alt::Base*>::iterator i = alts.begin();
      i != alts.end(); ++i) {
    bool b = (*i)->eliminate_lists();
    r = r || b;
    bool a = !(*i)->data_type()->simple()->is(::Type::LIST);
    x = x && a;
  }

  if (eliminated)
    return r;

  if (x && alts.size() == 1) {
    // FIXME dynamic cast?
    if (eval_decl && dynamic_cast<Fn_Def*>(eval_decl) &&
        dynamic_cast<Fn_Def*>(eval_decl)->choice_mode() != Mode::SYNOPTIC) {
      eval_decl = NULL;
      eval_fn = NULL;
      r = true;
    }
    if (!eval_decl && datatype->simple()->is(::Type::LIST)) {
      datatype = dynamic_cast< ::Type::List*>(datatype->simple())->of;
      r = true;
    }
  }

  if (eval_decl && dynamic_cast<Fn_Def*>(eval_decl) &&
      dynamic_cast<Fn_Def*>(eval_decl)->choice_mode().number == 1) {
      // list elimination takes place in product.cc
      datatype = eval_decl->return_type->simple();
      r = true;
  }
  if (r)
    eliminated = true;

  return r;
}

void Symbol::NT::reset_types() {
  datatype = NULL;
  eliminated = false;
}

bool Symbol::Terminal::init_list_sizes() {
  return false;
}

bool Symbol::NT::init_list_sizes() {
  bool r = false;
  if (eval_decl && dynamic_cast<Fn_Def*>(eval_decl)) {
    if (dynamic_cast<Fn_Def*>(eval_decl)->choice_mode().number < Yield::UP) {
      list_size_ = dynamic_cast<Fn_Def*>(eval_decl)->choice_mode().number;
      assert(list_size_ > 0);
    }
  }
  Yield::Poly t;
  bool uninit = false;
  for (std::list<Alt::Base*>::iterator i = alts.begin();
      i != alts.end(); ++i) {
    bool b = (*i)->init_list_sizes();
    r = r || b;
    const Yield::Poly &p = (*i)->list_size();
    if (p == 0)
      uninit = true;
    else
      t += p;
  }
  if (list_size_ == 0 && (t == Yield::UP || !uninit)) {
    assert(t > 0);
    list_size_ = t;
    assert(list_size_ > 0);
    return true;
  }
  return r;
}

void Symbol::Terminal::traverse(Visitor &v) {
  v.visit(*this);
}

void Symbol::NT::traverse(Visitor &v) {
  v.visit(*this);
  for (std::list<Alt::Base*>::iterator i = alts.begin();
      i != alts.end(); ++i) {
    (*i)->traverse(v);
    v.visit_itr(*this);
  }
  v.visit_end(*this);
}

void Symbol::NT::inline_nts(Grammar *grammar) {
  for (std::list<Alt::Base*>::iterator i = alts.begin();
      i != alts.end(); ++i) {
    if (!(*i)->is(Alt::LINK))
      continue;
    Alt::Link *l = dynamic_cast<Alt::Link*>(*i);
    if (!l->nt->is(Symbol::NONTERMINAL))
      continue;
    Symbol::NT *nt = dynamic_cast<NT*>(l->nt);
    if (nt->is_inlineable()) {
      assert(!nt->alts.empty());
      *i = nt->alts.front();
      delete l;
      grammar->remove(nt);
      delete nt;
    }
  }
}

bool Symbol::NT::is_inlineable() {
  if (terminal_type && alts.size() == 1)
    return true;
  if (alts.size() == 1 && !eval_decl && list_size_ == 1)
    return true;
  return false;
}

void Symbol::NT::init_indices(Expr::Vacc *left, Expr::Vacc *right,
                              unsigned int &k, size_t track,
                              Expr::Vacc *left_most, Expr::Vacc *right_most) {
  assert(track < left_indices.size());
  assert(left);
  assert(right);

  left_indices[track] = left;
  right_indices[track] = right;
  unsigned int c = 0;
  for (std::list<Alt::Base*>::iterator i = alts.begin();
       i != alts.end(); ++i, ++c) {
    if ((*i)->is_partof_outside()) {
      outside_init_indices(*i, left, right, k, track, left_most, right_most);
    } else {
      (*i)->init_indices(left, right, k, track);
    }
  }
}

Statement::Base *Symbol::NT::build_return_empty(const Code::Mode &mode) {
  if (mode == Code::Mode::BACKTRACK)
    return new Statement::Return(new Expr::Const(0));
  if (mode == Code::Mode::CYK && tabulated)
    return new Statement::Return();

  assert(zero_decl);
  return new Statement::Return(new Expr::Vacc(*zero_decl));
}

void Symbol::NT::init_guards(Code::Mode mode) {
  guards.clear();

  std::list<Expr::Base*> cond_list;
  // else, guards are generated in tablegen.cc
  if (!tabulated || mode == Code::Mode::CYK)
    gen_ys_guards(cond_list);
  marker_cond(mode, cond_list);

  if (cond_list.empty())
    return;

  Expr::Base *cond  = Expr::seq_to_tree<Expr::Base, Expr::Or>
    (cond_list.begin(), cond_list.end());

  Statement::Base *then = build_return_empty(mode);

  Statement::If *i = new Statement::If(cond, then);
  guards.push_back(i);
}

void Symbol::NT::gen_ys_guards(std::list<Expr::Base*> &ors) const {
  size_t t = 0;
  // std::vector<Table>::const_iterator b = tables_.begin();
  for (Yield::Multi::const_iterator a = m_ys.begin();
       a != m_ys.end(); ++a,
       /*++b,*/ ++t) {
    const Yield::Size &y = *a;
    // const Table &table = *b;

    Expr::Base *i = left_indices[t], *j = right_indices[t];

    Expr::Base *size = new Expr::Minus(j, i);

    if (y.low() != Yield::Poly(0))
      ors.push_back(new Expr::Less(size, new Expr::Const(y.low())));
    if (y.high() != Yield::Poly(Yield::UP))
      ors.push_back(new Expr::Greater(size, new Expr::Const(y.high())));
    // FIXME, check this
    // ors.push_back(new Expr::Greater(j, i));
  }
}

void Symbol::NT::put_guards(std::ostream &s) {
  s << *name << " = ";
  Printer::CC printer;
  for (std::list<Statement::If*>::iterator i = guards.begin();
      i != guards.end(); ++i) {
    printer.print(**i);
    s << std::endl;
  }
  s << std::endl;
}

::Type::Base *Symbol::NT::data_type_before_eval() {
  if (eval_decl) {
    return eval_decl->types.front();
  } else {
    return datatype;
  }
}


#include "statement/fn_call.hh"

void Symbol::NT::add_specialised_arguments(Statement::Fn_Call *fn,
                                           bool keep_coopts) {
    switch (adp_join) {
        case ADP_Mode::COMPERATOR:
            fn->add_arg(specialised_comparator_fn);
            break;
        case ADP_Mode::SORTER:
            fn->add_arg(specialised_sorter_fn);
            break;
        case ADP_Mode::SORTER_COMPERATOR:
            fn->add_arg(specialised_comparator_fn);
            fn->add_arg(specialised_sorter_fn);
            break;
        default:
            break;
    }

    if (ADP_Mode::is_coopt_param(adp_specialization)) {
        fn->add_arg(new Expr::Const(new Const::Bool(keep_coopts)));
    }
}

void Symbol::NT::set_ret_decl_rhs(Code::Mode mode) {
  ret_decl = new Statement::Var_Decl(data_type_before_eval(),
      new std::string("answers"));
  post_alt_stmts.clear();

  assert(datatype);
  if (!eval_decl && !datatype->simple()->is(::Type::LIST)) {
    assert(alts.size() == 1);
    Statement::Var_Assign *s = new Statement::Var_Assign(*ret_decl,
       *alts.front()->ret_decl);
    post_alt_stmts.push_back(s);
    return;
  }

  for (std::list<Alt::Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    Expr::Fn_Call *e = new Expr::Fn_Call(Expr::Fn_Call::NOT_EMPTY);
    assert((*i)->ret_decl);
    e->add_arg(*(*i)->ret_decl);
    Statement::If *cond = new Statement::If(e);

    if (!(*i)->data_type()->simple()->is(::Type::LIST)) {
      if ((mode != Code::Mode::BACKTRACK || !tabulated) &&
        adp_specialization != ADP_Mode::STANDARD ) {
        // directly join the elements in append function
        if (ADP_Mode::is_step(adp_specialization)) {
          Statement::Fn_Call *fn = new Statement::Fn_Call(
            Statement::Fn_Call::APPEND);
          fn->add_arg(*ret_decl);
          fn->add_arg(*(*i)->ret_decl);

          add_specialised_arguments(fn, mode.keep_cooptimal());

          cond->then.push_back(fn);

        } else {  // push element to list and mark position
          Statement::Fn_Call *fn = new Statement::Fn_Call(
            Statement::Fn_Call::PUSH_BACK);
          fn->add_arg(*ret_decl);
          fn->add_arg(*(*i)->ret_decl);

          cond->then.push_back(fn);

          Statement::Fn_Call *mark = new Statement::Fn_Call(
            Statement::Fn_Call::MARK_POSITION);
          mark->add_arg(*ret_decl);
          mark->add_arg(*marker);

          cond->then.push_back(mark);
        }

      } else {
           Statement::Fn_Call *fn = new Statement::Fn_Call(
             Statement::Fn_Call::PUSH_BACK);
           fn->add_arg(*ret_decl);
           fn->add_arg(*(*i)->ret_decl);

           cond->then.push_back(fn);
      }

      post_alt_stmts.push_back(cond);
    } else {
      assert(ret_decl->type->simple()->is(::Type::LIST));
      Expr::Vacc *e = new Expr::Vacc(*ret_decl);
      (*i)->ret_decl->rhs = e;

      if ((*i)->is(Alt::LINK)) {
        Statement::Fn_Call *fn = new Statement::Fn_Call(
          Statement::Fn_Call::APPEND);

        fn->add_arg(*ret_decl);
        fn->add_arg(*(*i)->ret_decl);

        if ((mode != Code::Mode::BACKTRACK || !tabulated) &&
            adp_specialization != ADP_Mode::STANDARD &&
            ADP_Mode::is_step(adp_specialization)) {  // direct join
            add_specialised_arguments(fn, mode.keep_cooptimal());
        }

        // if the link to another non-terminal is decorated by a filter,
        // add an "is_not_empty" guard around the addition of potentially
        // empty list of solutions. Can only be empty due to filtering.
        if (((*i)->filters.size() > 0) ||
            ((*i)->get_multi_filter_size() > 0)) {
          cond->then.push_back(fn);
          post_alt_stmts.push_back(cond);
          // avoid declarations like "ret_1 = answers" if potentially
          // empty ret_1 candidates shall be pushed onto answers later
          (*i)->ret_decl->rhs = NULL;
        } else {
          post_alt_stmts.push_back(fn);
        }
      } else {
        post_alt_stmts.push_back(NULL);
      }
    }
  }
}

void Symbol::NT::marker_code(const Code::Mode &mode,
    std::list<Statement::Base*> &ret_stmts,
    Expr::Base *v) const {
  if (!mode.marker())
    return;

  Expr::Fn_Call *f = new Expr::Fn_Call(Expr::Fn_Call::IS_EMPTY);
  f->add_arg(v);
  Expr::Base *e = new Expr::Not(f);
  Statement::Fn_Call *t = new Statement::Fn_Call(Statement::Fn_Call::MARK);
  t->add_arg(new Expr::Vacc(new std::string("marker_nt_" + *name)));

  // FIXME
  std::vector<Expr::Base*>::const_iterator j = right_indices.begin();
  for (std::vector<Expr::Base*>::const_iterator i = left_indices.begin();
      i != left_indices.end(); ++i, ++j) {
    t->add_arg((*i)->vacc()->name());
    t->add_arg((*j)->vacc()->name());
  }

  Statement::If *c = new Statement::If(e, t);
  ret_stmts.push_back(c);
}

void Symbol::NT::init_ret_stmts(Code::Mode mode) {
  assert(table_decl);
  ret_stmts.clear();
  Expr::Vacc *ret = NULL;
  if (mode == Code::Mode::SUBOPT) {
    eval_decl = 0;
  }
  if (eval_decl) {
    Statement::Var_Decl *v =
      new Statement::Var_Decl(datatype, new std::string("eval"));
    Expr::Fn_Call *choice = new Expr::Fn_Call(*eval_decl);
    choice->add_arg(*ret_decl);

    if (tabulated && mode == Code::Mode::BACKTRACK) {
      Expr::Fn_Call *get_tab = new Expr::Fn_Call(Expr::Fn_Call::GET_TABULATED);
      get_tab->add(*table_decl);

      choice->add_arg(get_tab);
    }

    v->rhs = choice;
    ret_stmts.push_back(v);
    Statement::Fn_Call *erase =
      new Statement::Fn_Call(Statement::Fn_Call::ERASE);
    erase->add_arg(*ret_decl);
    ret_stmts.push_back(erase);
    ret = new Expr::Vacc(*v);
  } else {
    ret = new Expr::Vacc(*ret_decl);
  }
  Statement::Return *r = new Statement::Return(ret);
  if (tabulated && mode != Code::Mode::BACKTRACK) {
    Statement::Fn_Call *tabfn =
      new Statement::Fn_Call(Statement::Fn_Call::TABULATE);
    tabfn->add(*table_decl);
    tabfn->add_arg(ret);
    ret_stmts.push_back(tabfn);

    Expr::Fn_Call *get_tab = new Expr::Fn_Call(Expr::Fn_Call::GET_TABULATED);
    get_tab->add(*table_decl);

    r = new Statement::Return(get_tab);
  }
  if (mode == Code::Mode::CYK && tabulated)
    r = new Statement::Return();
  if (mode == Code::Mode::BACKTRACK) {
    Expr::Fn_Call::Builtin b = mode.cooptimal() ?
      Expr::Fn_Call::EXECUTE_BACKTRACK : Expr::Fn_Call::EXECUTE_BACKTRACK_ONE;
    if (mode.kscoring()) {
      b = mode.cooptimal() ?
        Expr::Fn_Call::EXECUTE_BACKTRACK_K
          :
        Expr::Fn_Call::EXECUTE_BACKTRACK_K_ONE;
    }
    Expr::Fn_Call *execute_backtrack =
      new Expr::Fn_Call(b);
    execute_backtrack->add_arg(ret);
    Statement::Var_Decl *v = 0;
    if (!mode.kscoring()) {
      v = new Statement::Var_Decl(new ::Type::Backtrace_List(), "bt_list",
          execute_backtrack);
    } else {
      // FIXME
      // v = new Statement::Var_Decl(datatype, "bt_list",
      //    execute_backtrack);

      // not needed since Backtrace_Value<left-of-component-of-list>
      // could be used
      // execute_backtrack->type_param = ;
      v = new Statement::Var_Decl(new ::Type::Backtrace_List(), "bt_list",
          execute_backtrack);
    }
    ret_stmts.push_back(v);
    if (datatype->is(::Type::LIST)) {
      Statement::Fn_Call *erase =
        new Statement::Fn_Call(Statement::Fn_Call::ERASE);
      erase->add_arg(ret);
      ret_stmts.push_back(erase);
    }
    r = new Statement::Return(*v);
  }
  marker_code(mode, ret_stmts, ret);
  ret_stmts.push_back(r);
}

#include "tablegen.hh"

void Symbol::NT::init_table_decl(const AST &ast) {
  std::string n(*name + "_table");
  if (ast.code_mode() == Code::Mode::SUBOPT) {
    n = "bt_" + n;
  }
  std::string *t = new std::string(n);

  Tablegen tg;
  tg.set_window_mode(ast.window_mode);
  table_decl = tg.create(*this, t, ast.code_mode() == Code::Mode::CYK,
                         ast.checkpoint && !ast.checkpoint->is_buddy);
}

#include <boost/algorithm/string/replace.hpp>

void Symbol::NT::init_zero_decl() {
  std::ostringstream o;
  o << *datatype << "_zero";
  std::string n(o.str());
  boost::replace_all(n, "(", "B");
  boost::replace_all(n, ")", "E");
  boost::replace_all(n, "[", "L");
  boost::replace_all(n, "]", "M");
  boost::replace_all(n, "{", "I");
  boost::replace_all(n, "}", "J");
  boost::replace_all(n, "<", "K");
  boost::replace_all(n, ">", "L");
  boost::replace_all(n, " ", "_");
  boost::replace_all(n, "-", "S");
  boost::replace_all(n, ",", "G");
  zero_decl = new Statement::Var_Decl(datatype, new std::string(n));
}

void Symbol::NT::init_table_code(const Code::Mode &mode) {
  assert(table_decl);
  if (!tabulated)
    return;
  std::list<Statement::Base*> start;

  Expr::Fn_Call *is_tab = new Expr::Fn_Call(Expr::Fn_Call::IS_TABULATED);
  is_tab->add(*table_decl);
  Statement::If *if_tab = 0;
  if (mode == Code::Mode::BACKTRACK) {
    if_tab = new Statement::If(new Expr::Not(is_tab));
  } else {
    if_tab = new Statement::If(is_tab);
  }
  start.push_back(if_tab);
  if (mode == Code::Mode::FORWARD || mode == Code::Mode::SUBOPT) {
    Expr::Fn_Call *get_tab = new Expr::Fn_Call(Expr::Fn_Call::GET_TABULATED);
    get_tab->add(*table_decl);
    Statement::Return *tab = new Statement::Return( get_tab);
    if_tab->then.push_back(tab);
  } else {
    Expr::Const *c = new Expr::Const(0);
    if_tab->then.push_back(new Statement::Return(c));
  }

  table_guard = start;
}

void Symbol::NT::add_cyk_stub(AST &ast) {
  ::Type::Base *dt = new ::Type::Referencable(datatype);
  Fn_Def *f = new Fn_Def(dt, new std::string("nt_" + *name));
  f->add_para(*this);
  Expr::Fn_Call *get_tab = new Expr::Fn_Call(Expr::Fn_Call::GET_TABULATED);
  get_tab->add(*table_decl);
  Statement::Return *tab = new Statement::Return( get_tab);
  f->stmts.push_back(tab);
  code_.push_back(f);
}

void Symbol::NT::subopt_header(AST &ast, Fn_Def *score_code,
    Fn_Def *f,
    std::list<Statement::Base*> &stmts) {
  if (ast.code_mode() == Code::Mode::SUBOPT)  {
    ::Type::Base *score_type = datatype->component()->left();
    Expr::Fn_Call *score_fn = new Expr::Fn_Call(*f);
    Statement::Var_Decl *score = new Statement::Var_Decl(score_type,
        "score", score_fn);
    stmts.push_back(score);

    Expr::Fn_Call *e = new Expr::Fn_Call(Expr::Fn_Call::IS_EMPTY);
    e->add_arg(*score);
    Statement::If *c = new Statement::If(e,
        build_return_empty(ast.code_mode()));
    stmts.push_back(c);

    if (eval_decl) {
      Fn_Def *e = score_code;
      assert(e);
      for (std::list<Alt::Base*>::iterator i = alts.begin(); i != alts.end();
           ++i) {
        (*i)->set_choice_fn_type(e->choice_fn_type());
      }
    }
    f->add_para(score_type, new std::string("global_score"));
    f->add_para(score_type, new std::string("delta"));
  }
}

void Symbol::NT::marker_cond(Code::Mode &mode,
                             std::list<Expr::Base*> &cond) const {
  if (!mode.subopt_buddy())
    return;

  Expr::Fn_Call *is_marked =
    new Expr::Fn_Call(Expr::Fn_Call::MARKED);
  is_marked->add_arg(new Expr::Vacc(new std::string("marker_nt_" + *name)));

  std::vector<Expr::Base*>::const_iterator j = right_indices.begin();
  for (std::vector<Expr::Base*>::const_iterator i = left_indices.begin();
      i != left_indices.end(); ++i, ++j) {
    is_marked->add_arg((*i)->vacc()->name());
    is_marked->add_arg((*j)->vacc()->name());
  }

  cond.push_back(new Expr::Not(is_marked));
}


struct SetADPDisabled : public Visitor {
        bool disabled;
        bool keep_coopts;

        SetADPDisabled(bool b, bool k)
        :  disabled(b), keep_coopts(k) {}

  void visit(Alt::Base &b) {
            b.set_disable_specialisation(disabled);
            b.set_keep_coopts(keep_coopts);
  }
};

void Symbol::NT::codegen(AST &ast) {
  std::list<Statement::Base*> stmts;

  // disable specialisation if needed in backtrace mode
  SetADPDisabled v = SetADPDisabled(ast.code_mode() == Code::Mode::BACKTRACK &&
    tabulated, ast.code_mode().keep_cooptimal());

  for (std::list<Alt::Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
      (*i)->traverse(v);
  }

  Fn_Def *score_code = 0;
  if (!code_.empty()) {
    score_code = code_.back();
  }
  code_.clear();
  set_ret_decl_rhs(ast.code_mode());
  init_table_decl(ast);
  init_zero_decl();
  ::Type::Base *dt = datatype;
  if (tabulated) {
    dt = new ::Type::Referencable(datatype);
  }
  Fn_Def *f = 0;
  if (ast.cyk() && tabulated && ast.code_mode() != Code::Mode::BACKTRACK) {
    add_cyk_stub(ast);
    f = new Fn_Def(new ::Type::RealVoid(),
                   new std::string("nt_tabulate_" + *name));
  } else {
    f = new Fn_Def(dt, new std::string("nt_" + *name));
  }
  f->add_para(*this);

  subopt_header(ast, score_code, f, stmts);

  init_guards(ast.code_mode());
  init_table_code(ast.code_mode());

  stmts.insert(stmts.begin(), guards.begin(), guards.end());
  if (!ast.cyk() && tabulated) {
    stmts.insert(stmts.begin(), table_guard.begin(), table_guard.end());
  }

  if ((ast.code_mode() != Code::Mode::BACKTRACK || !tabulated) &&
      adp_specialization != ADP_Mode::STANDARD &&
      !ADP_Mode::is_step(adp_specialization) && marker) {  // block mode
      stmts.push_back(marker);
      stmts.push_back(new Statement::Fn_Call(
        Statement::Fn_Call::EMPTY, *marker));
  }

  stmts.push_back(ret_decl);
  stmts.push_back(new Statement::Fn_Call(
    Statement::Fn_Call::EMPTY, *ret_decl));
  std::list<Statement::Base*>::iterator j = post_alt_stmts.begin();
       // std::cout << "ALT START  ================ "
       // << alts.size() << std::endl;
  for (std::list<Alt::Base*>::iterator i = alts.begin();
       i != alts.end() && j != post_alt_stmts.end(); ++i, ++j) {
    (*i)->codegen(ast);
    stmts.insert(stmts.end(), (*i)->statements.begin(), (*i)->statements.end());
    if (*j) {
      stmts.push_back(*j);

      // this is a little shoed in, but set_ret_decl_rhs would need a
      // full rewrite otherwise
      if ((ast.code_mode() != Code::Mode::BACKTRACK || !tabulated) &&
          (*i)->data_type()->simple()->is(::Type::LIST) &&
          (*i)->is(Alt::LINK) &&
          adp_specialization != ADP_Mode::STANDARD &&
          !ADP_Mode::is_step(adp_specialization)
          && marker) {
          Statement::Fn_Call *mark = new Statement::Fn_Call(
            Statement::Fn_Call::MARK_POSITION);
          mark->add_arg(*ret_decl);
          mark->add_arg(*marker);

          stmts.push_back(mark);
      }
    }
  }
  // std::cout << "ALT END    ================" << std::endl;

  // for blocked mode call the finalize method
  if ((ast.code_mode() != Code::Mode::BACKTRACK || !tabulated) &&
      adp_specialization != ADP_Mode::STANDARD &&
      !ADP_Mode::is_step(adp_specialization) && marker) {
      Statement::Fn_Call *join = new Statement::Fn_Call(
        Statement::Fn_Call::JOIN_MARKED);
      join->add_arg(*ret_decl);
      join->add_arg(*marker);

      add_specialised_arguments(join, ast.code_mode().keep_cooptimal());

      stmts.push_back(join);
  }

  init_ret_stmts(ast.code_mode());
  stmts.insert(stmts.end(), ret_stmts.begin(), ret_stmts.end());
  f->stmts = stmts;

  if (eval_decl) {
    Fn_Def *e = dynamic_cast<Fn_Def*>(eval_decl);
    assert(e);
    f->set_choice_fn_type(e->choice_fn_type());
  }
  code_.push_back(f);
  // remove intermediary lists to answer list when right hand side is set
  eliminate_list_ass();
}


void Symbol::NT::replace(Statement::Var_Decl &decl,
                         Statement::iterator begin, Statement::iterator end) {
  if (begin == end)
    return;
  ++begin;
  for (; begin != end; ++begin) {
    (*begin)->replace(decl, decl.rhs);
  }
}

void Symbol::NT::eliminate_list_ass() {
  assert(!code_.empty());
  for (Statement::iterator i = Statement::begin(code_.back()->stmts);
       i != Statement::end(); ) {
    Statement::Base *s = *i;
    if (s->is(Statement::VAR_DECL)) {
      Statement::Var_Decl *decl = s->var_decl();

      if (decl->type->simple()->is(::Type::LIST) &&
          decl->rhs && decl->rhs->is(Expr::VACC)) {
        Statement::iterator a = i;
        ++a;
        if (a != Statement::end()) {
          if ((*a)->is(Statement::IF)) {
            Statement::If *t = dynamic_cast<Statement::If*>(*a);
            if (t->cond->is(Expr::FN_CALL)) {  // e.g. with filters
              ++i;
              continue;
            }
          }
        }

        replace(*decl, i, Statement::end());
        // i.erase();
        decl->disable();
        ++i;
        if ((*i)->is(Statement::FN_CALL)) {
          Statement::Fn_Call *fn = dynamic_cast<Statement::Fn_Call*>(*i);
          if (fn->builtin == Statement::Fn_Call::EMPTY) {
            fn->disable();
            ++i;
          }
        }
        continue;
      }
    }
    ++i;
  }
}


void Symbol::Terminal::print_dot_edge(std::ostream &out) {
}

void Symbol::NT::print_dot_edge(std::ostream &out) {
  for (std::list<Alt::Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    (*i)->print_dot_edge(out, *this);
  }
}


void Symbol::Base::print_dot_node(std::ostream &out) {
  if (tabulated) {
    out << *name << " [style=dotted];\n";
  } else {
    if (is(Symbol::TERMINAL)) {
      out << *name << " [shape=diamond];\n";
    } else {
      out << *name << " [shape=box];\n";
    }
  }
}

// This function is called to change the push type, for example to push_back_max
void Symbol::NT::optimize_choice(::Type::List::Push_Type push) {
  if (!ret_decl->type->is(::Type::LIST))
    return;
  ::Type::List *l = dynamic_cast< ::Type::List*>(ret_decl->type);
  assert(l);
  l->set_push_type(push);

  l = dynamic_cast< ::Type::List*>(datatype);
  if (l)
    l->set_push_type(push);
}

void Symbol::NT::optimize_choice(::Type::List::Push_Type push,
                                 Statement::Hash_Decl *h) {
  if (!ret_decl->type->is(::Type::LIST))
    return;
  optimize_choice(push);
  ::Type::List *l = dynamic_cast< ::Type::List*>(ret_decl->type);
  assert(l);
  l->set_hash_decl(h);

  l = dynamic_cast< ::Type::List*>(datatype);
  assert(l);
  l->set_hash_decl(h);

  Statement::Var_Decl *v = zero_decl;
  init_zero_decl();
  Statement::Var_Decl *t = zero_decl;
  zero_decl = v;
  zero_decl->type = t->type;
  zero_decl->name = t->name;
}


void Symbol::NT::set_alts(const std::list<Alt::Base*> &a) {
  alts = a;
  for (std::list<Alt::Base*>::iterator i = alts.begin(); i != alts.end(); ++i)
    (*i)->top_level = Bool(true);
}

void Symbol::NT::set_tracks(size_t x, size_t y) {
  if (tracks_) {
    assert(tracks_ == x);
  }
  tracks_ = x;
  assert(!track_pos_ || track_pos_ == y);
  track_pos_ = y;
  table_dims.resize(tracks_);

  left_indices.resize(tracks_);
  right_indices.resize(tracks_);
}

void Symbol::Base::set_tracks(size_t x, size_t y) {
  assert(0); std::abort();
}


void Symbol::Terminal::setup_multi_ys() {
  m_ys.set_tracks(1);
  m_ys(0) = ys;
}

void Symbol::NT::setup_multi_ys() {
  m_ys.set_tracks(tracks_);
  for (Yield::Multi::iterator i = m_ys.begin(); i != m_ys.end(); ++i)
    // with 0, UP ys of grammar/loopa is computed wrong!
    *i = Yield::Size(1, Yield::UP);
}

void Symbol::Terminal::init_multi_ys() {
}

void Symbol::NT::init_multi_ys() {
  Yield::Multi m;
  std::list<Alt::Base*>::iterator i = alts.begin();
  assert(i != alts.end());
  (*i)->init_multi_ys();
  m = (*i)->multi_ys();
  ++i;
  for (; i != alts.end(); ++i) {
    (*i)->init_multi_ys();
    m /= (*i)->multi_ys();
  }
  m_ys = m;
}

bool Symbol::Base::multi_detect_loop(const Yield::Multi &left,
    const Yield::Multi &right, Symbol::NT *nt) {
  return false;
}

bool Symbol::NT::multi_detect_loop(const Yield::Multi &left,
    const Yield::Multi &right, Symbol::NT *nt) {
  if (active)
    return false;
  active = true;
  bool r = false;
  for (std::list<Alt::Base*>::const_iterator i = alts.begin();
      i != alts.end(); ++i) {
    bool a = (*i)->multi_detect_loop(left, right, nt);
    r = r || a;
  }
  active = false;
  return  r;
}
bool Symbol::Base::multi_detect_loop() {
  assert(0);
  return false;
}

bool Symbol::NT::multi_detect_loop() {
  bool r = false;
  Yield::Multi p(tracks_);
  for (std::list<Alt::Base*>::const_iterator i = alts.begin();
      i != alts.end(); ++i) {
    if ((*i)->multi_detect_loop(p, p, this)) {
      r = r || true;
      Log::instance()->error(location, "(Multi)Nonterminal " + (*name) +
          " is part in an infinite recursion.");
    }
  }
  return r;
}


void Symbol::NT::multi_propagate_max_filter(
    std::vector<Yield::Multi> &nt_sizes,
    const Yield::Multi &max_size) {
  if (active)
    return;
  active = true;

  Yield::Multi m(max_size);
  m.min_high(m_ys);
  assert(grammar_index_ < nt_sizes.size());

  if (m.leq_high(nt_sizes[grammar_index_])) {
    active = false;
    return;
  }

  nt_sizes[grammar_index_].max_high(m);

  for (std::list<Alt::Base*>::const_iterator i = alts.begin();
      i != alts.end(); ++i) {
    (*i)->multi_propagate_max_filter(nt_sizes, m);
  }

  active = false;
}

void Symbol::NT::multi_propagate_max_filter(
  std::vector<Yield::Multi> &nt_sizes) {
  multi_propagate_max_filter(nt_sizes, m_ys);
}

void Symbol::NT::update_max_ys(const Yield::Multi &m) {
  if (Log::instance()->is_debug())
    std::cerr << "Multi max size of " << *name << " " << m << " old " << m_ys;
  m_ys.min_high(m);
  if (Log::instance()->is_debug())
    std::cerr << " new " << m_ys << '\n';
}

bool Symbol::NT::operator<(const NT &b) const {
  /*
     if (self_rec_count < b.self_rec_count)
     return true;
     if (self_rec_count > b.self_rec_count)
     return false;
     */
  Runtime::Poly s1 = score();
  Runtime::Poly s2 = b.score();
  Runtime::Poly c1(std::max(self_rec_count, 1u));
  Runtime::Poly c2(std::max(b.self_rec_count, 1u));
  s1 *= c1;
  s2 *= c2;
  if (s1 < s2)
    return true;
  if (s1 > s2)
    return false;
  if (table_dims[0].type() < b.table_dims[0].type())
    return true;
  if (table_dims[0].type() > b.table_dims[0].type())
    return false;
  return false;
}

void Symbol::NT::multi_init_calls() {
  for (std::list<Alt::Base*>::iterator i = alts.begin();
       i != alts.end(); ++i) {
    Runtime::Poly x(1);
    (*i)->multi_init_calls(x, tracks_);
  }
}


Symbol::NT *Symbol::NT::clone(size_t track_pos, bool keepname) {
  NT *nt = new NT(*this);
  std::ostringstream o;
  o << *orig_name << '_' << track_pos;
  if (keepname) {
    nt->name = this->name;
  } else {
    nt->name = new std::string(o.str());
  }
  nt->track_pos_ = track_pos;
  nt->alts.clear();
  for (std::list<Alt::Base*>::iterator i = alts.begin(); i != alts.end(); ++i)
    nt->alts.push_back((*i)->clone());
  return nt;
}

void Symbol::NT::window_table_dim() {
  assert(table_dims.size() == 1);
  Table &table = table_dims[0];

  if (table.type() != Table::QUADRATIC) {
    table |= Table::QUADRATIC;
  }
}

void Symbol::NT::set_ntargs(std::list<Para_Decl::Base*> *l) {
  if (!l)
    return;
  if (l->empty()) {
    Log::instance()->error(location, "No non-terminal parameters given.");
    return;
  }
  ntargs_ = *l;
  never_tabulate_ = Bool(true);
}

void Symbol::Base::set_tabulated() {
  if (never_tabulate_)
    return;
  tabulated = true;
}

void Symbol::Base::set_tabulated(bool b) {
  if (never_tabulate_)
    return;
  tabulated = b;
}


std::ostream & operator<<(std::ostream &s, const Symbol::Base &p) {
  return p.put(s);
}


void Symbol::Terminal::setPredefinedTerminalParser(bool isPredefined) {
  this->predefinedTerminalParser = isPredefined;
}


bool Symbol::Terminal::isPredefinedTerminalParser() {
  return this->predefinedTerminalParser;
}


