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
#include <vector>
#include <utility>
#include <string>
#include <list>

#include "alt.hh"

#include "fn_arg.hh"
#include "log.hh"
#include "filter.hh"
#include "fn_decl.hh"
#include "fn_def.hh"
#include "signature.hh"
#include "visitor.hh"

#include "expr.hh"
#include "const.hh"

#include "statement.hh"

#include "cc.hh"

#include "ast.hh"
#include "instance.hh"

#include "statement/fn_call.hh"


Alt::Base::Base(Type t, const Loc &l) :
  type(t), adp_specialization(ADP_Mode::STANDARD),
  eval_nullary_fn(NULL), specialised_comparator_fn(NULL),
  specialised_sorter_fn(NULL), marker(NULL), disabled_spec(false),
  productive(false),
  datatype(NULL), eliminated(false),
  terminal_type(false), location(l),
  ret_decl(NULL), filter_guards(NULL),
  choice_fn_type_(Expr::Fn_Call::NONE),
  tracks_(0), track_pos_(0) {
}


Alt::Base::~Base() {
}


Alt::Simple::Simple(std::string *n, const Loc &l)
  :  Base(SIMPLE, l), is_terminal_(false),
    name(n), decl(NULL), guards(NULL), inner_code(0) {
  hashtable<std::string, Fn_Decl*>::iterator j = Fn_Decl::builtins.find(*name);
  if (j != Fn_Decl::builtins.end()) {
    is_terminal_ = true;
    set_terminal_ys(j->second->yield_size());
  }
}


Alt::Base *Alt::Simple::clone() {
  Alt::Simple *a = new Alt::Simple(*this);
  a->args.clear();
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    a->args.push_back((*i)->clone());
  }
  // necessary to construct correct guards for outside code generation
  this->m_ys_inside = this->m_ys;
  return a;
}


Alt::Base *Alt::Block::clone() {
  Alt::Block *a = new Alt::Block(*this);
  a->alts.clear();
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    a->alts.push_back((*i)->clone());
  }
  return a;
}


Alt::Base *Alt::Link::clone() {
  Alt::Link *a = new Alt::Link(*this);
  return a;
}


Alt::Base *Alt::Multi::clone() {
  Alt::Multi *a = new Alt::Multi(*this);
  a->list.clear();
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    a->list.push_back((*i)->clone());
  }
  return a;
}


void Alt::Base::add_specialised_arguments(Statement::Fn_Call *fn) {
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


bool Alt::Base::init_links(Grammar &g) {
  return true;
}


bool Alt::Simple::init_links(Grammar &g) {
  if (has_index_overlay()) {
    index_overlay.front()->init_links(g);
  }

  bool r = true;
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    bool a = (*i)->init_links(g);
    r = r && a;
  }
  return r;
}


bool Alt::Link::init_links(Grammar &grammar) {
  if (grammar.NTs.find(*name) == grammar.NTs.end()) {
    Log::instance()->error(
      location, "Nonterminal " + *name + " is not defined in grammar " +
      *(grammar.name) + ".");
    return false;
  }
  Symbol::Base *n = grammar.NTs[*name];
  nt = n;

  bool r = check_ntparas();

  if (!nt->is_reachable()) {
    return nt->init_links(grammar);
  }
  return r;
}


bool Alt::Block::init_links(Grammar &grammar) {
  bool r = true;
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    bool a = (*i)->init_links(grammar);
    r = r && a;
  }
  return r;
}


bool Alt::Multi::init_links(Grammar &grammar) {
  bool r = true;
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    if ((*i)->is(BLOCK)) {
      Log::instance()->error(
        location, "Alternative is not allowed in Multi-Track link.");
      continue;
    }
    // simplifies init_calls analysis for example
    if ((*i)->is(SIMPLE) && !dynamic_cast<Simple*>(*i)->is_terminal()) {
      Log::instance()->error(
        location, "Function symbol is not allowed in Multi-Track link.");
      continue;
    }
    bool a = (*i)->init_links(grammar);
    r = r && a;
  }
  return r;
}


bool Alt::Simple::init_productive() {
  bool r = false;
  bool s = true;
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    bool a = (*i)->init_productive();
    r = r || a;
    a = (*i)->is_productive();
    s = s && a;
  }
  if (s != productive) {
    productive = s;
    return true;
  }
  return r;
}


bool Alt::Link::init_productive() {
  if (!nt) {
    return false;
  }
  if (productive != nt->is_productive()) {
    productive = nt->is_productive();
    return true;
  }
  return false;
}


bool Alt::Block::init_productive() {
  bool r = false;
  bool s = false;
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    bool a = (*i)->init_productive();
    r = r || a;
    a = (*i)->is_productive();
    s = s || a;
  }
  if (s != productive) {
    productive = s;
    return true;
  }
  return r;
}

bool Alt::Multi::init_productive() {
  bool r = false;
  bool s = true;
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    bool a = (*i)->init_productive();
    r = r || a;
    bool b = (*i)->is_productive();
    s = s && b;
  }
  if (s != productive) {
    productive = s;
    return true;
  }
  return r;
}


void Alt::Simple::collect_lr_deps(
  std::list<Symbol::NT*> &list, const Yield::Multi &l, const Yield::Multi &r) {
  Yield::Multi left(l);
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    Yield::Multi right(r);
    sum_rhs(right, i, args.end());

    if ((*i)->is(Fn_Arg::ALT)) {
      (*i)->alt_ref()->collect_lr_deps(list, left, right);
    }

    left += (*i)->multi_ys();
  }
}


void Alt::Link::collect_lr_deps(
  std::list<Symbol::NT*> &list, const Yield::Multi &left,
  const Yield::Multi &right) {
  if (!(left.is_low_zero() && right.is_low_zero())) {
    return;
  }

  if (nt->is(Symbol::NONTERMINAL)) {
    if (Log::instance()->is_debug()) {
      Log::o() << " collect: " << *nt->name;
    }
    list.push_back(dynamic_cast<Symbol::NT*>(nt));
  }
}


void Alt::Block::collect_lr_deps(
  std::list<Symbol::NT*> &list, const Yield::Multi &left,
  const Yield::Multi &right) {
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    (*i)->collect_lr_deps(list, left, right);
  }
}


void Alt::Multi::collect_lr_deps(
  std::list<Symbol::NT*> &nts, const Yield::Multi &left,
  const Yield::Multi &right) {
  assert(left.tracks() == tracks_);
  assert(right.tracks() == tracks_);
  assert(tracks_ == list.size());

  Yield::Multi::const_iterator l = left.begin();
  Yield::Multi::const_iterator r = right.begin();
  for (std::list<Base*>::iterator i = list.begin(); i != list.end();
       ++i, ++l, ++r) {
    Yield::Multi a(1), b(1);
    a(0) = *l;
    b(0) = *r;
    (*i)->collect_lr_deps(nts, a, b);
  }
}


size_t Alt::Simple::width() {
  size_t r = 0;
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    Fn_Arg::Base *b = *i;

    const Yield::Multi &m = b->multi_ys();
    for (Yield::Multi::const_iterator j = m.begin(); j != m.end(); ++j) {
      if ((*j).high() == Yield::UP) {
        r+= b->width();
        break;
      }
    }
  }
  return r;
}


size_t Alt::Link::width() {
  size_t r = 0;

  for (Yield::Multi::iterator i = m_ys.begin(); i != m_ys.end(); ++i) {
    if ((*i).high() == Yield::UP) {
      ++r;
    }
  }
  return r;
}


size_t Alt::Block::width() {
  size_t r = 0;
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    size_t t = (*i)->width();
    if (t > r) {
      r = t;
    }
  }
  return r;
}


size_t Alt::Multi::width() {
  size_t r = 0;
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    r += (*i)->width();
  }
  return r;
}


void Alt::Simple::init_table_dim(
  const Yield::Size &a, const Yield::Size &b,
  std::vector<Yield::Size> &temp_ls, std::vector<Yield::Size> &temp_rs,
  size_t track) {
  if (is_terminal()) {
    return;
  }

  Yield::Size l = a;
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    if ((*i)->is(Fn_Arg::ALT)) {
      Yield::Size r(b);
      std::list<Fn_Arg::Base*>::iterator j = i;
      ++j;
      for ( ; j != args.end(); ++j) {
        r += (*j)->multi_ys()(track);
      }

      (*i)->alt_ref()->init_table_dim(l, r, temp_ls, temp_rs, track);
    }

    l += (*i)->multi_ys()(track);
  }
}


void Alt::Link::init_table_dim(
  const Yield::Size &a, const Yield::Size &b,
  std::vector<Yield::Size> &temp_ls, std::vector<Yield::Size> &temp_rs,
  size_t track) {
  nt->init_table_dim(a, b, temp_ls, temp_rs, track);
}


void Alt::Block::init_table_dim(
  const Yield::Size &a, const Yield::Size &b,
  std::vector<Yield::Size> &temp_ls, std::vector<Yield::Size> &temp_rs,
  size_t track) {
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    (*i)->init_table_dim(a, b, temp_ls, temp_rs, track);
  }
}


void Alt::Multi::init_table_dim(
  const Yield::Size &a, const Yield::Size &b,
  std::vector<Yield::Size> &temp_ls, std::vector<Yield::Size> &temp_rs,
  size_t track) {
  size_t j = 0;
  assert(track < list.size());
  std::list<Base*>::iterator i = list.begin();
  for (; j < track; ++i, ++j) {}

  (*i)->init_table_dim(a, b, temp_ls, temp_rs, 0);
}


void Alt::Simple::print_link(std::ostream &s) {
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    (*i)->print_link(s);
  }
}


void Alt::Link::print_link(std::ostream &s) {
  s << " -> " << *nt->name << " (" << calls << ')';
}


void Alt::Block::print_link(std::ostream &s) {
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end();
       ++i) {
    (*i)->print_link(s);
  }
}


void Alt::Multi::print_link(std::ostream &s) {
  s << " < ";
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    (*i)->print_link(s); s << ", ";
  }
  s << " > ";
}


Runtime::Poly Alt::Simple::runtime(
  std::list<Symbol::NT*> &active_list, const Runtime::Poly &accum_rt) {
  Runtime::Poly rt(1);  // FIXME use higher constant for more realistic fn call
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    rt += (*i)->runtime(active_list, accum_rt);
  }
  return rt;
}


Runtime::Poly Alt::Link::runtime(
  std::list<Symbol::NT*> &active_list, const Runtime::Poly &accum_rt) {
  Runtime::Poly rt = calls;
  Runtime::Poly a(accum_rt);
  a *= calls;
  if (!nt->is_tabulated()) {
    rt *= nt->runtime(active_list, a);
  }
  return rt;
}


Runtime::Poly Alt::Block::runtime(
  std::list<Symbol::NT*> &active_list, const Runtime::Poly &accum_rt) {
  Runtime::Poly rt;
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    rt += (*i)->runtime(active_list, accum_rt);
  }
  return rt;
}


Runtime::Poly Alt::Multi::runtime(
  std::list<Symbol::NT*> &active_list, const Runtime::Poly &accum_rt) {
  Runtime::Poly rt;
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    rt += (*i)->runtime(active_list, accum_rt);
  }
  return rt;
}


Runtime::Poly Alt::Simple::init_in_out() {
  Runtime::Poly p;
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    p += (*i)->init_in_out();
  }
  return p;
}


Runtime::Poly Alt::Link::init_in_out() {
  nt->init_in_out(calls);
  return calls;
}


Runtime::Poly Alt::Block::init_in_out() {
  Runtime::Poly p;
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    p += (*i)->init_in_out();
  }
  return p;
}


Runtime::Poly Alt::Multi::init_in_out() {
  Runtime::Poly p;
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    p += (*i)->init_in_out();
  }
  return p;
}


void Alt::Simple::init_self_rec() {
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    if ((*i)->is(Fn_Arg::ALT)) {
      (*i)->alt_ref()->init_self_rec();
    }
  }
}


void Alt::Link::init_self_rec() {
  nt->init_self_rec();
}


void Alt::Block::init_self_rec() {
  for_each(alts.begin(), alts.end(), std::mem_fun(&Base::init_self_rec));
}


void Alt::Multi::init_self_rec() {
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    (*i)->init_self_rec();
  }
}


bool Alt::Base::set_data_type(::Type::Base *t, const Loc &l) {
  if (!t) {
    return true;
  }
  bool b = ::Type::set_if_compatible(datatype, t, location, l);
  return b;
}


bool Alt::Simple::insert_types(Signature_Base &s) {
  Fn_Decl *fn_decl = s.decl(*name);
  if (!fn_decl) {
    hashtable<std::string, Fn_Decl*>::iterator j =
      Fn_Decl::builtins.find(*name);
    if (j != Fn_Decl::builtins.end()) {
      fn_decl = j->second;
      terminal_type = true;
    } else {
      Log::instance()->error(location, "Function " + *name + " is not defined");
      Log::instance()->error(s.location, "in signature " + *s.name + ".");
      return false;
    }
  }
  decl = fn_decl;
  bool r = true;
  bool b = set_data_type(
    new ::Type::List(fn_decl->return_type), fn_decl->return_type->location);
  r = r && b;
  if (fn_decl->types.size() != args.size()) {
    std::ostringstream o1;
    o1  <<  "Function " << *name << " has " << args.size()
      << " arguments, but";
    Log::instance()->error(location, o1.str());
    std::ostringstream o2;
    o2  << "it is defined with " << fn_decl->types.size()
      << " arguments here.";
    Log::instance()->error(fn_decl->location, o2.str());
    r = false;
  }
  std::list< ::Type::Base*>::iterator j = fn_decl->types.begin();
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end() && j != fn_decl->types.end(); ++i, ++j) {
    b = (*i)->set_data_type(*j, location);
    r = r && b;
    if ((*i)->is(Fn_Arg::ALT)) {
      b = (*i)->alt_ref()->insert_types(s);
    }
    r = r && b;
  }

  if (fn_decl->nttypes().size() != ntparas.size()) {
    Log::instance()->error(location, "Number of nt parameters does not");
    Log::instance()->error(fn_decl->location, "match.");
    r = false;
  }

  return r;
}


bool Alt::Link::insert_types(Signature_Base &s) {
  return true;
}


bool Alt::Block::insert_types(Signature_Base &s) {
  bool r = true;
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    bool b = (*i)->insert_types(s);
    r = r && b;
  }
  return r;
}


bool Alt::Multi::insert_types(Signature_Base &s) {
  return true;
}


Type::Status Alt::Simple::infer_missing_types() {
  ::Type::Status r = ::Type::READY;
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    ::Type::Status b = (*i)->infer_missing_types();
    r = std::max(r, b);
  }
  return r;
}


Type::Status Alt::Link::infer_missing_types() {
  ::Type::Status r = ::Type::READY;
  if (!terminal_type && nt->terminal_type) {
    terminal_type = true;
    r = ::Type::RUNNING;
  }
  ::Type::Base *t = nt->data_type();
  if (!t) {
    if (datatype) {
      nt->set_data_type(new ::Type::List(datatype));
    }
    return ::Type::RUNNING;
  }
  t = t->simple();

  ::Type::List *l = dynamic_cast< ::Type::List*>(nt->data_type());
  if (l) {
    l = new ::Type::List(*l);
  }
  t = l ? l : nt->data_type();

  bool b = set_data_type(t, nt->location);
  if (!b) {
    r = ::Type::ERROR;
  }
  if (b && terminal_type) {
    datatype->set_terminal();
  }
  return r;
}


Type::Status Alt::Block::infer_missing_types() {
  ::Type::Status r = ::Type::READY;
  bool terminal_types = true;
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    ::Type::Status b = (*i)->infer_missing_types();
    r = std::max(r, b);
    bool x = set_data_type((*i)->data_type(), (*i)->location);
    if (!x) {
      r = ::Type::ERROR;
    }
    x = (*i)->set_data_type(datatype, location);
    if (!x) {
      r = ::Type::ERROR;
    }
    if (!datatype) {
      r = std::max(r, ::Type::RUNNING);
    }
    terminal_types = terminal_types && (*i)->terminal_type;
  }
  if (!terminal_type && terminal_types) {
    terminal_type = true;
    r = ::Type::RUNNING;
  }
  return r;
}


#include "type/multi.hh"


Type::Status Alt::Multi::infer_missing_types() {
  ::Type::Status r = ::Type::READY;
  std::list< ::Type::Base*> types;
  bool saw_list = false;
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    ::Type::Status b = (*i)->infer_missing_types();
    r = std::max(r, b);

    ::Type::Base *t = (*i)->data_type();
    if (!t) {
      return ::Type::RUNNING;
    }
    if (t->is(::Type::LIST)) {
      t = dynamic_cast< ::Type::List*>(t)->of;
      saw_list = true;
    }
    types.push_back(t);
  }
  ::Type::Multi *m = new ::Type::Multi(types);
  ::Type::Base *res = m;
  if (saw_list) {
    res = new ::Type::List(res);
  }
  bool b = set_data_type(res, location);
  if (!b) {
    return ::Type::ERROR;
  }
  return r;
}


void Alt::Simple::print_type(std::ostream &s) {
  if (datatype) {
    s << *datatype;
  } else {
    s << "NULL";
  }
  s << ' ' << *name << '(';
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    (*i)->print_type(s);
    s << ", ";
  }
  s << ')';
}


void Alt::Link::print_type(std::ostream &s) {
  if (datatype) {
    s << *datatype;
  } else {
    s << "NULL";
  }
}


void Alt::Block::print_type(std::ostream &s) {
  if (datatype) {
    s << *datatype;
  }
  s << "{ ";
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    (*i)->print_type(s);
    s << " | ";
  }
  s << "} ";
}


void Alt::Multi::print_type(std::ostream &s) {
  if (datatype) {
    s << *datatype;
  } else {
    s << "NULL";
  }
}


bool Alt::Simple::has_moving_k() {
  unsigned x = 0;
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    if ((*i)->multi_ys().has_moving()) {
      ++x;
    }
    if (x > 1) {
      return true;
    }
  }
  return false;
}


/* Returns true if no subfunctions are involved. This value is used to apply
 sorting, pareto or others for specialised ADP version, such as Pareto Eager
 or Sorted ADP*/
bool Alt::Simple::is_nullary() {
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    if (!(*i)->terminal_type && (*i)->choice_set()) {
        return false;
    }
  }
  return true;
}

bool Alt::Simple::eliminate_lists() {
  bool r = false;
  bool x = true;
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    if ((*i)->is(Fn_Arg::ALT)) {
      bool a = (*i)->alt_ref()->eliminate_lists();
      r = r || a;
    }
    bool b = !(*i)->returns_list();
    x = x && b;
  }
  if (!eliminated && x && datatype->simple()->is(::Type::LIST) &&
      !has_moving_k()) {
    eliminated = true;
    datatype = dynamic_cast< ::Type::List*>(datatype->simple())->of;
    return true;
  }
  return r;
}


bool Alt::Link::eliminate_lists() {
  if (!eliminated && nt->is_eliminated()) {
    eliminated = true;


    ::Type::List *l = dynamic_cast< ::Type::List*>(nt->data_type());
    if (l) {
      l = new ::Type::List(*l);
    }
    ::Type::Base *t = l ? l : nt->data_type();

    datatype = t;
    return true;
  }
  return false;
}


bool Alt::Block::eliminate_lists() {
  bool r = false;
  bool x = true;
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    bool b = (*i)->eliminate_lists();
    r = r || b;
    bool a = !(*i)->data_type()->simple()->is(::Type::LIST);
    x = x && a;
  }
  if (!eliminated && x && alts.size() == 1 &&
      datatype->simple()->is(::Type::LIST)) {
    eliminated = true;
    datatype = dynamic_cast< ::Type::List*>(datatype->simple())->of;
    return true;
  }
  return r;
}


bool Alt::Multi::eliminate_lists() {
  bool r = false;
  bool x = true;
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    bool b = (*i)->eliminate_lists();
    r = r || b;
    bool a = !(*i)->data_type()->simple()->is(::Type::LIST);
    x = x && a;
  }
  if (!eliminated && x && datatype->is(::Type::LIST)) {
    eliminated = true;
    datatype = dynamic_cast< ::Type::List*>(datatype->simple())->of;
    return true;
  }
  return r;
}


bool Alt::Simple::init_list_sizes() {
  bool r = false;
  if (has_moving_k() && list_size_ != Yield::Poly(Yield::UP)) {
    list_size_ = Yield::UP;
    r = true;
  }
  bool args_uninit = false;
  Yield::Poly t = Yield::Poly(1);
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    if ((*i)->is(Fn_Arg::ALT)) {
      bool b = (*i)->alt_ref()->init_list_sizes();
      r = r || b;
    }
    const Yield::Poly &p = (*i)->list_size();
    if (p == 0) {
      args_uninit = true;
    } else {
      t *= p;
    }
  }
  if (list_size_ == 0 && (t == Yield::UP || !args_uninit)) {
    assert(t > 0);
    list_size_ = t;
    assert(list_size_ > 0);
    return true;
  }
  return r;
}


bool Alt::Link::init_list_sizes() {
  if (nt->list_size() != Yield::Poly(0)) {
    if (list_size_ == 0) {
      list_size_ = nt->list_size();
      assert(list_size_ > 0);
      return true;
    }
  }
  return false;
}

bool Alt::Block::init_list_sizes() {
  Yield::Poly t;
  bool uninit = false;
  bool r = false;
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    bool b = (*i)->init_list_sizes();
    r = r || b;
    const Yield::Poly &p = (*i)->list_size();
    if (p == 0) {
      uninit = true;
    } else {
      t += p;
    }
  }
  if (list_size_ == 0 && (t == Yield::UP || !uninit)) {
    assert(t > 0);
    list_size_ = t;
    assert(list_size_ > 0);
    return true;
  }
  return r;
}


bool Alt::Multi::init_list_sizes() {
  Yield::Poly t;
  bool uninit = false;
  bool r = false;
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    bool b = (*i)->init_list_sizes();
    r = r || b;
    const Yield::Poly &p = (*i)->list_size();
    if (p == 0) {
      uninit = true;
    } else {
      t *= p;
    }
  }
  if (list_size_ == 0 && (t == Yield::UP || !uninit)) {
    assert(t > 0);
    list_size_ = t;
    assert(list_size_ > 0);
    return true;
  }
  return r;
}


void Alt::Base::reset_types() {
  datatype = NULL;
  eliminated = false;
}


void Alt::Simple::traverse(Visitor &v) {
  v.visit(*dynamic_cast<Base*>(this));
  v.visit_begin(*this);
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    (*i)->traverse(v);
    v.visit_itr(*this);
  }
  v.visit_end(*this);
}


void Alt::Link::traverse(Visitor &v) {
  v.visit(*dynamic_cast<Base*>(this));
  v.visit(*this);
}


void Alt::Block::traverse(Visitor &v) {
  v.visit(*dynamic_cast<Base*>(this));
  v.visit_begin(*this);
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    (*i)->traverse(v);
    v.visit_itr(*this);
  }
  v.visit_end(*this);
}


void Alt::Multi::traverse(Visitor &v) {
  v.visit(*dynamic_cast<Base*>(this));
  v.visit(*this);
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    (*i)->traverse(v);
    v.visit_itr(*this);
  }
  v.visit_end(*this);
}


void Alt::Base::init_indices(Expr::Base *left, Expr::Base *right,
    unsigned int &k, size_t track) {
  assert(track < left_indices.size());
  assert(left);
  assert(right);

  left_indices[track] = left;
  right_indices[track] = right;
}


void Alt::Simple::reset() {
  loops.clear();
  if (has_index_overlay()) {
    index_overlay.front()->reset();
  }
}


// ->minus is not problematic, since the unsigned computations are protected
// by local ys-guards


Expr::Base *Alt::next_index_var(unsigned &k, size_t track,
  Expr::Base *next_var, Expr::Base *last_var, Expr::Base *right,
  const Yield::Size &ys, const Yield::Size &lhs, const Yield::Size &rhs,
  std::list<Statement::For*> &loops,
  bool for_outside_generation, bool outmost, bool is_left_not_right) {
  /* only for outside NTs:
   * If left/right of the rhs outside NT are grammar components with at least
   * one moving boundary, we need to start leftmost/rightmost with a new
   * loop variable t_x_k.
   * Note that empty grammar components might occur on left/rightmost positions
   * like LOC. Once we encounter these components, we already did increase k
   * previously. Thus, it is not sufficient to "just" look at lhs, ys and rhs.
   */
  if (for_outside_generation &&
      outmost &&
       // nothing on the left, but moving boundary on the right
      (((lhs.low() == 0) && (lhs.high() == 0) && (rhs.low() != rhs.high())) ||
       // nothing on the right, but moving boundary on the left
       ((rhs.low() == 0) && (rhs.high() == 0) && (lhs.low() != lhs.high()))
      )
     ) {
    outmost = true;
  } else {
    outmost = false;
  }

  if ((ys.low() != ys.high()) || outmost) {
    if ((rhs.low() == rhs.high()) && !outmost) {
      return right;
    } else {
      std::ostringstream o;
      o << "t_" << track << "_k_" << k;
      k++;
      Expr::Vacc *ivar = new Expr::Vacc(new std::string(o.str()));

      // start generating for-loop
      assert((lhs.low() == lhs.high()) || outmost);
      std::pair<Expr::Base*, Expr::Base*> index(0, 0);

      Yield::Size lhs_ys(lhs);
      lhs_ys += ys;

      /* flip next/last variables for loops that belong to outside NTs and
       * are right of the rhs outside NT, since we need to iterate towards the
       * right end of the input sequence */
      if (for_outside_generation && !is_left_not_right) {
        Expr::Base* tmp = last_var;
        last_var = right;
        right = tmp;
      }

      if (outmost || (rhs.high() == Yield::UP)) {
        index.first = last_var->plus(lhs_ys.low());
      } else {
        // e.g. second maxsize filter in grammar/forloops5
        Expr::Cond *ce = new Expr::Cond(
          new Expr::Greater_Eq(right->minus(
            last_var->plus(lhs_ys.low())), rhs.high()),
          right->minus(rhs.high()), last_var->plus(lhs_ys.low()));
        index.first = ce;
      }

      index.second = right->minus(rhs.low());

      Expr::Base *cond = new Expr::Less_Eq(ivar, index.second);
      // e.g. first maxsize filter in grammar/forloops5
      if ((lhs_ys.high() < Yield::UP) && !outmost) {
        cond = new Expr::And(
          cond, new Expr::Less_Eq (ivar, last_var->plus(lhs_ys.high())));
      }

      Statement::Var_Decl *loopvariable = new Statement::Var_Decl(
        new ::Type::Size(), ivar, index.first);
      Statement::For *f = new Statement::For (loopvariable, cond);
      loops.push_back(f);

      return ivar;
    }
  } else {
    return next_var;
  }
}


void Alt::Simple::init_indices(
  Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track) {
  if (has_index_overlay()) {
    unsigned t = k;
    index_overlay.front()->init_indices(left, right, t, track);
  }

  Base::init_indices(left, right, k, track);
  Yield::Size lhs;
  Expr::Base *last_var = left;
  Expr::Base *next_var = NULL;

  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    const Yield::Size &ys = (*i)->multi_ys()(track);

    Yield::Size rhs;
    sum_rhs(rhs, i, args.end(), track);

    Yield::Size rhs_ys(rhs);
    rhs_ys += ys;

    next_var = next_index_var(
      k, track, next_var, last_var, right, ys, lhs, rhs,
      this->loops, false, false, false);

    std::pair<Expr::Base*, Expr::Base*> res(0, 0);
    if (lhs.low() == lhs.high()) {
      res.first = last_var->plus(lhs.low());
      if (ys.low() == ys.high()) {
        res.second = last_var->plus(lhs.low())->plus(ys.low());
        lhs += ys;
      } else {
        if (rhs.low() == rhs.high()) {
          res.second = next_var->minus(rhs.low());
          lhs += ys;
        } else {
          res.second = next_var;
          lhs.set(0, 0);
          last_var = next_var;
        }
      }
    } else {
      assert(rhs_ys.low() == rhs_ys.high());
      res.first = next_var->minus(rhs_ys.low());
      res.second = next_var->minus(rhs.low());
      lhs += ys;
    }

    (*i)->init_indices(res.first, res.second, k, track);
  }
}


void Alt::Link::init_indices(
  Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track) {
  Base::init_indices(left, right, k, track);
}


void Alt::Block::init_indices(
  Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track) {
  Base::init_indices(left, right, k, track);
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    (*i)->init_indices(left, right, k, track);
  }
}


void Alt::Multi::init_indices(
  Expr::Base *left, Expr::Base *right, unsigned int &k, size_t track) {
  Base::init_indices(left, right, k, track);
  size_t j = 0;
  assert(track < list.size());
  std::list<Base*>::iterator i = list.begin();
  for (; j < track; ++i, ++j) {}

  // each component is in a single-track context
  (*i)->init_indices(left, right, k, 0);
}

void Alt::Simple::put_indices(std::ostream &s) {
  print(s);
  s << std::endl;
  s << *name << "( ";
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    if (tracks_ == 1) {
      assert(left_indices.size() == 1);
      s << "\\" << *(*i)->left_indices.front()
      << " ," << *(*i)->right_indices.front() << "/, ";
    } else {
      s << " < ";
      assert((*i)->left_indices.size() == (*i)->right_indices.size());
      std::vector<Expr::Base*>::iterator r = (*i)->right_indices.begin();
      for (std::vector<Expr::Base*>::iterator l = (*i)->left_indices.begin();
           l != (*i)->left_indices.end(); ++l, ++r)
      s << "\\" << **l << ", " << **r << "/, ";
      s << " >, ";
    }
  }
  s << ")";
  s << std::endl;
  Printer::CC printer;
  for (std::list<Statement::For*>::iterator i = loops.begin();
       i != loops.end(); ++i) {
    printer.print(**i);
    s << std::endl;
  }
  s << std::endl;
}


void Alt::Simple::print(std::ostream &s) {
  s << *name << "( ";
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    (*i)->print(s);
    s << ", ";
  }
  s << ")";
}


void Alt::Block::print(std::ostream &s) {
  s << "{ ";
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    (*i)->print(s);
    s << " | ";
  }
  s << " }";
}


void Alt::Link::print(std::ostream &s) {
  s << *name;
}


void Alt::Multi::print(std::ostream &s) {
  assert(!list.empty());
  s << " < ";
  std::list<Base*>::iterator i = list.begin();
  (*i)->print(s);
  ++i;
  for (; i != list.end(); ++i) {
    s << ", ";
    (*i)->print(s);
  }
  s << " > ";
}


void Alt::Base::init_ret_decl(unsigned int i, const std::string &prefix) {
  std::ostringstream o;
  o << prefix << "ret_" << i;
  ret_decl = new Statement::Var_Decl(datatype, new std::string(o.str()));
}


void Alt::Multi::init_ret_decl(unsigned int i, const std::string &prefix) {
  Base::init_ret_decl(i, prefix);
  ret_decls_.clear();

  // note: types() via datatype ::Type::Multi (or list of ::Type::Multi)
  // does not capture list of types of single tracks
  std::list< ::Type::Base*> l;
  types(l);

  size_t t = 0;
  for (std::list< ::Type::Base*>::const_iterator a = l.begin();
       a != l.end(); ++a, ++t) {
    std::ostringstream o;
    o << prefix << "ret_" << i << "_" << t;
    Statement::Var_Decl *rdecl = new Statement::Var_Decl(
      *a, new std::string(o.str()));
    ret_decls_.push_back(rdecl);
  }
}


/*
void Alt::Link::init_ret_decl(unsigned int i) {
}
*/


void Alt::Simple::init_foreach() {
  foreach_loops.clear();
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    assert((*i)->var_decls().size() == (*i)->ret_decls().size());
    std::vector<Statement::Var_Decl*>::const_iterator k =
      (*i)->ret_decls().begin();
    for (std::vector<Statement::Var_Decl*>::const_iterator j =
         (*i)->var_decls().begin(); j != (*i)->var_decls().end(); ++j, ++k) {
      if (*j) {
        assert((*k)->type->simple()->is(::Type::LIST));
        foreach_loops.push_back(new Statement::Foreach(*j, *k));
      }
    }
  }
}


void Alt::Simple::ret_decl_empty_block(Statement::If *stmt) {
  if (!datatype->simple()->is(::Type::LIST)) {
    Statement::Fn_Call *e = new Statement::Fn_Call(Statement::Fn_Call::EMPTY);
    e->add_arg(*ret_decl);
    stmt->els.push_back(e);
  }
}


#include "type/backtrace.hh"


void Alt::Simple::deep_erase_if_backtrace(
  Statement::If *stmt, std::vector<Fn_Arg::Base*>::iterator start,
  std::vector<Fn_Arg::Base*>::iterator end) {
  // FIXME perhaps use alternative deep_erase() rtlib fn
  // which matches an List_Ref<std::pair<*, Backtrace<*>*>>
  // instead of this Foreach construct
  if (datatype->simple()->is(::Type::LIST)) {
    for (std::vector<Fn_Arg::Base*>::iterator i = start; i != end; ++i) {
      if ((*i)->is(Fn_Arg::CONST)) {
        continue;
      }
      for (std::vector<Statement::Var_Decl*>::const_iterator j =
           (*i)->ret_decls().begin(); j != (*i)->ret_decls().end(); ++j) {
        Statement::Var_Decl *rdecl = *j;
        assert(rdecl);
        ::Type::List *l = dynamic_cast< ::Type::List*>(rdecl->type->simple());
        if (!l) {
          continue;
        }
        ::Type::Tuple *t = dynamic_cast< ::Type::Tuple*>(l->of);
        if (!t) {
          continue;
        }
        ::Type::Backtrace *b = dynamic_cast< ::Type::Backtrace*>(
          t->list.back()->first->lhs);
        if (!b) {
          continue;
        }
        Statement::Var_Decl *v = new Statement::Var_Decl(t, "elem");
        Statement::Foreach *foreach = new Statement::Foreach(v, rdecl);
        Statement::Fn_Call *f = new Statement::Fn_Call(
          Statement::Fn_Call::ERASE);
        f->add_arg(new Expr::Vacc(new std::string("elem"), new std::string(
          "second")));
        foreach->statements.push_back(f);
        stmt->els.push_back(foreach);
      }
    }
  }
}


bool Alt::Simple::has_arg_list() {
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    for (std::vector<Statement::Var_Decl*>::const_iterator j =
         (*i)->var_decls().begin(); j != (*i)->var_decls().end(); ++j) {
      if (*j) {
        return true;
      }
    }
  }
  return false;
}


Expr::Base *Alt::Base::suchthat_code(Statement::Var_Decl &decl) const {
  Expr::Vacc *acc = new Expr::Vacc(decl);
  std::list<Expr::Fn_Call*> exprs;
  for (std::list<Filter*>::const_iterator i = filters.begin();
       i != filters.end(); ++i) {
    Filter *f = *i;
    if (f->is(Filter::SUCHTHAT)) {
      Expr::Fn_Call *t = new Expr::Fn_Call(*f);
      t->add_arg(acc);
      exprs.push_back(t);
    }
  }
  if (exprs.empty()) {
    delete acc;
    return 0;
  }
  Expr::Base *e = Expr::seq_to_tree<Expr::Base, Expr::And>(
    exprs.begin(), exprs.end());
  return e;
}


void Alt::Base::add_seqs(Expr::Fn_Call *fn_call, const AST &ast) const {
  if (tracks_ > 1) {
    fn_call->add(ast.seq_decls);
  } else {
    assert(track_pos_ < ast.seq_decls.size());
    fn_call->add_arg(*ast.seq_decls[track_pos_]);
  }
}


void Alt::Simple::init_body(AST &ast) {
  body_stmts.clear();

  std::list<Statement::Base*> *stmts = &body_stmts;
  add_with_overlay_code(stmts, ast);
  add_suchthat_overlay_code(stmts, ast);

  std::string *n = 0;
  if (ast.instance_) {
    n = ast.instance_->lookup(*name);
    if (!n) {
      n = name;
    }
  } else {
    n = name;
  }
  Expr::Fn_Call *fn_call = new Expr::Fn_Call(n);
  if (Fn_Decl::builtins.find(*name) != Fn_Decl::builtins.end()) {
    add_seqs(fn_call, ast);
    fn_call->add(left_indices, right_indices);
  }

  for (std::list<Fn_Arg::Base*>::iterator i = args.begin(); i != args.end();
       ++i) {
    if ((*i)->is(Fn_Arg::CONST)) {
      Fn_Arg::Const *c = dynamic_cast<Fn_Arg::Const*>(*i);
      assert(c);
      assert(!c->ret_decls().empty());
      fn_call->exprs.push_back(c->ret_decls().front()->rhs);
      continue;
    }
    std::vector<Statement::Var_Decl*>::const_iterator k =
      (*i)->ret_decls().begin();
    for (std::vector<Statement::Var_Decl*>::const_iterator j =
         (*i)->var_decls().begin(); j != (*i)->var_decls().end(); ++j, ++k) {
      Expr::Vacc *arg = NULL;
      if (*j) {
        arg = new Expr::Vacc(**j);
      } else {
        arg = new Expr::Vacc(**k);
      }
      fn_call->exprs.push_back(arg);
    }
  }

  fn_call->add(ntparas);

  if (has_moving_k() || has_arg_list()) {
    Statement::Var_Decl *vdecl = new Statement::Var_Decl(
      decl->return_type, new std::string("ans"));
    pre_decl.clear();
    pre_decl.push_back(vdecl);
    vdecl->rhs = fn_call;
    Statement::Fn_Call *fn = new Statement::Fn_Call(
      Statement::Fn_Call::PUSH_BACK);
    fn->add_arg(*ret_decl);
    fn->add_arg(*vdecl);
    stmts->push_back(vdecl);
    Expr::Base *suchthat = suchthat_code(*vdecl);
    if (suchthat) {
      Statement::If *c = new Statement::If(suchthat);
      c->then.push_back(fn);
      stmts->push_back(c);
    } else {
      stmts->push_back(fn);
    }
  } else {
    Statement::Var_Assign *ass = new Statement::Var_Assign(*ret_decl);
    pre_decl.clear();
    pre_decl.push_back(ret_decl);
    ass->rhs = fn_call;
    stmts->push_back(ass);
    Expr::Base *suchthat = suchthat_code(*ret_decl);
    if (suchthat) {
      Statement::If *c = new Statement::If(suchthat);
      Statement::Fn_Call *e = new Statement::Fn_Call(Statement::Fn_Call::EMPTY);
      e->add_arg(*ret_decl);
      c->els.push_back(e);
      stmts->push_back(c);
    }
  }
}


void Alt::Simple::init_guards() {
  std::list<Expr::Base*> l;
  assert(m_ys.tracks() == left_indices.size());
  Yield::Multi::iterator k = m_ys.begin();
  if (this->is_partof_outside()) {
    k = m_ys_inside.begin();
  }
  std::vector<Expr::Base*>::iterator j = right_indices.begin();
  for (std::vector<Expr::Base*>::iterator i = left_indices.begin();
       i != left_indices.end(); ++i, ++j, ++k) {
    Expr::Base *e = (*j)->minus(*i);
    l.push_back(new Expr::Greater_Eq(e, (*k).low()));
    if ((*k).high() != Yield::Poly(Yield::UP)) {
      l.push_back(new Expr::Less_Eq(e, (*k).high()));
    }
  }

  Expr::Base *cond  = Expr::seq_to_tree<Expr::Base, Expr::And>(
    l.begin(), l.end());

  guards = new Statement::If(cond);
  ret_decl_empty_block(guards);
}


void Alt::Base::push_back_ret_decl() {
  statements.push_back(ret_decl);
}


struct Fn_Arg_Cmp {
  bool operator() (const Fn_Arg::Base *a, const Fn_Arg::Base *b) {
    // return *p1 < *p2;
    if (a->is(Fn_Arg::CONST) && b->is(Fn_Arg::ALT))
      return false;
    if (a->is(Fn_Arg::ALT) && b->is(Fn_Arg::CONST))
      return true;
    if (a->is(Fn_Arg::CONST) && b->is(Fn_Arg::CONST))
      return true;
    if (!a->alt_ref()->calls_terminal_parser() &&
        b->alt_ref()->calls_terminal_parser())
      return false;
    if (a->alt_ref()->calls_terminal_parser() &&
        !b->alt_ref()->calls_terminal_parser())
      return true;
    if (a->alt_ref()->calls_terminal_parser() &&
        b->alt_ref()->calls_terminal_parser())
      return true;
    if (!a->alt_ref()->is_filtered() && b->alt_ref()->is_filtered())
      return false;
    if (a->alt_ref()->is_filtered() && !b->alt_ref()->is_filtered())
      return true;
    if (a->alt_ref()->is_filtered() && b->alt_ref()->is_filtered())
      return true;
    return true;
  }
};

Statement::If *Alt::Simple::add_empty_check(
  std::list<Statement::Base*> &stmts, const Fn_Arg::Base &b) {
  std::list<Expr::Base*> exprs;
  for (std::vector<Statement::Var_Decl*>::const_iterator i =
       b.ret_decls().begin(); i != b.ret_decls().end(); ++i) {
    Expr::Fn_Call *f = new Expr::Fn_Call(Expr::Fn_Call::NOT_EMPTY);
    f->add_arg(new Expr::Vacc(**i));
    exprs.push_back(f);
  }
  Expr::Base *e = Expr::seq_to_tree<Expr::Base, Expr::And>(
    exprs.begin(), exprs.end());
  Statement::If *c = new Statement::If(e);
  ret_decl_empty_block(c);
  stmts.push_back(c);
  return c;
}


void Alt::Simple::add_clear_code(
  std::list<Statement::Base*> &stmts, const Fn_Arg::Base &b) {
  for (std::vector<Statement::Var_Decl*>::const_iterator i =
       b.ret_decls().begin(); i != b.ret_decls().end(); ++i) {
    Statement::Fn_Call *e = new Statement::Fn_Call(Statement::Fn_Call::ERASE);
    e->add_arg(**i);
    stmts.push_back(e);
  }
}


std::list<Statement::Base*> *Alt::Simple::reorder_args_cg(
  AST &ast, std::list<Statement::Base*> &x) {
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    (*i)->codegen(ast);
  }
  return add_arg_code(ast, x);
}


std::list<Statement::Base*> *Alt::Simple::add_arg_code(
  AST &ast, std::list<Statement::Base*> &x) {
  std::list<Statement::Base*> *stmts = &x;

  std::vector<Fn_Arg::Base*> l(args.size());
  std::copy(args.begin(), args.end(), l.begin());
  std::sort(l.begin(), l.end(), Fn_Arg_Cmp());

  std::vector<Fn_Arg::Base*>::iterator i = l.begin();
  assert(i != l.end());

  Fn_Arg::Base *last = *i;
  stmts->insert(
    stmts->end(), (*i)->statements().begin(), (*i)->statements().end());

  if ((*i)->is(Fn_Arg::ALT)) {
    for (std::vector<Statement::Var_Decl*>::const_iterator j =
         (*i)->ret_decls().begin(); j != (*i)->ret_decls().end(); ++j) {
      stmts->push_back(*j);
    }
  }
  ++i;
  for (; i != l.end(); ++i) {
    if (last->is(Fn_Arg::CONST)) {
      last = *i;
      continue;
    }
    Statement::If *c = add_empty_check(*stmts, *last);
    deep_erase_if_backtrace(c, l.begin(), i);

    add_clear_code(*stmts, *last);

    stmts = &c->then;

    stmts->insert(
      stmts->end(), (*i)->statements().begin(), (*i)->statements().end());
    for (std::vector<Statement::Var_Decl*>::const_iterator j =
         (*i)->ret_decls().begin(); j != (*i)->ret_decls().end(); ++j) {
      stmts->push_back(*j);
    }
    last = *i;
  }
  if (last->is(Fn_Arg::CONST)) {
    return stmts;
  }

  Statement::If *c = add_empty_check(*stmts, *last);
  // deep_erase_if_backtrace(c, l.begin(), ++(l.begin()));

  add_clear_code(*stmts, *last);

  return &c->then;
}


void Alt::Simple::add_overlay_code(
  std::list<Statement::Base*> *& stmts,
  AST &ast, std::list<Expr::Fn_Call*> &exprs, Filter::Type t) const {
  std::list<Filter*> l;
  for (std::list<Filter*>::const_iterator i = filters.begin();
       i != filters.end(); ++i) {
    if ((*i)->is(t)) {
      l.push_back(*i);
    }
  }
  for (std::list<Filter*>::iterator i = l.begin(); i != l.end(); ++i) {
    Filter *filter = *i;
    Expr::Fn_Call *f = new Expr::Fn_Call(*filter);
    exprs.push_back(f);
  }
  if (exprs.empty()) {
    return;
  }
  Expr::Base *e = Expr::seq_to_tree<Expr::Base, Expr::And>(
    exprs.begin(), exprs.end());
  Statement::If *c = new Statement::If(e);
  stmts->push_back(c);
  stmts = & c->then;
  // see also grammar2/overlay.gap
  // c->els.push_back(
  //    new Statement::Fn_Call(Statement::Fn_Call::EMPTY, *ret_decl));
}


void Alt::Simple::add_with_overlay_code(
  std::list<Statement::Base*> *&stmts, AST &ast) const {
  std::list<Expr::Fn_Call*> exprs;
  add_overlay_code(stmts, ast, exprs, Filter::WITH_OVERLAY);
  for (std::list<Expr::Fn_Call*>::iterator i = exprs.begin();
       i != exprs.end(); ++i) {
    Expr::Fn_Call *f = *i;
    add_seqs(f, ast);
    for (std::list<Fn_Arg::Base*>::const_iterator j = args.begin();
         j != args.end(); ++j) {
      f->add((*j)->left_indices, (*j)->right_indices);
    }
  }
}


void Alt::Simple::add_suchthat_overlay_code(
  std::list<Statement::Base*> *&stmts, AST &ast) const {
  std::list<Expr::Fn_Call*> exprs;
  add_overlay_code(stmts, ast, exprs, Filter::SUCHTHAT_OVERLAY);
  for (std::list<Expr::Fn_Call*>::iterator i = exprs.begin();
       i != exprs.end(); ++i) {
    Expr::Fn_Call *f = *i;
    for (std::list<Fn_Arg::Base*>::const_iterator j = args.begin();
         j != args.end(); ++j) {
      std::vector<Statement::Var_Decl*>::const_iterator l =
        (*j)->ret_decls().begin();
      for (std::vector<Statement::Var_Decl*>::const_iterator k =
           (*j)->var_decls().begin(); k != (*j)->var_decls().end(); ++k, ++l) {
        if (*k) {
          f->add_arg(**k);
        } else {
          f->add_arg(**l);
        }
      }
    }
  }
}


void Alt::Simple::add_subopt_guards(
  std::list<Statement::Base*> *&stmts, AST &ast) {
  if (ast.code_mode() != Code::Mode::SUBOPT) {
    return;
  }

  // FIXME ? else foreach loops need to be in sync
  if (!foreach_loops.empty()) {
    return;
  }

  std::list<Statement::Base*> *loop_body = stmts;
  if (!foreach_loops.empty()) {
    stmts->push_back(foreach_loops.front());
    Statement::Foreach *f = nest_for_loops(
      foreach_loops.begin(), foreach_loops.end());
    loop_body = &f->statements;
  }

  assert(!pre_decl.empty());
  // if (*pre_decl.front()->name != "ans")
  // is equivalent to: ( || !foreach_loops.empty() <- not implemented yet )
  if (!has_moving_k()) {
    loop_body->push_back(pre_decl.front());
  }

  loop_body->insert(loop_body->end(), pre_stmts.begin(), pre_stmts.end());
  loop_body = pre_cond.front();

  for (Statement::iterator i = Statement::begin(body_stmts);
       i != Statement::end(); ++i) {
    Statement::Base *s = *i;
    if (!s->is(Statement::FN_CALL)) {
      continue;
    }
    Statement::Fn_Call *f = dynamic_cast<Statement::Fn_Call*>(s);
    if (f->builtin == Statement::Fn_Call::PUSH_BACK) {
      f->disable();
    }
  }
  loop_body->insert(loop_body->end(), body_stmts.begin(), body_stmts.end());

  assert(!pre_decl.empty());
  Expr::Vacc *ans = new Expr::Vacc(*pre_decl.front());
  Expr::Fn_Call *not_empty = new Expr::Fn_Call(Expr::Fn_Call::NOT_EMPTY);
  not_empty->add_arg(ans);
  Expr::Vacc *score = new Expr::Vacc(new std::string("score"));
  Expr::Vacc *delta = new Expr::Vacc(new std::string("delta"));
  Expr::Base *e = 0;
  // XXX case if fn_type == NONE because of eliminate_lists()
  // then subopt guards gain nothing, right?
  if (top_level && choice_fn_type_ != Expr::Fn_Call::NONE) {
    Expr::Minus *sub = 0;
    switch (choice_fn_type_) {
      case Expr::Fn_Call::MINIMUM :
        sub = new Expr::Minus(ans, score);
        break;
      case Expr::Fn_Call::MAXIMUM :
        sub = new Expr::Minus(score, ans);
        break;
      default:
        assert(false);
    }
    e = new Expr::And( not_empty, new Expr::Less_Eq(sub, delta));
  } else {
    e = not_empty;
  }
  Statement::If *c = new Statement::If(e);
  loop_body->push_back(c);
  stmts = &c->then;
}

std::list<Statement::Base*> *Alt::Simple::insert_index_stmts(
  std::list<Statement::Base*> *stmts) {
  if (inner_code) {
    stmts->insert(stmts->end(), index_stmts.begin(), index_stmts.end());
    inner_code->clear();
    return inner_code;
  }
  std::list<Statement::Base*> *ret = stmts;
  for (Statement::iterator i = Statement::begin(index_stmts);
       i != Statement::end(); ++i) {
    Statement::Base *s = *i;
    if (s->is(Statement::FN_CALL)) {
      Statement::Fn_Call *f = dynamic_cast<Statement::Fn_Call*>(s);
      assert(f);
      if (f->name_ && *f->name_ == "INNER") {
        Statement::Block *b = new Statement::Block();
        *i = b;
        ret = b->stmts();
        inner_code = ret;
        break;
      }
    }
  }
  stmts->insert(stmts->end(), index_stmts.begin(), index_stmts.end());
  return ret;
}

std::list<Statement::Base*> *Alt::Simple::add_filter_guards(
    std::list<Statement::Base*> *stmts,
    Statement::If *filter_guards) {
  if (filter_guards) {
    stmts->push_back(filter_guards);
    stmts = &filter_guards->then;
    ret_decl_empty_block(filter_guards);
  }
  return stmts;
}
std::list<Statement::Base*> *Alt::Simple::add_for_loops(
    std::list<Statement::Base*> *stmts,
    std::list<Statement::For *> loops,
    bool has_index_overlay) {
  if (!loops.empty() && !has_index_overlay) {
    std::list<Statement::For*> *l = &loops;
    /*
    if (has_index_overlay()) {
      l = &index_overlay.front()->loops;
    }
    */
    stmts->push_back(l->front());
    Statement::For *loop = nest_for_loops(l->begin(), l->end());
    stmts = &loop->statements;
  }
  return stmts;
}

std::list<Statement::Base*> *Alt::Simple::add_guards(
    std::list<Statement::Base*> *stmts, bool add_outside_guards) {
  Statement::If *use_guards = guards;
  if (add_outside_guards) {
    use_guards = guards_outside;
  }
  if (use_guards) {
    stmts->push_back(use_guards);
    if (datatype->simple()->is(::Type::LIST)) {
      // only call finalize on hash lists
      if (!ret_decl->rhs && adp_specialization == ADP_Mode::STANDARD) {
        Statement::Fn_Call *f = new Statement::Fn_Call(
          Statement::Fn_Call::FINALIZE);
        f->add_arg(*ret_decl);
        stmts->push_back(f);
      }
    }
    stmts = &use_guards->then;
  }
  return stmts;
}

void Alt::Simple::codegen(AST &ast) {
  // std::cout << "-----------Simple IN" << std::endl;

  bool nullary = false;
  Expr::Base *answer_list = NULL;

  // make the list not
  if (!disabled_spec && adp_specialization != ADP_Mode::STANDARD) {
      nullary = is_nullary();
      answer_list = ret_decl->rhs;

      if (nullary || ADP_Mode::is_step(adp_specialization)) {
          ret_decl->rhs = NULL;
      }
  }

  statements.clear();
  push_back_ret_decl();
  std::list<Statement::Base*> *stmts = &statements;


  init_guards();
  if (this->is_partof_outside()) {
    init_outside_guards();
  }
  stmts = add_guards(stmts, this->is_partof_outside());

  init_filter_guards(ast);

        // answer_list is always set when return type is a list
        // see symbol set_ret_decl_rhs
        if (nullary && answer_list && !disabled_spec) {
            // automatically && adp_specialization != ADP_Mode::STANDARD

            Expr::Fn_Call *eval_null = new Expr::Fn_Call(eval_nullary_fn);
            eval_null->add_arg(*ret_decl);

            std::string ret_eval = *ret_decl->name + "_eval";
            Statement::Var_Decl *input_list = new Statement::Var_Decl(
              ret_decl->type, ret_eval);
            input_list->rhs = eval_null;

            Statement::Fn_Call *append = new Statement::Fn_Call(
              Statement::Fn_Call::APPEND);

            append->add_arg(answer_list);
            append->add_arg(*input_list);


            statements.push_back(input_list);
            statements.push_back(append);

            if (ADP_Mode::is_step(adp_specialization)) {
                add_specialised_arguments(append);

            } else {
                Statement::Fn_Call *mark = new Statement::Fn_Call(
                  Statement::Fn_Call::MARK_POSITION);
                mark->add_arg(answer_list);
                mark->add_arg(*marker);

                statements.push_back(mark);
            }
        }

  if (this->is_partof_outside()) {
    // add for loops for moving boundaries
    stmts = add_for_loops(stmts, loops, has_index_overlay());

    stmts = add_guards(stmts, false);

    // add filter_guards
    stmts = add_filter_guards(stmts, filter_guards);
  } else {
    // add filter_guards
    stmts = add_filter_guards(stmts, filter_guards);

    // add for loops for moving boundaries
    stmts = add_for_loops(stmts, loops, has_index_overlay());
  }

  add_subopt_guards(stmts, ast);

  stmts = insert_index_stmts(stmts);

  std::list<Statement::Base*> *inner_guards_body = reorder_args_cg(ast, *stmts);
  pre_stmts.clear();
  pre_cond.clear();
  pre_cond.push_back(add_arg_code(ast, pre_stmts));

  init_foreach();
  std::list<Statement::Base*> *loop_body = inner_guards_body;
  if (!foreach_loops.empty()) {
    inner_guards_body->push_back(foreach_loops.front());
    Statement::Foreach *f = nest_for_loops(
      foreach_loops.begin(), foreach_loops.end());
    loop_body = &f->statements;
  }
  init_body(ast);
  if (!disabled_spec && !nullary && answer_list &&
     adp_specialization != ADP_Mode::STANDARD) {
      std::list<Statement::Base*> *app_stmts;
      // get statements after innermost loop
      if (foreach_loops.size() > 1) {
        // 2 or more nested loops
        std::list<Statement::Foreach *>::iterator it = foreach_loops.begin();
        std::advance(it, foreach_loops.size()-2);
        app_stmts = &(*it)->statements;
      } else {
        app_stmts = inner_guards_body;
      }

      if (ADP_Mode::is_step(adp_specialization)) {
        Statement::Fn_Call *append = new Statement::Fn_Call(
          Statement::Fn_Call::APPEND);
        append->add_arg(answer_list);
        append->add_arg(*ret_decl);

        add_specialised_arguments(append);

        app_stmts->push_back(append);

        Statement::Fn_Call *clear = new Statement::Fn_Call(
          Statement::Fn_Call::CLEAR);
        clear->add_arg(*ret_decl);

        app_stmts->push_back(clear);
      } else {
          Statement::Fn_Call *mark = new Statement::Fn_Call(
            Statement::Fn_Call::MARK_POSITION);
          mark->add_arg(answer_list);
          mark->add_arg(*marker);

          app_stmts->push_back(mark);
      }
  }

  loop_body->insert(loop_body->end(), body_stmts.begin(), body_stmts.end());

       // std::cout << "-----------Simple OUT" << std::endl;
}


void Alt::Link::add_args(Expr::Fn_Call *fn) {
  if (is_explicit()) {
    fn->add(indices);
    fn->add(ntparas);
    return;
  }

  if (nt->is(Symbol::TERMINAL)) {
    std::vector<Expr::Base*>::iterator j = right_indices.begin();
    for (std::vector<Expr::Base*>::iterator i = left_indices.begin();
         i != left_indices.end(); ++i, ++j) {
      fn->add_arg(*i);
      fn->add_arg(*j);
    }
    return;
  }

  Symbol::NT *x = dynamic_cast<Symbol::NT*>(nt);
  const std::vector<Table> &tables = x->tables();

  assert(left_indices.size() == tables.size());
  std::vector<Expr::Base*>::iterator k = right_indices.begin();
  std::vector<Expr::Base*>::iterator j = left_indices.begin();
  for (std::vector<Table>::const_iterator i = tables.begin();
       i != tables.end(); ++i, ++j, ++k) {
    if (!(*i).delete_left_index()) {
      fn->add_arg(*j);
    }
    if (!(*i).delete_right_index()) {
      fn->add_arg(*k);
    }
  }

  fn->add(ntparas);
}


void Alt::Link::codegen(AST &ast) {
  // std::cout << "link " << *name << std::endl;

  statements.clear();
  push_back_ret_decl();

  std::string *s = NULL;
  if (nt->is(Symbol::TERMINAL)) {
    s = name;
  } else {
    if (ast.backtrace()) {
      s = new std::string("bt_proxy_nt_" + *name);
    } else if (ast.code_mode() == Code::Mode::SUBOPT) {
      s = new std::string("bt_nt_" + *name);
    } else {
      s = new std::string("nt_" + *name);
    }
  }
  Expr::Fn_Call *fn = new Expr::Fn_Call(s);
  if (nt->is(Symbol::TERMINAL)) {
    add_seqs(fn, ast);
  }

  add_args(fn);

  if (nt->is(Symbol::NONTERMINAL) && ast.code_mode() == Code::Mode::SUBOPT) {
    fn->exprs.push_back(new Expr::Vacc(new std::string("global_score")));
    fn->exprs.push_back(new Expr::Vacc(new std::string("delta")));
  }

  init_filter_guards(ast);
  if (filter_guards) {
    Statement::Var_Assign *v = new Statement::Var_Assign(*ret_decl, fn);
    statements.push_back(filter_guards);
    filter_guards->then.push_back(v);
  } else {
    ret_decl->rhs = fn;
  }
  Expr::Base *suchthat = suchthat_code(*ret_decl);
  if (suchthat) {
    Statement::If *c = new Statement::If(suchthat);
    statements.push_back(c);
    if (ret_decl->type->is(::Type::LIST)) {
      Statement::Var_Decl *v = ret_decl->clone();
      c->els.push_back(v);
      Statement::Var_Assign *ass = new Statement::Var_Assign(*ret_decl, *v);
      c->els.push_back(ass);
    } else {
      Statement::Fn_Call *e = new Statement::Fn_Call(Statement::Fn_Call::EMPTY);
      e->add_arg(*ret_decl);
      c->els.push_back(e);
    }
  }
}


void Alt::Block::codegen(AST &ast) {
  // std::cout << "-----------------Block " << std::endl;
  statements.clear();
  push_back_ret_decl();
  Statement::Fn_Call *fn = new Statement::Fn_Call(Statement::Fn_Call::EMPTY);
  fn->add_arg(*ret_decl);
  statements.push_back(fn);
  init_filter_guards(ast);
  if (filter_guards) {
    filter_guards->els.clear();
  }

  std::list<Statement::Base*> *stmts = NULL;
  if (filter_guards) {
    statements.push_back(filter_guards);
    stmts = &filter_guards->then;
  } else {
    stmts = &statements;
  }
  for (std::list<Alt::Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    if ((*i)->data_type()->simple()->is(::Type::LIST)) {
      if (!(*i)->is(Alt::LINK)) {
        if (!((*i)->data_type()->simple()->is(::Type::LIST) &&
            datatype->simple()->is(::Type::LIST))) {
          (*i)->ret_decl->rhs = new Expr::Vacc(*ret_decl);
        }
      }
    }

    (*i)->codegen(ast);
    stmts->insert(
      stmts->end(), (*i)->statements.begin(), (*i)->statements.end());

    std::list<Statement::Base*> *inner_stmts = stmts;
    Expr::Base *suchthat = suchthat_code(*(*i)->ret_decl);
    if (suchthat) {
      Statement::If *c = new Statement::If(suchthat);
      inner_stmts = & c->then;
    }

    if ((*i)->data_type()->simple()->is(::Type::LIST) &&
        datatype->simple()->is(::Type::LIST)) {
      Statement::Fn_Call *fn = new Statement::Fn_Call(
        Statement::Fn_Call::APPEND);
      fn->add_arg(*ret_decl);
      fn->add_arg(*(*i)->ret_decl);

      if (!disabled_spec && adp_specialization != ADP_Mode::STANDARD &&
         ADP_Mode::is_step(adp_specialization) ) {
          add_specialised_arguments(fn);
      }
      inner_stmts->push_back(fn);

      if (!disabled_spec && adp_specialization != ADP_Mode::STANDARD &&
         !ADP_Mode::is_step(adp_specialization) ) {
           Statement::Fn_Call *mark = new Statement::Fn_Call(
             Statement::Fn_Call::MARK_POSITION);
            mark->add_arg(*ret_decl);
            mark->add_arg(*marker);

           inner_stmts->push_back(mark);
      }
    } else if (!datatype->simple()->is(::Type::LIST) &&
               !(*i)->data_type()->simple()->is(::Type::LIST)) {
      Statement::Var_Assign *v = new Statement::Var_Assign(
        *ret_decl, *(*i)->ret_decl);
      inner_stmts->push_back(v);
    } else if (datatype->simple()->is(::Type::LIST)) {
      //  std::cout << "Push back BLOCK" << *ret_decl << std::endl;

      Expr::Fn_Call *e = new Expr::Fn_Call(Expr::Fn_Call::NOT_EMPTY);
      e->add_arg(*(*i)->ret_decl);
      Statement::If *cond = new Statement::If(e);
      inner_stmts->push_back(cond);

      if (!disabled_spec && adp_specialization != ADP_Mode::STANDARD &&
          ADP_Mode::is_step(adp_specialization) ) {
          Statement::Fn_Call *fn = new Statement::Fn_Call(
            Statement::Fn_Call::APPEND);
          fn->add_arg(*ret_decl);
          fn->add_arg(*(*i)->ret_decl);

          add_specialised_arguments(fn);

          cond->then.push_back(fn);

      } else {
          Statement::Fn_Call *fn = new Statement::Fn_Call(
            Statement::Fn_Call::PUSH_BACK);
          fn->add_arg(*ret_decl);
          fn->add_arg(*(*i)->ret_decl);

          cond->then.push_back(fn);

          if (!disabled_spec && adp_specialization != ADP_Mode::STANDARD &&
              !ADP_Mode::is_step(adp_specialization) ) {
              Statement::Fn_Call *mark = new Statement::Fn_Call(
                Statement::Fn_Call::MARK_POSITION);
              mark->add_arg(*ret_decl);
              mark->add_arg(*marker);

              cond->then.push_back(mark);
         }
      }
    }
  }
  if (datatype->simple()->is(::Type::LIST)) {
    if (!ret_decl->rhs && adp_specialization == ADP_Mode::STANDARD) {
      Statement::Fn_Call *f = new Statement::Fn_Call(
        Statement::Fn_Call::FINALIZE);
      f->add_arg(*ret_decl);
      stmts->push_back(f);
    }
  }
       // std::cout << "-----------------End Block " << std::endl;
}


void Alt::Base::init_filter_guards(AST &ast) {
  if (filters.empty() && multi_filter.empty()) {
    return;
  }
  std::list<Expr::Fn_Call*> exprs;
  for (std::list<Filter*>::iterator i = filters.begin();
       i != filters.end(); ++i) {
    if (!(*i)->is(Filter::WITH)) {
      continue;
    }
    if ((*i)->is_stateful()) {
      Expr::Fn_Call *fn = new Expr::Fn_Call(new std::string("init"));
      add_seqs(fn, ast);
      fn->exprs.insert(fn->exprs.end(), (*i)->args.begin(), (*i)->args.end());
      ast.sf_filter_code.push_back(std::make_pair(*i, fn));
      Expr::Fn_Call *f = new Expr::Fn_Call(
        new std::string((*i)->id() + ".query"));
      f->add(left_indices, right_indices);
      exprs.push_back(f);
    } else {
      Expr::Fn_Call *fn = new Expr::Fn_Call((*i)->name);
      add_seqs(fn, ast);
      fn->add(left_indices, right_indices);
      fn->exprs.insert(fn->exprs.end(), (*i)->args.begin(), (*i)->args.end());
      exprs.push_back(fn);
    }
  }

  if (!multi_filter.empty()) {
    assert(multi_filter.size() == ast.seq_decls.size());
    assert(multi_filter.size() == left_indices.size());
    std::vector<Statement::Var_Decl*>::const_iterator k = ast.seq_decls.begin();
    std::vector<Expr::Base*>::iterator l = left_indices.begin();
    std::vector<Expr::Base*>::iterator m = right_indices.begin();
    for (std::vector<std::list<Filter*> >::iterator i = multi_filter.begin();
         i != multi_filter.end(); ++i, ++k, ++l, ++m) {
      if (!(*i).front()->is(Filter::WITH)) {
        continue;
      }
      for (std::list<Filter*>::iterator j = (*i).begin();
           j != (*i).end(); ++j) {
        Expr::Fn_Call *fn = new Expr::Fn_Call((*j)->name);
        fn->add_arg(**k);
        fn->add_arg(*l);
        fn->add_arg(*m);
        fn->exprs.insert(fn->exprs.end(), (*j)->args.begin(), (*j)->args.end());
        exprs.push_back(fn);
      }
    }
  }

  if (exprs.empty()) {
    return;
  }
  Expr::Base *arg = Expr::seq_to_tree<Expr::Base, Expr::And>(
    exprs.begin(), exprs.end());
  Statement::If *guard = new Statement::If(arg);
  if ((this->data_type()->is(::Type::LIST)) && (this->top_level)) {
    // don't add an else statement (which erases the return type) to the
    // filter since this would "empty"=erase the answer list of ALL
    // alternatives of a symbol due to the return decl replacement happening
    // in Symbol::NT::eliminate_list_ass
    // see https://github.com/jlab/gapc/pull/123
  } else {
    guard->els.push_back(new Statement::Fn_Call(
      Statement::Fn_Call::EMPTY, *ret_decl));
  }
  filter_guards = guard;
}


void Alt::Simple::print_dot_edge(std::ostream &out, Symbol::NT &nt) {
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    (*i)->print_dot_edge(out, nt);
  }
}


void Alt::Block::print_dot_edge(std::ostream &out, Symbol::NT &nt) {
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    (*i)->print_dot_edge(out, nt);
  }
}


void Alt::Link::print_dot_edge(std::ostream &out, Symbol::NT &n) {
  bool t = false;
  if (nt->is_tabulated()) {
    out << *n.name << " -> " << *nt->name;
  } else {
    out << *n.name << " -> " << *nt->name;
  }
  out << " [";
  if (nt->is_tabulated()) {
    out << "style=dotted";
    t = true;
  }
  if (calls != 0) {
    if (t) {
      out << ',';
    }
    out << "label=\"" << calls << '"';
  }
  out << "];\n";
}


void Alt::Multi::print_dot_edge(std::ostream &out, Symbol::NT &nt) {
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    (*i)->print_dot_edge(out, nt);
  }
}


void Alt::Base::optimize_choice(::Type::List::Push_Type push) {
  if (!ret_decl->type->is(::Type::LIST)) {
    return;
  }
  ::Type::List *l = dynamic_cast< ::Type::List*>(ret_decl->type);
  assert(l);
  l->set_push_type(push);
}


void Alt::Base::optimize_choice(
  ::Type::List::Push_Type push, Statement::Hash_Decl *h) {
  if (!ret_decl->type->is(::Type::LIST)) {
    return;
  }
  optimize_choice(push);
  ::Type::List *l = dynamic_cast< ::Type::List*>(ret_decl->type);
  assert(l);
  l->set_hash_decl(h);
}


void Alt::Link::optimize_choice() {
  if (!ret_decl->type->is(::Type::LIST)) {
    return;
  }
  ::Type::List *x = dynamic_cast< ::Type::List*>(nt->data_type());
  if (!x) {
    return;
  }
  ::Type::List *l = dynamic_cast< ::Type::List*>(ret_decl->type);
  assert(l);
  if ((nt->is(Symbol::NONTERMINAL) && dynamic_cast<Symbol::NT*>(nt)->eval_decl)
      || x->push_type()) {
    if (x->hdecl()) {
      l->set_hash_decl(x->hdecl());
    }
    Alt::Base::optimize_choice(x->push_type());
  } else if (l->push_type()) {
    if (nt->is(Symbol::NONTERMINAL) &&
        !dynamic_cast<Symbol::NT*>(nt)->eval_decl) {
      // XXX paraltest adpf shape5 vs. regresstest optchoice
      if (l->hdecl()) {
        x->set_hash_decl(l->hdecl());
        x->set_push_type(l->push_type());
      } else {
        // XXX remove
        // if (x->hdecl())
        //  l->set_hash_decl(x->hdecl());
        // was:
        x->set_push_type(l->push_type());
        // new:
        // l->set_push_type(::Type::List::NORMAL);
      }
    }
  }
}


bool Alt::Link::calls_terminal_parser() const {
  return nt->is(Symbol::TERMINAL);
}


bool Alt::Simple::calls_terminal_parser() const {
  return args.size() == 1 && args.front()->is(Fn_Arg::CONST);
}



namespace Alt {
Multi::Multi(const std::list<Alt::Base*> &t, const Loc &l) :
Base(MULTI, l), list(t) {
}


void Multi::codegen(AST &ast) {
  statements.clear();
  assert(ret_decls_.size() == list.size());

  init_filter_guards(ast);
  if (filter_guards) {
    for (std::list<Statement::Var_Decl*>::iterator i = ret_decls_.begin();
         i != ret_decls_.end(); ++i) {
      statements.push_back(*i);
    }
  }
  std::list<Statement::Base*> *stmts = &statements;
  if (filter_guards) {
    statements.push_back(filter_guards);
    stmts = &filter_guards->then;
    filter_guards->els.clear();
    for (std::list<Statement::Var_Decl*>::iterator i = ret_decls_.begin();
         i != ret_decls_.end(); ++i) {
      filter_guards->els.push_back(
        new Statement::Fn_Call(Statement::Fn_Call::EMPTY, **i));
    }
  }
  std::list<Statement::Var_Decl*>::iterator j = ret_decls_.begin();
  for (std::list<Alt::Base*>::iterator i = list.begin();
       i != list.end(); ++i, ++j) {
    (*i)->codegen(ast);
    stmts->insert(
      stmts->end(), (*i)->statements.begin(), (*i)->statements.end());
    assert(!(*j)->rhs);
    if (filter_guards) {
      stmts->push_back(
        new Statement::Var_Assign(**j, new Expr::Vacc(*(*i)->ret_decl)));
    } else {
      (*j)->rhs = new Expr::Vacc(*(*i)->ret_decl);
      stmts->push_back(*j);
    }
  }
}
}  // namespace Alt



void Alt::Base::init_multi_ys() {
  assert(tracks_ == m_ys.tracks());
  // FIXME remove filters and just use multi_filter
  if (0 && tracks_ > 1 && !filters.empty()) {
    Log::instance()->error(
      location, "Multi track rule, but single track filter.");
    return;
  }
  if (tracks_ == 1 && !multi_filter.empty()) {
    Log::instance()->error(
      location,  "Multi track filter with single track rule.");
    return;
  }
  if (tracks_ == 1) {
    m_ys(0).with(filters);
  } else {
    m_ys.with(multi_filter);
  }
}


void Alt::Simple::init_multi_ys() {
  m_ys = Yield::Multi(tracks_);

  if (is_terminal()) {
    assert(tracks_ == 1);

    /* a terminal like CHAR or ROPE can have one argument, that is an implicit
       filter like CHAR('a') or ROPE("stefan"), which shall only accept sub-
       words that are 'a' or 'stefan', respectively if this argument is of a
       certain length, it should determine the yield size of the terminal
       parser. Therefore we here iterate through all arguments (currently just
       one 2021-05-17) and look for the longest. If the result is > 0, we set
       the terminal_ys to this value.
       However, this mechanism is only valid for terminal parsers that consume
       input, i.e. NOT for CONST_xxx terminal parser, that inject values for
       later use in algebras.
    */
    if (name->rfind("CONST_", 0) != 0) {
        Yield::Poly max_terminal_arg_yield = Yield::Poly(0);
        for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
             i != args.end(); ++i) {
            Fn_Arg::Const *fn = dynamic_cast<Fn_Arg::Const*>(*i);
            if (fn) {
                max_terminal_arg_yield *= fn->expr().yield_size().high();
            }
        }
        if (max_terminal_arg_yield > 0) {
            terminal_ys.set(max_terminal_arg_yield, max_terminal_arg_yield);
        }
    }

    m_ys(0) = terminal_ys;
    Base::init_multi_ys();
    return;
  }

  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    Fn_Arg::Base *fn = *i;
    fn->init_multi_ys();
    m_ys += fn->multi_ys();
  }
  Base::init_multi_ys();

  if (has_index_overlay()) {
    index_overlay.front()->init_multi_ys();
    m_ys = index_overlay.front()->multi_ys();
  }
}


void Alt::Link::init_multi_ys() {
  if (nt->is_partof_outside()) {
    m_ys.set_tracks(tracks_);
    for (Yield::Multi::iterator i = m_ys.begin(); i != m_ys.end(); ++i) {
      *i = Yield::Size(0, Yield::UP);
    }
  } else {
    m_ys = nt->multi_ys();
  }
  Base::init_multi_ys();
}


void Alt::Block::init_multi_ys() {
  std::list<Base*>::iterator i = alts.begin();
  assert(i != alts.end());
  (*i)->init_multi_ys();
  m_ys = (*i)->multi_ys();
  ++i;
  for (; i != alts.end(); ++i) {
    (*i)->init_multi_ys();
    m_ys /= (*i)->multi_ys();
  }
  Base::init_multi_ys();
}


void Alt::Multi::init_multi_ys() {
  m_ys.set_tracks(list.size());
  Yield::Multi::iterator j = m_ys.begin();
  for (std::list<Base*>::iterator i = list.begin();
       i != list.end(); ++i, ++j) {
    (*i)->init_multi_ys();
    assert((*i)->multi_ys().tracks() == 1);
    *j = (*i)->multi_ys()(0);
  }
  Base::init_multi_ys();
}


void Alt::Base::add_multitrack_filter(
  const std::list<Filter*> &l, Filter::Type t, const Loc &loc) {
  if (multi_filter.empty()) {
    multi_filter.resize(l.size());
  }
  if (multi_filter.size() != l.size()) {
    std::ostringstream o;
    o << "Filter has more tracks than previous one: "
      << multi_filter.size() << " vs. " << l.size();
    Log::instance()->error(loc, o.str());
    return;
  }
  size_t j = 0;
  for (std::list<Filter*>::const_iterator i = l.begin();
       i != l.end(); ++i, ++j) {
    (*i)->type = t;
    multi_filter[j].push_back(*i);
  }
}


void Alt::Simple::sum_rhs(
  Yield::Multi &y, std::list<Fn_Arg::Base*>::const_iterator i,
  const std::list<Fn_Arg::Base*>::const_iterator &end) const {
  ++i;
  for (; i != end; ++i) {
    y +=  (*i)->multi_ys();
  }
}


void Alt::Simple::sum_rhs(
  Yield::Size &y, std::list<Fn_Arg::Base*>::const_iterator i,
  const std::list<Fn_Arg::Base*>::const_iterator &end, size_t track) const {
  ++i;
  for (; i != end; ++i) {
    y +=  (*i)->multi_ys()(track);
  }
}


bool Alt::Simple::multi_detect_loop(const Yield::Multi &left,
    const Yield::Multi &right, Symbol::NT *nt) const {
  if (is_terminal()) {
    return false;
  }

  bool ret = false;
  Yield::Multi l(left);
  for (std::list<Fn_Arg::Base*>::const_iterator i = args.begin();
       i != args.end(); ++i) {
    Yield::Multi r(right);
    sum_rhs(r, i, args.end());

    if ((*i)->is(Fn_Arg::ALT) && l.is_low_zero() && r.is_low_zero()) {
      bool a = (*i)->alt_ref()->multi_detect_loop(l, r, nt);
      ret = ret || a;
    }

    l += (*i)->multi_ys();
  }
  return ret;
}


bool Alt::Link::multi_detect_loop(
  const Yield::Multi &left, const Yield::Multi &right, Symbol::NT *n) const {
  if (nt == n) {
    return true;
  }
  return nt->multi_detect_loop(left, right, n);
}


bool Alt::Block::multi_detect_loop(
  const Yield::Multi &left, const Yield::Multi &right, Symbol::NT *nt) const {
  bool r = false;
  for (std::list<Base*>::const_iterator i = alts.begin();
       i != alts.end(); ++i) {
    bool a = (*i)->multi_detect_loop(left, right, nt);
    r = r || a;
  }
  return r;
}


bool Alt::Multi::multi_detect_loop(
  const Yield::Multi &left, const Yield::Multi &right, Symbol::NT *nt) const {
  bool ret = false;
  assert(left.tracks() == tracks_);
  assert(right.tracks() == tracks_);
  Yield::Multi::const_iterator j = left.begin();
  Yield::Multi::const_iterator k = right.begin();
  for (std::list<Base*>::const_iterator i = list.begin();
       i != list.end(); ++i, ++j, ++k) {
    Yield::Multi l(1), r(1);
    l(0) = *j;
    r(0) = *k;
    ret = ret || (*i)->multi_detect_loop(l, r, nt);
  }
  return ret;
}


// call from every derived class's method
void Alt::Base::multi_propagate_max_filter(
  std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size) {
  Yield::Multi m(max_size);
  m.min_high(m_ys);

  if (multi_ys_max_temp.tracks() != m.tracks()) {
    multi_ys_max_temp.set_tracks(m.tracks());
  }
  multi_ys_max_temp.max_high(m);
}


// call from visitor
void Alt::Base::multi_set_max_size() {
  if (!multi_ys_max_temp.tracks()) {
    return;
  }
  m_ys.min_high(multi_ys_max_temp);
  multi_ys_max_temp.set_tracks(0);
}


void Alt::Simple::multi_propagate_max_filter(
  std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size) {
  if (is_terminal()) {
    return;
  }

  Base::multi_propagate_max_filter(nt_sizes, max_size);

  Yield::Multi l(tracks_);
  for (std::list<Fn_Arg::Base*>::const_iterator i = args.begin();
       i != args.end(); ++i) {
    Yield::Multi r(tracks_);
    sum_rhs(r, i, args.end());

    if ((*i)->is(Fn_Arg::ALT)) {
      Yield::Multi m(max_size);
      m.min_high(m_ys);
      m.sub_high_low(l);
      m.sub_high_low(r);
      // - < ... , UP, ... > hat komponentenweise keinen Effekt
      // - saturiert auf 0
      // m -= l; // actually m.high()-l.low()
      // m -= r; // see above
      (*i)->alt_ref()->multi_propagate_max_filter(nt_sizes, m);
    }

    l += (*i)->multi_ys();
  }
}


void Alt::Block::multi_propagate_max_filter(
  std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size) {
  Base::multi_propagate_max_filter(nt_sizes, max_size);

  Yield::Multi m(max_size);
  m.min_high(m_ys);
  for (std::list<Base*>::const_iterator i = alts.begin();
       i != alts.end(); ++i) {
    (*i)->multi_propagate_max_filter(nt_sizes, m);
  }
}


void Alt::Link::multi_propagate_max_filter(
  std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size) {
  Base::multi_propagate_max_filter(nt_sizes, max_size);

  Yield::Multi m(max_size);
  m.min_high(m_ys);
  Symbol::NT *n = dynamic_cast<Symbol::NT*>(nt);
  if (n) {
    n->multi_propagate_max_filter(nt_sizes, m);
  }
}


void Alt::Multi::multi_propagate_max_filter(
  std::vector<Yield::Multi> &nt_sizes, const Yield::Multi &max_size) {
  Base::multi_propagate_max_filter(nt_sizes, max_size);

  Yield::Multi m(max_size);
  m.min_high(m_ys);
  assert(m.tracks() == tracks_);
  Yield::Multi::const_iterator j = m.begin();
  for (std::list<Base*>::const_iterator i = list.begin();
       i != list.end(); ++i, ++j) {
    Yield::Multi a(1);
    a(0) = *j;
    (*i)->multi_propagate_max_filter(nt_sizes, a);
  }
}


void Alt::Simple::multi_init_calls(
  const Runtime::Poly &rest, size_t base_tracks) {
  if (is_terminal_) {
    return;
  }
  Runtime::Poly left(rest);
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    if ((*i)->is(Fn_Arg::ALT)) {
      std::list<Fn_Arg::Base*>::iterator j = i;
      ++j;
      Runtime::Poly right(1);
      for (; j != args.end(); ++j) {
        (*j)->alt_ref()->multi_collect_factors(right);
      }

      Runtime::Poly r(left);
      r *= right;
      (*i)->alt_ref()->multi_init_calls(r, base_tracks);

      (*i)->alt_ref()->multi_collect_factors(left);
    }
  }
}


void Alt::Block::multi_init_calls(
  const Runtime::Poly &rest, size_t base_tracks) {
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    (*i)->multi_init_calls(rest, base_tracks);
  }
}


void Alt::Link::multi_init_calls(
  const Runtime::Poly &rest, size_t base_tracks) {
  if (top_level) {
    calls = 1;
    return;
  }
  Runtime::Poly p(rest);
  p *= Runtime::Poly(m_ys);
  calls = p;
  for (size_t i = 0; i < base_tracks; ++i) {
    calls.divide_by_n();
  }
}


void Alt::Multi::multi_init_calls(
  const Runtime::Poly &rest, size_t base_tracks) {
  Runtime::Poly up(rest);
  for (std::list<Base*>::iterator i = list.begin(); i != list.end(); ++i) {
    Runtime::Poly down(up);
    std::list<Base*>::iterator j = i;
    ++j;
    for (; j != list.end(); ++j) {
      (*j)->multi_collect_factors(down);
    }

    (*i)->multi_init_calls(down, base_tracks);

    (*i)->multi_collect_factors(up);
  }
}


void Alt::Simple::multi_collect_factors(Runtime::Poly &p) {
  if (is_terminal_) {
    p *= Runtime::Poly(m_ys);
    return;
  }
  for (std::list<Fn_Arg::Base*>::iterator i = args.begin();
       i != args.end(); ++i) {
    if (!(*i)->is(Fn_Arg::ALT)) {
      continue;
    }
    (*i)->alt_ref()->multi_collect_factors(p);
  }
}


void Alt::Block::multi_collect_factors(Runtime::Poly &p) {
  Runtime::Poly r;
  for (std::list<Base*>::iterator i = alts.begin(); i != alts.end(); ++i) {
    Runtime::Poly x(1);
    (*i)->multi_collect_factors(x);
    r |= x;
  }
  p *= r;
}


void Alt::Link::multi_collect_factors(Runtime::Poly &p) {
  p *= Runtime::Poly(m_ys);
}


void Alt::Multi::multi_collect_factors(Runtime::Poly &p) {
  Runtime::Poly a(m_ys);
  p *= a;
}


void Alt::Base::set_tracks(size_t t, size_t p) {
  tracks_ = t;
  track_pos_ = p;

  left_indices.resize(tracks_);
  right_indices.resize(tracks_);
}


void Alt::Base::set_index_stmts(const std::list<Statement::Base*> &l) {
  Log::instance()->error(
    location,
    "Index statements are only allowed before a simple function alternative.");
}


void Alt::Simple::set_index_stmts(const std::list<Statement::Base*> &l) {
  index_stmts = l;
}


void Alt::Base::set_index_overlay(Alt::Base *alt) {
  Log::instance()->error(
    location, "Alternative is only allowed after a simple function.");
}


void Alt::Simple::set_index_overlay(Alt::Base *alt) {
  Simple *f = dynamic_cast<Simple*>(alt);
  if (!alt) {
    Log::instance()->error(
      location, "As alternative only a simple function is allowed.");
    return;
  }
  index_overlay.push_back(f);
  f->is_index_overlay_ = Bool(true);
}


void Alt::Base::set_ntparas(const Loc &loc, std::list<Expr::Base*> *l) {
  Log::instance()->error(
    loc, "Non-terminal parameters need a non-terminal on the lhs!");
}


void Alt::Link::set_ntparas(const Loc &loc, std::list<Expr::Base*> *l) {
  if (!l || l->empty()) {
    Log::instance()->error(loc, "No non-terminal parser parameters given.");
    return;
  }
  ntparas = *l;
}


void Alt::Simple::set_ntparas(std::list<Expr::Base*> *l) {
  if (!l) {
    return;
  }
  ntparas = *l;
}


bool Alt::Link::check_ntparas() {
  assert(nt);
  Symbol::NT *x = dynamic_cast<Symbol::NT*>(nt);
  if (!x) {
    if (!ntparas.empty()) {
      Log::instance()->error(
        location, "Terminal parser parameters not supported.");
      return false;
    }
    return true;
  }
  if (ntparas.size() != x->ntargs().size()) {
    Log::instance()->error(location, "Number of nt paramenters does not");
    Log::instance()->error(x->location, "match.");
    return false;
  }
  return true;
}


void Alt::Multi::types(std::list< ::Type::Base*> &r) const {
  // FIXME use this
  // ::Type::Multi *m = dynamic_cast< ::Type::Multi*>(datatype());
  // assert(m);
  // std::list< ::Type::Base*> t& = m.types();
  for (std::list<Base*>::const_iterator i = list.begin();
       i != list.end(); ++i) {
    r.push_back((*i)->data_type());
  }
}


const std::list<Statement::Var_Decl*> &Alt::Multi::ret_decls() const {
  assert(!ret_decls_.empty());
  return ret_decls_;
}


bool Alt::Base::choice_set() {
    return datatype->simple()->is(::Type::LIST) && eval_nullary_fn;
}

