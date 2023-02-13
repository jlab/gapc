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


#include <iostream>
#include <algorithm>
#include <utility>
#include <set>

#include "alt.hh"

#include "grammar.hh"
#include "log.hh"
#include "arg.hh"
#include "terminal.hh"

#include "pointer_cmp.hh"

#include "visitor.hh"
#include "list_visitor.hh"
#include "list_size_terminate.hh"
#include "inline_nts.hh"

#include "expr.hh"

#include "index_visitor.hh"

#include "init_decls.hh"

#include "printer.hh"

#include "dep_analysis.hh"

#include "symbol.hh"



void Grammar::add_nt(Symbol::NT *nt) {
  assert(nt->name);
  if (NTs.find(*nt->name) != NTs.end()) {
    Log::instance()->error(
      nt->location, "Nonterminal " + *nt->name + " already defined");
    Log::instance()->error(NTs[*nt->name]->location, "here.");
  } else {
    NTs[*nt->name] = nt;
    nt->set_grammar_index(nt_list.size());
    nt_list.push_back(nt);
  }
}


void Grammar::add_nt_later(Symbol::NT *nt) {
  assert(nt->name);
  if (NTs.find(*nt->name) != NTs.end()) {
    Log::instance()->error(
      nt->location, "Nonterminal " + *nt->name + " already defined");
    Log::instance()->error_continue(NTs[*nt->name]->location, "here.");
  } else {
    nt->set_grammar_index(nt_list.size()+new_nts.size());
    new_nts.push_back(nt);
  }
}


void Grammar::move_new_nts() {
  for (std::list<Symbol::NT*>::iterator i = new_nts.begin();
       i != new_nts.end(); ++i) {
    Symbol::NT *nt = *i;

    if (NTs.find(*nt->name) != NTs.end()) {
      Log::instance()->error(
        nt->location, "Nonterminal " + *nt->name + " already defined");
      Log::instance()->error_continue(NTs[*nt->name]->location, "here.");
      return;
    }

    NTs[*nt->name] = nt;
  }
  nt_list.insert(nt_list.end(), new_nts.begin(), new_nts.end());
  new_nts.clear();
}


/*
 * Annotates each nonterminal that was marked in the source code
 * with the "tabulated" keyword internally and adds it to the
 * table of tabulated non-terminals (Grammar.tabulated).
 * This method also does some integrity checks on the source data
 * structures this method iterates through.
 */
bool Grammar::init_tabulated() {
  bool r = true;
  for (hashtable<std::string, Arg*>::iterator i = tab_names.begin();
       i != tab_names.end(); ++i) {
    if (NTs.find(i->first) == NTs.end()) {
      Log::instance()->error(
        i->second->location,  "Nonterminal " + i->first + " not defined.");
      r = false;
      continue;
    }
    Symbol::Base *s = NTs[i->first];
    if (s->is(Symbol::TERMINAL)) {
      Log::instance()->error(
        i->second->location,  i->first + " is not a Nonterminal.");
      r = false;
      continue;
    }
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>(s);
    nt->set_tabulated();
    tabulated[*(nt->name)] = nt;
  }
  return r;
}


/*
 * Assigns the non-terminal data structure belonging to the
 * axiom of the grammar to the field Grammar.axiom
 */
bool Grammar::init_axiom() {
  if (NTs.find(*axiom_name) == NTs.end()) {
    Log::instance()->error(
      axiom_loc,
      "Axiom " + *axiom_name + " not defined in grammar " + *name + ".");
    return false;
  }
  Symbol::Base *nt = NTs[*axiom_name];
  if (nt->is(Symbol::NONTERMINAL)) {
    axiom = dynamic_cast<Symbol::NT*>(nt);
    return true;
  }
  Log::instance()->error(
    axiom_loc, "Axiom " + *axiom_name + " is a Terminal!");
  return false;
}


/*
 * Marks all non-terminals as reachable if they are reachable
 * by the axiom. In addition the internal grammar graph is linked
 * while it is traversed.
 */
bool Grammar::init_nt_links() {
  return axiom->init_links(*this);
}


void Grammar::renumber_nts() {
  size_t j = 0;
  for (std::list<Symbol::NT*>::iterator i = nt_list.begin();
       i != nt_list.end(); ++i, ++j) {
    (*i)->set_grammar_index(j);
  }
}


bool Grammar::remove_unreachable() {
  bool r = false;
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end();  ) {
    Symbol::Base *nt = i->second;
    std::string s = i->first;
    assert(nt);
    if (nt->is(Symbol::NONTERMINAL) && !nt->is_reachable()) {
      Log::instance()->warning(
        nt->location,
        "Nonterminal " + *(nt->name) + " is not reachable from the axiom.");
      r = true;
      NTs.erase(i++);
      if (nt->is_tabulated()) {
        hashtable<std::string, Symbol::NT*>::iterator x =
          tabulated.find(*nt->name);
        assert(x != tabulated.end());
        tabulated.erase(x);
      }
      nt_list.remove(dynamic_cast<Symbol::NT*>(nt));
    } else {
      if (!nt->is_reachable()) {
        NTs.erase(i++);
      } else {
        ++i;
      }
    }
  }
  renumber_nts();
  return r;
}


void Grammar::print_nts() {
  std::cerr << "Nonterminals:" << std::endl;
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    Symbol::Base *nt = i->second;
    std::cerr << (*nt) << std::endl;
  }
  std::cerr << std::endl;
}


void Grammar::print_links() {
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    Symbol::Base *nt = i->second;
    nt->print_link(std::cerr);
  }
}

bool Grammar::has_nonproductive_NTs() {
  bool r = true;
  while (r) {
    r = false;
    for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
         i != NTs.end(); ++i) {
      Symbol::Base *nt = i->second;
      bool a = nt->init_productive();
      r = r || a;
    }
  }
  r = false;
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    Symbol::Base *nt = i->second;
    if (!nt->is_productive()) {
      r = true;
      Log::instance()->error(
        nt->location, "Nonterminal " + *(nt->name) + " is non-productive.");
    }
  }
  return r;
}


void Grammar::print_multi_ys() const {
  for (std::list<Symbol::NT*>::const_iterator i = nt_list.begin();
       i != nt_list.end(); ++i) {
    std::cerr << *(*i)->name << ' ' << (*i)->multi_ys() << '\n';
  }
  std::cerr << '\n';
}


void Grammar::init_multi_yield_sizes() {
  // First we create a list of yield-sizes which are used
  // as starting point. For each entry in the list of
  // non-terminals there is an entry in the list of yield-sizes.
  std::vector<Yield::Multi> old(nt_list.size());
  std::vector<Yield::Multi>::iterator j = old.begin();
  for (std::list<Symbol::NT*>::iterator i = nt_list.begin();
       i != nt_list.end(); ++i, ++j) {
    (*j).set_tracks((*i)->tracks());
    (*i)->setup_multi_ys();
  }
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    if (i->second->is(Symbol::TERMINAL)) {
      i->second->setup_multi_ys();
    }
  }

  // do some output if the logger is in debugging-mode.
  if (Log::instance()->is_debug()) {
    std::cerr << "mYSA init:\n";
    print_multi_ys();
  }

  // now iterate through the list of non-terminals as long
  // as there is a difference to the old list, but at most
  // four times the number of non-terminals in the list.
  // (four times - why is that?)
  bool cont = false;
  // size_t a = 0, m = 4*nt_list.size();
  size_t a = 0;
  do {
    cont = false;
    std::vector<Yield::Multi>::iterator j = old.begin();
    for (std::list<Symbol::NT*>::iterator i = nt_list.begin();
         i != nt_list.end(); ++i, ++j) {
      (*i)->init_multi_ys();
      if ((*i)->multi_ys() != *j) {
        cont = true;
      }
      *j = (*i)->multi_ys();
    }

    if (Log::instance()->is_debug()) {
      std::cerr << "mYSA iteration: " << a << '\n';
      print_multi_ys();
    }

    ++a;
  } while (cont);
}


bool Grammar::multi_detect_loops() {
  if (Log::instance()->is_debug()) {
    std::cerr << ">>> Multi Detect Loops\n";
  }
  bool r = false;
  for (std::list<Symbol::NT*>::iterator i = nt_list.begin();
       i != nt_list.end(); ++i) {
    bool a = (*i)->multi_detect_loop();
    r = r || a;
  }
  return r;
}


void Grammar::multi_propagate_max_filter() {
  std::vector<Yield::Multi> v(nt_list.size());
  std::list<Symbol::NT*>::iterator t = nt_list.begin();
  for (std::vector<Yield::Multi>::iterator i = v.begin();
       i != v.end(); ++i, ++t) {
    (*i).set_tracks((*t)->tracks());
  }

  axiom->multi_propagate_max_filter(v);
  std::vector<Yield::Multi>::iterator j = v.begin();
  for (std::list<Symbol::NT*>::iterator i = nt_list.begin();
       i != nt_list.end(); ++i, ++j) {
    (*i)->update_max_ys(*j);
  }

  // defines the visitor used a few lines later to set the max size
  struct Multi_Max_Visitor : public Visitor {
    void visit(Alt::Base &b) {
      b.multi_set_max_size();
    }
  };

  // create a visitor and traverse the
  Multi_Max_Visitor visitor;
  traverse(visitor);
}


void Grammar::init_table_dims() {
#ifndef NDEBUG
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    Symbol::Base *n = i->second;
    if (n->is(Symbol::NONTERMINAL)) {
      assert(!n->active);
    }
  }
#endif

  for (size_t track = 0; track < axiom->tracks(); ++track) {
    Yield::Size l(Yield::Poly(0), Yield::Poly(0));
    Yield::Size r(Yield::Poly(0), Yield::Poly(0));

    std::vector<Yield::Size> temp_ls(nt_list.size());
    std::vector<Yield::Size> temp_rs(nt_list.size());

    axiom->init_table_dim(l, r, temp_ls, temp_rs, track);
  }


#ifndef NDEBUG
  for (std::list<Symbol::NT*>::iterator i = nt_list.begin();
       i != nt_list.end(); ++i) {
    if ((*i)->tables().front().type() == Table::NONE) {
      Log::instance()->error(
        "Internal error: tables of " + *(*i)->name + " are not initialized.");
    }
    assert((*i)->tables().front().type() != Table::NONE);
  }
#endif
}


void Grammar::window_table_dims() {
  for (std::list<Symbol::NT*>::iterator i = nt_list.begin();
       i != nt_list.end(); ++i) {
    (*i)->window_table_dim();
  }
}


void Grammar::init_calls() {
  for (std::list<Symbol::NT*>::iterator i = nt_list.begin();
       i != nt_list.end(); ++i) {
    (*i)->multi_init_calls();
  }
}


#include "tracks_visitor.hh"
#include "ast.hh"


bool Grammar::init_tracks() {
  size_t tracks = ast.input.tracks();
  assert(tracks);
  axiom->set_tracks(tracks, 0);
  if (Log::instance()->is_debug()) {
    std::cerr << "Tracks axiom: " << tracks << '\n';
  }
  Tracks_Visitor v(*this);
  size_t i = 0;
  do {
    v.again = false;
    traverse(v);
    if (v.error) {
      return false;
    }
    move_new_nts();
    ++i;
  } while (v.again);
  return true;
}


/*
 * Checks the semantic of the grammar. This includes the following
 * things:
 * 1) setting the axiom by resolving the axiom name to a pointer
 *    that is of type  Symbol::NT*
 * 2) linking together all symbols, which creates a graph structure
 *    from the grammar whose root node is the axiom
 * 3)
 */
bool Grammar::check_semantic() {
  bool b, r = true;
  b = init_tabulated();
  r = r && b;
  b = init_axiom();
  if (!b) {
    return false;
  }
  r = r && b;
  // Create graph structure of the grammar.
  b = init_nt_links();
  r = r && b;
  // Remove the symbols that are not included in the
  // graph previously established.
  remove_unreachable();
  // Check: there must not be any non-productive
  // non-terminals in the grammar. If this check
  // fails, the remaining steps do not apply.
  b = !has_nonproductive_NTs();
  r = r && b;

  if (r) {
    b = init_tracks();
  }
  r = r && b;

  // calculate yield sizes, and detect loops
  if (r) {
    init_multi_yield_sizes();
    b = !multi_detect_loops();
    r = r && b;
    multi_propagate_max_filter();
  }
  // calculate table dimensions
  if (r) {
    init_table_dims();
    if (Log::instance()->is_debug()) {
      std::cerr << ">>> Init table dims: " << std::endl;
      print_nts();
    }
  }
  // detect self recurrence
  if (r) {
    init_calls();
    init_self_rec();
    if (Log::instance()->is_debug()) {
      std::cerr << ">>> Init runtime: " << std::endl;
      print_nts();
      print_links();
    }
  }
  return r;
}


const Runtime::Asm::Poly & Grammar::runtime_by_width() {
  if (asm_rt != 0) {
    return asm_rt;
  }
  // if all nt have table sizes <= lin, asm rt can be > lin
  // for example grammar/dep1
  size_t r = 0;
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    Symbol::Base *nt = i->second;
    size_t t = nt->width();
    if (t > r) {
      r = t;
    }
  }
  asm_rt =  Runtime::Asm::Poly(2 + uint32_t(r) - 1);
  return asm_rt;
}


Runtime::Poly Grammar::runtime() {
  std::list<Symbol::NT*> active_list;
  Runtime::Poly one(1);
  Runtime::Poly rt;
  rt = axiom->runtime(active_list, one);
  for (hashtable<std::string, Symbol::NT*>::iterator i =  tabulated.begin();
       i != tabulated.end(); ++i) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>(i->second);
    rt += nt->runtime(active_list, one) * Runtime::Poly(nt->tables());
    // std::cerr << "$$ " << *nt->name << " " << rt << std::endl;
  }
  // put_table_conf(std::cerr);
  // std::cerr << std::endl;
  return rt;
}


bool Grammar::set_tabulated(std::vector<std::string> &v) {
  bool r = true;
  for (std::vector<std::string>::iterator i = v.begin(); i != v.end(); ++i) {
    hashtable<std::string, Symbol::Base*>::iterator a = NTs.find(*i);
    if (a == NTs.end() || a->second->is(Symbol::TERMINAL)) {
      Log::instance()->error(
        "Cannot set NT " + (*i) + " as tabulated - it is not defined.");
      r = false;
    } else {
      a->second->set_tabulated();
      if (!a->second->never_tabulate()) {
        tabulated[*i] = dynamic_cast<Symbol::NT*>(a->second);
      }
    }
  }
  return r;
}


void Grammar::clear_runtime() {
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    i->second->clear_runtime();
  }
}


void Grammar::set_tabulated(hashtable<std::string, Symbol::NT*> &temp) {
  clear_runtime();
  clear_tabulated();
  for (hashtable<std::string, Symbol::NT*>::iterator i = temp.begin();
       i != temp.end(); ++i) {
    i->second->set_tabulated();
    if (!i->second->never_tabulate()) {
      tabulated[i->first] = i->second;
    }
  }
}


void Grammar::set_tabulated(Symbol::Base *nt) {
  clear_runtime();
  nt->set_tabulated();
  assert(nt->is(Symbol::NONTERMINAL));
  if (!nt->never_tabulate()) {
    tabulated[*nt->name] = dynamic_cast<Symbol::NT*>(nt);
  }
}


void Grammar::clear_tabulated() {
  clear_runtime();
  for (hashtable<std::string, Symbol::NT*>::iterator i = tabulated.begin();
       i != tabulated.end(); ++i) {
    i->second->set_tabulated(false);
  }
  tabulated.clear();
}


void Grammar::set_all_tabulated() {
  clear_runtime();
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    if (i->second->is(Symbol::NONTERMINAL)) {
      set_tabulated(i->second);
    }
  }
}


void Grammar::init_in_out() {
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    i->second->reset_in_out();
  }
  axiom->init_in_out(Runtime::Poly(1));
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    i->second->init_in_out();
  }
}


Runtime::Poly Grammar::asm_opt_runtime() {
  hashtable<std::string, Symbol::NT*> temp = tabulated;
  set_all_tabulated();
  Runtime::Poly r = runtime();

  clear_tabulated();
  Runtime::Poly s = runtime();
  set_tabulated(temp);
  return r > s ? s : r;
}


size_t Grammar::nt_number() {
  size_t r = 0;
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    if (i->second->is(Symbol::NONTERMINAL)) {
      r++;
    }
  }
  return r;
}


void Grammar::put_table_conf(std::ostream &s) {
  s << '{';
  for (hashtable<std::string, Symbol::NT*>::iterator i = tabulated.begin();
       i != tabulated.end(); ++i) {
    assert(i->second->is_tabulated());
    i->second->put_table_conf(s);
    s << ", ";
  }
  s << '}' << " #" << tabulated.size() << " of " << nt_number() << ' ';
#ifndef NDEBUG
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    if (i->second->is_tabulated()) {
      hashtable<std::string, Symbol::NT*>::iterator a = tabulated.find(
        *i->second->name);
      if (a == tabulated.end()) {
        // user might not have considered outside NTs to be tabulated in
        // his/her manual annotated table-design
        if (i->second->is_partof_outside == false) {
          assert(false);
        }
      }
    }
  }
#endif
}


void Grammar::approx_table_conf(bool opt_const, unsigned int const_div) {
  clear_tabulated();
  init_in_out();
  Runtime::Asm::Poly opt;
  Runtime::Poly t = asm_opt_runtime();
  opt = t;
  uint32_t const_factor = t[t.degree()];
  std::vector<Symbol::NT*> nts(nt_list.size());
  std::copy(nt_list.begin(), nt_list.end(), nts.begin());
  std::sort(nts.begin(), nts.end(), Pointer_Cmp<Symbol::NT>());
  Runtime::Poly r = runtime();
  for (std::vector<Symbol::NT*>::reverse_iterator i = nts.rbegin();
       i != nts.rend(); ++i) {
    if (opt == r) {
      if (!opt_const) {
        break;
      }
      uint32_t c = r[r.degree()];
      if (c <= const_factor + const_factor / const_div) {
        break;
      }
    }
    if (Log::instance()->is_debug()) {
      std::cerr << "## " << *(*i)->name << std::endl;
    }
    set_tabulated(*i);
    r = runtime();
  }
}


void Grammar::init_self_rec() {
  for (std::list<Symbol::NT*>::iterator i = nt_list.begin();
       i != nt_list.end(); ++i) {
    (*i)->init_self_rec();
    for (std::list<Symbol::NT*>::iterator i = nt_list.begin();
         i != nt_list.end(); ++i) {
      (*i)->active = false;
    }
  }
}


bool Grammar::check_signature(Signature_Base &s) {
  bool r = true;
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    bool b = i->second->insert_types(s);
    r = r && b;
  }
  if (!r) {
    return false;
  }
  ::Type::Status x = ::Type::READY;
  int z = 0;
  do {
    x = ::Type::READY;
    if (Log::instance()->is_debug()) {
      std::cerr << "Iteration: " << z << std::endl;
    }
    for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
         i != NTs.end(); ++i) {
      ::Type::Status b = i->second->infer_missing_types();
      x = std::max(x, b);
    }
    if (Log::instance()->is_debug()) {
      print_type(std::cerr);
    }
    z++;
    assert(z < 5);
  } while (x == ::Type::RUNNING);
  if (x == ::Type::ERROR) {
    return false;
  }
  return true;
}


void Grammar::print_type(std::ostream &s) {
  s << "Grammar " << *name << " types:" << std::endl;
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    i->second->print_type(s);
    s << std::endl;
  }
  s << std::endl;
}


void Grammar::eliminate_lists() {
  unsigned int z = 0;
  bool r = false;
  do {
    r = false;
    for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
         i != NTs.end(); ++i) {
      bool b = i->second->eliminate_lists();
      r = r || b;
    }
    if (Log::instance()->is_debug()) {
      std::cerr << "List elimination iteration: " << z <<
        std::endl << std::endl;
      print_type(std::cerr);
    }
    z++;
    assert(z < NTs.size() + 10);
  } while (r);
}


#include "fn_arg.hh"


struct Reset_Types : public Visitor {
  void visit(Symbol::NT &s) {
    s.reset_types();
  }
  void visit(Alt::Base &a) {
    a.reset_types();
  }
  void visit(Fn_Arg::Base &f) {
    f.reset_types();
  }
};


void Grammar::reset_types() {
  Reset_Types v;
  traverse(v);
}


void Grammar::init_list_sizes() {
  List_Visitor list_visitor;
  unsigned int z = 0;
  bool r = false;
  bool first = true;
  do {
    r = false;
    for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
         i != NTs.end(); ++i) {
      bool b = i->second->init_list_sizes();
      r = r || b;
    }
    if (Log::instance()->is_debug()) {
      std::cerr << std::endl << std::endl  <<
        "Const list annotation iteration: " << z << std::endl << std::endl;
      traverse(list_visitor);
      std::cerr << std::endl;
    }
    if (!r && first) {
      r = true;
      first = false;
    }
    z++;
    assert(z < NTs.size() + 5);
  } while (r);
  List_Size_Terminate lst;
  traverse(lst);

  if (Log::instance()->is_debug()) {
    std::cerr << '\n' << '\n' << "Const list post: " << z << '\n' << '\n';
    traverse(list_visitor);
    std::cerr << '\n';
  }
}


void Grammar::traverse(Visitor &v) {
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    i->second->traverse(v);
  }
}


void Grammar::inline_nts() {
  Inline_Nts inliner(this);
  traverse(inliner);
  if (Log::instance()->is_debug()) {
    std::cerr << std::endl << "Grammar after inlining:" << std::endl;
    print_type(std::cerr);
    std::cerr << std::endl;
  }
}


struct Clear_Loops : public Visitor {
  void visit_begin(Alt::Simple &a) {
    a.reset();
  }
};


void Grammar::init_indices() {
  Clear_Loops cl;
  traverse(cl);
  for (size_t track = 0; track < axiom->tracks(); ++track) {
    // variable access for all possible boundaries
    std::ostringstream i, j, lm, rm;
    i << "t_" << track << "_i";
    j << "t_" << track << "_j";
    lm << "t_" << track << "_left_most";
    rm << "t_" << track << "_right_most";
    Expr::Vacc *left = new Expr::Vacc(new std::string(i.str()));
    Expr::Vacc *right = new Expr::Vacc(new std::string(j.str()));
    Expr::Vacc *left_most = new Expr::Vacc(new std::string(lm.str()));
    Expr::Vacc *right_most = new Expr::Vacc(new std::string(rm.str()));


    for (std::list<Symbol::NT*>::iterator i = nt_list.begin();
         i != nt_list.end(); ++i) {
      // remove moving boundaries whenever possible
      size_t idx = (*i)->tracks() == 1 ? 0 : track;
      const Table &table = (*i)->tables()[idx];
      Expr::Vacc *l = table.delete_left_index() ? left_most : left;
      Expr::Vacc *r = table.delete_right_index() ? right_most : right;
      if ((*i)->tracks() == 1 && (*i)->track_pos() != track) {
        continue;
      }

      unsigned k = 0;

      // built up loops and boundaries to loop over inductively
      (*i)->init_indices(l, r, k, idx);
    }
  }
}


void Grammar::print_indices() {
  Index_Visitor iv;
  traverse(iv);
}


void Grammar::init_guards() {
  Code::Mode mode;
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>(i->second);
    if (nt) {
      nt->init_guards(mode);
    }
  }
}


void Grammar::print_guards() {
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>(i->second);
    if (nt) {
      nt->put_guards(std::cerr);
    }
  }
}


void Grammar::init_decls() {
  Init_Decls id;
  traverse(id);
}


void Grammar::init_decls(const std::string &prefix) {
  Init_Decls id(prefix);
  traverse(id);
}


void Grammar::codegen(AST &ast) {
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>(i->second);
    if (nt) {
      nt->codegen(ast);
    }
  }
}


void Grammar::print_code(Printer::Base &out) {
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>(i->second);
    if (nt) {
      std::list<Fn_Def*> &l = nt->code_list();
      for (std::list<Fn_Def*>::iterator i = l.begin(); i != l.end(); ++i) {
        out << **i;
      }
      out << endl;
    }
  }
}


void Grammar::print_dot(std::ostream &out) {
  out << "digraph G {\n";
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    i->second->print_dot_edge(out);
  }
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    i->second->print_dot_node(out);
  }
  out << "}\n";
}


void Grammar::dep_analysis() {
  Dep_Analysis d(NTs);
  d.sort();
  if (Log::instance()->is_debug()) {
    Log::o() << "\n\nNT Dependencies:\n";
    for (Dep_Analysis::iterator i = d.begin(); i!= d.end(); ++i) {
      assert(*i);
      assert((*i)->name);
      Log::o() << *(*i)->name << '\n';
    }
    Log::o() << '\n';
  }
  ordering = d.result();
}


void Grammar::remove(Symbol::NT *x) {
  [[maybe_unused]] size_t t = nt_list.size();
  nt_list.remove(x);
  assert(t-1 == nt_list.size());
  std::string nt(*x->name);
  assert(NTs.find(nt) != NTs.end());
  assert(NTs.find(nt)->second != axiom);
  NTs.erase(nt);
  tabulated.erase(nt);
}

void Grammar::resolve_blocks() {
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    (*i).second->resolve_blocks();
  }
}


void Grammar::flag_inside_terminal_ends() {
  // defines the visitor used a few lines later
  // Traverses grammar and flags non-terminals that NEITHER link to other
  // non-terminals NOR apply algebra functions, e.g. times = CHAR('*')
  // These NTs cannot translated into outside versions.
  struct Find_Inside_Ends : public Visitor {
    void visit_end(Alt::Link &alt) {
      alt.inside_end = alt.nt == NULL;
    }
    void visit_end(Alt::Simple &alt) {
      for (std::list<Fn_Arg::Base*>::iterator i = alt.args.begin();
           i != alt.args.end(); ++i) {
        if (dynamic_cast<Fn_Arg::Alt*>(*i)) {
          alt.inside_end = false;
          return;
        }
      }
      alt.inside_end = true;
    }
    void visit_end(Symbol::NT &nt) {
      for (std::list<Alt::Base*>::iterator i = nt.alts.begin();
           i != nt.alts.end(); ++i) {
        if ((*i)->inside_end == false) {
          nt.inside_end = false;
          return;
        }
      }
      nt.inside_end = true;
    }
  };

  // create a visitor and traverse the
  Find_Inside_Ends visitor;
  traverse(visitor);
}

void Grammar::inject_outside_nts() {
  std::vector<std::string> outside_nt_list;
  Grammar::inject_outside_nts(outside_nt_list);
}
void Grammar::inject_outside_nts(std::vector<std::string> outside_nt_list) {
  std::string OUTSIDE_NT_PREFIX = "outside_";
  hashtable<std::string, Symbol::Base*> outside_NTs;
  std::set<Symbol::NT*> outside_nts = std::set<Symbol::NT*>();

  // 0) check if all user provided NTs for cmd flag are actually in the grammar
  std::list<std::string> *warn_missing_nts = new std::list<std::string>();
  for (std::vector<std::string>::iterator i = outside_nt_list.begin();
       i != outside_nt_list.end(); ++i) {
    // we don't need to check of non-terminals are in inside grammar, if user
    // requests ALL non-terminals
    if ((*i).compare(std::string("ALL")) == 0) {
      break;
    }
    if (this->NTs.find(*i) == this->NTs.end()) {
      warn_missing_nts->push_back(*i);
    }
  }
  if (warn_missing_nts->size() > 0) {
    std::string msg = std::string(
      "You requested outside grammar generation and reporting of results for "
      "outside versions of non-terminal(s) ");
    for (std::list<std::string>::iterator i = warn_missing_nts->begin();
         i != warn_missing_nts->end(); ++i) {
      msg += "'" + *i + "'";
      if (next(i) != warn_missing_nts->end()) {
        msg += ", ";
      }
    }
    msg += ", which do(es) NOT exist in your grammar!";
    throw LogError(msg);
  }

  // 0a) semantic check: grammar must be able to parse the empty input in
  // order to provide recursion base for outside candidates
  Yield::Multi ys = this->axiom->multi_ys();
  for (std::vector<Yield::Size>::iterator i = ys.begin(); i != ys.end(); ++i) {
    if ((*i).low() > 0) {
      std::ostringstream msg;
      msg << "Minimal yield size of your grammar is ";
      (*i).low().put(msg);
      msg << ", i.e. it cannot parse the empty input\nstring ''."
          <<  " For outside grammar generation, this means you are lacking a\n"
          << "recursion basis which will result in empty results for\n"
          << "outside candidates!";
      Log::instance()->warning(msg.str());
      break;
    }
  }

  // traverse grammar and flag NTs that only use terminals on their rhs
  this->flag_inside_terminal_ends();

  // 1) collect non-terminals used in rhs of productions together with their
  // calling lhs non-terminal we thus exclude e.g. non-terminal foo in
  // production "foo = BASE;"
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>(i->second);
    if ((nt) && (!nt->inside_end)) {
      // skip terminals and NTs that only point to terminals
      for (std::list<Alt::Base*>::iterator alt = nt->alts.begin();
           alt != nt->alts.end(); ++alt) {
        std::list<Symbol::NT*> *rhs_nts = new std::list<Symbol::NT*>();
        (*alt)->get_nonterminals(rhs_nts);
        if (rhs_nts->size() > 0) {
          // if rhs uses at least one non-terminal, both (= calling lhs
          // non-terminal and all rhs non-terminal(s)) will be added as
          // novel outside non-terminals
          outside_nts.insert(nt);
          for (std::list<Symbol::NT*>::const_iterator rhs_nt = rhs_nts->begin();
               rhs_nt != rhs_nts->end(); ++rhs_nt) {
            if (!(*rhs_nt)->inside_end) {
              outside_nts.insert(*rhs_nt);
            }
          }
        }
      }
    }
  }

  // 2) create a novel outside non-terminal for each collected non-terminal
  // in the inside grammar add both (new outside and user provided inside)
  // non-terminal **names** as keys to the hash, pointing to the very same
  // novel outside non-terminal.
  if (outside_nts.size() == 0) {
    // the grammar does not have any non-terminals on the rhs of productions.
    // Thus, no new NTs have to be injected
    return;
  }
  for (std::set<Symbol::NT*>::iterator nt = outside_nts.begin();
       nt != outside_nts.end(); ++nt) {
    // use inside NT as template ...
    Symbol::NT *inside_nt = dynamic_cast<Symbol::NT*>(
      this->NTs.find(*(*nt)->name)->second);

    // skip inside NTs that only point to terminals like
    // times = CHAR('*')
    if (inside_nt->inside_end) {
      // inside terminal is NOT transformed into outside NT
      continue;
    }

    // and clone for outside version
    Symbol::NT *outside_nt = inside_nt->clone(inside_nt->track_pos());
    // remove all existing alternative production rules
    outside_nt->alts.clear();
    // and correct NT name, to now point to outside version
    outside_nt->name = new std::string(OUTSIDE_NT_PREFIX + (*(*nt)->name));

    // flag this new non-terminal as being part of the outside rules
    outside_nt->is_partof_outside = true;

    outside_NTs[OUTSIDE_NT_PREFIX + (*(*nt)->name)] = outside_nt;
    outside_NTs[(*(*nt)->name)] = outside_nt;
  }

  // flag inside axiom
  this->axiom->is_inside_axiom = true;

  // 3) create copy of inside alternative, replace called non-terminal with
  // calling outside version and append to called outside version
  std::set<Symbol::NT*> *axiom_candidates = new std::set<Symbol::NT*>();
  for (hashtable<std::string, Symbol::Base*>::iterator i = NTs.begin();
       i != NTs.end(); ++i) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>(i->second);
    // skip terminals and NTs that end in terminals
    if ((nt) && (!nt->inside_end)) {
      // don't directly operate on given inside NTs,
      // but first resolve Alt::Block use for outside rules
      Symbol::NT *nt_resolved = nt->clone(nt->track_pos());
      nt_resolved->resolve_blocks();
      for (std::list<Alt::Base*>::iterator alt = nt_resolved->alts.begin();
           alt != nt_resolved->alts.end(); ++alt) {
        std::list<Symbol::NT*> *rhs_nts = new std::list<Symbol::NT*>();
        (*alt)->get_nonterminals(rhs_nts);
        if (rhs_nts->size() > 0) {
          hashtable<std::string, unsigned int> skip_occurences;
          for (std::list<Symbol::NT*>::iterator it_nt = rhs_nts->begin();
               it_nt != rhs_nts->end(); ++it_nt) {
            skip_occurences[*((*it_nt)->name)] = 0;
          }
          // unsigned int skip_occurences = 0;
          for (std::list<Symbol::NT*>::iterator it_nt = rhs_nts->begin();
               it_nt != rhs_nts->end(); ++it_nt) {
            // skip inside NTs that only point to terminals like
            // times = CHAR('*')
            if ((*it_nt)->inside_end) {
              continue;
            }

            // create a copy (=clone) of the inside alternative
            Alt::Base *outside_alt = (*alt)->clone();
            // replace one of the inside non-terminals with the outside
            // version of the calling non-terminal
            outside_alt->replace_nonterminal(
              *it_nt, dynamic_cast<Symbol::NT*>(
                outside_NTs.find(i->first)->second), skip_occurences);
            // flag outside_alt as being part of an automatically generated
            // outside production rule
            outside_alt->set_partof_outside(true);

            outside_alt->m_ys_inside = outside_alt->multi_ys();
            // append copied+replaced alternative to the list of alternatives
            // for the outside version of the called non-terminal
            (dynamic_cast<Symbol::NT*>(
              outside_NTs.find(*((*it_nt)->name))->second))->alts.push_back(
                outside_alt);
            skip_occurences[*((*it_nt)->name)]++;
          }
        } else {
//          // e.g. struct = nil(LOC) has no non-terminal on the rhs, but
            // should become an alternative for outside version of struct
//          Alt::Base *outside_alt = (*alt)->clone();
//          // flag outside_alt as being part of an automatically generated
            // outside production rule
//          outside_alt->set_partof_outside(true);
//          (dynamic_cast<Symbol::NT*>(outside_NTs.find(*(nt->name))->second))
            // ->alts.push_back(outside_alt);

          // if production rule does not have any non-terminals on the right
          // hand side, the calling non-terminal is a candidate for being
          // called from the axiom for outside
          // e.g. hairpin = hl(BASE, REGION, BASE) --> outside_hairin
          // get's added
          axiom_candidates->insert(dynamic_cast<Symbol::NT*>(
            outside_NTs.find(*(nt->name))->second));
        }
      }
    }
  }
  Alt::Link *link = new Alt::Link(this->axiom_name, this->axiom_loc);
  link->nt = dynamic_cast<Symbol::Base*>(this->axiom);
  Filter *f = new Filter(new std::string("complete_track"), Loc());
  f->type = Filter::WITH;
  std::list<Filter*> *comptracks = new std::list<Filter*>();
  for (unsigned int track = 0; track < this->axiom->tracks(); ++track) {
    comptracks->push_back(f);
  }
  link->set_tracks(this->axiom->tracks(), this->axiom->track_pos());
  link->init_multi_ys();
  link->is_outside_inside_transition = true;
  if (link->nt->tracks() == 1) {
    link->filters.push_back(f);
  } else {
    link->add_multitrack_filter(*comptracks, f->type, Loc());
  }
  link->top_level = Bool(true);
  dynamic_cast<Symbol::NT*>(outside_NTs.find(
    *this->axiom_name)->second)->alts.push_back(link);

  // 4) add novel outside NTs to grammar
  for (std::pair<std::string, Symbol::Base*> nt : outside_NTs) {
    // only for NTs that are not already part of the grammar
    if (this->NTs.find(nt.first) == this->NTs.end()) {
      Symbol::NT *nt_nt = dynamic_cast<Symbol::NT*>(nt.second);
      /* calling NT might never occur on rhs of productions, e.g. "start" in:
       * grammar gra_nodangle uses sig_foldrna(axiom = start) {
       *   start = incl(struct) # h;
       *   struct = sadd(BASE, struct) | nil(LOC) # h; }
       * the outside version of such non terminals shall point to the inside 
       * axiom
       */
      if (nt_nt->alts.size() == 0) {
        Alt::Link *link = new Alt::Link(this->axiom_name, this->axiom_loc);
        link->nt = dynamic_cast<Symbol::Base*>(this->axiom);
        nt_nt->alts.push_back(link);
      }
      this->NTs.insert(nt);
      this->nt_list.push_back(nt_nt);
    }
  }

  // 5) inside production rules that have NO non-terminals on their right hand
  //    sides must be those that parse the final sub-words of the input.
  //    Therefore, they must be the smallest start-points for outside
  //    construction, i.e. the axiom should point to them. These non-terminals
  //    have been collected in step 3.
  if (axiom_candidates->size() == 1) {
    // if there is only one inside non-terminal to be accessed from the axiom
    // we can directly define its outside version as the axiom
    this->axiom_name = (*axiom_candidates->begin())->name;
    this->init_axiom();
  } else {
    // otherwise, we need to add another left hand side non-terminal
    // which points to multiple non-terminals WITHOUT making a choice
    std::string *nt_axiom_name = new std::string("outside_axioms");
    Symbol::NT *nt_axiom = new Symbol::NT(nt_axiom_name, Loc());
    // carry over tracks from original inside axiom
    nt_axiom->set_tracks(this->axiom->tracks(), this->axiom->track_pos());
    nt_axiom->setup_multi_ys();

    for (std::set<Symbol::NT*>::iterator i = axiom_candidates->begin();
         i != axiom_candidates->end(); ++i) {
      Alt::Link *link = new Alt::Link((*i)->name, Loc());
      link->nt = dynamic_cast<Symbol::NT*>(
        outside_NTs.find(*(*i)->name)->second);
      link->set_tracks((*i)->tracks(), (*i)->track_pos());
      link->init_multi_ys();
      nt_axiom->alts.push_back(link);
    }
    // add new lhs non-terminal to grammar
    this->NTs.insert(std::make_pair(
      *nt_axiom_name, dynamic_cast<Symbol::Base*>(nt_axiom)));
    this->nt_list.push_back(nt_axiom);

    this->axiom_name = nt_axiom_name;
    this->init_axiom();
  }
  // re-run parts of "check_semantics" to properly initialize novel non-
  // terminals and links to non-terminals, but explicitly do NOT
  // re-run yield size analysis since it does not respect outside
  // situations.
  this->init_calls();
  this->init_in_out();
  this->init_table_dims();

  // mark the modified grammar as an outside one
  this->outside = true;

  Log::instance()->verboseMessage(
    "Grammar has been modified into an outside version.");
}

unsigned int Grammar::to_dot(unsigned int *nodeID, std::ostream &out,
        int plot_grammar) {
  int start_node;
  unsigned int i = 1;
  out << "digraph " << *this->name << " {\n";
  out << "compound = True;\n";
  out << "newrank = True;\n";
  out << "ordering = out;\n";
  for (std::list<Symbol::NT*>::const_iterator nt = this->nt_list.begin();
       nt != this->nt_list.end(); ++nt, ++i) {
    if (nt != this->nt_list.begin()) {
      // except for the first unit, we add an invisible node (anchor) and
      // invisible edges from the anchor to the lhs non-terminal node of the
      // next unit to enable vertical alignment
      out << "node_" << start_node << " -> node_" << std::to_string(*nodeID)
          << " [ style=invis ];\n";
    }
    // let's organize all nodes of a lhs non-terminal in one subgraph cluster
    // such that it can be plotted as one unit and these units are
    // vertically stacked, while elements in the unit are horizontally aligned
    out << "subgraph cluster_" << i << " {\n";
    start_node = (*nt)->to_dot(nodeID, out, false, this->axiom, plot_grammar);
    out << "}\n";
  }
  out << "}\n";
  return ((unsigned int)*nodeID);
}
