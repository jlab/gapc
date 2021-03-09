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

#include <sstream>
#include <algorithm>

#include "tablegen.hh"
#include "expr.hh"
#include "expr/vacc.hh"
#include "expr/mod.hh"
#include "statement.hh"
#include "type.hh"
#include "statement/fn_call.hh"

typedef std::vector<Table>::const_iterator itr;


// FIXME Minus/Plus/Times optimizations like 1 * foo -> foo

// generate for each track left_most, right_most, ns class globaly
// what left_most, right_most, needed?
//
// size = offset(n) + 1
// bzw. size = (offset(t_0_n)+1) * (offset(t_1_n)+1) ...


Tablegen::Tablegen()
  : type(0), size(0), window_size(0), off(0), ret_zero(0),
  cond(0),
  dtype(0),
  cyk_(false),
  window_mode_(false) {
  // FIXME?
  type = new ::Type::Size();

  ret_zero = new Statement::Return(new Expr::Vacc(new std::string("zero")));
}


void Tablegen::head(Expr::Base *&i, Expr::Base *&j, Expr::Base *&n,
    const Table &table, size_t track) {
  std::ostringstream si, sj, sn, slm, srm;
  si << "t_" << track << "_i";
  sj << "t_" << track << "_j";
  sn << "t_" << track << "_n";
  slm << "t_" << track << "_left_most";
  srm << "t_" << track << "_right_most";

  n = new Expr::Vacc(new std::string(sn.str()));
  ns.push_back(new Statement::Var_Decl(type, new std::string(sn.str())));
  // XXX ns.pushback left/right most, instead of Cpp::print_most_decl ?

  Statement::Var_Decl *iv =
    new Statement::Var_Decl(type, new std::string(si.str()));
  i = new Expr::Vacc(*iv);
  if (table.delete_left_index()) {
    iv->rhs = new Expr::Const(0);
    code.push_back(iv);
  } else {
    paras.push_back(iv);
  }

  Statement::Var_Decl *jv =
    new Statement::Var_Decl(type, new std::string(sj.str()));
  j = new Expr::Vacc(*jv);
  if (table.delete_right_index()) {
    jv->rhs = n;
    code.push_back(jv);
  } else {
    paras.push_back(jv);
  }

  Statement::Fn_Call *a1 = new Statement::Fn_Call(Statement::Fn_Call::ASSERT);
  a1->add_arg(new Expr::Less_Eq(i, j));
  code.push_back(a1);
  Statement::Fn_Call *a2 = new Statement::Fn_Call(Statement::Fn_Call::ASSERT);
  a2->add_arg(new Expr::Less_Eq(j, n));
  code.push_back(a2);

  if (window_mode_) {
    Statement::Var_Assign *a = new Statement::Var_Assign(*iv,
        new Expr::Mod(i,
          new Expr::Plus(new Expr::Vacc(new std::string("wsize")),
              new Expr::Const(1))));
    window_code.push_back(a);
    Statement::Var_Assign *b = new Statement::Var_Assign(*jv,
        new Expr::Mod(j,
          new Expr::Plus(new Expr::Vacc(new std::string("wsize")),
              new Expr::Const(1))));
    window_code.push_back(b);
    Statement::Fn_Call *f = new Statement::Fn_Call("swap");
    f->add_arg(i);
    f->add_arg(j);
    Statement::If *x = new Statement::If(
        new Expr::Greater(i, j), f);
    window_code.push_back(x);
  }
}

void Tablegen::offset_const(titr track, itr first, const itr &end,
    Expr::Base *dim, Expr::Base *access) {
  const Table &table = *first;
  const Yield::Size &left = table.left_rest();
  const Yield::Size &right = table.right_rest();

  Expr::Base *i, *j, *n;
  head(i, j, n, table, *track);

  std::list<Expr::Base*> ors;
  ors.push_back(new Expr::Greater(i, new Expr::Const(left.high())) );
  ors.push_back(new Expr::Less(j,
          new Expr::Minus(n, new Expr::Const(right.high()))) );
  if (left.low() > 0)
    ors.push_back(new Expr::Less(i, new Expr::Const(left.low())));
  if (right.high() < Yield::UP)
    ors.push_back(new Expr::Greater(j,
          new Expr::Minus(n, new Expr::Const(right.high()))) );
  Expr::Base *cond = Expr::seq_to_tree<Expr::Base, Expr::Or>(
     ors.begin(), ors.end());

  Statement::If *guard = new Statement::If(cond, ret_zero);
  code.push_back(guard);

  std::ostringstream srj;
  srj << "t_" << *track << "_real_j";

  Expr::Base *real_jm = (right.low() == 0) ? new Expr::Minus(n, j)
    : new Expr::Minus(n, new Expr::Plus(j, new Expr::Const(right.low())));
  Statement::Var_Decl *real_jv = new Statement::Var_Decl(type,
      new std::string(srj.str()), real_jm);
  code.push_back(real_jv);
  Expr::Vacc *real_j = new Expr::Vacc(*real_jv);

  std::ostringstream sri;
  sri << "t_" << *track << "_real_i";

  Expr::Base *real_im = (left.low() == 0) ? i
    : new Expr::Minus(i, new Expr::Const(left.low()));
  Statement::Var_Decl *real_iv = new Statement::Var_Decl(type,
      new std::string(sri.str()), real_im);
  code.push_back(real_iv);
  Expr::Vacc *real_i = new Expr::Vacc(*real_iv);

//  access = new Expr::Plus(access, new Expr::Times(dim, new Expr::Plus(real_i,
//      new Expr::Times(real_j, new Expr::Const(left)))));

  Expr::Base *d = new Expr::Times(new Expr::Const(left),
      new Expr::Const(right));
  dim = new Expr::Times(dim, d);

  offset(++track, ++first, end, dim, access);
}

void Tablegen::offset_left_lin(titr track, itr first, const itr &end,
    Expr::Base *dim, Expr::Base *access ) {
  const Table &table = *first;
  const Yield::Size &left = table.left_rest();

  Expr::Base *i, *j, *n;
  head(i, j, n, table, *track);

  Statement::If *guard = new Statement::If(
      new Expr::Greater(i, new Expr::Const(left.high())),
      ret_zero);
  code.push_back(guard);

//  access = new Expr::Plus(access, new Expr::Times(dim,
//        new Expr::Plus(i, new Expr::Times(j, new Expr::Const(left)))));

  Expr::Base *d = new Expr::Times(new Expr::Const(left),
      new Expr::Plus(n, new Expr::Const(1)));
  dim = new Expr::Times(dim, d);

  offset(++track, ++first, end, dim, access);
}

void Tablegen::offset_right_lin(titr track, itr first, const itr &end,
    Expr::Base *dim, Expr::Base *access) {
  const Table &table = *first;
  const Yield::Size &right = table.right_rest();


  Expr::Base *i, *j, *n;
  head(i, j, n, table, *track);

  Expr::Base *cond = new Expr::Less(j, new Expr::Minus(
    n, new Expr::Const(right.high())));
  if (right.low() != 0) {
    cond = new Expr::Or(cond, new Expr::Greater(j, new Expr::Minus(
      n, new Expr::Const(right.low()))));
  }
  Statement::If *guard = new Statement::If(
      cond,
      ret_zero);
  code.push_back(guard);

  std::ostringstream srj;
  srj << "t_" << *track << "_real_j";

  Expr::Base  *minus = 0;
  if (right.low() == 0)
    minus = new Expr::Minus(n, j);
  else
    minus = new Expr::Minus(n, new Expr::Plus(j, new Expr::Const(right.low())));
  Statement::Var_Decl *real_jv = new Statement::Var_Decl(type,
      new std::string(srj.str()),
      minus);
  code.push_back(real_jv);
  Expr::Vacc *real_j = new Expr::Vacc(*real_jv);

//  access = new Expr::Plus(access, new Expr::Times(dim,
//        new Expr::Plus(i,
//          new Expr::Times(real_j, new Expr::Plus(n, new Expr::Const(1))))));

  Expr::Base *d = new Expr::Times(new Expr::Plus(n, new Expr::Const(1)),
      new Expr::Const(right));
  dim = new Expr::Times(dim, d);

  offset(++track, ++first, end, dim, access);
}

void Tablegen::offset_quad(titr track, itr first, const itr &end,
    Expr::Base *dim, Expr::Base *access) {
  const Table &table = *first;

  Expr::Base *i, *j, *n;
  head(i, j, n, table, *track);

//  access = new Expr::Plus(access, new Expr::Times(dim,
//    new Expr::Plus(
//      new Expr::Div(new Expr::Times(
//        j, new Expr::Plus(j, new Expr::Const(1))), new Expr::Const(2)), i)));

  Expr::Base *d = new Expr::Plus(new Expr::Plus(new Expr::Div(
      new Expr::Times(n, new Expr::Plus(n, new Expr::Const(1))),
      new Expr::Const(2)),
      n), new Expr::Const(1));
  dim = new Expr::Times(dim, d);

  if (window_mode_) {
    Expr::Base *wsize = new Expr::Vacc(new std::string("wsize"));
    window_size = new Expr::Plus(new Expr::Plus(new Expr::Div(
      new Expr::Times(wsize,
        new Expr::Plus(wsize, new Expr::Const(1))),
      new Expr::Const(2)),
      wsize), new Expr::Const(1));
  }

  offset(++track, ++first, end, dim, access);
}


void Tablegen::offset(titr track, itr first, const itr &end,
    Expr::Base *dim, Expr::Base *access) {
  if (first == end) {
    size = dim;
    off = new std::list<Expr::Base*>();
    off->push_back(access);
    //off = access;
    return;
  }

  if (window_mode_) {
    // FIXME do more specialized window mode codegen for lin/const tables ...
    offset_quad(track, first, end, dim, access);
    return;
  }

  const Table &table = *first;
  switch (table.type()) {
    case Table::NONE : assert(0); std::abort(); break;
    case Table::CONSTANT :
                offset_const(track, first, end, dim, access);
                break;
    case Table::LINEAR :
                if (table.sticky() == Table::LEFT)
                  offset_left_lin(track, first, end, dim, access);
                else
                  offset_right_lin(track, first, end, dim, access);
                break;
    case Table::QUADRATIC :
                offset_quad(track, first, end, dim, access);
                break;
  }
}


struct ParaCmp {
  bool operator()(const Statement::Var_Decl *a,
                  const Statement::Var_Decl *b) const {
    return *a->name < *b->name;
  }
};

void Tablegen::offset(size_t track_pos, itr f, const itr &e) {
  window_code.clear();
  code.clear();
  paras.clear();
  ns.clear();
  size = 0;

  std::vector<size_t> tracks;
  for (size_t i = track_pos; i < track_pos + size_t(e-f); ++i)
    // for (size_t i = 0; i < size_t(e-f); ++i)
    tracks.push_back(i);
  std::reverse(tracks.begin(), tracks.end());

  std::vector<Table> rev(e-f);
  std::reverse_copy(f, e, rev.begin());
  itr first(rev.begin());
  itr end(rev.end());

  Expr::Base *dim = new Expr::Const(1);
  Expr::Base *access = new Expr::Const(int(0));
  offset(tracks.begin(), first, end, dim, access);

  std::vector<Statement::Var_Decl*> p;
  p.insert(p.end(), paras.begin(), paras.end());
  std::sort(p.begin(), p.end(), ParaCmp());
  paras.clear();
  paras.insert(paras.end(), p.begin(), p.end());

  //  Statement::Return *ret = new Statement::Return(new Expr::Vacc(
  //        new Var_Acc::Array(new Var_Acc::Plain(new std::string("array")), off)));
  //  c.push_back(ret);

  std::list<Expr::Base*> *indexer = new std::list<Expr::Base*>();
  for (std::list<Statement::Var_Decl*>::iterator it = paras.begin(); it != paras.end(); ++it) {
	  indexer->push_back(new Expr::Vacc((*it)->name));
    //std::cerr << (*(*it)->name) << "\n";
  }
  *off = *indexer;

  std::reverse(ns.begin(), ns.end());
}

#include "const.hh"


#include "statement/table_decl.hh"
#include "symbol.hh"

Statement::Table_Decl *Tablegen::create(Symbol::NT &nt,
    std::string *name, bool cyk) {
  cyk_ = cyk;
  std::list<Expr::Base*> ors;
  nt.gen_ys_guards(ors);
  if (!ors.empty())
    cond  = Expr::seq_to_tree<Expr::Base, Expr::Or>
      (ors.begin(), ors.end());

  // no clone, for ast->optimize_classify list tag optimization ...
  // dtype = nt.data_type()->clone();
  dtype = nt.data_type();

  ret_zero = new Statement::Return(new Expr::Const(new Const::Bool(true)));
  offset(nt.track_pos(), nt.tables().begin(), nt.tables().end());
  Fn_Def *fn_is_tab = gen_is_tab();
  Fn_Def *fn_untab = gen_untab();

  ret_zero = new Statement::Return();
  offset(nt.track_pos(), nt.tables().begin(), nt.tables().end());
  Fn_Def *fn_tab = gen_tab();

  ret_zero = new Statement::Return(new Expr::Vacc(new std::string("zero")));
  offset(nt.track_pos(), nt.tables().begin(), nt.tables().end());
  Fn_Def *fn_get_tab = gen_get_tab();

  Fn_Def *fn_size = gen_size();
  Fn_Def *fn_init = gen_init(nt);

  Statement::Table_Decl *td = new Statement::Table_Decl(nt, dtype, name, cyk,
      fn_is_tab, fn_tab, fn_get_tab, fn_size, fn_init,
      ns);
  td->set_fn_untab(fn_untab);
  return td;
}

#include "fn_def.hh"
#include "var_acc.hh"

Fn_Def *Tablegen::gen_is_tab() {
  std::list<Statement::Base*> c;

//  // construct test for np.isnan
//  Expr::Fn_Call *indexer = new Expr::Fn_Call(new std::string("self.get"));
//  for (std::list<Statement::Var_Decl*>::iterator it = paras.begin(); it != paras.end(); ++it) {
//    indexer->add_arg((*it)->name);
//  }
//  Statement::Var_Decl *cell_value = new Statement::Var_Decl(new Type::NoneType, new std::string("cell_value"), indexer);
//  c.push_back(cell_value);
//
//  Expr::Fn_Call *isnan = new Expr::Fn_Call(new std::string("~np.isnan"));
//  isnan->add_arg(new Expr::Fn_Call(new std::string("cell_value.numpy")));
//
//  Statement::Return *r = new Statement::Return(isnan);
//  c.push_back(r);

  Statement::Return *ret = new Statement::Return(new Expr::Vacc(new Var_Acc::Array(new Var_Acc::Plain(new std::string("self.tabulated")), off)));
  c.push_back(ret);

  // function signature
  Fn_Def *f = new Fn_Def(new Type::Bool(), new std::string("is_tabulated"));
  //paras.push_front(new Statement::Var_Decl(new Type::NoneType(), new std::string("self")));
  f->add_para(new Type::NoneType(), new std::string("self"));
  f->add_paras(paras);

  f->set_statements(c);
  return f;
}

Fn_Def *Tablegen::gen_untab() {
  Fn_Def *f = new Fn_Def(new Type::RealVoid(), new std::string("un_tabulate"));
  f->add_paras(paras);

  std::list<Statement::Base*> c;

  c.insert(c.end(), code.begin(), code.end());
  c.insert(c.end(), window_code.begin(), window_code.end());

  Statement::Var_Assign *ass = new Statement::Var_Assign(
      new Var_Acc::Array(new Var_Acc::Plain(new std::string("tabulated")), off),
      new Expr::Const(new Const::Bool(false)));
  c.push_back(ass);

  f->set_statements(c);
  return f;
}

Fn_Def *Tablegen::gen_tab() {
  Fn_Def *f = new Fn_Def(new Type::NoneType(), new std::string("set"));
  //paras.push_front(new Statement::Var_Decl(new Type::NoneType(), new std::string("self")));
  f->add_para(new Type::NoneType(), new std::string("self"));
  f->add_paras(paras);
  // FIXME const & in dtype -> see cpp.cc in_fn_head
  f->add_para(dtype, new std::string("e"));

  std::list<Statement::Base*> c;
//
//  c.insert(c.end(), code.begin(), code.end());
//
//  Statement::Fn_Call *ass = new Statement::Fn_Call(Statement::Fn_Call::ASSERT);
//  ass->add_arg(new Expr::Const(0));
//
//  if (cond) {
//    Statement::If *i = new Statement::If(cond, ass);
//    c.push_back(i);
//  }
//
//  if (!cyk_) {
//    Statement::Fn_Call *a = new Statement::Fn_Call(Statement::Fn_Call::ASSERT);
//    Expr::Fn_Call *e = new Expr::Fn_Call(Expr::Fn_Call::IS_TABULATED);
//    for (std::list<Statement::Var_Decl*>::iterator i = paras.begin();
//         i != paras.end(); ++i) {
//      e->add_arg(**i);
//    }
//    a->add_arg(new Expr::Not(e));
//    c.push_back(a);
//  }
//
//  c.insert(c.end(), window_code.begin(), window_code.end());
//
//
//  Statement::Fn_Call *a = new Statement::Fn_Call(Statement::Fn_Call::ASSERT);
//  a->add_arg(new Expr::Less(off->front(), new Expr::Fn_Call(new std::string("size"))));
//  c.push_back(a);

  Statement::Var_Assign *x = new Statement::Var_Assign(
      new Var_Acc::Array(new Var_Acc::Plain(new std::string("self.array")), off),
      new Expr::Vacc(new std::string("e")));
  c.push_back(x);

//  if (!cyk_) {
//    Statement::Var_Assign *y = new Statement::Var_Assign(
//        new Var_Acc::Array(
//          new Var_Acc::Plain(new std::string("tabulated")), off),
//        new Expr::Const(new Const::Bool(true)));
//    c.push_back(y);
//  }

  f->set_statements(c);
  return f;
}

Fn_Def *Tablegen::gen_get_tab() {
  Fn_Def *f = new Fn_Def(new Type::Tensor, new std::string("get"));
  //paras.push_front(new Statement::Var_Decl(new Type::NoneType(), new std::string("self")));
  f->add_para(new Type::NoneType(), new std::string("self"));
  f->add_paras(paras);

  std::list<Statement::Base*> c;

//  if (cond) {
//    Statement::If *i = new Statement::If(cond, ret_zero);
//    code.push_back(i);
//  }
//
//  c.insert(c.end(), code.begin(), code.end());
//  c.insert(c.end(), window_code.begin(), window_code.end());
//
//  if (!cyk_) {
//    Statement::Fn_Call *a = new Statement::Fn_Call(Statement::Fn_Call::ASSERT);
//    a->add_arg(new Var_Acc::Array(
//      new Var_Acc::Plain(new std::string("tabulated")), off));
//    c.push_back(a);
//  }
//
//  Statement::Fn_Call *a = new Statement::Fn_Call(Statement::Fn_Call::ASSERT);
//  a->add_arg(new Expr::Less(off->front(), new Expr::Fn_Call(new std::string("size"))));
//  c.push_back(a);

  Statement::Return *ret = new Statement::Return(new Expr::Vacc(new Var_Acc::Array(new Var_Acc::Plain(new std::string("self.array")), off)));
    c.push_back(ret);

  f->set_statements(c);
  return f;
}

Fn_Def *Tablegen::gen_size() {
  Fn_Def *f = new Fn_Def(type, new std::string("size"));

  std::list<Statement::Base*> c;

  Statement::Return *r = new Statement::Return(
      window_mode_ ? window_size : size);
  c.push_back(r);

  f->set_statements(c);
  return f;
}

//py: def __init__(self, t_0_n_:int, t_1_n_:int):
//py:     self.array = torch.full(size=[t_0_n_, t_1_n_], fill_value=np.nan)
Fn_Def *Tablegen::gen_init(const Symbol::NT &nt) {
	Fn_Def *f = new Fn_Def(type, new std::string("__init__"));
	f->add_para(new Type::NoneType(), new std::string("self"));
	f->add_paras(paras);

	std::list<Statement::Base*> c;

	// initiate Tensor with np.nan values
	Expr::Fn_Call *rhs = new Expr::Fn_Call(new std::string("torch.full"));
	rhs->add_arg(new Var_Acc::Array(new Var_Acc::Plain(new std::string("")), off), new std::string("size"));
	rhs->add_arg(new std::string("np.nan"), new std::string("fill_value"));
	Statement::Var_Decl *tf = new Statement::Var_Decl(new Type::NoneType(), new std::string("self.array"), rhs);
	c.push_back(tf);

	Expr::Fn_Call *rhsTab = new Expr::Fn_Call(new std::string("torch.full"));
	rhsTab->add_arg(new Var_Acc::Array(new Var_Acc::Plain(new std::string("")), off), new std::string("size"));
	rhsTab->add_arg(new std::string("False"), new std::string("fill_value"));
	Statement::Var_Decl *tfTab = new Statement::Var_Decl(new Type::NoneType(), new std::string("self.tabulated"), rhsTab);
	c.push_back(tfTab);

	std::list<Statement::Var_Decl*>::const_iterator pit = paras.begin();
	for (size_t track = nt.track_pos(); (track < nt.track_pos() + nt.tracks()) && (pit != paras.end()); ++track, ++pit) {
		std::string varname;
		varname.append(std::string("self.t_"));
		varname.append(std::to_string(track));
		varname.append(std::string("_left_most"));
		c.push_back(new Statement::Var_Decl(new Type::Size(), varname, new Expr::Const(0)));

		std::string varname2;
		varname2.append(std::string("self.t_"));
		varname2.append(std::to_string(track));
		varname2.append(std::string("_right_most"));
		c.push_back(new Statement::Var_Decl(new Type::Size(), varname2, new Expr::Vacc((*pit)->name)));
	}


	f->set_statements(c);
	return f;
}
