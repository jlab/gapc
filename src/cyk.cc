/* {{{

    This file is part of gapc (GAPC - Grammars, Algebras, Products - Compiler;
      a system to compile algebraic dynamic programming programs)

    Copyright (C) 2011-2023  Stefan Janssen
         email: stefan.m.janssen@gmail.com or stefan.janssen@computational.bio.uni-giessen.de

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

#include "cyk.hh"

std::list<Statement::Base*> *get_tile_computation(const AST &ast, std::string name_maxtilen) {
  Statement::Var_Decl *tile_size = new Statement::Var_Decl(
        new Type::Size(),
        "tile_size",
        new Expr::Const(32));

  std::list<Statement::Base*> *res = new std::list<Statement::Base*>();

  if (!(ast.checkpoint && ast.checkpoint->cyk)) {
    res->push_back(tile_size);
    res->push_back(new Statement::CustomeCode("#ifdef TILE_SIZE"));
    res->push_back(new Statement::Var_Assign(*tile_size, new Expr::Const("TILE_SIZE")));
    res->push_back(new Statement::CustomeCode("#endif"));
  }
  res->push_back(new Statement::Fn_Call(Statement::Fn_Call::ASSERT, *tile_size));
  Statement::Var_Decl *max_tiles = new Statement::Var_Decl(
      new Type::Size(),
      "max_tiles",
      new Expr::Div(ast.grammar()->axiom->right_indices[0]->vacc(), new Expr::Vacc(*tile_size)));
  res->push_back(max_tiles);
  Statement::Var_Decl *max_tiles_n = new Statement::Var_Decl(
      new Type::Int(),
      name_maxtilen,
      new Expr::Times(new Expr::Vacc(*max_tiles), new Expr::Vacc(*tile_size)));
  res->push_back(max_tiles_n);

  return res;
}

std::tuple<Statement::For*, Statement::Var_Decl*> get_for_column(size_t track, Symbol::NT *first_nt, Statement::Var_Decl *input_seq) {
  // create loop variable addressing the DP column (=2nd index)
  // e.g.: for (unsigned int t_0_j = 0; t_0_j < t_0_seq.size(); ++t_0_j) {
  Expr::Base *col_start = new Expr::Const(0);

  Statement::Var_Decl *var_col = new Statement::Var_Decl(
      new Type::Size(),
      first_nt->right_indices.at(track)->vacc(),
      col_start);

  // create end point for loop variable
  Expr::Fn_Call *col_end = new Expr::Fn_Call(new std::string("size"));
  col_end->add_arg(input_seq->name);
  col_end->is_obj = Bool(true);

  // create condition of For loop
  Expr::Less *cond_col = new Expr::Less(new Expr::Vacc(*var_col), col_end);

  Statement::For *loop = new Statement::For(var_col, cond_col);

  Statement::Var_Decl *var_nonloop = var_col->clone();
  var_nonloop->rhs = col_end;

  return std::make_tuple(loop, var_nonloop);
}

std::tuple<Statement::For*, Statement::Var_Decl*> get_for_row(size_t track, Symbol::NT *first_nt, Statement::Var_Decl *input_seq) {
  // create loop variable addressing the DP row (=1st index)
  // e.g.: for (unsigned int t_0_i = t_0_j + 1; t_0_i > 1; t_0_i--) {
  Expr::Base *row_start = first_nt->right_indices.at(track)->vacc()->plus(new Expr::Const(1));

  Statement::Var_Decl *var_row = new Statement::Var_Decl(
      new Type::Size(),
      first_nt->left_indices.at(track)->vacc(),
      row_start);

  // create end point for loop variable
  Expr::Const *row_end = new Expr::Const(1);

  // create condition of For loop
  Expr::Greater *cond_row = new Expr::Greater(new Expr::Vacc(*var_row), row_end);

  Statement::For *loop = new Statement::For(var_row, cond_row);
  Statement::Var_Assign *x = new Statement::Var_Assign(*var_row, new Expr::Const(new Const::Int(-1)));
  x->set_op(::Expr::Type::PLUS);
  loop->inc = x;

  Statement::Var_Decl *var_nonloop = var_row->clone();
  var_nonloop->rhs = new Expr::Const(1);

  return std::make_tuple(loop, var_nonloop);
}

void add_nts(size_t tracks, std::list<Statement::Base*> &stmts, std::list<Symbol::NT*> &nts, Table::Dim type) {
  for (std::list<Symbol::NT*>::const_iterator i = nts.begin(); i != nts.end(); ++i) {
    if (!(*i)->is_tabulated()) {
      continue;
    }
    std::list<Expr::Base*> *args = new std::list<Expr::Base*>();
    for (size_t track = 0; track < tracks; ++track) {
      if (!(*i)->tables()[track].delete_left_index()) {
        args->push_back((*i)->left_indices.at(track)->vacc()->minus(new Expr::Const(1)));
      }
      if (!(*i)->tables()[track].delete_right_index()) {
        args->push_back((*i)->right_indices.at(track)->vacc());
      }
    }
    Statement::Fn_Call *nt_call = new Statement::Fn_Call((*(*i)->code_list().rbegin())->name, args, Loc());

    if ((type == Table::Dim::QUADRATIC) && (!(*i)->tables()[0].delete_left_index() && !(*i)->tables()[0].delete_right_index())) {
      stmts.push_back(nt_call);
    } else if ((type == Table::Dim::LINEAR) && (!(*i)->tables()[0].delete_left_index() || !(*i)->tables()[0].delete_right_index())) {
      stmts.push_back(nt_call);
    } else if (type == Table::Dim::CONSTANT) {
      stmts.push_back(nt_call);
    }
  }
}

/*
 *   |  0  1  2  3   4  5          |  0  1  2  3  4  5
 * --|-------------------        --|------------------
 * 0 |  0  2  5  9  14 20        0 |  B  B  B  B  B  D
 * 1 |     1  4  8  13 19        1 |     A  A  A  A  C
 * 2 |        3  7  12 18        2 |        A  A  A  C
 * 3 |           6  11 17        3 |           A  A  C
 * 4 |              10 16        4 |              A  C
 * 5 |                 15        5 |                 C
 */
std::list<Statement::Base*> *get_cyk_singletrack(size_t track, const AST &ast, Statement::Var_Decl *seq, std::list<Statement::Base*> *nested_stmts) {
  std::list<Statement::Base*> *stmts = new std::list<Statement::Base*>();

  std::tuple<Statement::For*, Statement::Var_Decl*> row = get_for_row(track, *ast.grammar()->topological_ord().begin(), seq);
  // A
  if (nested_stmts->size() == 0) {
    add_nts(ast.grammar()->axiom->tracks(), std::get<0>(row)->statements, ast.grammar()->topological_ord(), Table::Dim::QUADRATIC);
  } else {
    std::get<0>(row)->statements.insert(std::get<0>(row)->statements.end(), nested_stmts->begin(), nested_stmts->end());
  }

  std::tuple<Statement::For*, Statement::Var_Decl*> col = get_for_column(track, *ast.grammar()->topological_ord().begin(), seq);
  std::get<0>(col)->statements.push_back(std::get<0>(row));
  std::get<0>(col)->statements.push_back(std::get<1>(row));
  // B
  if (nested_stmts->size() == 0) {
    add_nts(ast.grammar()->axiom->tracks(), std::get<0>(col)->statements, ast.grammar()->topological_ord(), Table::Dim::QUADRATIC);
  } else {
    std::get<0>(col)->statements.insert(std::get<0>(col)->statements.end(), nested_stmts->begin(), nested_stmts->end());
  }
  stmts->push_back(std::get<0>(col));
  stmts->push_back(std::get<1>(col));

  // C
  std::tuple<Statement::For*, Statement::Var_Decl*> rowC = get_for_row(track, *ast.grammar()->topological_ord().begin(), seq);
  if (nested_stmts->size() == 0) {
    add_nts(ast.grammar()->axiom->tracks(), std::get<0>(rowC)->statements, ast.grammar()->topological_ord(), Table::Dim::LINEAR);
  } else {
    std::get<0>(rowC)->statements.insert(std::get<0>(rowC)->statements.end(), nested_stmts->begin(), nested_stmts->end());
  }
  stmts->push_back(std::get<0>(rowC));
  stmts->push_back(std::get<1>(rowC));

  // D
  if (nested_stmts->size() == 0) {
    add_nts(ast.grammar()->axiom->tracks(), *stmts, ast.grammar()->topological_ord(), Table::Dim::CONSTANT);
  } else {
    stmts->insert(stmts->end(), nested_stmts->begin(), nested_stmts->end());
  }

  return stmts;
}


Fn_Def *print_CYK(const AST &ast) {
  Fn_Def *fn_cyk = new Fn_Def(new Type::RealVoid(), new std::string("cyk"));
  fn_cyk->stmts.push_back(new Statement::CustomeCode("#ifndef _OPENMP"));

  // recursively reverse iterate through tracks and create nested for loop structures
  std::list<Statement::Base*> *stmts = new std::list<Statement::Base*>();
  std::vector<Statement::Var_Decl*>::const_reverse_iterator it_stmt_seq = ast.seq_decls.rbegin();
  for (int track = ast.grammar()->axiom->tracks() - 1; track >= 0; track--, ++it_stmt_seq) {
    stmts = get_cyk_singletrack(track, ast, *it_stmt_seq, stmts);
  }
  fn_cyk->stmts.insert(fn_cyk->stmts.end(), stmts->begin(), stmts->end());

  fn_cyk->stmts.push_back(new Statement::CustomeCode("#else"));
  if (ast.grammar()->axiom->tracks() == 1) {
    std::list<Statement::Base*> *stmts_tilesize = get_tile_computation(ast, std::string("max_tiles_n"));
    fn_cyk->stmts.insert(fn_cyk->stmts.end(), stmts_tilesize->begin(), stmts_tilesize->end());
    fn_cyk->stmts.push_back(new Statement::CustomeCode("#pragma omp parallel"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("// OPENMP < 3 requires signed int here ..."));
  }
  fn_cyk->stmts.push_back(new Statement::CustomeCode("#endif"));

  return fn_cyk;
}
