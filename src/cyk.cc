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
#include <tuple>
#include <vector>
#include <list>
#include <string>

static const char *MUTEX = "mutex";
static const char *VARNAME_OuterLoop1 = "outer_loop_1_idx";
static const char *VARNAME_OuterLoop2 = "outer_loop_2_idx";
static const char *VARNAME_InnerLoop2 = "inner_loop_2_idx";

static std::string
VARNAME_tile_size   = "tile_size";  // NOLINT [runtime/string]
static std::string
VARNAME_max_tiles   = "max_tiles";  // NOLINT [runtime/string]
static std::string
VARNAME_max_tiles_n = "max_tiles_n";  // NOLINT runtime/string
static std::string
// computes the maximal fitting number of tiles in one table dimension for
// outside computation
VARNAME_num_tiles_per_axis = "num_tiles_per_axis";

static Statement::Var_Decl tile_size_decl(new Type::Size(), VARNAME_tile_size);

Statement::Fn_Call *mutex_lock() {
  Statement::Fn_Call *fn = new Statement::Fn_Call("lock_shared");
  fn->add_arg(new std::string(MUTEX));
  fn->is_obj = Bool(true);
  return fn;
}
Statement::Fn_Call *mutex_unlock() {
  Statement::Fn_Call *fn = new Statement::Fn_Call("unlock_shared");
  fn->add_arg(new std::string(MUTEX));
  fn->is_obj = Bool(true);
  return fn;
}

std::tuple<std::list<Statement::Base*>*, std::string*>
get_tile_computation(const AST &ast, std::string *name_maxtilen,
                     Statement::Var_Decl *input_seq, bool just_tilesize) {
  Statement::Var_Assign *tile_size = new Statement::Var_Assign(
    new Var_Acc::Plain(&VARNAME_tile_size),
    new Expr::Vacc(new std::string("opts.tile_size")));

  std::list<Statement::Base*> *res = new std::list<Statement::Base*>();
  res->push_back(tile_size);

  if (just_tilesize) {
    return std::make_tuple(res, &VARNAME_tile_size);;
  }

  Expr::Fn_Call *end = new Expr::Fn_Call(new std::string("size"));
    end->add_arg(input_seq->name);
    end->is_obj = Bool(true);

  Statement::Var_Assign *max_tiles = new Statement::Var_Assign(
    new Var_Acc::Plain(&VARNAME_max_tiles),
    new Expr::Div(end, new Expr::Vacc(&VARNAME_tile_size)));
  res->push_back(max_tiles);

  Statement::Var_Assign *max_tiles_n = new Statement::Var_Assign(
      new Var_Acc::Plain(name_maxtilen),
      new Expr::Times(new Expr::Vacc(&VARNAME_max_tiles),
                      new Expr::Vacc(&VARNAME_tile_size)));
  res->push_back(max_tiles_n);

  return std::make_tuple(res, &VARNAME_tile_size);
}

/* since tiles shall always be complete squares, we cannot touch the main
 * anti-diagonal and thus leave a triangular space. This triangle must be
 * at most tile_size - 1 in size.
 * We divide (without rest, since we operate on integer) the remaining
 * sequence size by the tile_size and multiply with tile_size to obtain
 * the suffix of the input sequence which can be tiled */
Statement::Var_Decl *get_tile_computation_outside(
    Statement::Var_Decl *input_seq) {
  Expr::Fn_Call *seqsize = new Expr::Fn_Call(new std::string("size"));
  seqsize->add_arg(input_seq->name);
  seqsize->is_obj = Bool(true);

  Expr::Vacc *tl = new Expr::Vacc(new std::string("tile_size"));
  // max_tiles_n = ((t_0_seq.size() - (tile_size - 1)) / tile_size) *
  // tile_size;
  return new Statement::Var_Decl(
      new Type::Int,
      VARNAME_num_tiles_per_axis,
      new Expr::Cond(
          new Expr::Less(tl, seqsize->minus(new Expr::Const(1))),
          new Expr::Div(seqsize->minus(tl->minus(new Expr::Const(1))), tl),
          new Expr::Const(0)));
}

/* deep copy of a list of statements */
std::list<Statement::Base*> *copy_statements(
    std::list<Statement::Base*> *other) {
  std::list<Statement::Base*> *co = new std::list<Statement::Base*>();
  for (std::list<Statement::Base*>::iterator i = other->begin();
       i != other->end(); ++i) {
    co->push_back((*i)->copy());
  }
  return co;
}

/* data structure to bundle a Statement::For and a Statement::Var_Decl which
 * constitute a for loop to iterate over NT indices and the last index the loop
 * not yet iterated over.
 */
class CYKloop {
 public:
  Statement::For *loop;  // the constructed for loop statement
  Statement::Var_Decl *end_state;  // the variable declaration of index the
                                   // loop did not reach
  CYKloop(Statement::For *loop, Statement::Var_Decl *end_state) :
    loop(loop), end_state(end_state) {
    assert(loop->var_decl->name == end_state->name);
  }
};

enum CYKmode {SINGLETHREAD, OPENMP_PARALLEL, OPENMP_SERIAL,
              SINGLETHREAD_OUTSIDE,
              OPENMP_PARALLEL_OUTSIDE, OPENMP_SERIAL_OUTSIDE};

CYKloop get_for_column(Expr::Vacc *running_boundary,
    Expr::Base *start, Expr::Base *end,
    bool with_checkpoint, CYKmode mode) {
  // create loop variable addressing the DP column (=2nd index)
  // e.g.: for (unsigned int t_0_j = 0; t_0_j < t_0_seq.size(); ++t_0_j) {
  Type::Base *t = new Type::Size();
  if (with_checkpoint && (mode != CYKmode::OPENMP_PARALLEL)) {
    t = new Type::External("");  // ugly hack to avoid redeclaration of variable
    start = new Expr::Cond(
        new Expr::Vacc(new std::string(
            *running_boundary->name() + "_loaded++")),
        start,
        running_boundary);
  }

  Statement::Var_Decl *var_col = new Statement::Var_Decl(
      t,
      running_boundary,
      start);

  // create condition of For loop
  Expr::Base *cond_col = new Expr::Less(
      new Expr::Vacc(*var_col), end);
  if (mode == CYKmode::OPENMP_SERIAL_OUTSIDE) {
    cond_col = new Expr::Greater(
        (new Expr::Vacc(*var_col))->plus(new Expr::Const(1)),
        end);
  }

  Statement::For *loop = new Statement::For(var_col, cond_col);
  if (mode == CYKmode::OPENMP_SERIAL_OUTSIDE) {
    Statement::Var_Assign *x = new Statement::Var_Assign(
        *var_col, new Expr::Const(new Const::Int(-1)));
    x->set_op(::Expr::Type::PLUS);
    loop->inc = x;
  }

  Statement::Var_Decl *var_nonloop = var_col->clone();
  var_nonloop->rhs = end;

  return CYKloop(loop, var_nonloop);
}

CYKloop get_for_row(Expr::Vacc *running_boundary, Expr::Base *start,
    Expr::Base *end, bool with_checkpoint, CYKmode mode) {
  // create loop variable addressing the DP row (=1st index)
  // e.g.: for (unsigned int t_0_i = t_0_j + 1; t_0_i > 1; t_0_i--) {
  Type::Base *t = new Type::Size();
  if (mode == CYKmode::OPENMP_PARALLEL) {
    t = new Type::Int();
  }
  if (with_checkpoint && (mode != CYKmode::OPENMP_PARALLEL)) {
    t = new Type::External("");  // ugly hack to avoid redeclaration of variable
    start = new Expr::Cond(
        new Expr::Vacc(new std::string(
            *running_boundary->name() + "_loaded++")),
        start,
        running_boundary);
  }
  Statement::Var_Decl *var_row = new Statement::Var_Decl(
      t,
      running_boundary,
      start);

  // create condition of For loop
  Expr::Two *cond_row = new Expr::Greater(new Expr::Vacc(*var_row), end);
  if ((mode == CYKmode::SINGLETHREAD_OUTSIDE) ||
      (mode == CYKmode::OPENMP_SERIAL_OUTSIDE)) {
    cond_row = new Expr::Less(new Expr::Vacc(*var_row), end);
  }

  Statement::For *loop = new Statement::For(var_row, cond_row);
  // except for outside, we need to decrement the loop variable, i.e. t_x_i--
  // In outside, it must be ++t_x_i
  if ((mode != CYKmode::SINGLETHREAD_OUTSIDE) &&
      (mode != CYKmode::OPENMP_SERIAL_OUTSIDE)) {
    Statement::Var_Assign *x = new Statement::Var_Assign(
        *var_row, new Expr::Const(new Const::Int(-1)));
    x->set_op(::Expr::Type::PLUS);
    loop->inc = x;
  }

  Statement::Var_Decl *var_nonloop = var_row->clone();
  var_nonloop->rhs = new Expr::Const(1);

  return CYKloop(loop, var_nonloop);
}

Statement::For *get_for_openMP(Expr::Vacc *loopvar, Expr::Base *start,
    Expr::Base *end, Statement::Var_Decl *inc) {
  Statement::Var_Decl *var = new Statement::Var_Decl(
      new Type::Int(),
      loopvar,
      start);

  // create condition of For loop
  Expr::Less *cond_row = new Expr::Less(new Expr::Vacc(*var), end);

  Statement::For *loop = new Statement::For(var, cond_row);
  Statement::Var_Assign *x = new Statement::Var_Assign(*var, *inc);
  x->set_op(::Expr::Type::PLUS);
  loop->inc = x;

  return loop;
}

/*
 * Construct the loop traversal structure for CYK parsing of one track as below.
 * Note that this general structure gets recursively nested for multiple tracks!
 * The result will "only" contain loops, but they are empty for now.
 * Call function add_nt_call() to populate loops with concrete NT calls,
 * which depends on the NT actual table dimensions.
 * for (t_x_j ... {
 *   for (t_x_i ... {
 *     calls to triangular cells = A
 *     nt_tabulated_foo(t_x_i+1, t_x_j, ...)
 *   }
 *   calls to top row = B
 *   nt_tabulated_foo(0, t_x_j, ...)
 * }
 * for (t_x_i ... {
 *   calls to last column = C
 *   nt_tabulated_foo(t_x_i, x_n, ...)
 * }
 * calls to top right cell = D
 * nt_tabulated_foo(0, x_n, ...)
 *
 *   |  0  1  2  3   4  5          |  0  1  2  3  4  5
 * --|-------------------        --|------------------
 * 0 |  0  2  5  9  14 20        0 |  B  B  B  B  B  D
 * 1 |     1  4  8  13 19        1 |     A  A  A  A  C
 * 2 |        3  7  12 18        2 |        A  A  A  C
 * 3 |           6  11 17        3 |           A  A  C
 * 4 |              10 16        4 |              A  C
 * 5 |                 15        5 |                 C
 */
std::list<Statement::Base*> *cyk_traversal_singlethread_singletrack(
    size_t track, const AST &ast, Statement::Var_Decl *seq,
    std::list<Statement::Base*> *nested_stmts, bool with_checkpoint,
    CYKmode mode) {
  std::list<Statement::Base*> *stmts = new std::list<Statement::Base*>();

  Expr::Base *row_start = ast.grammar()->right_running_indices.at(
      track)->plus(new Expr::Const(1));
  // create t_X_seq.size() call
  Expr::Fn_Call *seqend = new Expr::Fn_Call(new std::string("size"));
  dynamic_cast<Expr::Fn_Call*>(seqend)->add_arg(seq->name);
  dynamic_cast<Expr::Fn_Call*>(seqend)->is_obj = Bool(true);

  // A: major cells in triangle below first row, left of last columns
  // A: t_x_i = row index
  std::list<Statement::Base*> *co = copy_statements(nested_stmts);
  CYKloop row = get_for_row(ast.grammar()->left_running_indices[track],
      row_start, new Expr::Const(1), with_checkpoint, mode);
  row.loop->statements.insert(
      row.loop->statements.end(), co->begin(), co->end());

  // A: t_x_j = column index
  Expr::Base *alt_start = new Expr::Const(0);
  if (mode == CYKmode::OPENMP_SERIAL) {
    alt_start = new Expr::Vacc(new std::string("max_tiles_n"));
  }
  CYKloop col = get_for_column(ast.grammar()->right_running_indices[track],
      alt_start, seqend, with_checkpoint, mode);
  col.loop->statements.push_back(row.loop);
  col.loop->statements.push_back(row.end_state);

  // B: first row
  co = copy_statements(nested_stmts);
  col.loop->statements.insert(
      col.loop->statements.end(), co->begin(), co->end());
  stmts->push_back(col.loop);
  stmts->push_back(col.end_state);

  // C: last column
  CYKloop rowC = get_for_row(ast.grammar()->left_running_indices[track],
      row_start, new Expr::Const(1), with_checkpoint, mode);
  co = copy_statements(nested_stmts);
  rowC.loop->statements.insert(
      rowC.loop->statements.end(), co->begin(), co->end());
  stmts->push_back(rowC.loop);
  stmts->push_back(rowC.end_state);

  // D: top right cell
  co = copy_statements(nested_stmts);
  stmts->insert(stmts->end(), co->begin(), co->end());

  return stmts;
}

/*
 * Construct the loop traversal structure for CYK parsing of one track for
 * outside parts of a grammar, as below.
 * Since in outside, all DP tables (except the outside_axiom) must be quadratic
 * by definition, we can have a much simpler structure than for the inside case.
 * Here, we don't need extra cases for tables with fewer indices that get
 * optimized away in linear or constant table dimensions.
 *
 * However, we need to make sure that DP cells are filled in the correct order
 * and this is in a triangular fashion starting top right and advancing towards
 * the main anti diagonal, e.g.
 *
 *   |  0  1  2  3   4  5          |  0  1  2  3  4  5
 * --|-------------------        --|------------------
 * 0 | 18 13  9  6  1  0         0 |  A  A  A  A  A  A
 * 1 |    19 14 10  4  2         1 |     A  A  A  A  A
 * 2 |       20 15 11  5         2 |        A  A  A  A
 * 3 |          21 16 12         3 |           A  A  A
 * 4 |             22 17         4 |              A  A
 * 5 |                23         5 |                 A
 *
 */
std::list<Statement::Base*> *cyk_traversal_singlethread_singletrack_outside(
    size_t track, const AST &ast, Statement::Var_Decl *seq,
    std::list<Statement::Base*> *nested_stmts, bool with_checkpoint,
    CYKmode mode) {
  std::list<Statement::Base*> *stmts = new std::list<Statement::Base*>();

  // create t_X_seq.size() call
  Expr::Fn_Call *seqend = new Expr::Fn_Call(new std::string("size"));
  dynamic_cast<Expr::Fn_Call*>(seqend)->add_arg(seq->name);
  dynamic_cast<Expr::Fn_Call*>(seqend)->is_obj = Bool(true);

  Expr::Vacc *idx_col = new Expr::Vacc(new std::string(
      *ast.grammar()->right_running_indices[track]->name() +
      OUTSIDE_IDX_SUFFIX));
  Expr::Vacc *idx_row = new Expr::Vacc(new std::string(
      *ast.grammar()->left_running_indices[track]->name() +
      OUTSIDE_IDX_SUFFIX));

  Expr::Base *col_start = seqend->minus(idx_row);
  Expr::Base *col_end = seqend->plus(new Expr::Const(1));
  CYKloop col = get_for_column(
      idx_col,
      col_start, col_end,
      with_checkpoint, mode);
  std::list<Statement::Base*> *co = copy_statements(nested_stmts);
  col.loop->statements.insert(
      col.loop->statements.end(), co->begin(), co->end());

  Expr::Base *row_start = new Expr::Const(0);
  CYKloop row = get_for_row(idx_row,
      row_start, seqend->plus(new Expr::Const(1)),
      with_checkpoint, mode);
  row.loop->statements.push_back(col.loop);

  stmts->push_back(row.loop);

  return stmts;
}

// recursively reverse iterate through tracks and create nested for loop
// structures
std::list<Statement::Base*> *cyk_traversal_singlethread(const AST &ast,
    CYKmode mode) {
  std::list<Statement::Base*> *stmts = new std::list<Statement::Base*>();

  assert(ast.seq_decls.size() == ast.grammar()->axiom->tracks());
  std::vector<Statement::Var_Decl*>::const_reverse_iterator it_stmt_seq =
      ast.seq_decls.rbegin();
  for (int track = ast.grammar()->axiom->tracks() - 1; track >= 0;
       track--, ++it_stmt_seq) {
    if (mode == CYKmode::SINGLETHREAD_OUTSIDE) {
      stmts = cyk_traversal_singlethread_singletrack_outside(
          track, ast, *it_stmt_seq, stmts,
          ast.checkpoint && ast.checkpoint->cyk, mode);
    } else {
      stmts = cyk_traversal_singlethread_singletrack(
          track, ast, *it_stmt_seq, stmts,
          ast.checkpoint && ast.checkpoint->cyk, mode);
    }
  }

  return stmts;
}

std::vector<Statement::Base*> *get_wait_omp(
  std::string var_outer, std::string var_inner, Expr::Base *step_size,
  std::string *z, bool no_inner_loop) {
  std::vector<Statement::Base*> *stmts = new std::vector<Statement::Base*>();

  stmts->push_back(new Statement::CustomCode(
      "#pragma omp ordered"));
  Statement::Block *blk_omp2 = new Statement::Block();
  blk_omp2->statements.push_back(
    new Statement::CustomCode("// force omp to wait for all threads to finish "
                              "their current batch (of size tile_size)"));
  if (!no_inner_loop) {
    blk_omp2->statements.push_back(new Statement::Var_Assign(
        new Var_Acc::Plain(new std::string(var_inner)),
        (new Expr::Vacc(new std::string(var_inner)))->plus(
            step_size)));
  }
  blk_omp2->statements.push_back(new Statement::Var_Assign(
      new Var_Acc::Plain(new std::string(var_outer)), new Expr::Vacc(z)));
  blk_omp2->statements.push_back(mutex_unlock());
  stmts->push_back(blk_omp2);

  return stmts;
}


/* Construct the loop traversal structure for CYK parsing of one track in
 * multi-threaded mode. Before we can start operating in parallel, we need to
 * compute all predecessor cells (part A). Thus, tiles of the DP matrix on the
 * diagonal can then be processed in parallel (part B)
 * Note: currently only works for single track!
 *  A: tile_size = 4, input = aaaaccccgggg
 *    |  0   1   2   3   4   5   6   7   8   9  10  11  12
 * ---|----------------------------------------------------
 *  0 |  0   2   5   9
 *  1 |      1   4   8
 *  2 |          3   7
 *  3 |              6
 *  4 |                 10  12  15  19
 *  5 |                     11  14  18
 *  6 |                         13  17
 *  7 |                             16
 *  8 |                                 20  22  25  29
 *  9 |                                     21  24  28
 * 10 |                                         23  27
 * 11 |                                             26
 * 12 |
 *
 *  B: tile_size = 4, input = aaaaccccgggg
 *    |  0   1   2   3   4   5   6   7   8   9  10  11  12
 * ---|----------------------------------------------------
 *  0 |                 33  37  41  45  65  69  73  77
 *  1 |                 32  36  40  44  64  68  72  76
 *  2 |                 31  35  39  43  63  67  71  75
 *  3 |                 30  34  38  42  62  66  70  74
 *  4 |                                 49  53  57  61
 *  5 |                                 48  52  56  60
 *  6 |                                 47  51  55  59
 *  7 |                                 46  50  54  58
 *  8 |
 *  9 |
 * 10 |
 * 11 |
 * 12 |
 *
 * Note: the below can be constructured by the cyk_traversal_singlethread
 * Construct the loop traversal structure for the non-parallel part in multi-
 * threaded mode, i.e. iterate over all DP cells that fall out of the tiling
 * pattern.
 *  C: tile_size = 4, input = aaaaccccgggg
 *    |  0  1  2  3  4  5  6  7  8  9 10 11 12
 * ---|----------------------------------------
 *  0 |                                     90
 *  1 |                                     89
 *  2 |                                     88
 *  3 |                                     87
 *  4 |                                     86
 *  5 |                                     85
 *  6 |                                     84
 *  7 |                                     83
 *  8 |                                     82
 *  9 |                                     81
 * 10 |                                     80
 * 11 |                                     79
 * 12 |                                     78
 *
 */
std::list<Statement::Base*> *cyk_traversal_multithread_parallel(const AST &ast,
    Statement::Var_Decl *seq, std::string *tile_size,
    std::string *name_maxtilen, bool with_checkpoint, CYKmode mode) {

  std::string var_ol1 = VARNAME_OuterLoop1;
  std::string var_ol2 = VARNAME_OuterLoop2;
  std::string var_il2 = VARNAME_InnerLoop2;

  size_t track = 0;  // as openMP currently only works for single track grammars
  std::list<Statement::Base*> *stmts = new std::list<Statement::Base*>();

  Expr::Vacc *z = new Expr::Vacc(new std::string("z"));
  Expr::Vacc *y = new Expr::Vacc(new std::string("y"));
  Statement::Var_Decl *x = new Statement::Var_Decl(
      new Type::Size(), "x", (y->minus(z))->plus(new Expr::Vacc(tile_size)));

  // part A: prepare for parallel tile phase, prepare predecessor DP cells for
  // later parallel computation
  Expr::Vacc *idx_row = ast.grammar()->left_running_indices[track];
  Expr::Vacc *idx_col = ast.grammar()->right_running_indices[track];
  Expr::Base *row_start = idx_col->plus(new Expr::Const(1));

  CYKloop row = get_for_row(idx_row, row_start, z, with_checkpoint, mode);
  CYKloop col = get_for_column(idx_col,
      z, z->plus(new Expr::Vacc(tile_size)), with_checkpoint, mode);
  col.loop->statements.push_back(row.loop);
  Expr::Base *start_z = new Expr::Const(0);
  if (with_checkpoint) {
    start_z = new Expr::Vacc(new std::string(std::string(
        var_ol1) + "_start"));
  }
  Statement::For *loop_z = get_for_openMP(z, start_z,
      new Expr::Vacc(name_maxtilen), &tile_size_decl);
  if (with_checkpoint) {
    loop_z->statements.push_back(mutex_lock());
  }
  loop_z->statements.push_back(col.loop);
  // code to wait for threads to finish
  if (with_checkpoint) {
    loop_z->statements.push_back(new Statement::CustomCode(
        "#pragma omp ordered"));
    Statement::Block *blk_omp = new Statement::Block();
    blk_omp->statements.push_back(new Statement::CustomCode(
        "// force omp to wait for all threads to finish their current batch "
        "(of size tile_size)"));
    blk_omp->statements.push_back(new Statement::Var_Assign(
        new Var_Acc::Plain(new std::string(var_ol1)),
        (new Expr::Vacc(new std::string(var_ol1)))->
          plus(new Expr::Vacc(tile_size))));
    blk_omp->statements.push_back(mutex_unlock());
    loop_z->statements.push_back(blk_omp);
  }
  stmts->push_back(loop_z);

  // part B: code for the actual parallel tile computation
  CYKloop rowB = get_for_row(idx_row, new Expr::Vacc(*x),
      (new Expr::Vacc(*x))->minus(new Expr::Vacc(tile_size)),
      with_checkpoint, mode);
  CYKloop colB = get_for_column(idx_col,
      y, y->plus(new Expr::Vacc(tile_size)), with_checkpoint, mode);
  colB.loop->statements.push_back(rowB.loop);

  Expr::Base *start_y = z;
  if (with_checkpoint) {
    start_y = new Expr::Cond(
        new Expr::Vacc(new std::string(std::string(
            var_il2) + "_loaded")),
        z,
        new Expr::Vacc(new std::string(std::string(
            var_il2) + "_start")));
  }
  Statement::For *loop_y = get_for_openMP(y, start_y,
      new Expr::Vacc(name_maxtilen), &tile_size_decl);
  // produce: unsigned int x = y - z + tile_size;
  if (with_checkpoint) {
    loop_y->statements.push_back(new Statement::CustomCode(
        "++inner_loop_2_idx_loaded;"));
    loop_y->statements.push_back(mutex_lock());
  }
  loop_y->statements.push_back(x);
  loop_y->statements.push_back(colB.loop);
  if (with_checkpoint) {
    std::vector<Statement::Base*> *omp_wait =
      get_wait_omp(var_ol2, var_il2, new Expr::Vacc(tile_size),
                   z->name(), false);
    loop_y->statements.insert(loop_y->statements.end(),
                              omp_wait->begin(), omp_wait->end());
  }


  Expr::Vacc *start_z2 = new Expr::Vacc(tile_size);
  if (with_checkpoint) {
    start_z2 = new Expr::Vacc(new std::string(std::string(
        var_ol2) + "_start"));
  }
  loop_z = get_for_openMP(z, start_z2, new Expr::Vacc(name_maxtilen),
      &tile_size_decl);
  if (with_checkpoint) {
    loop_z->statements.push_back(new Statement::CustomCode(
        "#pragma omp for ordered schedule(dynamic)"));
  } else {
    loop_z->statements.push_back(new Statement::CustomCode("#pragma omp for"));
  }
  loop_z->statements.push_back(loop_y);
  if (with_checkpoint) {
    loop_z->statements.push_back(new Statement::Var_Assign(new Var_Acc::Plain(
        new std::string(var_il2)), z));
  }

  stmts->push_back(loop_z);

  return stmts;
}

/* constructs two nested for-loops which realize a traversal of a square (or
 * only the upper right triangle) such that it starts in the top right corner
 * and follows the anti-diagonals, e.g.
 *   |  0  1  2  3
 * --|--------------
 * 0 |  6  3  1  0
 * 1 | 11  7  4  2
 * 2 | 17 12  8  5
 * 3 | 24 18 13  9
 * This order is necessary for outside computation, as all cells on top and
 * right of a cell are predecessors.
 */
enum OMP_OUTSIDE_CP {no, A, B, C};
Statement::For *get_triag_traversal(
    // variable name of outer for loop that addresses the anti diagonal
    std::string name_diag,
    // size of the square or triangle (4 in the above example)
    Expr::Base *diameter,
    std::string name_col,  // variable name of column index
    std::string name_row,  // variable name of row index
    bool full_square,  // traverse a square or "just" the upper right triangle
    // insert the "pragma omp for" statement within outer loop
    bool insert_pragma,
    // statements that shall be added to the inner loop, can be empty
    std::list<Statement::Base*> *nested_stmts,
    // first anti diagonal to start with. If != 1 we can skip first x cells,
    // e.g. diag_start=5 would start at cell 11 in above example
    Expr::Base *diag_start,
    OMP_OUTSIDE_CP cp) {
  Expr::Base *tl = diameter;

  Type::Base *type_diag = new Type::Size();
  if (cp == OMP_OUTSIDE_CP::A) {
    diag_start = new Expr::Vacc(
      new std::string(std::string(VARNAME_OuterLoop2) +
                      OUTSIDE_IDX_SUFFIX + "_start"));
  } else if (cp == OMP_OUTSIDE_CP::C) {
    type_diag = new Type::External(new std::string(""));
  }
  // tile_diag_x
  Statement::Var_Decl *lv_diag = new Statement::Var_Decl(
      type_diag,
      name_diag,
      diag_start);
  // short cut for tile_diag_x for variable access
  Expr::Vacc *diag = new Expr::Vacc(*lv_diag);

  // t_x_j
  Expr::Base *start_j = new Expr::Cond(
      new Expr::Less(diag, tl), tl->minus(diag), new Expr::Const(0));
  if (cp == OMP_OUTSIDE_CP::A) {
    start_j = new Expr::Cond(
        new Expr::Vacc(new std::string(std::string(VARNAME_InnerLoop2) +
                         OUTSIDE_IDX_SUFFIX +  "_loaded")),
        start_j,
        new Expr::Vacc(new std::string(std::string(VARNAME_InnerLoop2) +
                         OUTSIDE_IDX_SUFFIX + "_start")));
  } else if (cp == OMP_OUTSIDE_CP::C) {
    start_j = new Expr::Cond(
        new Expr::Vacc(new std::string(name_col + "_loaded++")),
        start_j,
        new Expr::Vacc(new std::string(name_col)));
  }
  Statement::Var_Decl *lv_j = new Statement::Var_Decl(
      type_diag,
      name_col,
      start_j);
  // short cut for t_x_j for variable access
  Expr::Vacc *j = new Expr::Vacc(*lv_j);

  Expr::Less *cond_j = new Expr::Less(j,
      new Expr::Cond(
          new Expr::Less(diag, tl),
          tl,
          (new Expr::Times(tl, new Expr::Const(2)))->minus(diag)));
  Statement::For *fl_j = new Statement::For(lv_j, cond_j);
  if ((cp == OMP_OUTSIDE_CP::A) || (cp == OMP_OUTSIDE_CP::C)) {
    if (cp == OMP_OUTSIDE_CP::A) {
      fl_j->statements.push_back(
        new Statement::CustomCode(std::string("++") + VARNAME_InnerLoop2 +
                                  OUTSIDE_IDX_SUFFIX + "_loaded;"));
    }
    fl_j->statements.push_back(mutex_lock());
  }
  fl_j->push_back(new Statement::Var_Decl(
      new Type::Size(),
      name_row,
      diag->plus(j)->minus(tl)));
  if (nested_stmts) {
    fl_j->statements.insert(fl_j->statements.end(),
                            nested_stmts->begin(), nested_stmts->end());
  }
  if (cp == OMP_OUTSIDE_CP::A) {
    std::vector<Statement::Base*> *omp_wait =
      get_wait_omp(std::string(VARNAME_OuterLoop2) + OUTSIDE_IDX_SUFFIX,
                   std::string(VARNAME_InnerLoop2) + OUTSIDE_IDX_SUFFIX,
                   new Expr::Const(1), new std::string(name_diag), false);
    fl_j->statements.insert(fl_j->statements.end(),
                            omp_wait->begin(), omp_wait->end());
  }

  Expr::Base *diag_end = tl->plus(new Expr::Const(1));
  if (full_square) {
    diag_end = new Expr::Times(tl, new Expr::Const(2));
  }
  Expr::Less *cond_diag = new Expr::Less(diag, diag_end);
  Statement::For *fl_diag = new Statement::For(lv_diag, cond_diag);
  std::string pragma = std::string("#pragma omp for");
  if (cp == OMP_OUTSIDE_CP::A) {
    pragma += " ordered schedule(dynamic)";
  }
  if (insert_pragma) {
    fl_diag->statements.push_back(new Statement::CustomCode(pragma));
  }

  fl_diag->statements.push_back(fl_j);

  if (cp == OMP_OUTSIDE_CP::A) {
    fl_diag->statements.push_back(new Statement::Var_Assign(new Var_Acc::Plain(
        new std::string(std::string(VARNAME_InnerLoop2) + OUTSIDE_IDX_SUFFIX)),
        new Expr::Vacc(new std::string(name_diag))));
  }

  return fl_diag;
}

/* This function also constructs traversal structure for outside openMP code.
 *
 *  A: tile_size = 4, input = aaaaccccgggg
 *    |  0  1  2  3  4  5  6  7  8  9 10 11 12
 * ---|---------------------------------------
 *  0 |            22  19  17  16   6  3  1  0
 *  1 |            26  23  20  18  10  7  4  2
 *  2 |            29  27  24  21  13 11  8  5
 *  3 |            31  30  28  25  15 14 12  9
 *  4 |                            38 35 33 32
 *  5 |                            42 39 36 34
 *  6 |                            45 43 40 37
 *  7 |                            47 46 44 41
 *  8 |
 *  9 |
 * 10 |
 * 11 |
 * 12 |
 *
 *
 *  B: tile_size = 4, input = aaaaccccgggg
 *    |  0  1  2  3  4  5  6  7  8  9 10 11 12
 * ---|---------------------------------------
 *  0 |    54 51 49 48
 *  1 |       55 52 50
 *  2 |          56 53
 *  3 |             57
 *  4 |                64 61 59 58
 *  5 |                   65 62 60
 *  6 |                      66 63
 *  7 |                         67
 *  8 |                            74 71 69 68
 *  9 |                               75 72 70
 * 10 |                                  76 73
 * 11 |                                     77
 * 12 |
 *
 *
 *  C: tile_size = 4, input = aaaaccccgggg
 *    |  0  1  2  3  4  5  6  7  8  9 10 11 12
 * ---|---------------------------------------
 *  0 | 78
 *  1 |    79
 *  2 |       80
 *  3 |          81
 *  4 |             82
 *  5 |                83
 *  6 |                   84
 *  7 |                      85
 *  8 |                         86
 *  9 |                            87
 * 10 |                               88
 * 11 |                                  89
 * 12 |                                     90
 *
 */
std::list<Statement::Base*> *cyk_traversal_multithread_outside(const AST &ast,
     Expr::Base *seqsize, std::string *tile_size,
     Statement::Var_Decl *max_tiles, bool with_checkpoint, CYKmode mode,
     std::string part) {
  // as openMP currently only works for single track grammars
  size_t track = 0;
  std::list<Statement::Base*> *stmts = new std::list<Statement::Base*>();

  if (part == "A") {
     // part A: square tiles
     std::list<Statement::Base*> *for_tile = new std::list<Statement::Base*>();
     for_tile->push_back(get_triag_traversal(
         std::string("tile_diag_" + std::to_string(track)),
         new Expr::Vacc(tile_size),
         std::string(*(ast.grammar()->right_running_indices[track])->name() +
             OUTSIDE_IDX_SUFFIX),
         std::string(*(ast.grammar()->left_running_indices[track])->name() +
             OUTSIDE_IDX_SUFFIX),
         true,
         false,
         nullptr,
         new Expr::Const(1),
         OMP_OUTSIDE_CP::no));

     stmts->push_back(get_triag_traversal(
         std::string("matrix_diag_" + std::to_string(track)),
         new Expr::Vacc(*max_tiles),
         "y",
         "x",
         false,
         true,
         for_tile,
         new Expr::Const(1),
         with_checkpoint ? OMP_OUTSIDE_CP::A : OMP_OUTSIDE_CP::no));
  }

  if (part == "B") {
     // part B: triangular tiles at edge
     Statement::For *for_tile_b = get_triag_traversal(
         std::string("tile_diag_" + std::to_string(track)),
         new Expr::Vacc(tile_size),
         std::string(*(ast.grammar()->right_running_indices[track])->name() +
             OUTSIDE_IDX_SUFFIX),
         std::string(*(ast.grammar()->left_running_indices[track])->name() +
             OUTSIDE_IDX_SUFFIX),
         false,
         false,
         nullptr,
         new Expr::Const(1),
         with_checkpoint ? OMP_OUTSIDE_CP::B : OMP_OUTSIDE_CP::no);

     std::string pragma = "#pragma omp for";
     Expr::Base *diag_start = (new Expr::Const(0))->minus(new Expr::Const(1));
     if (with_checkpoint) {
       pragma += " ordered schedule(dynamic)";
       diag_start = new Expr::Vacc(new std::string(
                                     std::string(VARNAME_OuterLoop1) +
                                     OUTSIDE_IDX_SUFFIX));
     }
     stmts->push_back(new Statement::CustomCode(pragma));
     Statement::Var_Decl *lv_diag = new Statement::Var_Decl(
         // one left of leftmost tile
         new Type::Int(), "y", diag_start);
     Statement::For *fl_diag = new Statement::For(
         lv_diag,
         new Expr::Less(new Expr::Vacc(*lv_diag), new Expr::Vacc(*max_tiles)));
     if (with_checkpoint) {
       fl_diag->statements.push_back(mutex_lock());
     }
     fl_diag->statements.push_back(new Statement::Var_Decl(
         new Type::Int(),
         "x",
         (new Expr::Vacc(*lv_diag))->plus(new Expr::Const(1))));
     fl_diag->statements.push_back(for_tile_b);
     if (with_checkpoint) {
       std::vector<Statement::Base*> *omp_wait =
         get_wait_omp(std::string(VARNAME_OuterLoop1) + OUTSIDE_IDX_SUFFIX,
                      std::string(VARNAME_InnerLoop2) + OUTSIDE_IDX_SUFFIX,
                      new Expr::Const(1), lv_diag->name, true);
       fl_diag->statements.insert(fl_diag->statements.end(),
                                  omp_wait->begin(), omp_wait->end());
     }
     stmts->push_back(fl_diag);
  }

  if (part == "C") {
     // part C: serial part to fill remaining gapc
    Expr::Base *start_diag = new Expr::Cond(
         // test the case that input is smaller than a single tile
         new Expr::Greater(seqsize, new Expr::Vacc(tile_size)),
         (new Expr::Times(
             new Expr::Vacc(tile_size),
             (new Expr::Vacc(&VARNAME_num_tiles_per_axis))->plus(
                 new Expr::Const(1))))->plus(new Expr::Const(1)),
         new Expr::Const(1));
    std::string diag_name =
      std::string("t_" + std::to_string(track) + "_diag_outside");
    if (with_checkpoint) {
      start_diag = new Expr::Cond(
          new Expr::Vacc(new std::string("t_0_diag_outside_loaded++")),
          start_diag,
          new Expr::Vacc(new std::string(diag_name)));
    }
    Statement::For *for_tile_c = get_triag_traversal(
        diag_name,
        seqsize->plus(new Expr::Const(1)),
        std::string(*(ast.grammar()->right_running_indices[track])->name() +
             OUTSIDE_IDX_SUFFIX),
        std::string(*(ast.grammar()->left_running_indices[track])->name() +
             OUTSIDE_IDX_SUFFIX),
        false,
        false,
        nullptr,
        start_diag,
        with_checkpoint ? OMP_OUTSIDE_CP::C : OMP_OUTSIDE_CP::no);
    stmts->push_back(for_tile_c);
  }

  return stmts;
}


size_t count_nt_calls_and_loops(Statement::For *loop) {
  size_t res = 0;
  for (std::list<Statement::Base*>::const_iterator i = loop->statements.begin();
       i != loop->statements.end(); ++i) {
    if ((*i)->is(Statement::FN_CALL) &&
        (dynamic_cast<Statement::Fn_Call*>(*i)->name().find(
            "nt_tabulate_", 0) == 0)) {
      res++;
    }
    if ((*i)->is(Statement::FOR)) {
      res++;
    }
  }
  return res;
}

/* This function will add NT calls (and mutex operations) into a given CYK
 * traversal structure in a recursive fashion. The challenge is to add an NT
 * call into the correct level of nested for loops, i.e. only as deep as the NT
 * table has indices. However, we can have left or right linear optimized tables
 * and we need to ensure to find the correct loop (row or column) at the same
 * level. Furthermore, last row/column in single thread CYK mode are called
 * AFTER the triangle (with cells A) has been computed, which means NTs also
 * have to be called outside the correct nesting level!
 *
 * The strategy here is to use a "stack" of loop variable names for the nesting
 * level and count how many indices actually are used by an NT. Depending on
 * single (see above problem with last row/col) or multi thread mode, NT calls
 * are only added IF the number of *used* indices (through a loop =
 * used_indices) coincide with nesting level or additionally if the NT has the
 * correct number of indices (nt_has_index), respectively.
 */
std::list<Statement::Base*> *add_nt_calls(std::list<Statement::Base*> &stmts,
    std::list<std::string*> *loop_vars, std::list<Symbol::NT*> orderedNTs,
    bool with_checkpoint, CYKmode mode, const AST &ast) {
  bool contains_nested_for = false;
  for (std::list<Statement::Base*>::iterator s = stmts.begin();
       s != stmts.end(); ++s) {
    // recurse into next for loop
    if ((*s)->is(Statement::FOR)) {
      contains_nested_for = true;
      Statement::For *fl = dynamic_cast<Statement::For*>(*s);
      std::list<std::string*> *next_loop_vars = new std::list<std::string*>();
      next_loop_vars->insert(
          next_loop_vars->end(), loop_vars->begin(), loop_vars->end());
      if (((mode != CYKmode::OPENMP_PARALLEL) &&
          (mode != CYKmode::OPENMP_PARALLEL_OUTSIDE)) ||
          (fl->var_decl->name->find("t_", 0) == 0)) {
        // openMP code adds in loops that do not traverse NT indices. Only add
        // loop variable, if it regard to NT indices, which all start with t_
        // e.g. t_0_i or t_1_j
        if (mode == CYKmode::OPENMP_SERIAL_OUTSIDE) {
          std::string *diagname = fl->var_decl->name;
          if (diagname->find("diag") == 0) {
            diagname->replace(4, 4, "i");  // "diag" -> "i"
            next_loop_vars->push_back(diagname);
          }
        } else {
          next_loop_vars->push_back(fl->var_decl->name);
        }
      }
      std::list<Statement::Base*> *new_stmts = add_nt_calls(
          fl->statements, next_loop_vars, orderedNTs, with_checkpoint,
          mode, ast);
      fl->statements.insert(
          fl->statements.end(), new_stmts->begin(), new_stmts->end());
    }
  }

  // remove loops that neither contain NT calls nor nested loops
  for (std::list<Statement::Base*>::iterator s = stmts.begin();
       s != stmts.end(); ++s) {
    if ((*s)->is(Statement::FOR)) {
      if (count_nt_calls_and_loops(dynamic_cast<Statement::For*>(*s)) == 0) {
        s = stmts.erase(s);
      }
    }
  }

  if (((mode == CYKmode::OPENMP_PARALLEL) ||
       (mode == CYKmode::SINGLETHREAD_OUTSIDE) ||
       (mode == CYKmode::OPENMP_PARALLEL_OUTSIDE) ||
       (mode == CYKmode::OPENMP_SERIAL_OUTSIDE)) && contains_nested_for) {
    // don't add NT calls in for loops that is not the innermost loop, if in
    // multi threaded mode.
    return new std::list<Statement::Base*>();
  }

  // add NTs
  std::list<Statement::Base*> *nt_stmts = new std::list<Statement::Base*>();
  if (with_checkpoint) {
    if ((mode == CYKmode::SINGLETHREAD) ||
        (mode == CYKmode::SINGLETHREAD_OUTSIDE)) {
      // don't add mutex on top level, as it's context would never end
      if (loop_vars->size() > 0) {
        nt_stmts->push_back(new Statement::CustomCode(
          "std::lock_guard<fair_mutex> lock(mutex);"));
      }
    } else {
      if (mode == CYKmode::OPENMP_SERIAL) {
        nt_stmts->push_back(mutex_lock());
      }
    }
  }
  for (std::list<Symbol::NT*>::const_iterator i = orderedNTs.begin();
       i != orderedNTs.end(); ++i) {
    if (!(*i)->is_tabulated()) {
      continue;
    }
    if ((*i)->is_partof_outside() == (
        (mode != CYKmode::SINGLETHREAD_OUTSIDE) &&
        (mode != CYKmode::OPENMP_PARALLEL_OUTSIDE) &&
        (mode != CYKmode::OPENMP_SERIAL_OUTSIDE))) {
      continue;
    }
    std::list<Expr::Base*> *args = new std::list<Expr::Base*>();
    size_t used_indices = 0;
    size_t nt_has_indices = 0;
    std::vector<Statement::Var_Decl*>::const_iterator it_stmt_seq =
        ast.seq_decls.begin();
    for (size_t t = 0; t < (*i)->tracks(); ++t, ++it_stmt_seq) {
      if (!(*i)->tables()[t].delete_left_index()) {
        Expr::Vacc *idx = (*i)->left_indices.at(t)->vacc();
        if ((mode == CYKmode::SINGLETHREAD_OUTSIDE) ||
            (mode == CYKmode::OPENMP_PARALLEL_OUTSIDE) ||
            (mode == CYKmode::OPENMP_SERIAL_OUTSIDE)) {
          idx = new Expr::Vacc(new std::string(
              *idx->name() + OUTSIDE_IDX_SUFFIX));
        }
        for (std::list<std::string*>::const_iterator lv = loop_vars->begin();
             lv != loop_vars->end(); ++lv) {
          if (**lv == *idx->name()) {
            used_indices++;
            break;
          }
        }
        nt_has_indices++;
        if (mode == CYKmode::SINGLETHREAD_OUTSIDE) {
          // create t_X_seq.size() call
          Expr::Fn_Call *seqsize = new Expr::Fn_Call(new std::string("size"));
          dynamic_cast<Expr::Fn_Call*>(seqsize)->add_arg((*it_stmt_seq)->name);
          dynamic_cast<Expr::Fn_Call*>(seqsize)->is_obj = Bool(true);
          args->push_back(idx->plus((new Expr::Vacc(
              new std::string(*(*i)->right_indices.at(t)->vacc()->name() +
                  OUTSIDE_IDX_SUFFIX))))->minus(seqsize));
        } else if ((mode == CYKmode::OPENMP_PARALLEL_OUTSIDE)) {
          Expr::Vacc *idx_i = new Expr::Vacc(new std::string(
              *(*i)->left_indices.at(t)->vacc()->name() + OUTSIDE_IDX_SUFFIX));
          // t_0_i_outside + x*tile_size
          args->push_back(idx_i->plus(new Expr::Times(
              new Expr::Vacc(new std::string("x")),
              new Expr::Vacc(&VARNAME_tile_size))));
        } else if ((mode == CYKmode::OPENMP_SERIAL_OUTSIDE)) {
          Expr::Vacc *idx_i = new Expr::Vacc(new std::string(
              *(*i)->left_indices.at(t)->vacc()->name() + OUTSIDE_IDX_SUFFIX));
          args->push_back(idx_i);
        } else {
          args->push_back(idx->minus(new Expr::Const(1)));
        }
      }
      if (!(*i)->tables()[t].delete_right_index()) {
        Expr::Vacc *idx = (*i)->right_indices.at(t)->vacc();
        if ((mode == CYKmode::SINGLETHREAD_OUTSIDE) ||
            (mode == CYKmode::OPENMP_PARALLEL_OUTSIDE) ||
            (mode == CYKmode::OPENMP_SERIAL_OUTSIDE)) {
          idx = new Expr::Vacc(new std::string(
              *idx->name() + OUTSIDE_IDX_SUFFIX));
        }
        for (std::list<std::string*>::const_iterator lv = loop_vars->begin();
             lv != loop_vars->end(); ++lv) {
          if (**lv == *idx->name()) {
            used_indices++;
            break;
          }
        }
        nt_has_indices++;
        if ((mode == CYKmode::OPENMP_PARALLEL_OUTSIDE)) {
          // (t_0_j_outside + y*tile_size + (t_0_seq.size() - num_tiles_per_axis
          // *tile_size + 1))

          // create t_X_seq.size() call
          Expr::Fn_Call *seqsize = new Expr::Fn_Call(new std::string("size"));
          dynamic_cast<Expr::Fn_Call*>(seqsize)->add_arg((*it_stmt_seq)->name);
          dynamic_cast<Expr::Fn_Call*>(seqsize)->is_obj = Bool(true);

          Expr::Vacc *idx_j = new Expr::Vacc(new std::string(
              *(*i)->right_indices.at(t)->vacc()->name() + OUTSIDE_IDX_SUFFIX));

          args->push_back(idx_j->plus(new Expr::Times(new Expr::Vacc(
              new std::string("y")), new Expr::Vacc(&VARNAME_tile_size)))->plus(
              seqsize->minus(new Expr::Times(
                  new Expr::Vacc(&VARNAME_num_tiles_per_axis),
                  new Expr::Vacc(&VARNAME_tile_size)))->plus(
                      new Expr::Const(1))));
        } else if ((mode == CYKmode::OPENMP_SERIAL_OUTSIDE)) {
          Expr::Vacc *idx_j = new Expr::Vacc(new std::string(
              *(*i)->right_indices.at(t)->vacc()->name() + OUTSIDE_IDX_SUFFIX));
          args->push_back(idx_j);
        } else {
          args->push_back(idx);
        }
      }
    }
    if (used_indices == loop_vars->size()) {
        assert((*i)->code_list().size() > 0);
        Statement::Fn_Call *nt_call = new Statement::Fn_Call(
            (*(*i)->code_list().rbegin())->name, args, Loc());
        nt_stmts->push_back(nt_call);
    }
  }
  if (with_checkpoint) {
    if ((mode == CYKmode::OPENMP_SERIAL) ||
        (mode == CYKmode::OPENMP_SERIAL_OUTSIDE)) {
      nt_stmts->push_back(mutex_unlock());
    }
  }

  return nt_stmts;
}

Fn_Def *print_CYK(const AST &ast) {
  Fn_Def *fn_cyk = new Fn_Def(new Type::RealVoid(), new std::string("cyk"));
  if (!ast.cyk()) {
    /* return empty function, if CYK was not requested. It is called in the
     * generic out_main.cc source, thus it has to be defined but can remain
     * empty.
     */
    return fn_cyk;
  }

  if (ast.checkpoint && ast.checkpoint->cyk) {
  /*
    define a boolean marker (as an int) for every loop idx
    to allow for the loading of the checkpointed loop indices;
    if the user wants to load a checkpoint (load_checkpoint == true)
    and the loaded idx value doesn't equal the default value 0
    (meaning that the checkpointed program made enough progress
     to get to the loop where the current idx lives),
    the markers will be set to "false" (== 0), which indicates
    that the respective loop idx hasn't been loaded yet and
    should be loaded when it is first requested;
    if the user does not want to load a checkpoint
    (load_checkpoint == false) or the load idx values are still 0,
    the respective markers will be set to "true" (== 1);
    this means that all idx values are already assumed to be
    loaded and won't be loaded when they are first requested;
    this ensures that the idx values start at whatever value
    they would normally start with
   */
    for (size_t track = 0; track < ast.grammar()->axiom->tracks(); ++track) {
      Expr::Vacc *idx_i = ast.grammar()->left_running_indices.at(track);
      Expr::Vacc *idx_j = ast.grammar()->right_running_indices.at(track);
      std::string suffix = std::string("_loaded");
      for (int io = 0; io < 2; ++io) {  // iterate through inside and outside
        fn_cyk->stmts.push_back(new Statement::Var_Decl(
            new Type::Int(),
            *(idx_i->name()) + suffix,
            new Expr::Or(
                new Expr::Not(new Expr::Vacc(
                    new std::string("load_checkpoint"))),
                new Expr::Not(idx_i))));

        fn_cyk->stmts.push_back(new Statement::Var_Decl(
            new Type::Int(),
            *(idx_j->name()) + suffix,
            new Expr::Or(
                new Expr::Not(new Expr::Vacc(new std::string(
                    "load_checkpoint"))),
                new Expr::Not(idx_j))));
        if (!ast.grammar()->is_partof_outside()) {
          break;
        } else {
          if (io == 0) {
            std::string i_outside = *(idx_i->name()) + OUTSIDE_IDX_SUFFIX;
            fn_cyk->stmts.push_back(new Statement::Var_Decl(
            new Type::Int(), i_outside + "_loaded",
            new Expr::Or(
                new Expr::Not(new Expr::Vacc(new std::string(
                    "load_checkpoint"))),
                new Expr::Not(new Expr::Vacc(new std::string(
                    i_outside))))));
          }

          idx_i = new Expr::Vacc(new std::string(
              std::string("t_") + std::to_string(track) +
              std::string("_diag") + OUTSIDE_IDX_SUFFIX));
          idx_j = new Expr::Vacc(new std::string(
              *idx_j->name() + OUTSIDE_IDX_SUFFIX));
        }
      }
    }
  }

  // ==== single thread version
  fn_cyk->stmts.push_back(new Statement::CustomCode("#ifndef _OPENMP"));
  // recursively reverse iterate through tracks and create nested for loop
  // structures
  // add NT calls to traversal structure
  std::list<Statement::Base*> *stmts = cyk_traversal_singlethread(
      ast, CYKmode::SINGLETHREAD);
  std::list<Statement::Base*> *new_stmts = add_nt_calls(*stmts,
      new std::list<std::string*>(), ast.grammar()->topological_ord(),
      ast.checkpoint && ast.checkpoint->cyk, CYKmode::SINGLETHREAD, ast);
  stmts->insert(stmts->end(), new_stmts->begin(), new_stmts->end());
  // finally add traversal structure with NT calls to function body
  if (ast.outside_generation()) {
    fn_cyk->stmts.push_back(new Statement::CustomCode(
      "// start computing inside DP matrices only ..."));
  }
  fn_cyk->stmts.insert(fn_cyk->stmts.end(), stmts->begin(), stmts->end());

  if (ast.outside_generation()) {
    fn_cyk->stmts.push_back(new Statement::CustomCode(
      "// ... now compute outside DP matrices"));
    fn_cyk->stmts.push_back(new Statement::CustomCode(
      "// they are by definition quadratic as every sub-word must "
      "be returned"));
    stmts = cyk_traversal_singlethread(ast, CYKmode::SINGLETHREAD_OUTSIDE);
    std::list<Statement::Base*> *new_stmts = add_nt_calls(*stmts,
        new std::list<std::string*>(), ast.grammar()->topological_ord(),
        ast.checkpoint && ast.checkpoint->cyk, CYKmode::SINGLETHREAD_OUTSIDE,
        ast);
    stmts->insert(stmts->end(), new_stmts->begin(), new_stmts->end());
    fn_cyk->stmts.insert(fn_cyk->stmts.end(), stmts->begin(), stmts->end());
  }

  // ==== multi thread version (only single-track possible for now)
  fn_cyk->stmts.push_back(new Statement::CustomCode("#else"));
  // FIXME generalize for multi-track ...
  if (ast.grammar()->axiom->tracks() == 1) {
    std::string *name_maxtilen = new std::string("max_tiles_n");
    std::vector<Statement::Var_Decl*>::const_reverse_iterator it_stmt_seq =
        ast.seq_decls.rbegin();

    // FIXME abstract from unsigned int, int -> perhaps wait for OpenMP 3
    // since OpenMP < 3 doesn't allow unsigned int in workshared fors

    // header
    if (ast.checkpoint && ast.checkpoint->cyk) {
      std::string suffix = "";
      std::string step = "tile_size";
      for (int io = 0; io < 2; ++io) {  // iterate through inside and outside
        fn_cyk->stmts.push_back(new Statement::CustomCode(
            "bool " + std::string(VARNAME_OuterLoop1) + suffix +
            "_loaded = !load_checkpoint || !" + VARNAME_OuterLoop1 + suffix +
            ";"));
        fn_cyk->stmts.push_back(new Statement::CustomCode(
            "bool " + std::string(VARNAME_OuterLoop2) + suffix +
            "_loaded = !load_checkpoint || !" + VARNAME_OuterLoop2 + suffix +
            ";"));
        fn_cyk->stmts.push_back(new Statement::CustomCode(
            "int " + std::string(VARNAME_InnerLoop2) + suffix +
            "_loaded = !load_checkpoint || !" + VARNAME_InnerLoop2 + suffix +
            ";"));
        fn_cyk->stmts.push_back(new Statement::CustomCode(
            "int " + std::string(VARNAME_OuterLoop1) + suffix +
            "_start = (" + VARNAME_OuterLoop1 + suffix + "_loaded) ? 0 : " +
            VARNAME_OuterLoop1 + suffix + ";"));
        fn_cyk->stmts.push_back(new Statement::CustomCode(
            "int " + std::string(VARNAME_OuterLoop2) + suffix +
            "_start = (" + VARNAME_OuterLoop2 + suffix +
            "_loaded) ? " + step + " : " +
            VARNAME_OuterLoop2 + suffix + ";"));
        fn_cyk->stmts.push_back(new Statement::CustomCode(
            "int " + std::string(VARNAME_InnerLoop2) + suffix +
            "_start = " + VARNAME_InnerLoop2 + suffix + ";"));
        if (!ast.grammar()->is_partof_outside()) {
          break;
        } else {
          suffix = OUTSIDE_IDX_SUFFIX;
          step = "1";
        }
      }
    }
    fn_cyk->stmts.push_back(new Statement::CustomCode("#pragma omp parallel"));
    Statement::Block *blk_parallel = new Statement::Block();

    if (ast.checkpoint && ast.checkpoint->cyk) {
      blk_parallel->statements.push_back(new Statement::CustomCode(
          "#pragma omp for ordered schedule(dynamic)"));
    } else {
      blk_parallel->statements.push_back(new Statement::CustomCode(
          "#pragma omp for"));
    }
    blk_parallel->statements.push_back(new Statement::CustomCode(
        "// OPENMP < 3 requires signed int here ..."));

    // parallel part
    std::list<Statement::Base*> *stmts = cyk_traversal_multithread_parallel(
        ast, *it_stmt_seq, &VARNAME_tile_size, name_maxtilen,
        ast.checkpoint && ast.checkpoint->cyk, CYKmode::OPENMP_PARALLEL);
    // inject NT calls
    std::list<Statement::Base*> *new_stmts = add_nt_calls(*stmts,
        new std::list<std::string*>(), ast.grammar()->topological_ord(),
        ast.checkpoint && ast.checkpoint->cyk, CYKmode::OPENMP_PARALLEL, ast);
    stmts->insert(stmts->end(), new_stmts->begin(), new_stmts->end());

    blk_parallel->statements.insert(blk_parallel->statements.end(),
        stmts->begin(), stmts->end());
    blk_parallel->statements.push_back(new Statement::CustomCode(
        "// end parallel"));
    fn_cyk->stmts.push_back(blk_parallel);

    // serial part
    stmts = cyk_traversal_singlethread(ast, CYKmode::OPENMP_SERIAL);
    // inject NT calls
    std::list<Statement::Base*> *new_serial_stmts = add_nt_calls(
        *stmts, new std::list<std::string*>(), ast.grammar()->topological_ord(),
        ast.checkpoint && ast.checkpoint->cyk, CYKmode::OPENMP_SERIAL, ast);
    stmts->insert(stmts->end(), new_serial_stmts->begin(),
        new_serial_stmts->end());
    fn_cyk->stmts.insert(fn_cyk->stmts.end(), stmts->begin(), stmts->end());

    if (ast.outside_generation()) {
      fn_cyk->stmts.push_back(new Statement::CustomCode(
        "// ... now compute outside DP matrices"));
      Expr::Vacc *tl = new Expr::Vacc(&VARNAME_tile_size);
      /* since tiles shall always be complete squares, we cannot touch the main
       * anti-diagonal and thus leave a triangular space. This triangle must be
       * at most tile_size - 1 in size.
       * We divide (without rest, since we operate on integer) the remaining
       * sequence size by the tile_size and multiply with tile_size to obtain
       * the suffix of the input sequence which can be tiled */
      Expr::Fn_Call *seqsize = new Expr::Fn_Call(new std::string("size"));
        dynamic_cast<Expr::Fn_Call*>(seqsize)->add_arg(
            (*ast.seq_decls.at(0)).name);
        dynamic_cast<Expr::Fn_Call*>(seqsize)->is_obj = Bool(true);
      // max_tiles_n = ((t_0_seq.size() - (tile_size - 1)) / tile_size) *
      // tile_size;
      Statement::Var_Decl *max_tiles = new Statement::Var_Decl(
          new Type::Int,
          VARNAME_num_tiles_per_axis,
          new Expr::Cond(
              new Expr::Less(tl, seqsize->minus(new Expr::Const(1))),
              new Expr::Div(seqsize->minus(tl->minus(new Expr::Const(1))), tl),
              new Expr::Const(0)));
      fn_cyk->stmts.push_back(max_tiles);

      Statement::If *input_large_enough = new Statement::If(
          new Expr::Greater(seqsize, tl));
      input_large_enough->then.push_back(new Statement::CustomCode(
          "#pragma omp parallel"));
      Statement::Block *blk_parallel_outside = new Statement::Block();
      blk_parallel_outside->statements.push_back(new Statement::CustomCode(
          "// OPENMP < 3 requires signed int here ..."));

    // parallel part
      // part A
      std::list<Statement::Base*> *stmts_outsideA =
          cyk_traversal_multithread_outside(ast, seqsize,
              &VARNAME_tile_size, max_tiles,
              ast.checkpoint && ast.checkpoint->cyk,
              CYKmode::OPENMP_PARALLEL_OUTSIDE, "A");
      // inject NT calls
      add_nt_calls(
          *stmts_outsideA, new std::list<std::string*>(),
          ast.grammar()->topological_ord(),
          ast.checkpoint && ast.checkpoint->cyk,
          CYKmode::OPENMP_PARALLEL_OUTSIDE, ast);
      blk_parallel_outside->statements.insert(
          blk_parallel_outside->statements.end(),
          stmts_outsideA->begin(), stmts_outsideA->end());

      // part B
      std::list<Statement::Base*> *stmts_outsideB =
          cyk_traversal_multithread_outside(ast, seqsize,
              &VARNAME_tile_size, max_tiles,
              ast.checkpoint && ast.checkpoint->cyk,
              CYKmode::OPENMP_PARALLEL_OUTSIDE, "B");
      add_nt_calls(
              *stmts_outsideB, new std::list<std::string*>(),
              ast.grammar()->topological_ord(),
              ast.checkpoint && ast.checkpoint->cyk,
              CYKmode::OPENMP_PARALLEL_OUTSIDE, ast);
      blk_parallel_outside->statements.insert(
          blk_parallel_outside->statements.end(),
          stmts_outsideB->begin(), stmts_outsideB->end());

      blk_parallel_outside->statements.push_back(new Statement::CustomCode(
              "// end parallel"));
      input_large_enough->then.push_back(blk_parallel_outside);
      fn_cyk->stmts.push_back(input_large_enough);

      // part C = serial part
      std::list<Statement::Base*> *stmts_outsideC =
          cyk_traversal_multithread_outside(ast, seqsize,
              &VARNAME_tile_size, max_tiles,
              ast.checkpoint && ast.checkpoint->cyk,
              CYKmode::OPENMP_SERIAL_OUTSIDE, "C");
      fn_cyk->stmts.insert(
          fn_cyk->stmts.end(),
          stmts_outsideC->begin(), stmts_outsideC->end());
      add_nt_calls(
          *stmts_outsideC, new std::list<std::string*>(),
          ast.grammar()->topological_ord(),
          ast.checkpoint && ast.checkpoint->cyk,
          CYKmode::OPENMP_SERIAL_OUTSIDE, ast);
    }
  }

  fn_cyk->stmts.push_back(new Statement::CustomCode("#endif"));

  return fn_cyk;
}
