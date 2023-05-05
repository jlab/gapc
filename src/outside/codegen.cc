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

#include "codegen.hh"


std::list<Symbol::NT*> *NTs_to_report(const AST &ast) {
  /* define which non-terminals shell be reported to the user
   * order of user arguments (--outside_grammar) shall take precedence over
   * NTs as occurring in source code of grammar.
   * - User shell be warned, if outside version of NT has not been generated.
   *   This happens for NTs without rhs NTs.
   * - User was already informed about NTs he/she requested but are not part of
   *   the grammar. */
  std::list<Symbol::NT*> *report_nts = new std::list<Symbol::NT*>();

  // iterate through user arguments
  for (std::vector<std::string>::const_iterator name_param =
       ast.get_outside_nt_list()->begin();
      name_param != ast.get_outside_nt_list()->end(); ++name_param) {
    if (name_param->compare(OUTSIDE_ALL) == 0) {
      // edge case: user requested to report ALL non terminals, by providing the
      // keyword "ALL"
      for (std::list<Symbol::NT*>::const_iterator i =
           ast.grammar()->nts().begin(); i != ast.grammar()->nts().end(); ++i) {
        // skip outside NTs
        if ((*i)->is_partof_outside()) {
          continue;
        }
        bool already_in = false;
        for (std::list<Symbol::NT*>::iterator rnt = report_nts->begin();
             rnt != report_nts->end(); ++rnt) {
          if (*rnt == *i) {
            already_in = true;
            break;
          }
        }
        if (already_in == false) {
          report_nts->push_back(*i);
        }
      }
    } else {
      report_nts->push_back(dynamic_cast<Symbol::NT*>(
          ast.grammar()->NTs.find(*name_param)->second));
    }
  }

  std::list<Symbol::NT*> nts_no_outside;
  for (std::list<Symbol::NT*>::iterator i = report_nts->begin();
       i != report_nts->end(); ++i) {
    hashtable<std::string, Symbol::Base*>::iterator outside =
        ast.grammar()->NTs.find(OUTSIDE_NT_PREFIX + *(*i)->name);
    if (outside == ast.grammar()->NTs.end()) {
      nts_no_outside.push_back(*i);
    }
  }
  if (nts_no_outside.size() > 0) {
    std::string msg = "Outside generation of your grammar will NOT lead to out"
                      "side versions of the following " +
                      std::to_string(nts_no_outside.size()) +
                      " non-terminals: ";
    for (std::list<Symbol::NT*>::iterator i = nts_no_outside.begin();
         i != nts_no_outside.end(); ++i) {
      msg += *(*i)->name;
      if (i != std::next(nts_no_outside.end())) {
        msg += ", ";
      }
    }
    msg += ", as they have no non-terminal on their right hand sides.";
    Log::instance()->warning(nts_no_outside.front()->location, msg);
  }

  return report_nts;
}


void print_insideoutside_report_fn(Printer::Cpp &stream, const AST &ast) {
  bool old_in_class = stream.in_class;
  stream.in_class = true;

  std::list<Symbol::NT*> *report_nts = NTs_to_report(ast);

  Fn_Def *fn = new Fn_Def(new Type::RealVoid(), new std::string(
      "report_insideoutside"));
  // fn->set
  // a hacky way to "inject" the non constant "std::ostream &out" parameter
  // into the function
  Type::TupleDef *t2 = new Type::TupleDef(Loc());
  t2->name = "std::ostream";
  fn->add_para(t2, new std::string("&out"));


  for (std::list<Symbol::NT*>::iterator i = report_nts->begin();
       i != report_nts->end(); ++i) {
    // do the same for inside and outside version of NT
    for (unsigned int inout = 0; inout <= 1; ++inout) {
      Symbol::NT *nt = (*i);
      if (inout == 1) {
        hashtable<std::string, Symbol::Base*>::iterator outside_nt =
            ast.grammar()->NTs.find(OUTSIDE_NT_PREFIX + *nt->name);
        if (outside_nt != ast.grammar()->NTs.end()) {
          nt = dynamic_cast<Symbol::NT*>(outside_nt->second);
        } else {
          continue;
        }
      }

      Expr::Fn_Call *fn_nt = new Expr::Fn_Call((nt->code()->name));

      // build up loops. For example:
      //  for (unsigned int t_0_i = t_0_left_most;
      //       t_0_i <= t_0_right_most; ++t_0_i) {
      //    for (unsigned int t_0_j = t_0_i;
      //         t_0_j <= t_0_right_most; ++t_0_j) {
      std::list<Statement::For*> *loops = new std::list<Statement::For*>();
      std::vector<std::string*> *idx = new std::vector<std::string*>();
      for (size_t track = nt->track_pos(); track < nt->tracks(); ++track) {
        if (!nt->tables()[track].delete_left_index()) {
          // we don't need a for loop, if left index is leftmost end
          // (which is constant)
          if (nt->left_indices[track] != nt->left_most_indices[track]) {
            // linear and quadratic tables
            Statement::Var_Decl *loopvariable = new Statement::Var_Decl(
              new ::Type::Size(), nt->left_indices[track],
                                  nt->left_most_indices[track]);
            loopvariable->set_itr(true);
            Expr::Less_Eq *cond = new Expr::Less_Eq(
                nt->left_indices[track],
                nt->right_most_indices[track]);
            loops->push_back(new Statement::For(loopvariable, cond));
            fn_nt->add_arg(nt->left_indices[track]);
            idx->push_back(nt->left_indices[track]->vacc()->name());
          }
        }

        if (!nt->tables()[track].delete_right_index()) {
          if (nt->right_indices[track] != nt->right_most_indices[track]) {
            // only quadratic tables
            Statement::Var_Decl *loopvariable = new Statement::Var_Decl(
              new ::Type::Size(), nt->right_indices[track],
                                  nt->left_indices[track]);
            loopvariable->set_itr(true);
            Expr::Less_Eq *cond = new Expr::Less_Eq(nt->right_indices[track],
                                     nt->right_most_indices[track]);
            loops->push_back(new Statement::For(loopvariable, cond));
            fn_nt->add_arg(nt->right_indices[track]);
            idx->push_back(nt->right_indices[track]->vacc()->name());
          }
        }
      }

      std::string idx_param = "";
      for (std::vector<std::string*>::iterator i_idx = idx->begin();
           i_idx != idx->end(); ++i_idx) {
        idx_param += " << " + *(*i_idx) + " << ";
        if (std::next(i_idx) != idx->end()) {
          idx_param += "\",\"";
        }
      }
      std::list<Statement::Base*> *stmts = new std::list<Statement::Base*>();
      if (loops->size() > 0) {
        stmts = &((*loops->rbegin())->statements);
      }

      stmts->push_back(new Statement::CustomeCode(
          "out << \"start answers " + *nt->name + "(\"" + idx_param +
          "\"):\\n\";"));

      Statement::Var_Decl *res = new Statement::Var_Decl(
          (*nt->code_list().begin())->return_type, "res_" + *nt->name, fn_nt);
      stmts->push_back(res);

      // produce call of "print_result"
      Statement::Fn_Call *fnp = new Statement::Fn_Call("print_result");
      fnp->add_arg(new std::string("std::cout"));
      fnp->add_arg(res->name);
      stmts->push_back(fnp);

      // produce: out << "//end answers outside_iloop(" << t_0_i << ","
      //              << t_0_j << ")\n";
      stmts->push_back(new Statement::CustomeCode(
          "out << \"//end answers " + *nt->name + "(\"" + idx_param +
          "\")\\n\";"));

      // nest for loops, first will then contain the others
      if (loops->size() > 0) {
        nest_for_loops(loops->begin(), loops->end());
        // add outmost for loop into body of report function
        fn->stmts.push_back(*loops->begin());
      } else {
        fn->stmts = *stmts;
      }
    }
  }
  stream << *fn << endl;

  stream.in_class = old_in_class;
}


Fn_Def *print_CYK(const AST &ast) {
  Fn_Def *fn_cyk = new Fn_Def(new Type::RealVoid(), new std::string("cyk"));

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
    for (size_t track_i = 0; track_i < ast.grammar()->axiom->tracks(); ++track_i) {
      fn_cyk->stmts.push_back(new Statement::CustomeCode("int t_" + std::to_string(track_i) + "_i_loaded = !load_checkpoint || !t_" + std::to_string(track_i) + "_i;"));
      fn_cyk->stmts.push_back(new Statement::CustomeCode("int t_" + std::to_string(track_i) + "_j_loaded = !load_checkpoint || !t_" + std::to_string(track_i) + "_j;"));
    }
    fn_cyk->stmts.push_back(new Statement::CustomeCode("#ifdef _OPENMP"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("unsigned int tile_size = 32;"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("#ifdef TILE_SIZE"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("tile_size = TILE_SIZE;"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("#endif"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("int outer_loop_1_idx_loaded = !load_checkpoint || !outer_loop_1_idx;"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("int outer_loop_2_idx_loaded = !load_checkpoint || !outer_loop_2_idx;"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("int inner_loop_2_idx_loaded = !load_checkpoint || !inner_loop_2_idx;"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("int outer_loop_1_idx_start = (outer_loop_1_idx_loaded++) ? 0 : outer_loop_1_idx;"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("int outer_loop_2_idx_start = (outer_loop_2_idx_loaded++) ? tile_size : outer_loop_2_idx;"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("int inner_loop_2_idx_start = inner_loop_2_idx;"));
    fn_cyk->stmts.push_back(new Statement::CustomeCode("#endif"));
  }

  fn_cyk->stmts.push_back(new Statement::CustomeCode("#ifndef _OPENMP"));

  std::list<Statement::Var_Decl*>::const_iterator it_stmt_n = (*ast.grammar()->topological_ord().begin())->table_decl->ns().begin();
  std::vector<Statement::Var_Decl*>::const_iterator it_stmt_seq = ast.seq_decls.begin();
  for (size_t track = 0; track < ast.grammar()->axiom->tracks(); ++track, ++it_stmt_n, ++it_stmt_seq) {
    std::list<Statement::Base*> *stmts_const = new std::list<Statement::Base*>();
    std::list<Statement::Base*> *stmts_linear = new std::list<Statement::Base*>();
    std::list<Statement::Base*> *stmts_quadratic = new std::list<Statement::Base*>();
    std::list<Statement::Base*> *stmts_checkpoint_outer = new std::list<Statement::Base*>();
    std::list<Statement::Base*> *stmts_checkpoint_inner = new std::list<Statement::Base*>();

    // A: quadratic loops
    Statement::For *loop_quadratic_outer = nullptr;
    Statement::For *loop_quadratic_inner = nullptr;
    Statement::For *loop_linear = nullptr;
    Statement::Var_Decl *t_i = nullptr;
    Statement::Var_Decl *t_j = nullptr;
    for (std::list<Symbol::NT*>::const_iterator it_nt = ast.grammar()->topological_ord().begin(); it_nt != ast.grammar()->topological_ord().end(); ++it_nt) {
      if ((*it_nt)->is_tabulated()) {
        if (!(*it_nt)->tables()[track].delete_right_index()) {
          if (!loop_quadratic_outer) {
            Statement::Var_Decl *checkpoint_help_var = nullptr;
            if (ast.checkpoint && ast.checkpoint->cyk) {
              /*
                 in checkpointing mode, the loop indices
                 (e.g. t_0_i and t_0_j in single-track mode;
                  names can differ in multi-track mode)
                 are members of the out class instead of local loop variables
                 so they can be archived/loaded;
                 if they are loaded, the loop variable (e.g. t_0_j) starts
                 from whatever value was loaded from the checkpoint,
                 which is handled through a marker
                 for every loop variable (e.g. "t_0_j_loaded");
                 initially, all of these marker values are set to 0, which will
                 set each loop variable to whatever value was loaded from the checkpoint;
                 after that, the respective marker value will be set to 1,
                 so the loop variable will be set to whatever
                 value is usually would be set to
              */
              checkpoint_help_var = new Statement::Var_Decl(
                  new ::Type::Size(),
                  "help_" + *(*it_nt)->right_indices[track]->vacc()->name(),
                  (*it_nt)->right_indices[track]);
              stmts_checkpoint_outer->push_back(checkpoint_help_var);
              stmts_checkpoint_outer->push_back(
                  new Statement::If(
                      (new Expr::Vacc(new std::string("t_" + std::to_string(track) + "_j_loaded")))->plus(new Expr::Const(1)),
                      new Statement::Var_Assign(*checkpoint_help_var, (*it_nt)->left_most_indices[track])));
            }

            // create outer J loop
            Statement::Var_Decl *loopvariable = new Statement::Var_Decl(
                new ::Type::Size(),
                (*it_nt)->right_indices[track],
                checkpoint_help_var ? new Expr::Vacc(checkpoint_help_var->name) : (*it_nt)->left_most_indices[track]);
            loopvariable->set_itr(true);
            Expr::Less *cond = new Expr::Less(
                (*it_nt)->right_indices[track],
                (*it_nt)->right_most_indices[track]);
            loop_quadratic_outer = new Statement::For(loopvariable, cond);
            loop_quadratic_outer->statements.push_back(new Statement::CustomeCode("// A: quadratic loops"));

            t_j = (*loopvariable).clone();
            t_j->rhs = (*it_nt)->right_most_indices[track];
            t_j->set_itr(false);
          }
        }

        if (!(*it_nt)->tables()[track].delete_right_index() &&
            !(*it_nt)->tables()[track].delete_left_index()) {
          if (!loop_quadratic_inner) {
            Statement::Var_Decl *checkpoint_help_var = nullptr;
            if (ast.checkpoint && ast.checkpoint->cyk) {
              /*
                 in checkpointing mode, the inner loop variable (e.g. t_0_i)
                 is either regularly set to e.g. t_0_j + 1 or to whatever value
                 was loaded from the checkpoint archive;
                 however, this loaded checkpoint value will only be the start value
                 for t_0_i in the first inner loop pass;
                 afterwards it will regularly be set to t_0_j + 1;
              */
              checkpoint_help_var = new Statement::Var_Decl(
                  new ::Type::Size(),
                  "help_" + *(*it_nt)->left_indices[track]->vacc()->name(),
                  (*it_nt)->left_indices[track]);
              stmts_checkpoint_inner->push_back(checkpoint_help_var);
              //loop_quadratic_outer->statements.push_back(checkpoint_help_var);
              //loop_quadratic_outer->push_back(
              stmts_checkpoint_inner->push_back(
                  new Statement::If(
                      (new Expr::Vacc(new std::string("t_" + std::to_string(track) + "_i_loaded")))->plus(new Expr::Const(1)),
                      new Statement::Var_Assign(*checkpoint_help_var, (*(*it_nt)->right_indices[track]).plus(new Expr::Const(1)))));
            }

            Statement::Var_Decl *loopvariable = new Statement::Var_Decl(
              new ::Type::Size(),
              (*it_nt)->left_indices[track],
              checkpoint_help_var ? new Expr::Vacc(checkpoint_help_var->name) : (*(*it_nt)->right_indices[track]).plus(new Expr::Const(1)));
            loopvariable->set_itr(true);
            Expr::Greater *cond = new Expr::Greater(
                (*it_nt)->left_indices[track],
                new Expr::Const(1));
            loop_quadratic_inner = new Statement::For(loopvariable, cond);
            loop_quadratic_inner->decrement = true;

            loop_linear = dynamic_cast<Statement::For*>(loop_quadratic_inner->copy());

            t_i = (*loopvariable).clone();
            t_i->rhs = new Expr::Const(1);
            t_i->set_itr(false);
          }
        }

        // produce: nt_tabulate_iloop(t_0_i-1, t_0_j);
        std::list<Expr::Base*> *args = new std::list<Expr::Base*>();
        // t_0_i - 1
        Statement::Var_Decl *idx_i = (*loop_quadratic_inner->var_decl).clone();
        idx_i->set_itr(false);
        if (!(*it_nt)->tables()[track].delete_left_index()) {
          args->push_back((new Expr::Vacc(*idx_i))->minus(new Expr::Const(1)));
        }
        // t_0_j
        Statement::Var_Decl *idx_j = (*loop_quadratic_outer->var_decl).clone();
        idx_j->set_itr(false);
        if (!(*it_nt)->tables()[track].delete_right_index()) {
          args->push_back(new Expr::Vacc(*idx_j));
        }
        Statement::Fn_Call *nt_call = new Statement::Fn_Call((*(*it_nt)->code_list().rbegin())->name, args, Loc());
        //stmts->push_back(nt_call);

        if (!(*it_nt)->tables()[track].delete_right_index() &&
            !(*it_nt)->tables()[track].delete_left_index()) {
          stmts_quadratic->push_back(nt_call);
        }
        if (!(*it_nt)->tables()[track].delete_right_index() ||
            !(*it_nt)->tables()[track].delete_left_index()) {
          stmts_linear->push_back(nt_call);
        }
        stmts_const->push_back(nt_call);
      }
    }

    if (ast.checkpoint && ast.checkpoint->cyk) {
      Statement::CustomeCode *mutex = new Statement::CustomeCode("std::lock_guard<fair_mutex> lock(mutex);");
      stmts_quadratic->push_front(mutex);
      stmts_linear->push_front(mutex);
      stmts_const->push_front(mutex);
    }

    if (loop_quadratic_outer && loop_quadratic_inner) {
      fn_cyk->stmts.insert(fn_cyk->stmts.end(), stmts_checkpoint_outer->begin(), stmts_checkpoint_outer->end());

      loop_quadratic_outer->statements.insert(loop_quadratic_outer->statements.end(), stmts_checkpoint_inner->begin(), stmts_checkpoint_inner->end());
      loop_quadratic_inner->statements = *stmts_quadratic;
      std::list<Statement::For*> *loops = new std::list<Statement::For*>();
      loops->push_back(loop_quadratic_outer);
      loops->push_back(loop_quadratic_inner);
      nest_for_loops(loops->begin(), loops->end());

      loop_quadratic_outer->statements.push_back(new Statement::CustomeCode("// B: inner quadratic loops"));
      loop_quadratic_outer->statements.push_back(t_i);
      loop_quadratic_outer->statements.insert(loop_quadratic_outer->statements.end(), stmts_quadratic->begin(), stmts_quadratic->end());

      fn_cyk->stmts.push_back(*loops->begin());
    }

    if (loop_linear) {
      fn_cyk->stmts.push_back(new Statement::CustomeCode("// C: linear loops"));
      fn_cyk->stmts.insert(fn_cyk->stmts.end(), stmts_checkpoint_inner->begin(), stmts_checkpoint_inner->end());
      fn_cyk->stmts.push_back(t_j);
      loop_linear->statements = *stmts_linear;
      fn_cyk->stmts.push_back(loop_linear);
    }

    fn_cyk->stmts.push_back(new Statement::CustomeCode("// D: constant loops"));
    fn_cyk->stmts.push_back(t_i);
    fn_cyk->stmts.insert(fn_cyk->stmts.end(), stmts_const->begin(), stmts_const->end());
  }

  fn_cyk->stmts.push_back(new Statement::CustomeCode("#else"));
  fn_cyk->stmts.push_back(new Statement::CustomeCode("#endif"));

  return fn_cyk;
}
