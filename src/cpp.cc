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

#include <cmath>
#include <set>
#include <utility>
#include <boost/tokenizer.hpp>

#include "cpp.hh"

#include "statement.hh"
#include "statement/backtrace_decl.hh"
#include "statement/hash_decl.hh"
#include "statement/marker_decl.hh"
#include "statement/fn_call.hh"
#include "statement/while.hh"
#include "expr.hh"
#include "expr/new.hh"
#include "type.hh"
#include "type/backtrace.hh"
#include "type/multi.hh"

#include "fn_def.hh"

#include "var_acc.hh"

#include "ast.hh"
#include "grammar.hh"

#include "options.hh"


static std::string make_comments(const std::string &s, const std::string &c) {
  std::ostringstream o;
  boost::char_separator<char> nl("\n", "", boost::keep_empty_tokens);
  boost::tokenizer<boost::char_separator<char> > lines(s, nl);
  for (boost::tokenizer<boost::char_separator<char> >::iterator i =
       lines.begin(); i != lines.end(); ++i) {
    o << c;
    if (!(*i).empty()) {
      o << " ";
    }
    o << *i << '\n';
  }
  return o.str();
}


void Printer::Cpp::print(const std::list<Statement::Base*> &stmts) {
  stream << '{' << endl;
  inc_indent();
  for (std::list<Statement::Base*>::const_iterator i = stmts.begin();
       i != stmts.end(); ++i)
    stream << **i << endl;
  dec_indent();
  stream << indent() << '}';
}

void Printer::Cpp::print(const Statement::For &stmt) {
  stream << indent() << "for (";
  stream << *stmt.var_decl;
  stream << ' ' << *stmt.cond << "; ";
  if (!stmt.inc) {
    stream << "++" << *stmt.var_decl->name << ")";
  } else {
    bool t = in_fn_head;
    in_fn_head = true;
    stream << *stmt.inc << ")";
    in_fn_head = t;
  }
  stream << " ";
  stream << stmt.statements;
}

void Printer::Cpp::print(const Statement::While &stmt) {
  stream << indent() << "while(" << stmt.expr() << ")\n";
  stream << stmt.statements;
}

void Printer::Cpp::print(const Statement::Var_Decl &stmt) {
  assert(stmt.type);
  assert(stmt.name);

  // std::cerr << "JJJ " << *stmt.type << '\n';
  if (stmt.type->is(::Type::MULTI) || (stmt.type->is(::Type::LIST) &&
      stmt.type->component()->is(::Type::MULTI)) ) {
    ::Type::Base  *tbase = stmt.type;
    if (stmt.type->is(::Type::LIST))
      tbase = stmt.type->component();
    ::Type::Multi *t = dynamic_cast< ::Type::Multi*>(tbase);
    size_t j = 0;
    const std::list< ::Type::Base*> &l = t->types();
    for (std::list< ::Type::Base*>::const_iterator i = l.begin();
        i != l.end(); ++i, ++j) {
      stream << indent() << **i << ' ' << *stmt.name << "_" << j;
      if (stmt.rhs)
        stream << " = " << *stmt.rhs << "_" << j;
      stream << ";\n";
    }
    return;
  }

  if (!stmt.is_itr()) {
    stream << indent();
  }
  stream << *stmt.type << ' ' << *stmt.name;
  if (stmt.rhs)
    stream << " = " << *stmt.rhs;
  stream << ';';

  if (!in_class && stmt.type->is(Type::LIST) && !stmt.rhs) {
    stream << endl << indent() << "empty(" << *stmt.name << ");";
  }
}

void Printer::Cpp::print(const Statement::If &stmt) {
  stream << indent() << "if (" << *stmt.cond << ") ";
  stream << stmt.then;
  if (stmt.els.empty())
    return;
  stream << " else ";
  stream << stmt.els;
}

void Printer::Cpp::print(const Statement::Switch &stmt) {
  stream << indent() << "switch (" << *stmt.cond << ") {" << endl;
  inc_indent();
  for (std::list<std::pair<std::string,
                           std::list<Statement::Base*> > >::const_iterator i =
      stmt.cases.begin(); i!= stmt.cases.end(); ++i) {
      stream << indent() << "case " << i->first << " :" << endl;
      inc_indent();
            stream << i->second;
            stream << indent() << "break;" << endl;
      dec_indent();
  }
  if (!stmt.defaul.empty()) {
      stream << indent() << "default :" << endl;
      inc_indent();
            stream << stmt.defaul;
            stream << indent() << "break;" << endl;
      dec_indent();
  }

  dec_indent();
  stream << indent() << " }" << endl;
}


void Printer::Cpp::print(const Statement::Return &stmt) {
  if (stmt.expr)
    stream << indent() << "return " << *stmt.expr << ';';
  else
    stream << indent() << "return;";
}

void Printer::Cpp::print(const Statement::Break &stmt) {
  stream << indent() << "break;";
}

void Printer::Cpp::print(const Statement::Continue &stmt) {
  stream << indent() << "continue;";
}

void Printer::Cpp::print(const Statement::Decrease &stmt) {
  stream << indent() << *stmt.name << "--;";
}

void Printer::Cpp::print(const Statement::Increase &stmt) {
  stream << indent() << *stmt.name << "++;";
}

void Printer::Cpp::print(const Statement::Sorter &stmt) {
    stream << indent() << "sort_list(";
    if (stmt.list->type->simple()->is(Type::RANGE)) {
        stream << *stmt.list->name << ".first, ";
        stream << *stmt.list->name << ".second ";
    } else if (stmt.list->type->simple()->is(Type::LIST))  {
        stream << *stmt.list->name << ".ref().begin(), ";
        stream << *stmt.list->name << ".ref().end() ";
    } else {
        // TODO(who?): Implement if needed
    }
    stream << ", " << *stmt.op << ");" << endl;
}

void Printer::Cpp::print(const Statement::Foreach &stmt) {
  std::string itr(*stmt.elem->name + "_itr");

  bool started_loop = true;
  if (stmt.container->type->simple()->is(Type::RANGE)) {
    if (stmt.elem->is_itr())
      itr = *stmt.elem->name;
    Type::Range *range = dynamic_cast<Type::Range*>(stmt.container->type);
    assert(range);
    stream << indent();
    stream << "for (";
    if (choice_range) {
      if (choice_range->is_eq(*range))
        stream << "Iterator ";
    } else {
     stream << "List<" << *range->element_type << ">::iterator ";
    }
    stream
      << itr
      << " = " << *stmt.container->name << ".first; " << itr << " != "
      << *stmt.container->name << ".second; ";
      if (stmt.iteration) {
            stream << "++" << itr;
      }
     stream <<  ") {" << endl;
    inc_indent();
    if (!stmt.elem->is_itr()) {
      stream << indent() << *range->element_type << ' ' << *stmt.elem->name
        << " = *" << itr
        << ';' << endl;
    }
  } else if (stmt.container->type->simple()->is(Type::EVAL_LIST)) {  // ||
    // stmt.container->type->simple()->is(Type::BACKTRACE_LIST)) {
    pointer_as_itr = true;
    stream << indent()
      << "for (typename " << *stmt.container->type << "::iterator " << itr
      << " = " << *stmt.container->name << "->begin(); "
      << itr << " != " << *stmt.container->name << "->end(); ";
    if (stmt.iteration) {
          stream << "++" << itr;
    }
    stream <<  ")";

    pointer_as_itr = false;
    stream << " {" << endl;
    inc_indent();
    stream << indent() << *stmt.elem->type << ' ' << *stmt.elem->name << " = "
      << "*" << itr << ';' << endl;
  // } else if (stmt.container->type->simple()->is(Type::BACKTRACE_LIST)) {
  } else if (stmt.container->type->simple()->is(Type::BACKTRACE)) {
    std::string t = *stmt.container->name + "_t";
    stream << indent()
      << "intrusive_ptr<Backtrace_List<Value, pos_int> > " << t
      << " = boost::dynamic_pointer_cast<Backtrace_List<Value, pos_int> >("
      << *stmt.container->name << ");" << endl
      << indent() << "if (!" << t << ") {" << endl;
    inc_indent();
    stream << indent() << *stmt.elem->type << ' ' << *stmt.elem->name << " = "
      << *stmt.container->name << ';' << endl;
    for (std::list<Statement::Base*>::const_iterator i =
         stmt.statements.begin();
         i != stmt.statements.end(); ++i)
      stream << **i << endl;
    dec_indent();

    pointer_as_itr = true;
    stream << indent() << "} else " << endl << indent()
      << "for (typename Backtrace_List<Value, pos_int>::iterator " << itr
      << " = " << t << "->begin(); "
      << itr << " != " << t << "->end(); ";

      if (stmt.iteration) {
            stream << "++" << itr;
      }
    stream <<  ")";
    pointer_as_itr = false;
    stream << " {" << endl;
    inc_indent();
    stream << indent() << *stmt.elem->type << ' ' << *stmt.elem->name << " = "
      << "*" << itr << ';' << endl;
  } else if (stmt.container->type->simple()->is(Type::LIST)) {
    if (stmt.elem->is_itr())
      itr = *stmt.elem->name;
    Type::List *l = dynamic_cast<Type::List*>(stmt.container->type->simple());
    assert(l);

    if (l->push_type() > Type::List::NORMAL
        && l->push_type() < Type::List::MIN_OTHER) {
      stream << indent() << *stmt.elem->type << ' ' << *stmt.elem->name << " = "
        << *stmt.container->name << ';' << endl;
      started_loop = false;

    } else {
      pure_list_type = true;
      stream << indent() << "for (" << *stmt.container->type << "::iterator "
        << itr << " = " << *stmt.container->name << ".ref().begin(); "
        << itr << "!=" << *stmt.container->name << ".ref().end(); ";
      if (stmt.iteration) {
            stream << "++" << itr;
      }
     stream <<  ")";
      pure_list_type = false;
      stream << '{' << endl;
      inc_indent();
      if (!stmt.elem->is_itr())
        stream << indent() << *stmt.elem->type << ' '
          << *stmt.elem->name << " = "
          << "*" << itr << ';' << endl;
    }
  } else {
    assert(false);
  }
  for (std::list<Statement::Base*>::const_iterator i = stmt.statements.begin();
      i != stmt.statements.end(); ++i)
    stream << **i << endl;

  if (started_loop) {
    dec_indent();
    stream << indent() << '}';
  }
}

void Printer::Cpp::print(const Statement::Var_Assign &stmt) {
  stream << indent();
  stream << *stmt.acc;
  stream << ' ' << stmt.op_str() << ' ' << *stmt.rhs;
  if (!in_fn_head)
     stream << ";";
}


// FIXME unify with Expr::Fn_Call::put_arg
void Printer::Cpp::print_arg(Expr::Base *e) {
  assert(e);

  if (e->is(Expr::VACC)) {
    Expr::Vacc *x = dynamic_cast<Expr::Vacc*> (e);
    if (x->var_acc->is(Var_Acc::PLAIN)) {
      Var_Acc::Plain *v = dynamic_cast<Var_Acc::Plain*> (x->var_acc);
      if (v->vdecl) {
        if (v->vdecl->type->is(::Type::MULTI) ||
            (v->vdecl->type->simple()->is(::Type::LIST) &&
            v->vdecl->type->component()->is(::Type::MULTI))) {
          ::Type::Base *tbase = v->vdecl->type;
          if (v->vdecl->type->simple()->is(::Type::LIST)) {
            tbase = v->vdecl->type->component();
          }

          ::Type::Multi *t = dynamic_cast< ::Type::Multi*>(tbase);
          std::list< ::Type::Base*>::const_iterator i = t->types().begin();
          stream << *v->vdecl->name << "_0";
          ++i;
          size_t j = 1;
          for (; i != t->types().end(); ++i, ++j) {
            stream << ", " << *v->vdecl->name << "_" << j;
          }
          return;
        }
      }
      }
  }

  stream << *e;
}


// TODO(who?): move this up to where all includes are located
#include "const.hh"


// Calculates the size of a string literal as its compiled
// size which treats escaped characters as a single byte
// value, as they are stored in a single byte when the compiler
// transforms the literal into object code.
// NOTE: the implementation is incomplete: it does not handle
// escaped character numbers like \x6D
unsigned int literal_size_of_string(Loc& location, std::string* str) {
  unsigned int length = 0;
  for (unsigned int i = 0; i < str->size(); i++) {
    if (str->at(i) == '\\') {
      // Check the next character if it matches one
      // of the escape characters.
      unsigned int nextPos = i + 1;
      if (nextPos < str->size()) {
        switch (str->at(nextPos)) {
          case 'n':
          case 'r':
          case 't':
          case 'b':
          case 'v':
          case 'f':
          case '"':
          case '\'':
          case '\\': {
            i = nextPos;
            break;
          }
          case 'u':
          case 'U': {
            Log::instance()->error(location,
              "Unicode escape sequences in string literals not supported yet.");
            break;
          }
          case 'x':
          default : {
            // The list of characters that are expected as
            // next characters, stored as a std::string.
            std::string allowedChars;
            unsigned int maxDigitsAllowed = 0;
            if (str->at(nextPos) == 'x') {
              // we expect up to two hex characters!
              allowedChars = "0123456789aAbBcCdDeEfF";
              maxDigitsAllowed = 2;
              // Also forward the next position to read,
              // because we consumed the 'x' character.
              nextPos++;
            } else {
              // we expect up to three octal characters!
              allowedChars = "01234567";
              maxDigitsAllowed = 3;
            }

            unsigned int numberOfDigitsFound = 0;
            while (nextPos < str->size() &&
                   allowedChars.find(str->at(nextPos)) != std::string::npos) {
              nextPos++;
              numberOfDigitsFound++;
            }
            // Not more than two digits are allowed, but there must be
            // at least one digit.
            if (numberOfDigitsFound > maxDigitsAllowed) {
              Log::instance()->error(location,
                "An excape sequence is unknown, or can not be parsed correctly"
                " in the literal string.");
            } else if (numberOfDigitsFound < 1) {
              Log::instance()->error(location,
                "Unknown escape sequence in string literal. No auto length"
                " detection possible. Try to provide explicitely the length"
                " of the literal as a third parameter.");
            }
            // We went one character too far.
            i = nextPos - 1;
            break;
          }
        }
      }
    }
    length++;
  }
  return length;
}


void Printer::Cpp::print(const Statement::Fn_Call &stmt) {
  std::list<Expr::Base*>::const_iterator i = stmt.args.begin();
  if (stmt.is_obj == false) {
    if (stmt.builtin == Statement::Fn_Call::PUSH_BACK ||
        stmt.builtin == Statement::Fn_Call::APPEND) {
      // assert(stmt.args.size() == 2);
      Statement::Var_Decl *v = stmt.args.front()->var_decl();
      if (v && v->type->is(::Type::LIST)) {
        Type::List *l = dynamic_cast<Type::List*> (v->type);
        assert(l);
        stream << indent() << stmt.name();
        if (l->push_type() != Type::List::NORMAL &&
            l->push_type() != Type::List::HASH) {
          stream << "_";
          stream << l->push_str();
        }
        stream << "(";
      } else {
        stream << indent() << stmt.name() << "(";
      }
    } else {
      stream << indent() << stmt.name() << "(";

      // If this is a function call inside of a used defined
      // algebra function, its builtin-type is not set properly
      // and we have to match its name directly with a verbatim
      // string constant (I know, this is a 'Bad Thing'(TM))
      // For one special case, when the second argument to
      // the append-function is of type string, and there is
      // no third argument, we add the third argument automatically.
      // The thrird argument represents the length of the string
      // given in the second argument.
      if (stmt.name() == "append") {
        if (stmt.args.size() == 2) {
          print_arg(*i);
          i++;
          Expr::Base* expr = *i;
          stream << ", ";
          print_arg(expr);

          // Check the second argument's type: If it is a string
          // we throw in a third argument which is the length of
          // that string.
          if (expr->is(Expr::CONST)) {
            Expr::Const* constantExpr = dynamic_cast<Expr::Const*> (expr);
            Const::Base* constant = constantExpr->base;
            if (constant->is(Const::STRING)) {
              Const::String* str = dynamic_cast<Const::String*> (constant);
              std::string* string = str->s;
              stream << ", ";
              unsigned int literalLength = literal_size_of_string(
                expr->location, string);
              stream << literalLength;
            }
          }

          // Consume the last function parameter, becuase we have
          // already written it out to the stream.
          i++;
        }
      }
    }
  } else {
    stream << indent() << **i << '.' << stmt.name() << "(";
    ++i;
  }
  if (i != stmt.args.end()) {
    print_arg(*i);
    for (++i; i != stmt.args.end(); ++i) {
      stream << ", ";
      print_arg(*i);
    }
  }
  stream << ");";
}


void Printer::Cpp::print(const Statement::Block &stmt) {
  stream << indent() << "{" << endl;
  inc_indent();
  for (std::list<Statement::Base*>::const_iterator i = stmt.statements.begin();
       i != stmt.statements.end(); ++i) {
    stream << **i << endl;
  }
  dec_indent();
  stream << indent() << "}" << endl;
}


void Printer::Cpp::print(const std::list<Type::Base*> &types,
                         const std::list<std::string*> &names) {
  stream << '(';
  std::list<std::string*>::const_iterator j = names.begin();
  std::list<Type::Base*>::const_iterator i = types.begin();
  if (i != types.end() && j != names.end()) {
    stream << **i << ' ' << **j;
  }
  ++i; ++j;
  for (; i != types.end() && j != names.end(); ++i, ++j) {
    stream << ", " << **i << ' ' << **j;
  }
  stream << ')';
}


#include "para_decl.hh"


void Printer::Cpp::print(Para_Decl::Base *p) {
  assert(p);
  Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(p);
  if (s) {
    stream << *s->type() << ' ' << *s->name();
    return;
  }

  Para_Decl::Multi *m = dynamic_cast<Para_Decl::Multi*>(p);
  std::list<Para_Decl::Simple*>::const_iterator i = m->list().begin();
  print(*i);
  ++i;
  for (; i != m->list().end(); ++i) {
    stream << ", ";
    print(*i);
  }
  assert(m);
}


void Printer::Cpp::print(const std::list<Para_Decl::Base*> &paras) {
  // stream << '(';

  if (!paras.empty()) {
    std::list<Para_Decl::Base*>::const_iterator i = paras.begin();
    print(*i);
    ++i;
    for (; i != paras.end(); ++i) {
      stream << ", ";
      print(*i);
    }
  }
  // stream << ')';
}


void Printer::Cpp::print(const Fn_Def &fn_def) {
  if (fn_def.disabled())
    return;

  if (fn_def.adaptor)
    stream << *fn_def.adaptor;

  if (fn_def.comparator) {
            stream << *fn_def.comparator;
        }
        if (fn_def.sorter) {
            stream << *fn_def.sorter;
        }


  if (fn_def.choice_fn && fn_def.types.front()->is(Type::RANGE)) {
    assert(fn_def.types.size() == 1 || fn_def.types.size() == 2);
    choice_range = dynamic_cast<Type::Range*>(fn_def.types.front());
    stream << "template <typename Iterator>" << endl;
    stream << indent() << *fn_def.return_type << ' ';
    if (!fwd_decls && !in_class) {
      stream << class_name << "::";
    }
    stream << fn_def.target_name();
    stream << '(';
    stream << "std::pair<Iterator, Iterator> " << *fn_def.names.front();
    if (fn_def.types.size() > 1) {
      std::list<std::string*>::const_iterator a = fn_def.names.begin();
      ++a;
      std::list<Type::Base*>::const_iterator b = fn_def.types.begin();
      ++b;
      for ( ; a != fn_def.names.end(); ++a, ++b)
      stream << ", " << **b << ' ' << **a;
    }
    stream << ')' << endl;
  } else {
    stream << indent() << *fn_def.return_type << ' ';
    if (!fwd_decls && !in_class) {
      stream << class_name << "::";
    }
    if (fn_def.target_name().empty()) {
      stream << *fn_def.name;
    } else {
      stream << fn_def.target_name();
    }
    if (!fn_def.choice_fn) {
      in_fn_head = true;
    }

    stream << '(';
    print(fn_def.paras);
    if (!fn_def.paras.empty() && !fn_def.ntparas().empty()) {
      stream << ", ";
    }
    print(fn_def.ntparas());
    stream << ')';
    if (!fn_def.choice_fn) {
      in_fn_head = false;
    }
  }
  if (fwd_decls) {
    stream << ';' << endl;
    return;
  }
  stream << ' ' << '{' << endl;
  inc_indent();
  lines_start_mark(fn_def.stmts);
  for (std::list<Statement::Base*>::const_iterator s = fn_def.stmts.begin();
       s != fn_def.stmts.end(); ++s) {
    stream << **s << endl;
  }
  lines_end_mark(fn_def.stmts);
  dec_indent();
  stream << indent() << '}' << endl;
  choice_range = NULL;
}


void Printer::Cpp::print(const Operator &op) {
    if (!fwd_decls) {
        return;
    }

    stream << indent();
    stream << "struct " << *op.name << " {" << endl;


    for (std::list<Statement::Var_Decl*>::const_iterator i =
         op.const_values.begin(); i!= op.const_values.end(); ++i) {
        stream <<  indent() << "static const ";
        stream << **i << endl;
    }

    stream << indent() << *op.return_type << " operator () (";
    print(op.paras);
    stream << ") {" << endl;

    for (std::list<Statement::Base*>::const_iterator i = op.stmts.begin();
         i != op.stmts.end(); ++i) {
        stream << indent() << "    " << **i << endl;
    }

    stream << indent() << "   }" << endl;
    stream << indent() << "} " <<  *op.object << " ;" << endl;
}



void Printer::Cpp::lines_start_mark(const std::list<Statement::Base*> &stmts) {
  if (stmts.empty()) {
    return;
  }
  if (stmts.front()->location.begin.column ==
      stmts.front()->location.end.column) {
    return;
  }
  stream << "#line " << stmts.front()->location.begin.line <<
    " \"" << in_name << "\"" << endl;
}


void Printer::Cpp::lines_end_mark(const std::list<Statement::Base*> &stmts) {
  if (stmts.empty()) {
    return;
  }
  if (stmts.front()->location.begin.column ==
      stmts.front()->location.end.column) {
    return;
  }
  stream << "#line " << line_number + 2 << " \"" << out_name << "\"" << endl;
}


void Printer::Cpp::print(const Expr::Base &expr) {
  // FIXME
  if (expr.is(Expr::NEW)) {
    print(*dynamic_cast<const Expr::New*>(&expr));
    return;
  }
  // Default pretty print
  external_out() << expr;
  // if needed use dispatch code like for statement
  // which is called by base class
}


void Printer::Cpp::print(const Expr::New &expr) {
  pointer_as_itr = true;
  stream << "new " << *expr.obj() << '(';
  std::list<Expr::Base*>::const_iterator i = expr.args().begin();
  if (i != expr.args().end()) {
    stream << **i;
    ++i;
  }
  for (; i != expr.args().end(); ++i) {
    stream << ", " << **i;
  }
  stream << ')';
  pointer_as_itr = false;
}


void Printer::Cpp::print(const Var_Acc::Base &b) {
  // Default pretty print
  external_out() << b;
  // if needed use dispatch code like for statement
  // which is called by base class
}


void Printer::Cpp::print(const Type::List &t) {
  if (t.push_type() > Type::List::NORMAL &&
      t.push_type() < Type::List::MIN_OTHER) {
    stream << *t.of;
    return;
  }
  if (t.push_type() == Type::List::HASH) {
    if (in_fn_head) {
      stream << " const ";
    }
    stream << t.hash_decl().name();
    if (in_fn_head) {
      stream << " & ";
    }
    return;
  }

  bool old = in_fn_head;
  in_fn_head = false;
  stream << "List_Ref<" << *t.of << ">";
  in_fn_head = old;
}


void Printer::Cpp::print(const Type::Tuple &t) {
  assert(t.list.size() == 2);
  bool flag = in_fn_head;
  if (in_fn_head) {
    in_fn_head = false;
  }
  if (flag) {
    stream << "const ";
  }
  stream << "std::pair<";
  std::list<std::pair<Type::Name *, std::string*>*>::const_iterator i =
    t.list.begin();
  stream << *(*i)->first->lhs << ", ";
  ++i;
  stream << *(*i)->first->lhs;
  stream << "> ";
  if (flag) {
    stream << '&';
  }
  in_fn_head = flag;
}


void Printer::Cpp::print(const Type::TupleDef &t) {
  stream << t.name;
}


void Printer::Cpp::print(const Type::Signature &t) {
  external_out() << t;
}


void Printer::Cpp::print(const Type::Alphabet &t) {
  if (t.temp) {
    stream << *t.temp;
    return;
  }
  external_out() << t;
}


void Printer::Cpp::print(const Type::Def &t) {
  stream << *t.name;
}


void Printer::Cpp::print(const Type::Choice &t) {
  external_out() << t;
}


void Printer::Cpp::print(const Type::Void &t) {
  stream << "bool";
}


void Printer::Cpp::print(const Type::RealVoid &t) {
  stream << "void";
}


void Printer::Cpp::print(const Type::Int &t) {
  external_out() << t;
}


void Printer::Cpp::print(const Type::Integer &t) {
  stream << "uint64_t";
}


void Printer::Cpp::print(const Type::Size &t) {
  // FIXME
  stream << "unsigned int";
}


void Printer::Cpp::print(const Type::Float &t) {
  stream << "double";
}


void Printer::Cpp::print(const Type::Single &t) {
  stream << "float";
}


void Printer::Cpp::print(const Type::String &t) {
  if (in_fn_head) {
    stream << "const String &";
  } else {
    stream << "String";
  }
}


void Printer::Cpp::print(const Type::Char &t) {
  external_out() << t;
}


void Printer::Cpp::print(const Type::Bool &t) {
  external_out() << t;
}


void Printer::Cpp::print(const Type::Usage &t) {
  stream << *t.base;
}


void Printer::Cpp::print(const Type::Range &t) {
  if (t.original_tuple) {
    assert(t.component != Type::Range::NONE);
    std::string x;
    if (t.component == Type::Range::LEFT) {
      x = "Proxy::Iterator<Iterator, select1st<typename Iterator::"
          "value_type> > ";
    } else {
      x = "Proxy::Iterator<Iterator, select2nd<typename Iterator::"
          "value_type> > ";
    }
    stream << "std::pair<" << x << " ," << x << "> ";
    return;
  }
  if (choice_range) {
    if (choice_range->is_eq(t)) {
      stream << "std::pair<Iterator, Iterator>";
      return;
    }
  }
  stream << "std::pair<" << "List<" << *t.element_type << ">::iterator";
  stream << ", " << "List<" << *t.element_type << ">::iterator" << ">";
}


void Printer::Cpp::print(const Type::Seq &t) {
  if (t.element_type) {
    stream << "Basic_Sequence<" << *t.element_type << '>';
  } else {
    stream << "Sequence";
  }
}


void Printer::Cpp::print(const Type::Table &t) {
  assert(t.table->type() != Table::NONE);
  // FIXME extra case for bounded() && QUADRATIC -> 'DIAG_LINEAR' ...
  // see also table.hh | elmamun times/plus are example nt for this ...
  switch (t.table->type()) {
    case Table::CONSTANT :
      stream << "Table::Constant<" << *t.element_type;
      break;
    case Table::LINEAR :
      assert(t.table->sticky() != Table::NO_INDEX);
      if (t.table->sticky() == Table::LEFT) {
        stream << "Table::Linear<Table::Left, " << *t.element_type;
      } else {
        stream << "Table::Linear<Table::Right, " << *t.element_type;
      }
      break;
    case Table::QUADRATIC :
      stream << "Table::Quadratic<" << *t.element_type;
      break;
    default:
      assert(0);
      std::abort();
      break;
  }
  if (t.cyk) {
    stream << ", Table::CYK> ";
  } else {
    stream << " > ";
  }
}


#include "statement/table_decl.hh"


void Printer::Cpp::print(const std::list<Statement::Var_Decl*> &l) {
  for (std::list<Statement::Var_Decl*>::const_iterator i = l.begin();
       i != l.end(); ++i) {
    stream << **i << "\n";
  }
}


void Printer::Cpp::print_paras(
  const std::list<Statement::Var_Decl*> &l, char c) {
  std::list<Statement::Var_Decl*>::const_iterator i = l.begin();
  stream << *(*i)->type << ' ' << *(*i)->name  << c;
  ++i;
  for (; i != l.end(); ++i) {
    stream << ", " << *(*i)->type << ' ' << *(*i)->name << c;
  }
}


void Printer::Cpp::print_names(
  const std::list<Statement::Var_Decl*> &l, char c) {
  std::list<Statement::Var_Decl*>::const_iterator i = l.begin();
  stream << *(*i)->name << c;
  ++i;
  for (; i != l.end(); ++i) {
    stream << ", " << *(*i)->name << c;
  }
}


void Printer::Cpp::print_eqs(const std::list<Statement::Var_Decl*> &l, char c) {
  for (std::list<Statement::Var_Decl*>::const_iterator i = l.begin();
       i != l.end(); ++i) {
    stream << indent() << *(*i)->name << " = " << *(*i)->name << c << ";";
    stream << endl;
  }
}


void Printer::Cpp::print_most_decl(const Symbol::NT &nt) {
  ::Type::Base *type = new Type::Size();
  for (size_t t = nt.track_pos(); t < nt.track_pos() + nt.tracks(); ++t) {
    stream << indent() << *type << " t_" << t << "_left_most;" << endl;
    stream << indent() << *type << " t_" << t << "_right_most;" << endl;
  }
}


void Printer::Cpp::print_window_inc(const Symbol::NT &nt) {
  static const char w[] =
    "void window_increment()\n{\n"
    "unsigned inc = winc;\n"
    "if (t_0_left_most + winc > t_0_n) {\n"
    "  inc = std::min(t_0_n - t_0_left_most, winc);\n"
    "  assert(inc);\n"
    "}\n"
    "for (unsigned i = t_0_left_most; i < t_0_left_most + inc; ++i)\n"
    "  for (unsigned j = i; j <= t_0_right_most; ++j) {\n"
    "    un_tabulate(";
    static const char u[] = ");\n"
    "  }\n"
    "t_0_left_most += inc;\n"
    "t_0_right_most = std::min(t_0_right_most + inc, t_0_n);\n"
    "}\n\n";
  stream << w;
  if (!nt.tables()[0].delete_left_index() &&
      !nt.tables()[0].delete_right_index()) {
    stream << "i, j";
  }
  if (!nt.tables()[0].delete_left_index() &&
      nt.tables()[0].delete_right_index()) {
    stream << "i";
  }
  if (nt.tables()[0].delete_left_index() &&
      !nt.tables()[0].delete_right_index()) {
    stream << "j";
  }
  stream << u;
}


void Printer::Cpp::print(const Statement::Table_Decl &t) {
  in_class = true;
  bool wmode = ast && ast->window_mode;
  bool pytorch = ast && ast->as_pytorch_module;

  std::string tname(t.name() + "_t");
  const Type::Base &dtype = t.datatype();
  const Type::Base &ptype = t.pos_type();
  bool cyk = t.cyk();
  const std::list<Statement::Var_Decl*> &ns = t.ns();

  stream << indent() << "class " << tname << " {" << endl;
  inc_indent();

  dec_indent();
  stream << indent() << " private:" << endl;
  inc_indent();

  if (wmode) {
    stream << indent() << "unsigned wsize;" << endl;
    stream << indent() << "unsigned winc;" << endl;
  }

  print_most_decl(t.nt());

  stream << indent() << "std::vector<" << dtype << "> array;" << endl;
  if (t.for_derivatives) {
    stream << indent() << "std::vector<std::vector<std::tuple<std::string, "
           << "std::vector<unsigned int>, " << dtype << "> > > traces;" << endl;
  }
  if  (!cyk) {
    stream << indent() << "std::vector<bool> tabulated;" << endl;
  }
  print(ns);
  stream << indent() << dtype << " zero;" << endl;

  stream << t.fn_size() << endl;

  dec_indent();
  stream << indent() << " public:" << endl;
  inc_indent();

  stream << indent() << tname << "() {" << endl;
  inc_indent();
  stream << indent() << "empty(zero);" << endl;
  dec_indent();
  stream << indent() << "}" << endl << endl;

  if (pytorch) {
    stream << indent() << dtype << " *get_array_data() {" << endl;
    inc_indent();
    stream << indent() << "return array.data();" << endl;
    dec_indent();
    stream << indent() << "}" << endl << endl;
  }

  // start "void init()"
  stream << indent() << "void init(";
  print_paras(ns, '_');

  if (wmode) {
    stream << ", unsigned wsize_, unsigned winc_";
  }

  stream << ", const std::string &tname";
  stream << ") {" << endl;
  inc_indent();
  print_eqs(ns, '_');

  for (size_t track = t.nt().track_pos();
       track < t.nt().track_pos() + t.nt().tracks(); ++track) {
    stream << indent() << "t_" << track << "_left_most = 0;" << endl;
    stream << indent() << "t_" << track << "_right_most = t_";
    stream << track << "_n;" << endl;
  }

  if (wmode) {
    stream << indent() << "wsize = wsize_;" << endl;
    stream << indent() << "winc = winc_;" << endl;
    stream << indent() << "t_0_right_most = wsize;" << endl;
  }

  stream << indent() << ptype << " newsize = size(";
  stream << ");" << endl;
  if (pytorch) {
    stream << indent() << "array.clear();" << endl;
    stream << indent() << "traces.clear();" << endl;
  }
  stream << indent() << "array.resize(newsize);" << endl;
  if (t.for_derivatives) {
    stream << indent() << "traces.resize(newsize);" << endl;
  }
  if (!cyk) {
    stream << indent() << "tabulated.clear();" << endl;
    stream << indent() << "tabulated.resize(newsize);" << endl;
  }
  dec_indent();
  stream << indent() << "}" << endl << endl;
  // end "void init()"

  if (wmode) {
    stream << t.fn_untab();
    print_window_inc(t.nt());
  }

  if (!cyk) {
    stream << t.fn_is_tab() << endl;
    // needed by subopt classify
    stream << indent() << "void clear() {" << endl;
    inc_indent();
    stream << indent() << "tabulated.clear();" << endl;
    dec_indent();
    stream << indent() << "}" << endl << endl;
  }

  stream << t.fn_get_tab() << endl;

  stream << t.fn_tab();

  if (t.for_derivatives && *t.nt().name != OUTSIDE_AXIOMS) {
    stream << endl << t.fn_set_traces();
    stream << endl << t.fn_get_traces();
  }

  dec_indent();
  stream << indent() << "};" << endl;
  stream << indent() << tname << ' ' << t.name() << ";" << endl;

  in_class = false;
}


void Printer::Cpp::print(const Type::Subseq &t) {
  if (in_fn_head) {
    stream << "const TUSubsequence &";
  } else {
    stream << "TUSubsequence";
  }
}


void Printer::Cpp::print(const Type::Shape &t) {
  if (in_fn_head) {
    stream << "const Shape &";
  } else {
    stream << "Shape";
  }
}


void Printer::Cpp::print(const Type::Referencable &t) {
  stream << *t.base << " & ";
}


void Printer::Cpp::print(const Type::Rational &t) {
  if (in_fn_head) {
    stream << "const Rational &";
  } else {
    stream << "Rational";
  }
}


void Printer::Cpp::print(const Type::BigInt &t) {
  if (in_fn_head) {
    stream << "const BigInt &";
  } else {
    stream << "BigInt";
  }
}


void Printer::Cpp::print(const Type::External &t) {
  if (in_fn_head) {
    stream << "const " << *t.name << " &";
  } else {
    stream << *t.name;
  }
}


void Printer::Cpp::print(const Type::Eval_List &t) {
  /*
  stream << "Eval_List<Value>";
  if (!pointer_as_itr)
  stream << " *";
  */
  if (pointer_as_itr) {
    stream << "Eval_List<Value>";
  } else {
    stream << "intrusive_ptr<Eval_List<Value> >";
  }
}


void Printer::Cpp::print(const Type::Backtrace &t) {
  bool old_in_fn_head = in_fn_head;
  in_fn_head = false;
  switch (t.subclass()) {
    case Type::Backtrace::NONE :
      if (pointer_as_itr) {
        stream << "Backtrace<Value, pos_int> ";
      } else {
        stream << "intrusive_ptr<Backtrace<Value, pos_int> > ";
      }
      break;
    case Type::Backtrace::FN :
      stream << "Backtrace_" << *t.name() << "<Value, pos_int> ";
      break;
    case Type::Backtrace::FN_USE :
      stream << "Backtrace_" << *t.name() << "<" << *t.value_type() << ", ";
      stream << *t.pos_type() << "> ";
      break;
    case Type::Backtrace::FN_SPEC :
      if (pointer_as_itr) {
        stream << "Backtrace" << "<" << *t.value_type() << ", ";
        stream << *t.pos_type() << "> ";
      } else {
        stream << "intrusive_ptr<Backtrace" << "<" << *t.value_type() << ", ";
        stream << *t.pos_type() << "> > ";
      }
      break;
    case Type::Backtrace::NT_BACKEND :
      if (t.body_context()) {
        stream << "intrusive_ptr<";
      }
      stream << "Backtrace_" << *t.name() << "_Back<" << class_name << ", "
      << *t.value_type() << ", " << *t.pos_type() << "> ";
      if (t.body_context()) {
        stream << " > ";
      }
      break;
    case Type::Backtrace::NT_FRONTEND :
      stream << "Backtrace_" << *t.name() << "_Front<" << *t.value_type();
      stream << ", " << *t.pos_type() << "> ";
      break;
    case Type::Backtrace::NT :
      stream << "Backtrace_" << *t.name() << "<" << class_name << ", ";
      stream << *t.value_type() << ", " << *t.pos_type() << "> ";
      break;
    default:
      assert(false);
  }
  if (!pointer_as_itr && t.subclass() != Type::Backtrace::NONE &&
      t.subclass() != Type::Backtrace::FN_SPEC && !t.body_context()) {
    stream << " *";
  }
  in_fn_head = old_in_fn_head;
}


void Printer::Cpp::print(const Type::Backtrace_List &t) {
  stream << "Backtrace_List<Value, pos_int> ";
  if (!pointer_as_itr) {
    stream << " *";
  }
}


void Printer::Cpp::print(const Type::Multi &t) {
  external_out() << t;
}


void Printer::Cpp::print_type_defs(const AST &ast) {
  for (std::list<Type::Base*>::const_iterator i = ast.type_def_list.begin();
       i != ast.type_def_list.end(); ++i) {
    Type::Base *t = *i;
    if (t->is(Type::DEF)) {
      Type::Def *def = dynamic_cast<Type::Def*>(t);
      assert(def);
      if (def->rhs->is(Type::TUPLEDEF)) {
        stream << indent() << "struct " << *def->name << " {" << endl;
        Type::TupleDef *tuple = dynamic_cast<Type::TupleDef*>(def->rhs);
        assert(tuple);
        inc_indent();
        for (std::list<std::pair<Type::Name*, std::string*>*>::const_iterator
             i = tuple->list.begin(); i != tuple->list.end(); ++i) {
          stream << indent() << *(*i)->first->lhs << ' ' << *(*i)->second <<
            ';' << endl;
        }
        stream << indent() << "bool empty_;" << endl << indent() << *def->name
          << "() : empty_(false) {}" << endl;
        dec_indent();

        // FIXME let user decide how to compare user defined tuples ...
        if (tuple->list.front()->first->lhs->const_simple()->is(Type::INT) ||
            tuple->list.front()->first->lhs->const_simple()->is(Type::FLOAT) ||
            tuple->list.front()->first->lhs->const_simple()->is(Type::SINGLE)) {
          stream << "bool operator>(const " << *def->name << "& other) const {"
            << " return " << *tuple->list.front()->second << " > "
            << "other." << *tuple->list.front()->second << "; }" << endl;
          stream << "bool operator<(const " << *def->name << "& other) const {"
            << " return " << *tuple->list.front()->second << " < "
            << "other." << *tuple->list.front()->second << "; }" << endl;
          stream << "bool operator==(const " << *def->name
            << "& other) const {"
            << " return " << *tuple->list.front()->second << " == "
            << "other." << *tuple->list.front()->second << "; }" << endl;
          stream << "template <typename T> bool operator>(const T &other) "
            << "const {"
            << "return " << *tuple->list.front()->second << " > other; }"
            << endl;
          stream << "template <typename T> bool operator<(const T &other) "
            << "const {"
            << "return " << *tuple->list.front()->second << " < other; }"
            << endl;
          stream << "template <typename T> bool operator==(const T &other) "
            << "const {"
            << "return " << *tuple->list.front()->second << " == other; }"
            << endl;

          // Subopt bt operators
          stream << endl << endl;
          stream << *def->name << "(int i) : " << *tuple->list.front()->second
            << "(i), empty_(false) {}" << endl;
          stream << *def->name << " operator+(const " << *def->name
            << " &other) const" << endl << '{' << endl
            << "assert(!empty_); assert(!other.empty_);" << endl
            << "return " << *def->name << '(' << *tuple->list.front()->second
            << " + other." << *tuple->list.front()->second << ");" << endl
            << '}' << endl;
          stream << *def->name << " operator-(const " << *def->name
            << " &other) const" << endl << '{' << endl
            << "assert(!empty_);" << endl
            << "if (other.empty_) return " << *def->name << '('
            << *tuple->list.front()->second << ");" << endl
            << "return " << *def->name << '(' << *tuple->list.front()->second
            << " - other." << *tuple->list.front()->second << ");" << endl
            << '}' << endl;
          stream << "bool operator<=(const " << *def->name
            << "& other) const {"
            << endl
            << "assert(!empty_); assert(!other.empty_);" << endl
            << "return " << *tuple->list.front()->second << " <= "
            << "other." << *tuple->list.front()->second << ";"
            << endl << "}" << endl;
        }

        stream << endl << indent() << "};" << endl << endl;

        stream << indent()
          << "inline std::ostream &operator<<(std::ostream &o, const "
          << *def->name << " &tuple) {" << endl;
        inc_indent();
        stream << indent() << "o << '('";
        assert(!tuple->list.empty());
        std::list<std::pair<Type::Name*, std::string*>*>::const_iterator j =
          tuple->list.begin();
        stream << indent() << " << tuple." << *(*j)->second;
        ++j;
        for ( ; j != tuple->list.end(); ++j) {
          stream << indent() << " << \", \" << tuple." << *(*j)->second
            << endl;
        }
        stream << indent() << " << ')' ;" << endl;
        stream << indent() << "return o;" << endl;
        dec_indent();
        stream << indent() << '}' << endl << endl;
        stream << "inline void empty(" << *def->name << " &e) {"
          << "e.empty_ = true; }" << endl;
        stream << "inline bool isEmpty(const " << *def->name << " &e) {"
          << " return e.empty_; }" << endl;
      } else {
        stream << indent() << "typedef " << *def->rhs << ' '
          << *def->name << ';' << endl;
      }
    }
  }
}


void Printer::Cpp::print_zero_decls(const Grammar &grammar) {
  bool old = in_class;
  in_class = true;

  std::set<std::string> seen;
  for (std::list<Symbol::NT*>::const_iterator i = grammar.nts().begin();
       i != grammar.nts().end(); ++i) {
    std::string n(*(*i)->zero_decl->name);
    if (seen.find(n) != seen.end()) {
      continue;
    }
    seen.insert(n);
    stream << *(*i)->zero_decl << endl;
  }
  stream << endl;

  in_class = old;
}


void Printer::Cpp::print_table_decls(const Grammar &grammar) {
  for (hashtable<std::string, Symbol::NT*>::const_iterator i =
       grammar.tabulated.begin();
       i != grammar.tabulated.end(); ++i)
    stream << *i->second->table_decl << endl;
}


void Printer::Cpp::print_seq_init(const AST &ast) {
  assert(inps.size() == ast.input.modes().size());
  std::vector<Input::Mode>::const_iterator l = ast.input.modes().begin();

  assert(inps.size() == ast.seq_decls.size());

  stream << indent() << "if (inp.size() != " << ast.seq_decls.size() << ")\n"
    << indent() << indent() << "throw gapc::OptException(\"Number of input "
    << "sequences does not match.\");\n\n";

  size_t track = 0;
  for (std::vector<Statement::Var_Decl*>::const_iterator
       i = ast.seq_decls.begin(); i != ast.seq_decls.end();
       ++i, ++l, ++track) {
    stream << indent() << *(*i)->name << ".copy("
      << "inp[" << track << "].first"
      << ", "
      << "inp[" << track << "].second"
      << ");\n";

    switch (*l) {
      case Input::RAW:
        break;
      case Input::RNA:
        stream << indent() << "char_to_rna(" << *(*i)->name << ");\n";
        break;
      case Input::UPPER:
        stream << indent() << "char_to_upper(" << *(*i)->name << ");\n";
        break;
      default:
        assert(false);
    }
  }
}


void Printer::Cpp::print_filter_decls(const AST &ast) {
  for (std::list<std::pair<Filter*, Expr::Fn_Call*> >::const_iterator i =
       ast.sf_filter_code.begin(); i != ast.sf_filter_code.end(); ++i) {
    Filter *f = (*i).first;
    stream << *f->name << "_filter<char, unsigned> " << f->id() << ";\n";
  }
}


void Printer::Cpp::print_filter_init(const AST &ast) {
  for (std::list<std::pair<Filter*, Expr::Fn_Call*> >::const_iterator i =
       ast.sf_filter_code.begin(); i != ast.sf_filter_code.end(); ++i) {
    Filter *f = (*i).first;
    Expr::Fn_Call *fn = (*i).second;
    stream << f->id() << "." << *fn << ";\n";
  }
}


void Printer::Cpp::print_table_init(const AST &ast) {
  for (hashtable<std::string, Symbol::NT*>::const_iterator i =
       ast.grammar()->tabulated.begin(); i != ast.grammar()->tabulated.end();
       ++i) {
    stream << indent() << i->second->table_decl->name() << ".init(";
    size_t a = 0;
    for (std::vector<Statement::Var_Decl*>::const_iterator j =
         ast.seq_decls.begin(); j != ast.seq_decls.end(); ++j, ++a) {
      if (a < i->second->track_pos() ||
          a >= i->second->track_pos() + i->second->tracks()) {
        continue;
      }
      stream << *(*j)->name << ".size(), ";
    }
    if (ast.window_mode) {
      stream << " opts.window_size, opts.window_increment, ";
    }
    stream << "\""<< i->second->table_decl->name() << "\");" << endl;
  }
}


void Printer::Cpp::print_zero_init(const Grammar &grammar) {
  std::set<std::string> seen;
  for (std::list<Symbol::NT*>::const_iterator i = grammar.nts().begin();
       i != grammar.nts().end(); ++i) {
    std::string n(*(*i)->zero_decl->name);
    if (seen.find(n) != seen.end()) {
      continue;
    }
    seen.insert(n);
    stream << indent() << "empty(" << *(*i)->zero_decl->name << ");\n";
  }
  stream << endl;
}


void Printer::Cpp::print_buddy_init(const AST &ast) {
  if (!ast.code_mode().subopt_buddy()) {
    return;
  }

  stream << "buddy = new " << class_name << "_buddy();" << endl
    << "buddy->init(opts);" << endl
    << "buddy->cyk();" << endl
    << "buddy->print_subopt(std::cout, opts.delta);" << endl << endl;
  for (std::list<Symbol::NT*>::const_iterator i = ast.grammar()->nts().begin();
       i != ast.grammar()->nts().end(); ++i) {
    stream << "marker_nt_" << *(*i)->name << " = &buddy->marker_nt_"
      << *(*i)->name << ';' << endl;
  }
  stream << endl << endl;
}


void Printer::Cpp::set_tracks(const AST &ast) {
  ns.clear();
  inps.clear();
  for (size_t i = 0; i < ast.grammar()->axiom->tracks(); ++i) {
    std::ostringstream n, inp;
    n << "t_" << i << "_n";
    Statement::Var_Decl *nv = new Statement::Var_Decl(
      new ::Type::Size(), new std::string(n.str()));
    ns.push_back(nv);
    inp << "t_" << i << "_inp";
    inps.push_back(inp.str());
  }
}


void Printer::Cpp::print_most_init(const AST &ast) {
  size_t t = 0;
  for (std::vector<Statement::Var_Decl*>::iterator j = ns.begin();
       j != ns.end(); ++j, ++t) {
    stream << indent() << "t_" << t << "_left_most = 0;\n";
    stream << indent() << "t_" << t << "_right_most = " << "t_" << t
      << "_seq.size();\n";
  }
  if (ast.window_mode) {
    stream << indent() << "t_0_right_most = opts.window_size;\n";
  }
}


void Printer::Cpp::print_init_fn(const AST &ast) {
  stream << indent() << "void init(";
  stream << "const gapc::Opts &opts";
  for (unsigned int i = 1; i < ast.current_derivative; ++i) {
    stream << ", " << get_class_name_lower_derivative(ast.current_derivative, i)
           << " *derivative" << std::to_string(i);
  }
  stream << ") {" << endl;

  inc_indent();
  stream << indent() << "const std::vector<std::pair<const char *, unsigned> >"
    << " &inp = opts.inputs;" << endl << endl;

  print_buddy_init(ast);
  print_seq_init(ast);
  print_filter_init(ast);
  print_table_init(ast);
  print_zero_init(*ast.grammar());
  print_most_init(ast);
  if (ast.window_mode)
  stream << "wsize = opts.window_size;\nwinc = opts.window_increment;\n";

  if (ast.kbest) {
    for (std::list<Statement::Hash_Decl*>::const_iterator i =
         ast.hash_decls().begin(); i != ast.hash_decls().end(); ++i) {
      stream << (*i)->ext_name() << "::set_k(opts.k);\n";
    }
  }

  if (ast.requested_derivative > 0) {
    stream << endl;
  }
  for (unsigned int i = 1; i < ast.current_derivative; ++i) {
    stream << indent() << "this->derivative" << std::to_string(i)
           << " = derivative" << std::to_string(i) << ";" << endl;
  }

  dec_indent();
  stream << indent() << '}' << endl << endl;
}

void Printer::Cpp::print_window_inc_fn(const AST &ast) {
  if (!ast.window_mode) {
    return;
  }

  stream << "void window_increment()" << endl << '{' << endl;

  inc_indent();
  for (hashtable<std::string, Symbol::NT*>::const_iterator i =
       ast.grammar()->tabulated.begin();
       i != ast.grammar()->tabulated.end(); ++i) {
    stream << indent() << i->second->table_decl->name()
      << ".window_increment();" << endl;
  }

  stream << "t_0_left_most += winc;\n" <<
    "t_0_right_most = std::min(t_0_seq.size(), t_0_left_most + wsize);\n";


  dec_indent();

  stream << '}' << endl << endl;
}


void Printer::Cpp::includes() {
  stream << "#include \"rtlib/adp.hh\"" << endl << endl;
}


void Printer::Cpp::print_hash_decls(const AST &ast) {
  const std::list<Statement::Hash_Decl*> &h = ast.hash_decls();
  for (std::list<Statement::Hash_Decl*>::const_iterator i = h.begin();
       i != h.end(); ++i) {
    stream << **i << endl << endl;
  }
}


void Printer::Cpp::print_buddy_decls(const AST &ast) {
  if (!ast.code_mode().subopt_buddy()) {
    return;
  }

  stream << class_name << "_buddy *buddy;" << endl;
  stream << '~' << class_name << "()" << endl
    << '{' << endl
    << "  delete buddy;" << endl
    << '}' << endl << endl;

  for (std::list<Symbol::NT*>::const_iterator i = ast.grammar()->nts().begin();
       i != ast.grammar()->nts().end(); ++i) {
    stream << "Marker<unsigned int> *marker_nt_" << *(*i)->name << ';' << endl;
  }
  stream << endl;
}


void Printer::Cpp::print_subseq_typedef(const AST &ast) {
  hashtable<std::string, Type::Base*>::const_iterator i = ast.types.find(
    "alphabet");
  assert(i != ast.types.end());
  Type::Base *t = dynamic_cast<Type::Alphabet*>(i->second)->temp;
  assert(t);

  stream << "typedef Basic_Subsequence<" << *t
    << ", unsigned> TUSubsequence;\n\n";
}


void Printer::Cpp::header(const AST &ast) {
  if (!ast.code_mode().subopt_buddy()) {
    stream << endl << make_comments(id_string, "//") << endl << endl;
    stream << "#ifndef " << class_name << "_hh" << endl
      << "#define " << class_name << "_hh" << endl << endl;
    if (ast.window_mode) {
      stream << "#define WINDOW_MODE\n";
    }
    if (ast.code_mode().sample()) {
      stream << "#define USE_GSL\n";
    }
    if (ast.get_float_acc() > 0) {
      stream << "#define FLOAT_ACC " << ast.get_float_acc() << "\n";
    }
    if ((*ast.grammar()).is_outside()) {
      stream << "#define OUTSIDE\n";
      if (ast.current_derivative == 1) {
        stream << "#define DERIVATIVES\n";
      } else if (ast.current_derivative == 2) {
        stream << "#define SECOND_DERIVATIVE\n";
      }
    }

    stream << "#define GAPC_CALL_STRING \"" << gapc_call_string << "\""
           << endl;
    stream << "#define GAPC_VERSION_STRING \"" << gapc_version_string << "\""
           << endl << endl;
    includes();
    if (ast.as_pytorch_module) {
      stream << "#include \"torch/extension.h\"" << endl;
    }
    print_subseq_typedef(ast);
    print_type_defs(ast);
  }

  imports(ast);

  print_hash_decls(ast);

  stream << indent() << "class " << class_name << " {" << endl;

  stream << indent() << " public:" << endl;
  inc_indent();
  if (ast.as_pytorch_module) {
    stream << indent() << "std::vector<int64_t> tensor_size;"
           << endl;
  }

  for (std::vector<Statement::Var_Decl*>::const_iterator i =
       ast.seq_decls.begin(); i != ast.seq_decls.end(); ++i) {
    stream << **i << endl;
  }

  print_most_decl(*ast.grammar()->axiom);

  if (ast.window_mode) {
    stream << indent() << "unsigned wsize;" << endl;
    stream << indent() << "unsigned winc;" << endl;
  }

  /* create pointer to lower derivative results */
  for (unsigned int i = 1; i < ast.current_derivative; ++i) {
    stream << indent()
           << get_class_name_lower_derivative(ast.current_derivative, i)
           << " *derivative" << i << ";" << endl;
  }

  stream << endl;

  print_zero_decls(*ast.grammar());
  print_table_decls(*ast.grammar());
  print_filter_decls(ast);
  print_buddy_decls(ast);
  set_tracks(ast);
  if (ast.as_pytorch_module) {
    print_pytorch_init_fn(ast);
  } else {
    print_init_fn(ast);
  }
  print_window_inc_fn(ast);
  dec_indent();
  stream << indent();
  if ((ast.current_derivative > 0) &&
      (ast.current_derivative < ast.requested_derivative)) {
    // let higher derivatives access lower DP results, e.g. second needs first
    // however, last derivative can stay private
    stream << " public:";
  } else {
    stream << " private:";
  }
  stream << endl;
  inc_indent();
}


void Printer::Cpp::print_openmp_cyk_nt_calls(const AST &ast,
                                             bool for_outsideNTs) {
  std::list<Symbol::NT*> &tord = ast.grammar()->topological_ord();
  assert(tord.size() <= ast.grammar()->NTs.size());
  std::ostringstream o1;
  for (std::list<Symbol::NT*>::iterator i = tord.begin();
       i != tord.end(); ++i) {
    if ((*i)->is_partof_outside == for_outsideNTs) {
      assert(!(*i)->tables().empty());
      if ((*i)->is_tabulated() && !(*i)->tables().front().is_cyk_right()) {
        o1 << indent() << "nt_tabulate_" << *(*i)->name << "("
          << multi_index_str((*i)->tables(), (*i)->multi_ys(),
                             (*i)->is_partof_outside)
          << ");\n";
      }
    }
  }
  std::string nt_calls = o1.str();
  stream << nt_calls;
}

void Printer::Cpp::print_openmp_cyk_all_nt_calls(const AST &ast,
                                                 bool for_outsideNTs) {
  std::list<Symbol::NT*> &tord = ast.grammar()->topological_ord();
  std::ostringstream o2;
  for (std::list<Symbol::NT*>::iterator i = tord.begin();
       i != tord.end(); ++i) {
    if ((*i)->is_partof_outside == for_outsideNTs) {
      if ((*i)->is_tabulated()) {
        o2 << indent() << "nt_tabulate_" << *(*i)->name << "("
          << multi_index_str((*i)->tables(), (*i)->multi_ys(),
                             (*i)->is_partof_outside)
          << ");\n";
      }
    }
  }
  std::string all_nt_calls = o2.str();
  stream << all_nt_calls;
}

void Printer::Cpp::print_openmp_cyk_helpervars(bool for_outsideNTs) {
  stream << indent() << "unsigned int ";
  if (for_outsideNTs) {
    stream << "t_0_";
  }
  stream << "n = t_0_seq.size();" << endl;
  if (!for_outsideNTs) {
    stream << indent() << "unsigned int tile_size = 32;" << endl;
    stream << "#ifdef TILE_SIZE" << endl;
    stream << indent() << "tile_size = TILE_SIZE;" << endl;
    stream << "#endif" << endl;
    stream << indent() << "assert(tile_size);" << endl;
    stream << indent() << "unsigned int max_tiles = n / tile_size;" << endl;
    stream << indent() << "int max_tiles_n = max_tiles * tile_size;" << endl;
  }
}
void Printer::Cpp::print_openmp_cyk_loops_diag(const AST &ast,
                                               bool for_outsideNTs) {
  stream << indent() << "#pragma omp for" << endl;
  stream << indent() << "// OPENMP < 3 requires signed int here ..." << endl;
  stream << indent();
  stream << "for (int z = 0; z < max_tiles_n; z+=tile_size) {" << endl;
  inc_indent();
  stream << indent();
  stream << "for (unsigned int t_0_j = z; t_0_j < z + tile_size; ++t_0_j) {";
  stream << endl;
  inc_indent();
  stream << indent() << "for (int t_0_i = t_0_j + 1; t_0_i > z; --t_0_i) {";
  stream << endl;
  inc_indent();
  print_openmp_cyk_nt_calls(ast, for_outsideNTs);
  dec_indent();
  stream << indent() << "}" << endl;
  dec_indent();
  stream << indent() << "}" << endl;
  dec_indent();
  stream << indent() << "}" << endl;
}
void Printer::Cpp::print_openmp_cyk_loops_middle(const AST &ast,
                                                 bool for_outsideNTs) {
  stream << endl;
  stream << indent()
         << "for (int z = tile_size; z < max_tiles_n; z+=tile_size) {"
         << endl;
  inc_indent();
  stream << indent() << "#pragma omp for" << endl;
  stream << indent()
         << "for (int y = z; y < max_tiles_n; y+=tile_size) {" << endl;
  inc_indent();
  stream << indent() << "unsigned int x = y - z + tile_size;" << endl;
  stream << indent()
         << "for (unsigned int t_0_j = y; t_0_j < y + tile_size; ++t_0_j) {"
         << endl;
  inc_indent();
  stream << indent()
         << "for (unsigned int t_0_i = x - 1 + 1; t_0_i > x - tile_size;"
         << " --t_0_i) {"
         << endl;
  inc_indent();

  print_openmp_cyk_nt_calls(ast, for_outsideNTs);

  // paral_end
  dec_indent();
  stream << indent() << "}" << endl;
  dec_indent();
  stream << indent() << "}" << endl;
  dec_indent();
  stream << indent() << "}" << endl;
  dec_indent();
  stream << indent() << "}" << endl;
  dec_indent();
  stream << indent() << "}  // end parallel" << endl;
  stream << endl;
}
void Printer::Cpp::print_openmp_cyk_loops_single(const AST &ast,
                                                 bool for_outsideNTs) {
  stream << indent()
         << "for (unsigned int t_0_j = max_tiles_n; t_0_j <= n; ++t_0_j) {"
         << endl;
  inc_indent();
  stream << indent()
         << "for (unsigned int t_0_i=t_0_j+1; t_0_i > 0; --t_0_i) {" << endl;
  inc_indent();

  print_openmp_cyk_all_nt_calls(ast, for_outsideNTs);

  // single_end
  dec_indent();
  stream << indent() << "}" << endl;
  dec_indent();
  stream << indent() << "}" << endl;
}
// FIXME adjust for multi-track (> 1 track)
void Printer::Cpp::print_openmp_cyk(const AST &ast) {
  // FIXME abstract from unsigned int, int -> perhaps wait for OpenMP 3
  // since OpenMP < 3 doesn't allow unsigned int in workshared fors

  // paral_start
  stream << indent() << "#pragma omp parallel" << endl;
  stream << indent() << '{' << endl;
  inc_indent();

  // helper_vars
  print_openmp_cyk_helpervars();

  // diag
  print_openmp_cyk_loops_diag(ast);

  // middle
  print_openmp_cyk_loops_middle(ast);

  // REPEAT: helper_vars
  print_openmp_cyk_helpervars();

  // single
  print_openmp_cyk_loops_single(ast);

  // same as above but for outside non terminals
  if (ast.grammar()->is_outside()) {
    // paral_start
    stream << endl << indent() << "// repeat for outside non-terminals" << endl;
    stream << indent() << "#pragma omp parallel" << endl;
    stream << indent() << '{' << endl;
    inc_indent();

    // helper_vars
    print_openmp_cyk_helpervars(true);

    // diag
    print_openmp_cyk_loops_diag(ast, true);

    // middle
    print_openmp_cyk_loops_middle(ast, true);

    // REPEAT: helper_vars
    print_openmp_cyk_helpervars(true);

    // single
    print_openmp_cyk_loops_single(ast, true);
  }
}


std::string Printer::Cpp::multi_index_str(
  const std::vector<Table> &tables, const Yield::Multi &mys,
  bool is_outside) {
  assert(tables.size() == mys.tracks());
  std::ostringstream o;
  size_t track = 0;
  Yield::Multi::const_iterator j = mys.begin();
  for (std::vector<Table>::const_iterator i = tables.begin();
       i != tables.end(); ++i, ++j, ++track) {
    if (!(*i).delete_left_index()) {
      if (is_outside == false) {
        o << ", t_" << track << "_i-1";
      } else {
        o << ", (t_" << track << "_j - t_" << track << "_i + 1)";
      }
    }
    if (!(*i).delete_right_index()) {
      if (is_outside == false) {
        o << ", t_" << track << "_j";
      } else {
        o << ", (t_" << track << "_n - t_" << track << "_i + 1)";
      }
    }
  }
  std::string r(o.str());
  if (r.size() > 2) {
    return r.substr(2);
  } else {
    return r;
  }
}


void Printer::Cpp::multi_print_inner_cyk(
  const std::list<Symbol::NT*> &l,
  const std::list<Symbol::NT*> &tord,
  size_t track, size_t tracks, size_t track_pos, Type::Base *t,
  bool for_outsideNTs) {
  assert(track < tracks);
  if (track+1 != tracks) {
    multi_print_cyk(tord, track+1, tracks, track_pos, t);
    return;
  }
  for (std::list<Symbol::NT*>::const_iterator i = l.begin();
       i != l.end(); ++i) {
    if ((*i)->is_partof_outside == for_outsideNTs) {
      std::string index_str = multi_index_str((*i)->tables(), (*i)->multi_ys(),
                                              (*i)->is_partof_outside);
      stream << indent() << "nt_tabulate_" << *(*i)->name << '('
             << index_str << ");" << endl;
    }
  }
}


void Printer::Cpp::multi_partition_nts(
    const std::list<Symbol::NT*> &tord, std::list<Symbol::NT*> &all,
    std::list<Symbol::NT*> &inner, std::list<Symbol::NT*> &left,
    std::list<Symbol::NT*> &right,
    size_t track, size_t tracks, size_t track_pos) {
  for (std::list<Symbol::NT*>::const_iterator i = tord.begin();
       i != tord.end(); ++i) {
    if (!(*i)->is_tabulated()) {
      continue;
    }
    if ((*i)->tracks() != tracks) {
      continue;
    }
    if ((*i)->track_pos() != track_pos) {
      continue;
    }
    const Table &table = (*i)->tables()[track];

    if (!table.is_cyk_left() && !table.is_cyk_right() &&
        !table.is_cyk_const()) {
      inner.push_back(*i);
    }
    if (!table.is_cyk_right() && !table.is_cyk_const()) {
      left.push_back(*i);
    }
    if (!table.is_cyk_left() && !table.is_cyk_const()) {
      right.push_back(*i);
    }
    all.push_back(*i);
  }
}


void Printer::Cpp::multi_print_cyk_loops_quadratic(
    const std::list<Symbol::NT*> &tord,
    size_t track,
    size_t tracks,
    size_t track_pos,
    Type::Base *t,
    std::list<Symbol::NT*> *inner,
    std::list<Symbol::NT*> *left,
    std::string *is,
    std::string *js,
    std::string *ns, bool for_outsideNTs) {
  if (!inner->empty()) {
    stream << indent() << "for (" << *t << " " << *js << " = 0; " << *js
           << " < " << *ns << "; " << "++" << *js << ") {" << endl;
    inc_indent();
    stream << indent() << "for (" << *t << " " << *is << " = " << *js
           << " + 1; " << *is << " > 1; " << *is << "--) {" << endl;
    inc_indent();

    multi_print_inner_cyk(*inner, tord, track, tracks, track_pos, t,
                          for_outsideNTs);
    dec_indent();
    stream << indent() << "}" << endl << endl;

    if (!left->empty()) {
      stream << indent();
      if (!for_outsideNTs) {
        stream << *t << " ";
      }
      stream << *is << " = 1;" << endl;
      multi_print_inner_cyk(*left, tord, track, tracks, track_pos, t,
                            for_outsideNTs);
    }
    dec_indent();
    stream << indent() << "}" << endl << endl;
  }
}
void Printer::Cpp::multi_print_cyk_loops_linear(
    const std::list<Symbol::NT*> &tord,
    size_t track,
    size_t tracks,
    size_t track_pos,
    Type::Base *t,
    std::list<Symbol::NT*> *inner,
    std::list<Symbol::NT*> *left,
    std::list<Symbol::NT*> *right,
    std::string *is,
    std::string *js,
    std::string *ns, bool for_outsideNTs
    ) {
  if (inner->empty() && !left->empty()) {
    stream << indent() << "for (" << *t << " " << *js << " = 0; "
           << *js << " < " << *ns
           << "; " << "++" << *js << ") {" << endl;
    inc_indent();
    stream << indent();
    if (!for_outsideNTs) {
      stream << *t << " ";
    }
    stream << *is << " = 1;" << endl;
    multi_print_inner_cyk(*left, tord, track, tracks, track_pos, t,
                          for_outsideNTs);
    dec_indent();
    stream << indent() << "}" << endl << endl;
  }
  if (!right->empty()) {
    stream << indent();
    if (!for_outsideNTs) {
      stream << *t << " ";
    }
    stream << *js << " = " << *ns << ";" << endl;
    stream << indent() << "for (" << *t << " "<< *is << " = " << *js
           << " + 1; " << *is << " > 1; " << *is << "--) {" << endl;
    inc_indent();
    multi_print_inner_cyk(*right, tord, track, tracks, track_pos, t,
                          for_outsideNTs);
    dec_indent();
    stream << indent() << "}" << endl << endl;
  }
}
void Printer::Cpp::multi_print_cyk_loops_constant(
    const std::list<Symbol::NT*> &tord,
    size_t track,
    size_t tracks,
    size_t track_pos,
    Type::Base *t,
    std::list<Symbol::NT*> *all,
    std::string *is, bool for_outsideNTs
    ) {
  if (!all->empty()) {
    stream << indent();
    if (!for_outsideNTs) {
      stream << *t << " ";
    }
    stream << *is << " = 1;" << endl;
    multi_print_inner_cyk(*all, tord, track, tracks, track_pos, t,
                          for_outsideNTs);
  }
}
void Printer::Cpp::multi_print_cyk(
  const std::list<Symbol::NT*> &tord, size_t track, size_t tracks,
  size_t track_pos, Type::Base *t) {
  std::list<Symbol::NT*> all, inner, left, right;
  multi_partition_nts(tord, all, inner, left, right, track, tracks, track_pos);

  size_t real_track = !track_pos ? track : track_pos;

  std::ostringstream sis, sjs, sns;
  sis << "t_" << track << "_i";
  sjs << "t_" << track << "_j";
  sns << "t_" << real_track << "_n";
  std::string is(sis.str()), js(sjs.str()), ns(sns.str());

  stream << indent() << *t << " t_" << track << "_n = t_" << real_track
    << "_seq.size();" << endl << endl;

  // inside NTs
  multi_print_cyk_loops_quadratic(
    tord, track, tracks, track_pos, t, &inner, &left, &is, &js, &ns, false);
  multi_print_cyk_loops_linear(
    tord, track, tracks, track_pos, t, &inner, &left, &right, &is, &js, &ns,
    false);
  multi_print_cyk_loops_constant(
    tord, track, tracks, track_pos, t, &all, &is, false);

  // outside NTs (which access inside NT values)
  if (this->ast->grammar()->is_outside()) {
    stream << endl << endl;
    multi_print_cyk_loops_linear(
      tord, track, tracks, track_pos, t, &inner, &left, &right, &is, &js, &ns,
      true);
    multi_print_cyk_loops_constant(
      tord, track, tracks, track_pos, t, &all, &is, true);
    multi_print_cyk_loops_quadratic(
      tord, track, tracks, track_pos, t, &inner, &left, &is, &js, &ns, true);
  }
}


void Printer::Cpp::print_cyk_fn(const AST &ast) {
  if (fwd_decls) {
    stream << indent() << "void cyk();" << endl << endl;
    return;
  }

  stream << indent() << "void " << class_name << "::cyk() {" << endl;
  inc_indent();
  if (!ast.cyk()) {
    dec_indent();
    stream << indent() << '}' << endl << endl;
    return;
  }

  stream << "#ifndef _OPENMP" << endl;

  // FIXME?
  Type::Base *t = new ::Type::Size();

  /*
  stream << "for (" << *t << " j = 0; j <= n; ++j)" << endl;
  stream << "  for (" << *t << " i = j + 1; i > 0; i--) {" << endl;
  std::list<Symbol::NT*> &tord = ast.grammar()->topological_ord();
  assert(tord.size() <= ast.grammar()->NTs.size());
  for (std::list<Symbol::NT*>::iterator i = tord.begin(); i != tord.end(); ++i)
  if ((*i)->is_tabulated())
  stream << "nt_tabulate_" << *(*i)->name << "(i-1, j);" << endl;
  stream << endl << "}" << endl;
  */

  if (ast.grammar()->axiom->tracks() > 1) {
    for (size_t track_pos = 0; track_pos < ast.grammar()->axiom->tracks();
         ++track_pos) {
      stream << endl << "{" << endl;
      multi_print_cyk(ast.grammar()->topological_ord(), 0, 1, track_pos, t);
      stream << endl << "}" << endl;
    }
  }

  multi_print_cyk(
    ast.grammar()->topological_ord(), 0, ast.grammar()->axiom->tracks(), 0, t);


  stream << "#else" << endl;
  // FIXME generalize for multi-track ...
  if (ast.grammar()->axiom->tracks() == 1) {
    print_openmp_cyk(ast);
  }
  stream << "#endif" << endl;

  dec_indent();
  stream << indent() << "}" << endl;

  stream << endl;
}

void Printer::Cpp::print_insideoutside(Symbol::NT *nt) {
  // aggregated level (=dim + tracks) of nested loops
  unsigned int nesting = 0;
  std::vector<std::string> *args = new std::vector<std::string>();
  // opening for loops
  for (size_t track = 0; track < nt->tracks(); ++track) {
    unsigned int dim = 2;
    if (nt->tables()[track].delete_left_index()) {
      dim--;
    }
    if (nt->tables()[track].delete_right_index()) {
      dim--;
    }
    if (dim >= 1) {
      stream << indent() << "for (unsigned int t_" << track << "_i = t_"
             << track << "_left_most; t_" << track << "_i <= t_" << track
             << "_right_most; ++t_" << track << "_i) {" << endl;
      inc_indent();
      args->push_back("t_" + std::to_string(track) + "_i");
    }
    if (dim >= 2) {
      stream << indent() << "for (unsigned int t_" << track << "_j = t_"
             << track << "_i; t_" << track << "_j <= t_" << track
             << "_right_most; ++t_" << track << "_j) {" << endl;
      inc_indent();
      args->push_back("t_" + std::to_string(track) + "_j");
    }
    nesting += dim;
  }

  // loop body
  std::list<Fn_Def*> &l = nt->code_list();
  std::stringstream list_args;
  std::stringstream list_args_print;
  bool first = true;
  for (std::vector<std::string>::const_iterator a = args->begin();
       a != args->end(); ++a) {
    if (!first) {
      list_args << ", ";
      list_args_print << " << \",\"";
    }
    first = false;
    list_args << *a;
    list_args_print << " << " << *a;
  }
  stream << indent() << "out << \"start answers " << *nt->name
         << "(\"" << list_args_print.str() << " << \"):\\n\";\n";
  for (std::list<Fn_Def*>::iterator i = l.begin(); i != l.end(); ++i) {
    std::string res = "res_" + *(nt->name);
    stream << indent() << *((*i)->return_type) << " " << res << " = nt_"
           << *nt->name << "(" << list_args.str() << ");\n"
           << indent() << "print_result(std::cout, " << res << ");\n";
    // only for first return statement
    break;
  }
  stream << indent() << "out << \"//end answers " << *nt->name << "(\""
         << list_args_print.str() << " << \")\\n\";\n";

  // close loops
  for (unsigned int d = 0; d < nesting; ++d) {
    dec_indent();
    stream << indent() << "}" << endl;
  }
}
void Printer::Cpp::print_insideoutside_report_fn(
    std::vector<std::string> outside_nt_list, const AST &ast) {
  stream << indent() << "void report_insideoutside(std::ostream &out) {"
         << endl;
  inc_indent();
  std::list<Symbol::NT*> *reported_nts = new std::list<Symbol::NT*>();
  for (std::vector<std::string>::iterator i = outside_nt_list.begin();
       i != outside_nt_list.end(); ++i) {
    if ((*i).compare(std::string("ALL")) == 0) {
      reported_nts->clear();
      for (hashtable<std::string, Symbol::Base*>::iterator
           j = (*ast.grammar()).NTs.begin();
           j != (*ast.grammar()).NTs.end(); ++j) {
        Symbol::NT *inside_nt = dynamic_cast<Symbol::NT*>(j->second);
        if (inside_nt) {
          if (inside_nt->name->compare(std::string("outside_axioms")) == 0) {
            continue;
          }
          if (!inside_nt->is_partof_outside) {
            reported_nts->push_back(inside_nt);
          }
        }
      }
      break;
    } else {
      reported_nts->push_back(
        dynamic_cast<Symbol::NT*>((*ast.grammar()->NTs.find(*i)).second));
    }
  }

  for (std::list<Symbol::NT*>::iterator nt_inside = reported_nts->begin();
       nt_inside != reported_nts->end(); ++nt_inside) {
    hashtable<std::string, Symbol::Base*>::iterator outside_ntpair =
      ast.grammar()->NTs.find(std::string("outside_") + *((*nt_inside)->name));
    if (outside_ntpair == ast.grammar()->NTs.end()) {
      // not all inside NTs will be translated into outside NTs, e.g.
      // times = CHAR('*') won't have an outside counterpart.
      continue;
    }

    print_insideoutside(*nt_inside);
    Symbol::NT *nt_outside =
      dynamic_cast<Symbol::NT*>((*outside_ntpair).second);
    print_insideoutside(nt_outside);
  }
  dec_indent();
  stream << indent() << "}" << endl << endl;
}

void Printer::Cpp::print_derivative(Symbol::NT *nt) {
  stream << indent() << "std::cout << \"" << ast->current_derivative
         << ". derivatives for non-terminal \\\""
         << (*nt->name).substr(sizeof(OUTSIDE_NT_PREFIX)-1,
                               (*nt->name).length())
         << "\\\":\\n\";" << endl;
  // aggregated level (=dim + tracks) of nested loops
  unsigned int nesting = 0;
  std::vector<std::string> *args = new std::vector<std::string>();
  // opening for loops
  for (size_t track = 0; track < nt->tracks(); ++track) {
    unsigned int dim = 2;
    if (nt->tables()[track].delete_left_index()) {
      dim--;
    }
    if (nt->tables()[track].delete_right_index()) {
      dim--;
    }
    if (dim >= 1) {
      stream << indent() << "for (unsigned int t_" << track << "_i = t_"
             << track << "_left_most; t_" << track << "_i <= t_" << track
             << "_right_most; ++t_" << track << "_i) {" << endl;
      inc_indent();
      args->push_back("t_" + std::to_string(track) + "_i");
    }
    if (dim >= 2) {
      stream << indent() << "// properly indent following results as"
             << " an upper right triangle" << endl;
      stream << indent() << "for (unsigned int t_" << track << "_j = t_"
             << track << "_left_most; t_" << track << "_j < t_" << track
             << "_i; ++t_" << track << "_j) {" << endl;
      inc_indent();
      stream << indent() << "std::cout << \"\\t\";" << endl;
      dec_indent();
      stream << indent() << "}" << endl;
      stream << indent() << "for (unsigned int t_" << track << "_j = t_"
             << track << "_i; t_" << track << "_j <= t_" << track
             << "_right_most; ++t_" << track << "_j) {" << endl;
      inc_indent();
      args->push_back("t_" + std::to_string(track) + "_j");
    }
    nesting += dim;
  }

  // loop body
  std::list<Fn_Def*> &l = nt->code_list();
  std::stringstream list_args;
  std::stringstream list_args_print;
  bool first = true;
  for (std::vector<std::string>::const_iterator a = args->begin();
       a != args->end(); ++a) {
    if (!first) {
      list_args << ", ";
      list_args_print << " << \",\"";
    }
    first = false;
    list_args << *a;
    list_args_print << " << " << *a;
  }
  for (std::list<Fn_Def*>::iterator i = l.begin(); i != l.end(); ++i) {
    std::string res = "res_" + *(nt->name);
    stream << indent() << *((*i)->return_type) << " " << res << " = nt_"
           << *nt->name << "(" << list_args.str() << ");\n"
           << indent() << "std::cout << " << res << ";" << endl;
    // only for first return statement
    break;
  }

  // close loops
  for (unsigned int d = 0; d < nesting; ++d) {
    if (d > 0) {
      stream << indent() << "std::cout << \"\\n\";" << endl;
    } else {
      stream << indent() << "std::cout << \"\\t\";" << endl;
    }
    dec_indent();
    stream << indent() << "}" << endl;
  }
  if (nesting == 1) {
    stream << indent() << "std::cout << \"\\n\";" << endl;
  }
}
void Printer::Cpp::print_run_derivative_fn(const AST &ast) {
  stream << indent() << "void report_derivative(std::ostream &out) {"
         << endl;
  inc_indent();

  stream << indent() << "// forward pass has already been executed through "
         << "obj.run(), called via XXX_main.cc" << endl;

  stream << indent() << "// execute backward pass" << endl;
  for (hashtable<std::string, Symbol::Base*>::iterator
       i = (*ast.grammar()).NTs.begin();
       i != (*ast.grammar()).NTs.end(); ++i) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>((*i).second);
    if (nt && nt->is_partof_outside) {
      print_derivative(nt);
    }
  }


  dec_indent();
  stream << indent() << "}" << endl << endl;
}

void Printer::Cpp::print_run_fn(const AST &ast) {
  Symbol::NT *axiom = ast.grammar()->axiom;
  if (ast.current_derivative > 0) {
    axiom = dynamic_cast<Symbol::NT*>(
      ast.grammar()->NTs[*ast.grammar()->axiom_name_inside]);
  }

  stream << indent() << *axiom->code()->return_type;
  stream << " run() {" << endl;
  inc_indent();
  stream << indent() << "return nt_" << *axiom->name << '(';

  bool first = true;
  size_t track = 0;
  const std::vector<Table> &tables = axiom->tables();
  for (std::vector<Table>::const_iterator i = tables.begin();
       i != tables.end(); ++i, ++track) {
    Table t = *i;
    if (!t.delete_left_index()) {
      if (!first) {
        stream << ", ";
      }
      first = false;
      stream << "t_" << track << "_left_most";
    }
    if (!t.delete_right_index()) {
      if (!first) {
        stream << ", ";
      }
      first = false;
      stream << "t_" << track << "_right_most";
    }
  }

  stream << ");" << endl;
  dec_indent();
  stream << indent() << '}' << endl << endl;
}


void Printer::Cpp::print_stats_fn(const AST &ast) {
  stream << indent() << "void print_stats(std::ostream &o) {" << endl;

  stream << "#ifdef STATS" << endl;

  inc_indent();
  stream << indent() << "o << \"\\n\\nN = \" << seq.size() << '\\n'" << ';'
  << endl;
  for (hashtable<std::string, Symbol::NT*>::const_iterator i =
       ast.grammar()->tabulated.begin();
       i != ast.grammar()->tabulated.end(); ++i) {
    stream << indent() << i->second->table_decl->name()
      << ".print_stats(o, \"" << i->second->table_decl->name() << "\");"
      << endl;
  }
  dec_indent();

  stream << "#endif" << endl;

  stream << indent() << '}' << endl << endl;
}


void Printer::Cpp::header_footer(const AST &ast) {
  dec_indent();
  stream << indent() << " public:" << endl;
  inc_indent();
  print_run_fn(ast);
  print_stats_fn(ast);
}


#include "version.hh"


void Printer::Cpp::print_id() {
  if (fwd_decls) {
    return;
  }
  stream
    << "#ident \"$Id: Compiled with gapc "
    << gapc::version_id
    << " $\""
    << endl;
}


void Printer::Cpp::footer(const AST &ast) {
  if (fwd_decls) {
    dec_indent();
    stream << indent() << " public:" << endl;
    inc_indent();
  }
  print_cyk_fn(ast);
  print_id();
}


#include "instance.hh"
#include "product.hh"


void Printer::Cpp::print_backtrack_fn(const AST &ast) {
  if (ast.code_mode() != Code::Mode::BACKTRACK) {
    return;
  }

  stream << indent() << *ast.grammar()->axiom->code()->return_type;
  stream << " backtrack";
  print(
    ast.grammar()->axiom->code()->types, ast.grammar()->axiom->code()->names);
  stream << " {" << endl;
  inc_indent();

  bool axiom_use_btproxy = ast.code_mode().kscoring()
    // FIXME workaround mfe*pp, axiom without h, axiom returns list of scores
    // see helene.gap, adpf_hl
    && ast.instance_->product->algebra()->is_compatible(Mode::KSCORING);
  if (axiom_use_btproxy) {
    stream << indent() << *ast.grammar()->axiom->data_type()
    << " bt  = bt_proxy_nt_"  << *ast.grammar()->axiom_name << '(';
  } else {
    stream << indent() << "return bt_nt_" << *ast.grammar()->axiom_name << '(';
  }

  std::list<std::string*>::const_iterator i =
    ast.grammar()->axiom->code()->names.begin();
  // assert(ast.grammar()->axiom->code()->names.size() > 1);
  if (i != ast.grammar()->axiom->code()->names.end()) {
    stream << **i;
    ++i;
  }
  for (; i != ast.grammar()->axiom->code()->names.end(); ++i) {
    stream << ", " << **i;
  }
  stream << ");" << endl;

  if (axiom_use_btproxy) {
    stream << indent() << "return execute_backtrack_k(bt);" << endl;
  }
  dec_indent();
  stream << indent() << '}' << endl << endl;
}


bool Printer::Cpp::print_axiom_args(const AST &ast) {
  bool first = true;
  size_t t = 0;
  for (std::vector<Table>::const_iterator i =
       ast.grammar()->axiom->tables().begin();
       i != ast.grammar()->axiom->tables().end(); ++i, ++t) {
    if (!(*i).delete_left_index()) {
      if (!first) {
        stream << ", ";
      }
      stream << "t_" << t << "_left_most";
      first = false;
    }
    if (!(*i).delete_right_index()) {
      if (!first) {
        stream << ", ";
      }
      stream << "t_" << t << "_right_most";
      first = false;
    }
  }
  return !first;
}


void Printer::Cpp::print_kbacktrack_pp(const AST &ast) {
  Type::Backtrace *bt_type = dynamic_cast<Type::Backtrace*>(
    ast.grammar()->axiom->code()->return_type);
  const Type::Base *bt_value = bt_type->value_type();
  stream << "intrusive_ptr<Backtrace<" << *bt_value
    << ", unsigned int> >  bt    = backtrack(";

  print_axiom_args(ast);

  stream << ");" << endl
    << "intrusive_ptr<Backtrace_List<" << *bt_value << ", unsigned int> > l ="
    << endl
    << "  boost::dynamic_pointer_cast<Backtrace_List<" << *bt_value
    << ", unsigned int> > (bt);" << endl
    << "assert(!bt || (bt && l));" << endl
    << "if (l) {\n"
    << "for (Backtrace_List<" << *bt_value
    << ", unsigned int>::iterator i = l->begin();"
    << endl
    << "     i != l->end(); ++i)" << endl
    << "  (*i)->print(out);" << endl
    << "}\n";
}


void Printer::Cpp::print_backtrack_pp(const AST &ast) {
  stream << indent() << "template <typename Value>";
  stream << " void print_backtrack(std::ostream &out, "
    << "Value&" << " value) {" << endl;
  inc_indent();

  if (ast.code_mode() != Code::Mode::BACKTRACK) {
    dec_indent();
    stream << indent() << '}' << endl << endl;
    return;
  }

  if (ast.code_mode().kscoring()) {
    print_kbacktrack_pp(ast);
    dec_indent();
    stream << indent() << '}' << endl;
    return;
  }

  stream << indent() << *ast.grammar()->axiom->code()->return_type;
  stream << " bt = backtrack(";

  print_axiom_args(ast);

  stream << ");" << endl;

  Type::Backtrace *bt_type = dynamic_cast<Type::Backtrace*>(
    ast.grammar()->axiom->code()->return_type);

  // FIXME
  if (bt_type) {
    assert(bt_type);
    assert(bt_type->value_type());

    stream << indent() << "if (!bt)" << endl;
    inc_indent();
    stream << indent() << "return;" << endl;
    dec_indent();

    stream << indent() << "intrusive_ptr<Eval_List<" << *bt_type->value_type()
      << "> > elist = bt->eval();"
      << endl
      << indent() << "elist->print(out, value);" << endl
      << indent() << "erase(elist);" << endl
      << indent() << "erase(bt);" << endl;
  }

  dec_indent();
  stream << indent() << '}' << endl << endl;
}


void Printer::Cpp::print_marker_init(const AST &ast) {
  if (!ast.code_mode().marker()) {
    return;
  }

  // FIXME
  assert(ast.grammar()->axiom->tracks() == 1);

  stream << endl;
  for (std::list<Symbol::NT*>::const_iterator i =
       ast.grammar()->nts().begin(); i != ast.grammar()->nts().end(); ++i) {
    stream << "marker_nt_" << *(*i)->name << ".init(t_0_seq.size());" << endl;
  }
  stream << endl << endl;
}


void Printer::Cpp::print_marker_clear(const AST &ast) {
  if (!ast.code_mode().marker()) {
    return;
  }

  for (hashtable<std::string, Symbol::NT*>::const_iterator i =
       ast.grammar()->tabulated.begin();
       i != ast.grammar()->tabulated.end(); ++i) {
    stream << indent() << i->second->table_decl->name() << ".clear();" << endl;
  }
}


void Printer::Cpp::print_subopt_fn(const AST &ast) {
  if (ast.code_mode() != Code::Mode::SUBOPT) {
    stream << indent() << "void print_subopt(std::ostream &out, "
    << "int " << " delta = 0) {" << endl;
    stream << indent() << '}' << endl;
    return;
  }
  Fn_Def *f = ast.grammar()->axiom->code();
  ::Type::Base *score_type = f->return_type->deref()->component()->left();
  ::Type::Base *bt_type = f->return_type->deref()->component()->right();
  ::Type::Base *pp_type = f->return_type->deref()->component()->right()
  ->component();
  stream << indent() << "void print_subopt(std::ostream &out, " << *score_type
  << " delta = 0) {" << endl;
  inc_indent();
  print_table_init(ast);
  print_zero_init(*ast.grammar());
  stream << endl << endl;

  print_marker_init(ast);

  stream << "  " << *score_type << " global_score = " << *f->name << "(";

  print_axiom_args(ast);

  stream << ");" << endl;

  if (!ast.code_mode().marker()) {
    stream << "  " << *f->return_type << " l = ";
  }

  stream << f->target_name() << "(";

  bool b = print_axiom_args(ast);

  if (b) {
    stream << ", ";
  }

  stream << "global_score, delta);" << endl;

  if (!ast.code_mode().marker()) {
    stream << "  for (" << *f->return_type->deref()
      << "::iterator i " << endl
      << "       = l.ref().begin(); i != l.ref().end(); ++i) {" << endl;
    stream << "    " << *bt_type << " bt = (*i).second;" << endl
      << "    " << *score_type << " v = (*i).first;" << endl;
    stream << "    intrusive_ptr<Eval_List<" << *pp_type
      << "> > elist = bt->eval();" << endl
      << "    elist->print(out, v);" << endl;
    stream << "  }" << endl;
  }

  print_marker_clear(ast);

  dec_indent();
  stream << indent() << '}' << endl;
}


void Printer::Cpp::backtrack_footer(const AST &ast) {
  print_value_pp(ast);
  print_backtrack_fn(ast);
  print_backtrack_pp(ast);
  print_subopt_fn(ast);
}


void Printer::Cpp::print_value_pp(const AST &ast) {
  stream << indent() << "template <typename Value>";
  stream << " void  print_result(std::ostream &out, "
         << "Value&" << " res) {" << endl;
  inc_indent();
  if (ast.code_mode() == Code::Mode::BACKTRACK ||
      ast.code_mode() == Code::Mode::SUBOPT) {
    dec_indent();
    stream << indent() << '}' << endl;
    return;
  }

  stream << indent() << "if (isEmpty(res)) {" << endl;
  inc_indent();
  stream << indent() << "out << \"[]\\n\";" << endl;
  dec_indent();
  stream << indent() << "} else {" << endl;
  inc_indent();
  stream << indent() << "out << res << '\\n';" << endl;
  dec_indent();
  stream << indent() << '}' << endl;

  dec_indent();
  stream << indent() << '}' << endl << endl;
}


void Printer::Cpp::close_class() {
  dec_indent();
  stream << indent() << "};" << endl << endl;
}


void Printer::Cpp::typedefs(Code::Gen &code, unsigned int current_derivative) {
  stream << "#ifndef NO_GAPC_TYPEDEFS" << endl;
  stream << indent() << "namespace gapc {" << endl;
  inc_indent();
  stream << indent() << "typedef " << class_name << " class_name";
  if (current_derivative >= 2) {
    stream << "_D2";
  }
  stream << ";" << endl;
  if (current_derivative <= 1) {
    stream << indent() << "typedef " << *code.return_type()
           << " return_type;" << endl;
  }
  dec_indent();
  stream << indent() << '}' << endl;
  stream << "#endif" << endl;
  stream << endl;
  stream << "#endif" << endl;
}

void Printer::Cpp::prelude(const Options &opts, const AST &ast) {
  if (ast.code_mode().subopt_buddy()) {
    return;
  }

  stream << endl << make_comments(id_string, "//") << endl << endl;

  stream << "#define GAPC_MOD_TRANSLATION_UNIT" << endl;

  stream << "#include \"" << remove_dir(opts.header_file) << '"'
    << endl << endl;

  if (ast.kbest) {
    for (std::list<Statement::Hash_Decl*>::const_iterator i =
         ast.hash_decls().begin(); i != ast.hash_decls().end(); ++i) {
      stream << "uint32_t " << (*i)->ext_name() << "::k_ = 3;\n";
    }
  }
}


static const char deps[] =
"basenameCXX=$(shell basename $(CXX))\n"
"ifneq ($(filter $(basenameCXX),g++ icc),)\n"
"-include $(DEPS)\n"
"\n"
"%.o : %.cc\n"
"\t$(CXX) $(CPPFLAGS) $(CXXFLAGS) $< -c -o $@ \n"
// "         && $(SED) -e 's/[^ ]\\+boost[^ \\n]\\+//' $*.d "
// "> _t && mv _t $*.d\n"
""
"endif\n";


#include "prefix.hh"


void Printer::Cpp::makefile(const Options &opts, const AST &ast) {
  stream << endl << make_comments(id_string, "#") << endl << endl;

  // stream << "SED = sed\n";
  // stream << "RTLIB = rtlib\n\n";
  stream << "RTLIB_LDFLAGS = $(RT_LDFLAGS)\n";
  stream << "RTLIB_LDLIBS = $(RT_LDLIBS)\n";
  stream << "RTLIB_CPPFLAGS = $(RT_CPPFLAGS)\n\n";
  if (*gapc::prefix) {
    std::string mf = std::string(gapc::prefix) + "/share/gapc/config" +
      std::string(gapc::systemsuffix) + ".mf";
    stream << "ifeq \"$(origin NO_CONFIG_MF)\" \"undefined\"" << endl
      << "$(info Including global makefile " << mf << ")" << endl
      << "-include " << mf << endl
      << "endif" << endl << endl;
  }
  stream << "-include gapc_local.mf" << endl << endl;
  stream << "ifdef MF" << endl
           << "$(info Including extra makefile $(MF))" << endl
           << "include $(MF)" << endl
       << "endif" << endl << endl;

  std::string base = opts.class_name;  // basename(opts.out_file);
  std::string out_file = "";
  if (ast.requested_derivative > 1) {
    for (unsigned int i = 1; i <= ast.requested_derivative; ++i) {
      out_file += basename(remove_dir(opts.out_file)) + \
                  "_derivative" + std::to_string(i) + ".cc ";
    }
  } else {
    out_file = remove_dir(opts.out_file);
  }
  std::string header_file = remove_dir(opts.header_file);
  stream << "CXXFILES =  " << base << "_main.cc "
    << out_file << endl << endl;
  stream << "DEPS = $(CXXFILES:.cc=.d)" << endl
    << "OFILES = $(CXXFILES:.cc=.o) string.o" << endl << endl;
  stream << opts.class_name << " : $(OFILES)" << endl
    << "\t$(CXX) -o $@ $^  $(LDFLAGS) $(LDLIBS)";

  // if (opts.sample) {
  //  stream << " $(GSLLIBS) ";
  // }

  if (opts.cyk) {
    stream << " $(CXXFLAGS_OPENMP) ";
  }

  stream << endl << endl
    << base << "_main.cc : $(RTLIB)/generic_main.cc " << out_file << endl;
  if (ast.requested_derivative > 1) {
    for (unsigned int i = 1; i <= ast.requested_derivative; ++i) {
      stream << "\techo '#include \"" << basename(remove_dir(opts.out_file))
             << "_derivative" << std::to_string(i) << ".hh\"' >";
      if (i > 1) {
        stream << ">";
      }
      stream << " $@" << endl;
    }
  } else {
    stream << "\techo '#include " << "\"" << header_file << "\""
           << "' > $@" << endl;
  }

  stream << "\tcat $(RTLIB)/generic_main.cc >> " << base << "_main.cc" << endl
    << endl;
  stream << deps << endl;
  stream << ".PHONY: clean" << endl << "clean:" << endl
    << "\trm -f $(OFILES) " << opts.class_name << ' ' << base << "_main.cc"
    << endl << endl;

  stream <<
    "string.o: $(RTLIB)/string.cc" << endl <<
    "\t$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@" << endl;
}

void Printer::Cpp::pytorch_makefile(const Options &opts, const AST &ast) {
  // generate a Makefile for automatic setup.py generation
  // and installation of a Python module of the generated GAPC code
  stream << endl << make_comments(id_string, "#") << endl << endl;

  std::string base = opts.class_name;  // basename(opts.out_file);
  std::string out_files = "";
  std::string header_files = "";
  if (ast.requested_derivative > 1) {
    for (unsigned int i = 1; i <= ast.requested_derivative; ++i) {
      out_files += "\"" + basename(remove_dir(opts.out_file)) + \
                  "_derivative" + std::to_string(i) + ".cc\"";
      header_files += "\"" + basename(remove_dir(opts.header_file)) + \
                  "_derivative" + std::to_string(i) + ".hh\"";
      if (i < ast.requested_derivative) {
        out_files += ", ";
        header_files += ", ";
      }
    }
  } else {
    out_files = "\"" + remove_dir(opts.out_file) + "\"";
    header_files = "\"" + remove_dir(opts.header_file) + "\"";
  }

  stream << "RTLIB_LDFLAGS = $(RT_LDFLAGS)\n";
  stream << "RTLIB_LDLIBS = $(RT_LDLIBS)\n";
  stream << "RTLIB_CPPFLAGS = $(RT_CPPFLAGS)\n\n";
  if (*gapc::prefix) {
    std::string mf = std::string(gapc::prefix) + "/share/gapc/config" +
      std::string(gapc::systemsuffix) + ".mf";
    stream << "ifeq \"$(origin NO_CONFIG_MF)\" \"undefined\"" << endl
      << "$(info Including global makefile " << mf << ")" << endl
      << "-include " << mf << endl
      << "endif" << endl << endl;
  }
  stream << "-include gapc_local.mf" << endl << endl;
  stream << "ifdef MF" << endl
           << "$(info Including extra makefile $(MF))" << endl
           << "include $(MF)" << endl
       << "endif" << endl << endl;

  stream << "CXXFLAGS := $(CXXFLAGS) | sed 's/c++17/c++14/'" << endl;
  stream << "compiler_args := $(CXXFLAGS) | sed -e 's/\\s/\", \"/g'" << endl;
  stream << "LDFLAGS := $(LDFLAGS) | sed 's/-L\\//\\//g'" << endl;
  stream << "LDFLAGS := $(LDFLAGS) | sed -E 's/ -Xlinker -rpath -Xlinker.+//'"
         << endl;
  stream << "xlinker_args := $(LDFLAGS) | "
         << "sed -e 's/\\s/\", \"-Xlinker\", \"-rpath\", \"-Xlinker\", \"/g'"
         << endl;
  stream << "library_dirs := $(LDFLAGS) | sed -e 's/\\s/\", \"/g'" << endl;
  stream << "LDLIBS := $(LDLIBS) | sed 's/-l//g'" << endl;
  stream << "libraries := $(LDLIBS) | sed -e 's/\\s/\", \"/g'" << endl;
  stream << "CPPFLAGS := $(CPPFLAGS) | sed 's/-I\\//\\//g'" << endl;
  stream << "include_dirs := $(CPPFLAGS) | sed -e 's/\\s/\", \"/g'"
         << endl << endl;

  stream << "gen_files: setup.py pytorch_interface.cc" << endl;
  stream << "\techo 'Generated setup.py and python_interface.cc'"
         << endl << endl;
  stream << "install:" << endl;
  stream << "\tpip3 install ." << endl << endl;

  stream << "setup.py:" << endl;
  stream << "\techo 'from setuptools import setup' > $@" << endl;
  stream << "\techo -e 'from torch.utils.cpp_extension import "
         << "BuildExtension, CppExtension\\n' >> $@" << endl;
  stream << "\techo -n 'compiler_args = [\"' >> $@" << endl;
  stream << "\techo -n $(compiler_args) >> $@" << endl;
  stream << "\techo -e '\"]\\n' >> $@" << endl;
  stream << "\techo -n 'library_dirs = [\"' >> $@" << endl;
  stream << "\techo -n $(library_dirs) >> $@" << endl;
  stream << "\techo -e '\"]\\n' >> $@" << endl;
  stream << "\techo -n 'xlinker_args = "
         << "[\"-Xlinker\", \"-rpath\", \"-Xlinker\", \"' >> $@"
         << endl;
  stream << "\techo -n $(xlinker_args) >> $@" << endl;
  stream << "\techo -e '\"]\\n' >> $@" << endl;
  stream << "\techo -n 'libraries = [\"' >> $@" << endl;
  stream << "\techo -n $(libraries) >> $@" << endl;
  stream << "\techo -e '\"]\\n' >> $@" << endl;
  stream << "\techo -n 'include_dirs = [\"' >> $@" << endl;
  stream << "\techo -n $(include_dirs) >> $@" << endl;
  stream << "\techo -e '\"]\\n' >> $@" << endl;
  stream << "\techo -e 'gapc_extension = CppExtension(name = \""
         << opts.pytorch_module_mame << "\",' >> $@" << endl;
  stream << "\techo -n '                              depends = [' >> $@"
         << endl;
  stream << "\techo '" << header_files << "],' >> $@" << endl;
  stream << "\techo -n '                              sources = "
         << "[\"pytorch_interface.cc\", ' >> $@" << endl;
  stream << "\techo '" << out_files << "],' >> $@" << endl;
  stream << "\techo -e '                              include_dirs = "
         << "include_dirs,' >> $@" << endl;
  stream << "\techo -e '                              extra_compile_args = "
         << "compiler_args,' >> $@" << endl;
  stream << "\techo -e '                              extra_link_args = "
         << "xlinker_args,' >> $@" << endl;
  stream << "\techo -e '                              libraries = "
         << "libraries,' >> $@" << endl;
  stream << "\techo -e '                              library_dirs = "
         << "library_dirs)\\n' >> $@" << endl;
  stream << "\techo 'setup(name = \"" << opts.pytorch_module_mame
         << "\",' >> $@" << endl;
  stream << "\techo '      version = \"0.0.1\",' >> $@" << endl;
  stream << "\techo '      ext_modules = [gapc_extension],' >> $@" << endl;
  stream << "\techo '      cmdclass = {\"build_ext\": "
         << "BuildExtension})' >> $@" << endl << endl;

  stream << "pytorch_interface.cc: $(RTLIB)/generic_pytorch_interface.cc"
         << endl;
  stream << "\ttouch $@" << endl;
  if (ast.requested_derivative > 1) {
    for (unsigned int i = 1; i <= ast.requested_derivative; ++i) {
      stream << "\techo '#include \"" << basename(remove_dir(opts.header_file))
                + "_derivative" + std::to_string(i) + ".hh\"' >> $@" << endl;
    }
  } else if (ast.requested_derivative == 1) {
    stream << "\techo '#include " << header_files << "' >> $@" << endl;
  }
  stream << "\tcat $(RTLIB)/generic_pytorch_interface.cc >> $@"
         << endl << endl;;

  stream << ".PHONY: clean" << endl << "clean:" << endl
    << "\trm -rf build dist *.egg-info pytorch_interface.cc setup.py"
    << endl << endl;
}

void Printer::Cpp::print_pytorch_init_fn(const AST &ast) {
  // print init function which handles the
  // inputs from Pytorch and makes them accessible

  stream << indent() << "void init(";
  stream << "const torch::Tensor &inp, const char *seq1, const char *seq2";
  for (unsigned int i = 1; i < ast.current_derivative; ++i) {
    stream << ", " << get_class_name_lower_derivative(ast.current_derivative, i)
           << " *derivative" << std::to_string(i);
  }
  stream << ") {" << endl;

  inc_indent();

  int n = 1;
  for (std::vector<Statement::Var_Decl*>::const_iterator
       i = ast.seq_decls.begin(); i != ast.seq_decls.end();
       ++i, n++) {
    std::string seq_name = "seq" + std::to_string(n);
    stream << indent() << *(*i)->name << ".copy(" << seq_name
           << ", std::strlen(" << seq_name << "));" << endl;
  }

  print_filter_init(ast);
  print_table_init(ast);
  print_zero_init(*ast.grammar());

  if (ast.as_pytorch_module) {
    stream << indent() << "tensor_size.clear();" << endl;
  }
  size_t t = 0;
  for (std::vector<Statement::Var_Decl*>::iterator j = ns.begin();
       j != ns.end(); ++j, ++t) {
    stream << indent() << "t_" << t << "_left_most = 0;\n";
    stream << indent() << "t_" << t << "_right_most = " << "t_" << t
      << "_seq.size();\n";
    stream << indent() << "tensor_size.push_back(t_" << t
           << "_right_most + 1);" << endl;
  }

  if (ast.requested_derivative > 0) {
    stream << endl;
  }
  for (unsigned int i = 1; i < ast.current_derivative; ++i) {
    stream << indent() << "this->derivative" << std::to_string(i)
           << " = derivative" << std::to_string(i) << ";" << endl;
  }

  dec_indent();
  stream << indent() << '}' << endl << endl;
}

void Printer::Cpp::print_pytorch_forward_fn(const AST &ast) {
  // convert the forward score matrix to a Pytorch tensor
  stream << indent() << "void get_forward_score_matrices"
         << "(std::vector<torch::Tensor> &matrices) {" << endl;
  inc_indent();
  for (auto i = ast.grammar()->NTs.begin();
      i != ast.grammar()->NTs.end(); ++i) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>((*i).second);
    if (nt && !(nt->is_partof_outside)) {
      stream << indent() << "matrices.push_back(torch::from_blob("
             << *(nt->name) << "_table.get_array_data(), "
             << "tensor_size, torch::kFloat64));" << endl;
    }
  }
  dec_indent();
  stream << indent() << "}" << endl << endl;
}

void Printer::Cpp::print_pytorch_backward_fn(const AST &ast) {
  // convert the backward score matrix to a Pytorch tensor
  stream << indent() << "void get_backward_score_matrices"
         << "(std::vector<torch::Tensor> &matrices) {" << endl;
  inc_indent();
  stream << indent() << "for (int t_0_i = t_0_left_most; "
         << "t_0_i <= t_0_right_most; ++t_0_i) {" << endl;
  inc_indent();
  stream << indent() << "for (int t_1_i = t_1_left_most; "
         << "t_1_i <= t_1_right_most; ++t_1_i) {" << endl;
  inc_indent();
  stream << indent() << "// need to calculate everything first "
         << "(results will be added to tensor all at once afterwards)"
         << endl;
  for (auto i = ast.grammar()->NTs.begin();
      i != ast.grammar()->NTs.end(); ++i) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>((*i).second);
    if (nt && nt->is_partof_outside) {
      stream << indent() << "nt_" << *(nt->name) << "(t_0_i, t_1_i);"
             << endl;
    }
  }
  dec_indent();
  stream << indent() << "}" << endl;
  dec_indent();
  stream << indent() << "}" << endl;
  for (auto i = ast.grammar()->NTs.begin();
      i != ast.grammar()->NTs.end(); ++i) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>((*i).second);
    if (nt && nt->is_partof_outside) {
      stream << indent() << "matrices.push_back(torch::from_blob("
             << *(nt->name) << "_table.get_array_data(), "
             << "tensor_size, torch::kFloat64));" << endl;
    }
  }
  dec_indent();
  stream << indent() << "}" << endl << endl;
}

void Printer::Cpp::imports(const AST &ast) {
  if (fwd_decls) {
    return;
  }

  if (ast.current_derivative > 0) {
    stream << "#include \"rtlib/traces.hh\"" << endl;
  }

  if (ast.code_mode() != Code::Mode::SUBOPT) {
    stream << "#include \"rtlib/subopt.hh\"" << endl;
  }

  switch (ast.get_rtlib_header()) {
    case ADP_Mode::PARETO_NOSORT_BLOCK:
      stream << "#include \"rtlib/adp_specialization/pareto_0_nosort_block.hh\""
        << endl;
      break;
    case ADP_Mode::PARETO_NOSORT_STEP:
      stream << "#include \"rtlib/adp_specialization/pareto_0_nosort_step.hh\""
        << endl;
      break;
    case ADP_Mode::PARETO_SORT_BLOCK:
      stream << "#include \"rtlib/adp_specialization/pareto_1_sorted_block.hh\""
        << endl;
      break;
    case ADP_Mode::PARETO_SORT_STEP:
      stream << "#include \"rtlib/adp_specialization/pareto_1_sorted_step.hh\""
        << endl;
      break;
    case ADP_Mode::PARETO_YUK_BLOCK:
      stream << "#include \"rtlib/adp_specialization/pareto_3_yukish_block.hh\""
        << endl;
      break;
    case ADP_Mode::PARETO_YUK_STEP:
      stream << "#include \"rtlib/adp_specialization/pareto_3_yukish_step.hh\""
        << endl;
      break;
    case ADP_Mode::SORT_BLOCK:
      stream << "#include \"rtlib/adp_specialization/sort_block.hh\"" << endl;
      break;
    case ADP_Mode::SORT_STEP:
      stream << "#include \"rtlib/adp_specialization/sort_step.hh\"" << endl;
      break;
    default: break;
  }

  for (std::list<Import*>::const_iterator i = ast.imports.begin();
       i != ast.imports.end(); ++i) {
    // if this import-declaration is verbatim, do not append
    // a ".hh" suffix.
    if ((*i)->verbatimDeclaration) {
      stream << "#include \"" << *(*i)->name << "\"" << endl;
    } else {
      stream << "#include \"" << *(*i)->name << ".hh\"" << endl;
    }
  }
  stream << endl;
  stream << "#include \"rtlib/generic_opts.hh\"" << endl;
  stream << "#include \"rtlib/pareto_dom_sort.hh\"" << endl;
  stream << "#include \"rtlib/pareto_yukish_ref.hh\"" << endl;

  /* include code of lower derivatives */
  for (unsigned int i = 1; i < ast.current_derivative; ++i) {
    stream << indent() << "#include \""
           << get_class_name_lower_derivative(ast.current_derivative, i)
           <<  ".hh\"" << endl;
  }

  stream << endl;
}


void Printer::Cpp::global_constants(const AST &ast) {
  if (ast.get_pareto_cutoff() != -1) {
    stream << "const int yukish_cutoff = " << ast.get_pareto_cutoff()
      << ";\n\n";
  }

  if (ast.get_float_acc() != 0) {
    stream << "#include \"rtlib/float_accuracy_operators.hh\"\n";
    stream << "const double depsilon = " << std::pow(0.1, ast.get_float_acc())
      << ";\n\n";
  }
}

void Printer::Cpp::print(const Statement::Backtrace_Decl &d) {
  const std::list<Para_Decl::Base*> &p = d.ntparas();
  const std::list<Statement::Var_Decl*> &paras = d.paras();

  in_class = true;

  stream << indent() << "template <typename Value, typename pos_int>" << endl;
  stream << indent() << "struct " << d.name() << " : ";

  if (d.derive_bt_score()) {
    stream << "Backtrace_Score<" << d.score_type() << ", Value, pos_int>";
  } else {
    stream << "Backtrace<Value, pos_int>";
  }

  stream << " {" << endl;
  inc_indent();

  for (std::list<Statement::Var_Decl*>::const_iterator i = paras.begin();
       i != paras.end(); ++i) {
    stream << **i << endl;
  }
  for (std::list<Para_Decl::Base*>::const_iterator i = p.begin();
       i != p.end(); ++i) {
    print(*i);
    stream << ";\n";
  }
  stream << endl;
  stream << indent() << d.name() << "(";
  std::list<Statement::Var_Decl*>::const_iterator i = paras.begin();
  if (i != paras.end()) {
    stream << *(*i)->type << ' ' << (*(*i)->name + "_");
    ++i;
  }
  for (; i != paras.end(); ++i) {
    stream << ", " << *(*i)->type << ' ' << (*(*i)->name + "_");
  }
  for (std::list<Para_Decl::Base*>::const_iterator i = p.begin();
       i != p.end(); ++i) {
    Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
    assert(s);
    stream << ", " << *s->type() << ' ' << *s->name() << '_';
  }
  stream << ")" << " : ";
  i = paras.begin();
  if (i != paras.end()) {
    stream << *(*i)->name << '(' << (*(*i)->name + "_") << ')';
    ++i;
  }
  for (; i != paras.end(); ++i) {
    stream << ", " << *(*i)->name << '(' << (*(*i)->name + "_") << ')';
  }
  for (std::list<Para_Decl::Base*>::const_iterator i = p.begin();
       i != p.end(); ++i) {
    Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
    assert(s);
    stream << ", " << *s->name() << '(' << *s->name() << "_)";
  }
  stream << " {" << endl;
  stream << indent() << "}" << endl << endl;

  stream << indent() << "~" << d.name() << "() {" << endl;
  inc_indent();
  for (std::list<Statement::Var_Decl*>::const_iterator i = paras.begin();
       i != paras.end(); ++i) {
    Statement::Var_Decl *v = *i;
    if (v->type->is(Type::BACKTRACE)) {
      stream << indent() << "erase(" << *v->name << ");" << endl;
    }
  }
  dec_indent();
  stream << indent() << '}' << endl << endl;


  stream << indent() << "intrusive_ptr<Backtrace<Value, pos_int> > "
         << "backtrack() {" << endl;
  inc_indent();
  stream << indent() << "return intrusive_ptr<Backtrace<Value, pos_int> >"
         << "(this);" << endl;
  dec_indent();
  stream << indent() << "}" << endl << endl;

  const std::list<Fn_Def*> &l = d.algebra_code_deps();
  for (std::list<Fn_Def*>::const_iterator i = l.begin(); i != l.end(); ++i) {
    stream << **i << endl;
  }
  stream << d.algebra_code() << endl;
  stream << d.eval_code();
  dec_indent();
  stream << indent() << "};" << endl << endl;
  in_class = false;
}


void Printer::Cpp::print(const Statement::Backtrace_NT_Decl &d) {
  const std::list<std::string> &l = d.track_args();
  const std::list<Para_Decl::Base*> &p = d.ntparas();
  if (d.score_type()) {
    std::string name;
    name = "Backtrace_nt_" + d.name() + "_Back";
    ::Type::Base *single = d.score_type();
    if (single->is(Type::LIST)) {
      single = single->component();
    }
    stream << "template <typename Klass, typename Value, typename pos_int> "
      << "struct " << name  << " : public Backtrace_NT_Back_Base<"
      << *single << ", Klass, Value, pos_int>" << endl
      << "{" << endl;

    for (std::list<std::string>::const_iterator i = l.begin(); i != l.end();
         ++i) {
      stream << "pos_int " << *i << ";\n";
    }
    for (std::list<Para_Decl::Base*>::const_iterator i = p.begin();
         i != p.end(); ++i) {
      Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
      assert(s);
      stream << *s->type() << ' ' << *s->name() << ";\n";
    }

    stream << name << "(Klass *klass_";

    for (std::list<std::string>::const_iterator i = l.begin();
         i != l.end(); ++i) {
      stream << ", pos_int " << *i << "_";
    }
    for (std::list<Para_Decl::Base*>::const_iterator i = p.begin();
         i != p.end(); ++i) {
      Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
      assert(s);
      stream << ", " << *s->type() << ' ' << *s->name() << '_';
    }

    stream << ")" << endl
      << "  : Backtrace_NT_Back_Base<" << *single << ", Klass, Value, pos_int>"
      << "(klass_";

    stream << ")";

    for (std::list<std::string>::const_iterator i = l.begin();
         i != l.end(); ++i) {
      stream << ", " << *i << "(" << *i << "_)";
    }
    for (std::list<Para_Decl::Base*>::const_iterator i = p.begin();
         i != p.end(); ++i) {
      Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
      assert(s);
      stream << ", " << *s->name() << '(' << *s->name() << "_)";
    }

    stream << " {}" << endl
      << "void backtrack()" << endl
      << "{" << endl
      << "assert(this->scores == 0);" << endl
      << "this->scores = boost::dynamic_pointer_cast<"
    "Backtrace_List<Value, pos_int> >"
      << endl
      << "  (this->klass->bt_nt_" << d.name() << "(";

    std::list<std::string>::const_iterator i = l.begin();
    if (i != l.end()) {
      stream << *i;
      ++i;
    }
    for (; i != l.end(); ++i) {
      stream << ", " << *i;
    }
    for (std::list<Para_Decl::Base*>::const_iterator i = p.begin();
         i != p.end(); ++i) {
      Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
      assert(s);
      stream << ", " << *s->name();
    }

    stream << "));" << endl
      << "assert(this->scores != 0);" << endl
      << "}" << endl
      << endl
      << "};" << endl << endl;

    std::string back_name(name);
    name = "Backtrace_nt_" + d.name() + "_Front";
    stream << "template <typename Value, typename pos_int> "
      << "struct " << name << " : public Backtrace_Score<" << *single
      << " , Value, pos_int> " << endl
      << "{" << endl
      << "  intrusive_ptr<" << back_name << "<" << class_name
      << ", Value, pos_int> > back;" << endl << endl

      << name << "(intrusive_ptr<" << back_name << "<" << class_name
      << ", Value, pos_int> > b) : back(b) {}" << endl

      << "intrusive_ptr<Backtrace<Value, pos_int> > backtrack()" << endl
      << "{" << endl
      << "  intrusive_ptr<Backtrace_NT_Back_Base<" << *single << ", "
      << class_name << ", Value, pos_int> > t = back;" <<endl
      << "  return t->backtrack(this->score());" << endl
      << "}" << endl << endl

      << "intrusive_ptr<Eval_List<Value> > eval()" << endl << "{" << endl
      << "  intrusive_ptr<Backtrace<Value, pos_int> > t;" << endl
      << "  t = backtrack();" << endl
      << "  return t->eval();" << endl
      << "}" << endl


      << "};" << endl << endl;

    return;
  }

  std::string name;
  name = "Backtrace_nt_" + d.name();
  stream << indent() << "template <typename Klass, typename Value, "
         << "typename pos_int>" << endl;
  stream << indent() << "struct " << name << " : public Backtrace<Value, "
         << "pos_int> {" << endl;
  inc_indent();
  stream << indent() << "Klass *klass;" << endl;

  for (std::list<std::string>::const_iterator i = l.begin();
       i != l.end(); ++i) {
    stream << indent() << "pos_int " << *i << ";" << endl;
  }
  for (std::list<Para_Decl::Base*>::const_iterator i = p.begin();
       i != p.end(); ++i) {
    Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
    assert(s);
    stream << *s->type() << ' ' << *s->name() << ";\n";
  }
  stream << endl;

  stream << indent() << "intrusive_ptr<Backtrace<Value, pos_int> > proxy;"
         << endl << endl;
  stream << indent() << name << "(Klass *klass_";

  for (std::list<std::string>::const_iterator i = l.begin();
       i != l.end(); ++i) {
    stream << ", pos_int " << *i << "_";
  }
  for (std::list<Para_Decl::Base*>::const_iterator i = p.begin();
       i != p.end(); ++i) {
    Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
    assert(s);
    stream << ", " << *s->type() << ' ' << *s->name() << '_';
  }

  stream << ") " << ": klass(klass_)";

  for (std::list<std::string>::const_iterator i = l.begin();
       i != l.end(); ++i) {
    stream << ", " << *i << "(" << *i << "_)";
  }
  for (std::list<Para_Decl::Base*>::const_iterator i = p.begin();
       i != p.end(); ++i) {
    Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
    assert(s);
    stream << ", " << *s->name() << '(' << *s->name() << "_)";
  }

  stream << ", proxy(0) {" << endl;
  stream << indent() << "}" << endl << endl;
  stream << indent() << "~" << name << "() {" << endl;
  inc_indent();
  stream << indent() << "erase(proxy);" << endl;
  dec_indent();
  stream << indent() << "}" << endl << endl;
  stream << indent()
    << "intrusive_ptr<Backtrace<Value, pos_int> > backtrack() {" << endl;
  inc_indent();
  stream << indent() << "return klass->bt_nt_" << d.name() << "(";

  std::list<std::string>::const_iterator i = l.begin();
  if (i != l.end()) {
    stream << *i;
    ++i;
  }
  for (; i != l.end(); ++i) {
    stream << ", " << *i;
  }
  for (std::list<Para_Decl::Base*>::const_iterator i = p.begin();
       i != p.end(); ++i) {
    Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
    assert(s);
    stream << ", " << *s->name();
  }

  stream << ");" << endl;
  dec_indent();
  stream << indent() << '}' << endl << endl;
  // stream << "Eval_List<Value>* eval() { assert(false); }" << endl;
  stream << indent() << "intrusive_ptr<Eval_List<Value> > eval() {" << endl;
  inc_indent();
  stream << indent() << "proxy = backtrack();" << endl;
  stream << indent() << "return proxy->eval();" << endl;
  dec_indent();
  stream << indent() << "}" << endl;
  dec_indent();
  stream << indent() << "};" << endl << endl;
  // stream << "bool is_proxy() const { return true; }" << endl;
}


void Printer::Cpp::print(const Statement::Hash_Decl &d) {
  stream << "class " << d.ext_name() << " {" << endl;
  stream << " public:" << endl;
  inc_indent();
  stream << indent() << "typedef " << d.answer_type() << " type;" << endl;
  stream << " private:" << endl;
  const std::list<Statement::Var_Decl*> &f = d.filters();
  for (std::list<Statement::Var_Decl*>::const_iterator i = f.begin();
       i != f.end(); ++i) {
    stream << indent() << *(*i)->type << "<type> " << *(*i)->name << ";"
      << endl;
  }

  if (d.kbest()) {
    stream << indent() << "static uint32_t k_;\n";
  }

  stream << " public:" << endl;
  stream << indent() << "uint32_t hash(const type &x) const {" << endl;
  inc_indent();
  stream << indent() << "return hashable_value(left_most(x));" << endl;
  dec_indent();
  stream << indent() << "}" << endl << endl;

  stream << indent() << "type init(const type &src) const {" << endl;
  inc_indent();
  stream << indent() << "type dst(src);" << endl;
  stream << d.init_code();
  dec_indent();
  stream << indent() << "}" << endl << endl;

  stream << indent() << "void update(type &dst, const type &src) " << endl
    << d.code() << endl;

  stream << indent() << "bool equal(const type &a, const type &b) const {"
    << endl;
  inc_indent();
  stream << indent() << "return left_most(a) == left_most(b);" << endl;
  dec_indent();
  stream << indent() << "}" << endl << endl;

  stream << indent() << "bool filter() const {" << endl;
  inc_indent();
  if (f.empty()) {
    stream << indent() << "return false;" << endl;
  } else {
    stream << indent() << "return true;" << endl;
  }
  dec_indent();
  stream << indent() << "}" << endl << endl;

  stream << indent() << "bool filter(const type &x) const {" << endl;
  inc_indent();
  int a = 0;
  for (std::list<Statement::Var_Decl*>::const_iterator i = f.begin();
       i != f.end(); ++i, ++a) {
    stream << indent() << "bool b" << a << " = !" << *(*i)->name << ".ok(x);"
      << endl;
  }
  a = 0;
  std::list<Statement::Var_Decl*>::const_iterator i = f.begin();
  if (i != f.end()) {
    stream << endl << "return b" << a << " ";
    ++i;
    ++a;
  }
  for ( ; i != f.end(); ++i, ++a) {
    stream << " && b" << a;
  }
  if (f.empty()) {
    stream << indent() << "assert(0);" << endl;
    stream << indent() << "return false";
  }
  stream << ";" << endl;
  dec_indent();
  stream << indent() << "}" << endl << endl;

  stream << indent() << "void finalize(type &src) const" << endl
    << d.finalize_code() << endl;

  if (d.kbest()) {
    stream << indent() << "static void set_k(uint32_t a) {" << endl;
    inc_indent();
    stream << indent() << "k_ = a;" << endl;
    dec_indent();
    stream << indent() << "}" << endl << endl;
  } else {
    stream << indent() << "static void set_k(uint32_t a) {" << endl
      << indent() << "}" << endl << endl;
  }
  stream << indent() << "uint32_t k() const" << endl << d.k_code() << endl;
  stream << indent() << "bool cutoff() const" << endl << d.cutoff_code()
    << endl;
  stream << indent()
    << "bool equal_score(const type &src, const type &dst) const" << endl
    << d.equal_score_code() << endl;
  stream << indent() << "struct compare {" << endl;
  inc_indent();
  stream << indent()
    << "bool operator()(const type &src, const type &dst) const" << endl
    << d.compare_code();
  dec_indent();
  stream << indent() << "};" << endl;
  dec_indent();
  stream << indent() << "};" << endl << endl;

  stream << indent() << "typedef Hash::Ref<" << d.answer_type() << ", "
    << d.ext_name()
    << " > " << d.name() << ";"
    << endl;
}


void Printer::Cpp::print(const Statement::Marker_Decl &d) {
  stream << "Marker<unsigned int> " << d.name() << ';' << endl;
}
