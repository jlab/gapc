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

#include <utility>
#include <list>

#include "signature.hh"
#include "arg.hh"
#include "fn_decl.hh"

#include "fn_def.hh"
#include "expr.hh"
#include "statement.hh"
#include "expr/new.hh"
#include "type/backtrace.hh"


Signature_Base::~Signature_Base() {}


Fn_Decl* Signature::decl(const std::string &s) {
  hashtable<std::string, Fn_Decl*>::iterator i = decls.find(s);
  if (i == decls.end()) {
    return NULL;
  }
  return i->second;
}

void Signature::setDecls(hashtable <std::string, Fn_Decl*> &d) {
  for (hashtable<std::string, Fn_Decl*>::iterator i = d.begin();
       i != d.end(); ++i) {
    Fn_Decl *f  = i->second;
    if (f->is_Choice_Fn()) {
      choice_fns[* f->name] = f;
    }
  }
  decls = d;
}


void Signature::replace(Type::Base *a, Type::Base *b) {
  for (hashtable <std::string, Fn_Decl*>::iterator i = decls.begin();
       i != decls.end(); ++i) {
    i->second->replace(a, b);
  }
}


std::ostream &operator<< (std::ostream &s, const Signature &sig) {
  s << "Signature:" << std::endl;
  for (hashtable <std::string, Fn_Decl*>::const_iterator i = sig.decls.begin();
       i != sig.decls.end(); ++i) {
    s << *i->second << std::endl;
  }
  return s;
}


void Signature::print() {
  std::cerr << *this;
}


bool Signature::check() {
  if (choice_fns.empty()) {
    Log::instance()->error(
      location,
      "There is no choice function defined in signature " + *name + ".");
    return false;
  }
  bool r = true;
  for (hashtable <std::string, Fn_Decl*>::iterator i = choice_fns.begin();
       i != choice_fns.end(); ++i) {
    Fn_Decl *f = i->second;
    if (f->types.size() != 1) {
      Log::instance()->error(
        f->location,
        "Choice function " + *f->name + " has not only one argument.");
      r = r && false;
    }
    if (!f->return_type->simple()->is(Type::LIST)) {
      Log::instance()->error(
        f->location,
        "Choice function " + *f->name + " does not return a list.");
      r = r && false;
    } else {
      if (f->types.size() == 1 && !f->return_type->is_eq(*f->types.front())) {
        Log::instance()->error(
          f->location,
          "The return type of choice function " + *f->name +
          " does not match the argument type.");
        r = r && false;
      }
    }
  }
  return r;
}


void Signature::set_args(hashtable<std::string, Arg*> &h) {
  args = h;
}


Type::Base *Signature::var_lookup(const Type::Signature *t) {
  if (!algebra)
    return NULL;
  hashtable<std::string, Type::Base*>::iterator i = algebra->params.find(
    t->name());
  if (i == algebra->params.end())
    return NULL;
  return i->second;
}


Type::Base *Signature::var_lookup(const Type::Alphabet *t) {
  if (!algebra)
    return NULL;
  hashtable<std::string, Type::Base*>::iterator i = algebra->params.find(
    "alphabet");
  if (i == algebra->params.end())
    return NULL;
  return i->second;
}


Algebra *Signature::generate(std::string *n, std::string *mode) {
  if (*mode == "count")
    return generate_count(n);
  if (*mode == "enum")
    return generate_enum(n);
  if (*mode == "trees")
    return generate_trees(n);
  return NULL;
}




struct Generate_Stmts {
  virtual void apply(Fn_Def &fn) const = 0;
  virtual ~Generate_Stmts() {}
};


Algebra *Signature::generate_algebra(
  std::string *n, Mode::Type mode_type, Type::Base *answer_type,
  const Generate_Stmts &generate_stmts) {
  // FIXME make count/enum alphabet agnostic
  Type::Base *alph = new Type::Char();
  return generate_algebra(n, mode_type, answer_type, alph, generate_stmts);
}


Algebra *Signature::generate_algebra(
  std::string *n, Mode::Type mode_type, Type::Base *answer_type,
  Type::Base *alpha, const Generate_Stmts &generate_stmts) {
  Algebra *a = new Algebra(n);
  a->signature = this;
  hashtable<std::string, Type::Base*> eqs;
  Loc l;
  std::pair<std::string*, Type::Base*> alph;
  std::pair<std::string*, Type::Base*> answer;
  assert(args.size() >= 2);
  alph.first = new std::string("alphabet");
  alph.second = alpha;
  answer.first = NULL;
  for (hashtable<std::string, Arg*>::iterator i = args.begin();
       i != args.end(); ++i) {
    if (*i->second->name != "alphabet") {
      answer.first = i->second->name;
      answer.second = answer_type;
      Algebra::add_sig_var(eqs, answer, l);
    }
  }
  assert(answer.first);
  Algebra::add_sig_var(eqs, alph, l);
  a->set_params(&eqs);

  hashtable<std::string, Fn_Def*> fns;
  for (hashtable<std::string, Fn_Decl*>::iterator i = decls.begin();
       i != decls.end(); ++i) {
    Fn_Def *fn = new Fn_Def(*i->second);
    if (fn->is_Choice_Fn()) {
      fn->choice_mode().set(mode_type);
    }
    generate_stmts.apply(*fn);

    Type::Base *t = alph.second;
    alph.second = new Type::Usage(t);
    fn->replace_types(alph, answer);
    alph.second = t;

    fns[*i->second->name] = fn;
  }
  a->set_fns(fns);
  a->check_params(*this);
  return a;
}


#include "para_decl.hh"


struct Generate_Count_Stmts : public Generate_Stmts {
  void apply(Fn_Def &fn) const {
    if (fn.is_Choice_Fn()) {
      Expr::Fn_Call *expr = new Expr::Fn_Call(Expr::Fn_Call::LIST);
      Expr::Fn_Call *sum = new Expr::Fn_Call(Expr::Fn_Call::SUM);
      expr->add_arg(sum);
      sum->add_arg(fn.names.front());
      Statement::Return *ret = new Statement::Return(expr);
      fn.stmts.push_back(ret);
      return;
    }
    std::list<Expr::Vacc*> l;

    for (std::list<Para_Decl::Base*>::iterator i = fn.paras.begin();
         i != fn.paras.end(); ++i) {
      Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
      if (s) {
        if (s->type()->simple()->is(Type::SIGNATURE)) {
          l.push_back(new Expr::Vacc(s->name()));
        }
      } else {
        Para_Decl::Multi *m = dynamic_cast<Para_Decl::Multi*>(*i);
        assert(m);
        for (std::list<Para_Decl::Simple*>::const_iterator i=m->list().begin();
             i != m->list().end(); ++i) {
          Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
          assert(s);
          if (s->type()->simple()->is(Type::SIGNATURE)) {
            l.push_back(new Expr::Vacc(s->name()));
          }
        }
      }
    }

    Expr::Base *expr = NULL;
    if (l.empty()) {
      expr = new Expr::Const(1);
    } else {
      expr = Expr::seq_to_tree<Expr::Base, Expr::Times> (l.begin(), l.end());
    }
    Statement::Return *ret = new Statement::Return(expr);
    fn.stmts.push_back(ret);
  }
};


Algebra *Signature::generate_count(std::string *n) {
  return generate_algebra(n, Mode::SYNOPTIC, new Type::BigInt(),
                          Generate_Count_Stmts());
}


#include "statement/fn_call.hh"


struct Generate_Enum_Stmts : public Generate_Stmts {
 private:
  void apply(std::list<Statement::Base*> &l, Para_Decl::Simple *s,
             Statement::Var_Decl *&cur) const {
    Statement::Fn_Call *f = new Statement::Fn_Call(
      Statement::Fn_Call::STR_APPEND);
    f->add_arg(*cur);
    f->add_arg(s->name());
    l.push_back(f);
  }


  void apply(std::list<Statement::Base*> &l, Para_Decl::Multi *m,
             Statement::Var_Decl *&cur) const {
    Statement::Fn_Call * f = new Statement::Fn_Call(
      Statement::Fn_Call::STR_APPEND);
    f->add_arg(*cur);
    f->add_arg(new Expr::Const("< "));
    f->add_arg(new Expr::Const(2));
    l.push_back(f);

    const std::list<Para_Decl::Simple*> &p = m->list();
    std::list<Para_Decl::Simple*>::const_iterator j = p.begin();
    if (j != p.end()) {
      apply(l, *j, cur);
      ++j;
    }
    for (; j != p.end(); ++j) {
      f = new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
      f->add_arg(*cur);
      f->add_arg(new Expr::Const(", "));
      f->add_arg(new Expr::Const(2));
      l.push_back(f);
      apply(l, *j, cur);
    }

    f = new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
    f->add_arg(*cur);
    f->add_arg(new Expr::Const(" >"));
    f->add_arg(new Expr::Const(2));
    l.push_back(f);
  }


  void apply(std::list<Statement::Base*> &l,
             const std::list<Para_Decl::Base*> &paras,
             Statement::Var_Decl *&cur) const {
    std::list<Statement::Base*> apps;
    unsigned int a = 0;
    for (std::list<Para_Decl::Base*>::const_iterator i = paras.begin();
         i != paras.end(); ++i) {
      if (a > 0 && !(a % 3)) {
        std::ostringstream o;
        o << "ret_" << a;
        Type::External *str = new Type::External("Rope");
        Statement::Var_Decl *t = new Statement::Var_Decl(str, o.str());
        l.push_back(t);
        Statement::Fn_Call *f =
        new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
        f->add_arg(*cur);
        f->add_arg(*t);
        cur = t;
        apps.push_front(f);
      }
      Statement::Fn_Call *f = new Statement::Fn_Call(
        Statement::Fn_Call::STR_APPEND);
      f->add_arg(*cur);
      f->add_arg(new Expr::Const(' '));

      l.push_back(f);

      Para_Decl::Multi *m = dynamic_cast<Para_Decl::Multi*>(*i);
      if (m) {
        apply(l, m, cur);
      } else {
        Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
        assert(s);
        apply(l, s, cur);
      }
      a++;
    }
    l.insert(l.end(), apps.begin(), apps.end());
  }


 public:
  void apply(Fn_Def &fn) const {
    if (fn.is_Choice_Fn()) {
      Statement::Return *ret = new Statement::Return(fn.names.front());
      fn.stmts.push_back(ret);
      return;
    }
    Type::External *str = new Type::External("Rope");
    Statement::Var_Decl *ret = new Statement::Var_Decl(str, "ret");
    fn.stmts.push_back(ret);
    Statement::Fn_Call *f = new Statement::Fn_Call(
      Statement::Fn_Call::STR_APPEND);
    f->add_arg(*ret);
    std::string t = *fn.name + "(";
    f->add_arg(new Expr::Const(t));
    f->add_arg(new Expr::Const(static_cast<int>(t.size())));
    fn.stmts.push_back(f);

    Statement::Var_Decl *cur = ret;
    std::list<Statement::Base*> l;
    apply(l, fn.paras, cur);
    fn.stmts.insert(fn.stmts.end(), l.begin(), l.end());

    if (fn.ntparas().size() > 0) {
      f = new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
      f->add_arg(*ret);
      f->add_arg(new Expr::Const(';'));
      fn.stmts.push_back(f);
      std::list<Statement::Base*> lntparas;
      apply(lntparas, fn.ntparas(), ret);
      fn.stmts.insert(fn.stmts.end(), lntparas.begin(), lntparas.end());
    }

    f = new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
    f->add_arg(*ret);
    f->add_arg(new Expr::Const(' '));
    fn.stmts.push_back(f);

    f = new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
    f->add_arg(*ret);
    f->add_arg(new Expr::Const(')'));
    fn.stmts.push_back(f);

    Statement::Return *r = new Statement::Return(*ret);
    fn.stmts.push_back(r);
  }
};


Algebra *Signature::generate_enum(std::string *n) {
  return generate_algebra(n, Mode::PRETTY, new Type::External("Rope"),
                          Generate_Enum_Stmts());
}



//------------------------------------------------------------------------------------------------------------


struct Generate_Tree_Stmts : public Generate_Stmts {
 private:
  // closes nodes for simple tracks
  void apply(std::list<Statement::Base*> &l, Para_Decl::Simple *s,
             Statement::Var_Decl *&cur) const {
    Statement::Fn_Call *f = new Statement::Fn_Call(
      Statement::Fn_Call::STR_APPEND);
    f->add_arg(*cur);
    f->add_arg(s->name());
    l.push_back(f);
  }

  // Generating Child Nodes for grammar operations in Multitrack
  void apply(std::list<Statement::Base*> &l, Para_Decl::Multi *m,
             Statement::Var_Decl *&cur) const {
    Statement::Fn_Call * f = new Statement::Fn_Call(
      Statement::Fn_Call::STR_APPEND);
    f->add_arg(*cur);
    f->add_arg(new Expr::Const("child {node {"));
    l.push_back(f);

    const std::list<Para_Decl::Simple*> &p = m->list();
    std::list<Para_Decl::Simple*>::const_iterator j = p.begin();
    if (j != p.end()) {
      apply(l, *j, cur);
      ++j;
    }
    for (; j != p.end(); ++j) {
      f = new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
      f->add_arg(*cur);
      f->add_arg(new Expr::Const(", "));
      f->add_arg(new Expr::Const(2));
      l.push_back(f);
      apply(l, *j, cur);
    }

    f = new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
    f->add_arg(*cur);
    f->add_arg(new Expr::Const("}}"));
    f->add_arg(new Expr::Const(2));
    l.push_back(f);
  }


  void apply(std::list<Statement::Base*> &l,
             const std::list<Para_Decl::Base*> &paras,
             Statement::Var_Decl *&cur) const {
    std::list<Statement::Base*> apps;
    unsigned int a = 0;
    for (std::list<Para_Decl::Base*>::const_iterator i = paras.begin();
         i != paras.end(); ++i) {
      if (a > 0 && !(a % 3)) {
        std::ostringstream o;
        o << "ret_" << a;
        Type::External *str = new Type::External("Rope");
        Statement::Var_Decl *t = new Statement::Var_Decl(str, o.str());
        l.push_back(t);
        Statement::Fn_Call *f =
        new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
        f->add_arg(*cur);
        f->add_arg(*t);
        cur = t;
        apps.push_front(f);
      }
      Statement::Fn_Call *f = new Statement::Fn_Call(
        Statement::Fn_Call::STR_APPEND);
      f->add_arg(*cur);
      //f->add_arg(new Expr::Const(' '));
      Para_Decl::Multi *m = dynamic_cast<Para_Decl::Multi*>(*i);
     if (m) {
        f->add_arg(new Expr::Const(' '));
        l.push_back(f);
        apply(l, m, cur);
      } else {
        Para_Decl::Simple *s = dynamic_cast<Para_Decl::Simple*>(*i);
	    f->add_arg(new Expr::Const("child {node {"));
	    l.push_back(f);
        assert(s);
        apply(l, s, cur);
      }
      a++;

      if (!m) {
	 Statement::Fn_Call *g = new Statement::Fn_Call(
              Statement::Fn_Call::STR_APPEND);
     g->add_arg(*cur);
     g->add_arg(new Expr::Const("}}"));
     l.push_back(g);
      }
    }

    l.insert(l.end(), apps.begin(), apps.end());
  }

 public:
  void apply(Fn_Def &fn) const {
    if (fn.is_Choice_Fn()) {
      Statement::Return *ret = new Statement::Return(fn.names.front());
      fn.stmts.push_back(ret);
      return;
    }
    Type::External *str = new Type::External("Rope");
    Statement::Var_Decl *ret = new Statement::Var_Decl(str, "ret");
    fn.stmts.push_back(ret);
    Statement::Fn_Call *f = new Statement::Fn_Call(
      Statement::Fn_Call::STR_APPEND);
    f->add_arg(*ret);
    std::string t = "child {node {" + *fn.name + "}";
    f->add_arg(new Expr::Const(t));
    f->add_arg(new Expr::Const(static_cast<int>(t.size())));
    fn.stmts.push_back(f);

    Statement::Var_Decl *cur = ret;
    std::list<Statement::Base*> l;
    apply(l, fn.paras, cur);
    fn.stmts.insert(fn.stmts.end(), l.begin(), l.end());

    if (fn.ntparas().size() > 0) {
      f = new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
      f->add_arg(*ret);
      f->add_arg(new Expr::Const(';'));
      fn.stmts.push_back(f);
      std::list<Statement::Base*> lntparas;
      apply(lntparas, fn.ntparas(), ret);
      fn.stmts.insert(fn.stmts.end(), lntparas.begin(), lntparas.end());
    }
    f = new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
    f->add_arg(*ret);
    f->add_arg(new Expr::Const(' '));
    fn.stmts.push_back(f);
	f = new Statement::Fn_Call(Statement::Fn_Call::STR_APPEND);
	f->add_arg(*ret);
	f->add_arg(new Expr::Const('}'));
	fn.stmts.push_back(f);

    Statement::Return *r = new Statement::Return(*ret);
    fn.stmts.push_back(r);
  }
};


Algebra *Signature::generate_trees(std::string *n) {
  return generate_algebra(n, Mode::PRETTY, new Type::External("Rope"),
                          Generate_Tree_Stmts());
}
//------------------------------------------------------------------------------------------------------------


struct Generate_Backtrace_Stmts : public Generate_Stmts {
  Generate_Backtrace_Stmts() : value_type(0), pos_type(0) {}
  Type::Base *value_type;
  Type::Base *pos_type;
  void apply(Fn_Def &fn) const {
    if (fn.is_Choice_Fn()) {
      Statement::Return *ret = new Statement::Return(fn.names.front());
      fn.stmts.push_back(ret);
      return;
    }
    assert(value_type);
    assert(pos_type);
    Expr::New *f =
    new Expr::New(new Type::Backtrace(value_type, pos_type, fn.name));
    f->add_args(fn.paras);
    f->add_args(fn.ntparas());
    Statement::Return *ret = new Statement::Return(f);
    fn.stmts.push_back(ret);
  }
};


Algebra *Signature::generate_backtrace(
  std::string *n, Type::Base *value_type, Type::Base *pos_type,
  Type::Base *alph) {
  Generate_Backtrace_Stmts gen;
  gen.value_type = value_type;
  gen.pos_type = pos_type;
  return generate_algebra(n, Mode::PRETTY,
    new Type::Backtrace(pos_type, value_type), alph, gen);
}
