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

#include <cassert>
#include <sstream>
#include <map>
#include <algorithm>
#include <vector>
#include <utility>
#include <list>
#include <string>

#include "ast.hh"
#include "log.hh"
#include "terminal.hh"

#include "signature.hh"

#include "instance.hh"
#include "product.hh"
#include "list_warn.hh"

#include "expr.hh"
#include "const.hh"

#include "arg.hh"

#include "statement.hh"

#include "opt_choice_visitor.hh"

#include "fn_def.hh"

#include "backtrack.hh"
#include "subopt.hh"

AST::AST()
  :  product_(0),
    grammars_(0),
    selected_grammar(0),
    back_track_paretosort(Product::NONE),
    adp_specialization(ADP_Mode::STANDARD),
    pareto_cutoff(-1),
    float_acc(0),
    signature(NULL),
    first_instance(NULL), instance_(0),
    backtrack_product(0),
    fwd_product(0),
    backtrack_filter(0),
    original_product(0),
    char_type(0),
    outside_nt_list(nullptr),
    checkpoint(nullptr) {
  Type::add_predefined(types);
}


Type::Base *AST::get_type(const std::string &name, const Loc &l) {
  hashtable<std::string, Type::Base*>::iterator i = types.find(name);
  if (i != types.end()) {
    return i->second;
  }
  Log::instance()->error(l, "Usage of unknown type " + name + ".");
  return NULL;
}


Type::Base *AST::get_type(const std::string &name) {
  hashtable<std::string, Type::Base*>::iterator i = types.find(name);
  if (i != types.end()) {
    return i->second;
  }
  Log::instance()->error("Usage of unknown type " + name + ".");
  return NULL;
}


void AST::add_type(std::string *name, const Loc &l, Type::Base *base) {
  hashtable<std::string, Type::Base*>::iterator i = types.find(*name);
  if (i == types.end()) {
    types[*name] = base;
    type_def_list.push_back(base);
    return;
  }
  Log::instance()->error(l, "Type " + *name + " already defined");
  Log::instance()->error(i->second->location, "here.");
}


void AST::add_sig_types(hashtable<std::string, Arg*> & args, Signature *s) {
  for (hashtable<std::string, Arg*>::iterator i = args.begin();
       i != args.end(); ++i) {
    Arg *arg = i->second;
    if (*arg->name == "alphabet")
      continue;
    Type::Signature *t = new Type::Signature(arg->name, arg->location, s);
    add_type(arg->name, arg->location, t);
  }
}


bool AST::check_signature() {
  bool r = true;
  bool b = signature->check();
  r = r && b;
  b = grammar()->check_signature(*signature);
  r = r && b;
  return r;
}


bool AST::check_algebras() {
  bool r = true;
  for (hashtable<std::string, Algebra*>::iterator i = algebras.begin();
       i != algebras.end(); ++i) {
    bool b = i->second->check_signature(*signature);
    r = r && b;
  }
  return r;
}


bool AST::check_instances(Instance *instance) {
  for (hashtable<std::string, Algebra*>::iterator i = algebras.begin();
       i != algebras.end(); ++i) {
    i->second->annotate_terminal_arguments(*signature);
  }

  instance->check_alphabets();

  bool r = true;
  for (hashtable<std::string, Instance*>::iterator i = instances.begin();
       i != instances.end(); ++i) {
    bool b = i->second->init(instance);
    r = r && b;
  }

  bool b = instance->check_multiple_answer_types(this->outside_generation());
  r = r && b;

  return r;
}


void AST::print_instances(std::ostream &s) {
  for (hashtable<std::string, Instance*>::iterator i = instances.begin();
       i != instances.end(); ++i) {
    s << *i->second << std::endl;
  }
}


/*
 * Returns the selected instance of this gap program.
 * the selected instance is that one is either
 * a) the first one found in the source code, or
 * b) that one defined in the hashtable "instances"
 *    for the given instance name
 */
Instance *AST::instance(const std::string &n) {
  if (n.empty()) {
    if (first_instance) {
      return first_instance;
    } else {
      Log::instance()->error("No instance defined in input file.");
      return NULL;
    }
  }
  hashtable<std::string, Instance*>::iterator i = instances.find(n);
  if (i == instances.end()) {
    Log::instance()->error("Could not find instance " + n + ".");
    return NULL;
  }
  Instance *instance = i->second;
  return instance;
}


bool AST::insert_instance(std::string &n) {
  Instance *inst = instance(n);
  if (!inst) {
    return false;
  }
  bool b = insert_instance(inst);
  return b;
}

bool AST::insert_instance(Instance *inst) {
  assert(inst);
  instance_ = inst;
  grammar()->reset_types();

  update_alphabet_types(inst->product->algebra()->params["alphabet"]);

  bool b = grammar()->check_signature(*inst->product->algebra());
  if (b) {
    warn_unused_fns(*inst);
  }

  return b;
}


bool AST::instance_grammar_eliminate_lists(std::string &n) {
  Instance *inst = instance(n);
  if (!inst) {
    return false;
  }
  bool b = instance_grammar_eliminate_lists(inst);
  return b;
}


bool AST::instance_grammar_eliminate_lists(Instance *inst) {
  assert(inst);
  inst->eliminate_lists();
  grammar()->eliminate_lists();
  return true;
}


// callable after insert_instance(), instance_grammar_eliminate_lists()
//   and Grammar::init_list_sizes() are called
void AST::warn_missing_choice_fns(Instance *instance) {
  List_Warn lw(instance);
  grammar()->traverse(lw);
}


void AST::warn_missing_choice_fns() {
  List_Warn lw;
  grammar()->traverse(lw);
}


bool AST::tableDesignIsOptimal() {
  Runtime::Asm::Poly rt;
  rt  = grammar()->runtime();
  Runtime::Asm::Poly opt;
  opt  = grammar()->asm_opt_runtime();
  // check if both runtimes are equal
  return rt == opt;
}


void AST::warn_user_table_conf_suboptimal() {
  Runtime::Asm::Poly rt;
  rt  = grammar()->runtime();
  Runtime::Asm::Poly opt;
  opt  = grammar()->asm_opt_runtime();
  if (rt != opt) {
    std::ostringstream o;
    if (grammar()->tabulated.empty()) {
      // first add the auto generated table configuration to the grammar...
      grammar()->approx_table_conf();
      // ...then print an information
      o  <<  "No automatic table design is selected and no table\n"
          "configuration is supplied: Yields an asymptotically suboptimal\n"
          "runtime of " << rt;
      o  << "\n\t  (optimal rt is " << opt << ").\n"
        << "\nAdded option -t for automatic table design for this compile run.";
      Log::instance()->verboseMessage(grammar()->location, o.str());
    } else {
      // Print out a plain warning, because this might turn out to be
      // a problem, or is unwanted by the gapc user.
      o  <<  "Specified table configuration "
          " yields an asymptotically suboptimal runtime:\n"
        <<  '\t' << "Runtime under user supplied table configuration is " << rt;
      o  << "\n\t  (optimal rt is " << opt << ").\n"
        << "\nTry adding option -t for automatic table design.";
      Log::instance()->warning(grammar()->location, o.str());
    }
  }
}


void AST::codegen() {
  sf_filter_code.clear();
  grammar()->codegen(*this);
}


void AST::print_code(Printer::Base &out) {
  grammar()->print_code(out);
}


void AST::derive_roles() {
  for (hashtable<std::string, Algebra*>::iterator i = algebras.begin();
      i != algebras.end(); ++i) {
    i->second->derive_role();
  }
}


struct Push_Type_Cmp {
  bool operator() (const std::pair<Type::List::Push_Type, std::string> &a,
      const std::pair<Type::List::Push_Type, std::string>  &b) {
    // return *p1 < *p2;
    Type::List::Push_Type x = a.first;
    Type::List::Push_Type y = b.first;
    if (x == Type::List::NORMAL && y != Type::List::NORMAL)
      return false;
    if (x != Type::List::NORMAL && y == Type::List::NORMAL)
      return true;
    if (x == Type::List::NORMAL && y == Type::List::NORMAL)
      return true;
    return true;
  }
};


struct FixLink : public Visitor {
  void visit(Alt::Link &a) {
    a.optimize_choice();
  }
};


void AST::optimize_choice(Instance &inst) {
  if (!inst.product->contains_only_times())
    return;

  unsigned int width = inst.product->width();
  unsigned int n = 0;
  Algebra *l = inst.product->nth_algebra(n);

  std::vector<std::pair<Type::List::Push_Type, std::string> > v;

  for (hashtable<std::string, Fn_Def*>::iterator i = l->choice_fns.begin();
       i != l->choice_fns.end(); ++i) {
    Fn_Def *fn = i->second;
    Expr::Fn_Call::Builtin choice_type = fn->choice_fn_type();
    Type::List::Push_Type push = Type::List::NORMAL;
    if (fn->choice_mode() != Mode::KSCORING) {
      if (width == 1) {
        switch (choice_type) {
        case Expr::Fn_Call::SUM : push = Type::List::SUM; break;
        case Expr::Fn_Call::MINIMUM : push = Type::List::MIN; break;
        case Expr::Fn_Call::MAXIMUM : push = Type::List::MAX; break;
        default: {}
        }
        // FIXME add this choice fn optimization to max_other etc.
        // -> generic comparison predicates are needed for this
        if (push != Type::List::NORMAL) {
          fn->add_simple_choice_fn_adaptor();
        }
      } else {
        switch (choice_type) {
        case Expr::Fn_Call::MINIMUM : push = Type::List::MIN_OTHER; break;
        case Expr::Fn_Call::MAXIMUM : push = Type::List::MAX_OTHER; break;
        default: {}
        }
        if (push != Type::List::NORMAL && width == 2 &&
            inst.product->right()->algebra()->choice_fn(fn)->choice_mode() ==
            Mode::PRETTY) {
          Fn_Def *f = inst.product->algebra()->choice_fn(fn);
          f->stmts.clear();
          f->add_simple_choice_fn_adaptor();
        }
      }
    }
    v.push_back(std::make_pair(push, i->first));
  }
  std::sort(v.begin(), v.end(), Push_Type_Cmp());
  for (std::vector<std::pair<Type::List::Push_Type, std::string> >::iterator i
       = v.begin(); i != v.end(); ++i) {
    hashtable<std::string, Fn_Def*>::iterator j =
      inst.product->algebra()->choice_fns.find(i->second);
    assert(j != inst.product->algebra()->choice_fns.end());
    Opt_Choice_Visitor v(j->second, i->first);
    grammar()->traverse(v);
  }
  FixLink fl;
  grammar()->traverse(fl);
}


#include "classify_visitor.hh"
#include "operator.hh"


void AST::optimize_classify(Instance &inst) {
  assert(inst.product);
  if (!inst.product->left_is_classify() || !inst.product->one_per_class()) {
    if (kbest) {
      throw LogError(
        "You specified --kbest but the product is not classifying");
    }
    return;
  }

  for (hashtable<std::string, Fn_Def*>::iterator i =
       inst.product->algebra()->choice_fns.begin();
       i != inst.product->algebra()->choice_fns.end(); ++i) {
    Statement::Hash_Decl *hdecl = inst.generate_hash_decl(*i->second, kbest);
    hash_decls_.push_back(hdecl);

    Type::List::Push_Type t = Type::List::HASH;
    Opt_Choice_Visitor v(i->second, t, hdecl);
    grammar()->traverse(v);

    Classify_Visitor w;
    grammar()->traverse(w);

    if (inst.product->is(Product::SINGLE)) {
      i->second->disable();
    } else {
      i->second->optimize_classify();
    }
  }

  FixLink fl;
  grammar()->traverse(fl);

  // get the iterator over products and test whether it is already
  // at the end!
  Product::iterator i = Product::begin(inst.product);
  assert(i != Product::end());
  ++i;
  // then proceed with the iterator elements as usual
  for (; i != Product::end(); ++i) {
    for (hashtable<std::string, Fn_Def*>::iterator j =
         (*i)->algebra()->choice_fns.begin();
         j != (*i)->algebra()->choice_fns.end(); ++j) {
      j->second->disable();
    }
  }
}

void AST::set_pareto_version(Instance &inst, int version) {
    for (Product::iterator i = Product::begin(inst.product);
         i != Product::end(); ++i) {
        Product::Pareto *p = dynamic_cast<Product::Pareto*>(*i);
        if (!p) {
            continue;
        }
        p->set_pareto_type(version);
    }
}

void AST::set_pareto_dim(Instance &inst, bool dim) {
    for (Product::iterator i = Product::begin(inst.product);
         i != Product::end(); ++i) {
        Product::Pareto *p = dynamic_cast<Product::Pareto*>(*i);
        if (!p) {
            continue;
        }
        p->set_multi_dim(dim);
    }
}

void AST::set_pareto_cutoff(Instance &inst, int cutoff) {
    pareto_cutoff = cutoff;
    for (Product::iterator i = Product::begin(inst.product);
         i != Product::end(); ++i) {
        Product::Pareto *p = dynamic_cast<Product::Pareto*>(*i);
        if (!p) {
            continue;
        }
        p->set_cutoff(cutoff);
    }
}

void AST::set_float_accuracy(Instance &inst, int float_accuracy) {
    this->float_acc = float_accuracy;
    for (Product::iterator i = Product::begin(inst.product);
         i != Product::end(); ++i) {
        (*i)->set_float_accuracy(float_accuracy);
    }
}

struct SetADPVersion : public Visitor {
  ADP_Mode::Adp_Specialization spec;
  ADP_Mode::Adp_Join join;
  std::string duplicate_suffix;
  std::string comperator_suffix;
  std::string sorter_suffix;
  SetADPVersion(
    ADP_Mode::Adp_Specialization s, ADP_Mode::Adp_Join j,
    std::string d, std::string cs, std::string ss) :
    spec(s), join(j), duplicate_suffix(d), comperator_suffix(cs),
    sorter_suffix(ss) {}

  void visit(Alt::Base &b) {
    b.set_adp_specialization(spec);
    b.set_adp_join(join);
  }

  void visit(Symbol::NT &n) {
       n.set_adp_join(join);
       if (spec != ADP_Mode::STANDARD) {
           n.set_adp_specialization(spec, duplicate_suffix,
              comperator_suffix, sorter_suffix);
       } else {
           n.Base::set_adp_specialization(spec);
       }
  }
};

const char AST::duplicate_suffix[] = "_nullary";
const char AST::comperator_suffix[] = "_spec_comperator";
const char AST::sorter_suffix[] = "_spec_sorter";

void AST::set_adp_version(Instance &inst, int i, int step, int pareto) {
    // what to set?
    ADP_Mode::Adp_Specialization spec;
    ADP_Mode::Adp_Join join = ADP_Mode::EMPTY;

    switch (i) {
        case 1:
             if (step) {
                 spec = ADP_Mode::SORTED_STEP;
             } else {
                 spec = ADP_Mode::SORTED_BLOCK;
             }
             break;
        case 2:
             if (step) {
                 spec = ADP_Mode::PARETO_EAGER_STEP;
             } else {
                 spec = ADP_Mode::PARETO_EAGER_BLOCK;
             }
             break;
        default:
            spec = ADP_Mode::STANDARD;
            break;
    }

    if (i != 0) {
        if (i == 1) {
            join = ADP_Mode::SORTER;
        } else if (pareto == 0) {
            join = ADP_Mode::COMPERATOR;
        } else {
            join = ADP_Mode::SORTER_COMPERATOR;
        }
    }

    adp_specialization = spec;
    adp_join = join;

    if (spec != ADP_Mode::STANDARD) {
        Algebra *a = instance_->product->algebra();
        if (a->choice_fns.size() > 1) {
            Log::instance()->error(
              "ADP specialization set but 2 choice functions defined. Mixing"
              " choice functions may cause undefined behaviour.");
        }
    }

    // set for all products to modify choice functions
    for (Product::iterator i = Product::begin(inst.product);
         i != Product::end(); ++i) {
        (*i)->set_adp_specialization(spec);

        if (spec != ADP_Mode::STANDARD) {
            Algebra *a = (*i)->algebra();
            duplicate_choice_functions(a, duplicate_suffix,
              comperator_suffix, sorter_suffix, "");
        }
    }
    if (backtrack_product) {
        for (Product::iterator i = Product::begin(backtrack_product);
             i != Product::end(); ++i) {
            (*i)->set_adp_specialization(spec);

            if (spec != ADP_Mode::STANDARD) {
                Algebra *a = (*i)->algebra();
                // _bt is set in the backtracking object on the existing choice
                // functions but there is no elegant way to propagate it back
                // to Sorter structs
                duplicate_choice_functions(
                  a, duplicate_suffix, comperator_suffix, sorter_suffix, "_bt");
            }
        }
    }


    // set for ALT and Symbol (NT)
    SetADPVersion v = SetADPVersion(
      spec, join, duplicate_suffix, comperator_suffix, sorter_suffix);
    grammar()->traverse(v);
}

void AST::duplicate_choice_functions(
  Algebra *a, std::string duplicate_suffix, std::string comperator_suffix,
  std::string sorter_suffix, std::string nullary_sort_suffix) {
    hashtable<std::string, Fn_Def*> new_fncts;

    for (hashtable<std::string, Fn_Def*>::iterator i = a->fns.begin();
         i != a->fns.end(); ++i) {
        new_fncts[i->first] = i->second;
        if (i->second->is_Choice_Fn()) {
            Fn_Def* f = i->second;

            f->comperator_suffix = new std::string(comperator_suffix);
            f->sorter_suffix = new std::string(sorter_suffix);

            // duplicate choice function
            Fn_Def* fnew = f->copy();
            fnew->name = new std::string(*f->name);
            fnew->name->append(duplicate_suffix);
            fnew->nullary_sort_ob = new std::string(*f->name);
            fnew->nullary_sort_ob->append(nullary_sort_suffix);

            fnew->set_gen_type(Fn_Def::NULLARY);

            new_fncts[*fnew->name] = fnew;
        }
    }
    a->set_fns(new_fncts);
}

void AST::backtrack_gen(Backtrack_Base &bt) {
  assert(instance_);
  Algebra *score = instance_->product->bt_score_algebra();

  bt.gen_backtraces(backtrack_product, *score);
  bt.gen_nt_decls(grammar()->nts());

  Type::Base *t = get_type("alphabet");
  Type::Alphabet *alph = dynamic_cast<Type::Alphabet*>(t);
  assert(alph);
  assert(!alph->simple()->is(Type::ALPHABET));

  // generate right algebra of new product
  bt.gen_algebra(*signature, alph->temp ? alph->temp : new Type::Char());

  if (adp_specialization != ADP_Mode::STANDARD) {
      // _bt is set in the backtracking object on the existing
      // choice functions
      // but there is no elegant way to propagate it back to NTs
      // so.. yeah... this works
      std::string backtrace_prefix = "_bt";

      duplicate_choice_functions(
        bt.get_gen_algebra(), duplicate_suffix, comperator_suffix,
        sorter_suffix, backtrace_prefix);

       // set for ALT and Symbol (NT) again, because choice function has
       // been renamed to _bt by now
      SetADPVersion v = SetADPVersion(
        adp_specialization, adp_join,  backtrace_prefix+duplicate_suffix,
        backtrace_prefix+comperator_suffix, backtrace_prefix+sorter_suffix);
      grammar()->traverse(v);
  }

  // bt.gen_instance(score, back_track_paretosort);
  bt.gen_instance(
    score, instance_->product->bt_score_product(), back_track_paretosort);

  if (adp_specialization != ADP_Mode::STANDARD) {
      for (Product::iterator i = Product::begin(bt.get_instance()->product);
           i != Product::end(); ++i) {
          (*i)->set_adp_specialization(adp_specialization);
      }
  }

  // bt.gen_nt_decls(grammar()->nts());
  bt.apply_filter(backtrack_filter);
  if (original_product &&
  (original_product->is(Product::TAKEONE) || original_product->no_coopt()) )
  cg_mode.set_cooptimal(false);
  bt.gen_backtrack(*this);
  bt.gen_instance_code(*this);
}


void AST::set_adp_header(
  int spec, int pareto, bool multi_pareto, int step_mode) {
    rtlib_header = ADP_Mode::NONE;
    if (spec == 1) {
        if (step_mode) {
            rtlib_header = ADP_Mode::SORT_STEP;
        } else {
            rtlib_header = ADP_Mode::SORT_BLOCK;
        }
    } else if (spec == 2) {
        switch (pareto) {
            case 0:
                if (step_mode) {
                    rtlib_header = ADP_Mode::PARETO_NOSORT_STEP;
                } else {
                    rtlib_header = ADP_Mode::PARETO_NOSORT_BLOCK;
                }
                break;
            case 3:
                if (step_mode) {
                    rtlib_header = ADP_Mode::PARETO_YUK_STEP;
                } else {
                    rtlib_header = ADP_Mode::PARETO_YUK_BLOCK;
                }
                break;
            case 1:
            case 2:
                if (step_mode) {
                    rtlib_header = ADP_Mode::PARETO_SORT_STEP;
                } else {
                    rtlib_header = ADP_Mode::PARETO_SORT_BLOCK;
                }
                break;
        }
    }
}

Instance *AST::split_instance_for_backtrack(std::string &n) {
  Instance *i = instance(n);
  if (!i) {
    return 0;
  }
  check_instances(i);
  Product::Two *two = dynamic_cast<Product::Two*>(i->product);
  if (!two) {
    throw LogError("You have to use a product instance with --backtrack.");
  }
  /* well, !PRETTY is legitimate, too ...
  Algebra *pp = two->right()->algebra();
  if (!pp->is_compatible(Mode::PRETTY))
  throw LogError("--backtrack is not possible, because RHS of product is not"
  " of type pretty");
  */

  original_product = two;

  backtrack_product = two->right();
  fwd_product = two->left();
  backtrack_filter = two->filter();
  Instance *score = new Instance(i->name(), two->left(), grammar());
  return score;
}


std::pair<Instance*, Instance*> AST::split_classified(const std::string &n) {
  Instance *i = instance(n);
  if (!i) {
    throw LogError("Instance does not exist.");
  }
  check_instances(i);
  if (!i->product->left_is_classify()) {
    throw LogError("Left algebra is not classifying.");
  }
  Instance *score = 0;
  for (Product::iterator j = Product::begin(i->product);
       j != Product::end(); ++j) {
    if (!(*j)->is(Product::SINGLE)) {
      continue;
    }
    bool scoring = (*j)->algebra()->is_compatible(Mode::SCORING);
    if (scoring) {
      score = new Instance(i->name(), *j, grammar());
      break;
    }
  }
  for (Product::iterator j = Product::begin(i->product);
       j != Product::end(); ++j) {
    if (!(*j)->is(Product::SINGLE)) {
      continue;
    }
    bool pretty = (*j)->algebra()->is_compatible(Mode::PRETTY);
    if (pretty) {
      backtrack_product = *j;
      break;
    }
  }
  return std::make_pair(score, i);
}


#include "unused_visitor.hh"


void AST::warn_unused_fns(Instance &inst) {
  Unused_Visitor v;
  grammar()->traverse(v);
  hashtable<std::string, Fn_Def*> &fns = inst.product->algebra()->fns;
  for (hashtable<std::string, Fn_Def*>::iterator i = fns.begin();
       i != fns.end(); ++i) {
    Fn_Def *f = i->second;
    if (!f->in_use()) {
      Fn_Decl *x = signature->decl(*f->name);
      if (x) {
        Log::instance()->verboseMessage(x->location, "Signature symbol " +
          *f->name + " unused in grammar " + *grammar()->name + ".");
      }
    }
    inst.product->set_in_use(*f);
  }
}


#include "options.hh"


void AST::check_backtrack(const Options &opts) {
  if (opts.backtrack) {
    Algebra *a = instance_->product->algebra();
    for (hashtable<std::string, Fn_Def*>::iterator i = a->choice_fns.begin();
         i != a->choice_fns.end(); ++i) {
      if (i->second->choice_mode() == Mode::MANY) {
        throw LogError(
          i->second->location, "k-scoring choice function " + *i->second->name
          + " from instance " + *instance_->name() +
          " is not allowed with --backtrace. Try --kbacktrace instead.");
      }
    }
  }
  if (instance_->product->contains(Product::OVERLAY) && !opts.backtrack) {
    throw LogError(
      instance_->loc(),
      "The overlay product needs a --backtrace or --sample switch.");
  }
  cg_mode.set_sample(opts.sample);
}


#include "char_visitor.hh"


void AST::set_tracks() {
  size_t tracks = grammar()->axiom->tracks();
  input.set_tracks(tracks);

  size_t i = seq_decls.size();
  assert(i <= tracks);
  seq_decls.resize(tracks);
  for (; i < tracks; ++i) {
    std::ostringstream o;
    o << "t_" << i << "_seq";
    seq_decls[i] =
    new Statement::Var_Decl(new Type::Seq(), new std::string(o.str()));
  }
}


void AST::derive_temp_alphabet() {
  set_tracks();

  Char_Visitor v;

  // traverse will go through all reachable non-terminals
  // Char_Vistor will count CHAR types
  // this code will make sure only one datatype has been set for reading
  // the input (CHAR is type to read one char from inpustream)
  grammar()->traverse(v);
  const std::list<Type::Base*> &list = v.list();
  Type::Base *res = 0;
  switch (list.size()) {
  case 0 :
    res = new Type::Char();  // use default input
    break;
  case 1 :
    res = list.front();  // use found type
    char_type = res;
    break;
  default:
    throw LogError("Found multiple CHAR() terminal parsers with different"
            " argument types in the grammar."
            " This implies an ill defined alphabet.");
    break;
  }
  update_alphabet_types(res);
}

// set the type of the input to read
void AST::update_alphabet_types(Type::Base *res) {
  hashtable<std::string, Type::Base*>::iterator i = types.find("alphabet");
  assert(i != types.end());
  dynamic_cast<Type::Alphabet*>(i->second)->temp = res;

  hashtable<std::string, Symbol::Base*>::iterator j = grammar()->NTs.find(
    "CHAR");
  if (j != grammar()->NTs.end()) {
    Symbol::Terminal *t = dynamic_cast<Symbol::Terminal*>(j->second);
    assert(t);
    t->force_type(res);
  }
  j = grammar()->NTs.find("CHAR_SEP");
  if (j != grammar()->NTs.end()) {
    Symbol::Terminal *t = dynamic_cast<Symbol::Terminal*>(j->second);
    assert(t);
    t->force_type(res);
  }
  j = grammar()->NTs.find("NON");
  if (j != grammar()->NTs.end()) {
    Symbol::Terminal *t = dynamic_cast<Symbol::Terminal*>(j->second);
    assert(t);
    t->force_type(res);
  }
  hashtable<std::string, Fn_Decl*>::iterator k = Fn_Decl::builtins.find("CHAR");
  assert(k != Fn_Decl::builtins.end());
  k->second->return_type = res;
  k->second->types.clear();
  k->second->types.push_back(res);
}


void AST::update_seq_type(Instance &inst) {
  Algebra *a = inst.product->algebra();
  assert(a);
  hashtable<std::string, Type::Base*>::iterator i = a->params.find("alphabet");
  assert(i != a->params.end());
  Type::Base *type = i->second;
  if (char_type && !type->is_eq(*char_type)) {
    std::ostringstream o;
    o  << "Algebra alphabet type (" << *type << ") does not match the detected"
      << " CHAR() terminal parser type (" << *char_type << ").";
    throw LogError(inst.loc(), o.str());
  }
  for (std::vector<Statement::Var_Decl*>::iterator x = seq_decls.begin();
       x != seq_decls.end(); ++x) {
    Type::Seq *seq = dynamic_cast<Type::Seq*>((*x)->type);
    assert(seq);
    seq->element_type = type;
  }
}


void AST::set_window_mode(bool w) {
  if (!w)
    return;
  if (grammar()->axiom->tracks() > 1)
    throw LogError("Window mode currently just works with one-track grammars.");

  window_mode = Bool(w);

  grammar()->window_table_dims();
}


bool AST::grammar_defined(const std::string &n) const {
  assert(grammars_);
  for (std::list<Grammar*>::iterator i = grammars_->begin();
       i != grammars_->end(); ++i) {
    if (*(*i)->name == n) {
      return true;
    }
  }
  return false;
}


/*
 * Return the selected grammar.
 */
Grammar *AST::grammar() const {
  assert(selected_grammar);
  return selected_grammar;
}


/*
 * Returns the grammar with the given name.
 */
Grammar *AST::grammar(const std::string &n) {
  for (std::list<Grammar*>::iterator i = grammars_->begin();
       i != grammars_->end(); ++i) {
    if (*(*i)->name == n) {
      return *i;
    }
  }
  assert(0);
  return 0;
}

void AST::set_grammars(std::list<Grammar*> *g) {
  grammars_ = g;
  if (!grammars_->empty()) {
    selected_grammar = grammars_->back();
  }

  std::map<std::string, Grammar*> h;
  for (std::list<Grammar*>::iterator i = grammars_->begin();
       i != grammars_->end(); ++i) {
    std::map<std::string, Grammar*>::iterator a = h.find(*(*i)->name);
    if (a != h.end()) {
      Log::instance()->error((*i)->location, "Grammar name is used");
      Log::instance()->error(a->second->location, "before.");
      return;
    } else {
      h[*(*i)->name] = *i;
    }
  }
}


/*
 * Sets the selected grammar for the next steps of processing.
 * The only place this method is called from is the global
 * compiler driver gapc.cc, at that moment when the front end
 * sets the instance name found in the command line options.
 */
void AST::select_grammar(const std::string &instname) {
  // If the instance name given by the parameter is empty, this
  // next assignment gets an instance that is defined first
  // in the source-code file.
  Instance *i = instance(instname);
  if (!i) {
    return;
  }
  selected_grammar = i->grammar();
}


#include "statement/hash_decl.hh"


void AST::set_class_name(const std::string &n) {
  for (std::list<Statement::Hash_Decl*>::iterator i = hash_decls_.begin();
       i != hash_decls_.end(); ++i) {
    (*i)->set_class_name(n);
  }
}
