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
#include <cassert>
#include <iostream>
#include <list>
#include <string>

#include "product.hh"
#include "log.hh"
#include "yieldsize.hh"
#include "fn_def.hh"
#include "var_acc.hh"
#include "statement.hh"

Product::Base::Base(Type t, const Loc &l)
  : type_(t), adp_specialization(ADP_Mode::STANDARD),
    location(l), algebra_(NULL),
    bt_score_algebra_(0), sorted_choice(NONE),
    float_accuracy(0), filter_(0),
    src_vacc(0), dst_vacc(0), sort_product(0) {
}

Product::Base::Base(Type t)
  : type_(t), adp_specialization(ADP_Mode::STANDARD),
    algebra_(NULL), bt_score_algebra_(0), sorted_choice(NONE),
    float_accuracy(0), filter_(0),
    src_vacc(0), dst_vacc(0), sort_product(0) {
}

Product::Base::~Base() {}

Product::Single::Single(Algebra *a) : Base(SINGLE) {
  algebra_ = a;
}

void Product::Base::set_sorted_choice(Product::Sort_Type st) {
  sorted_choice = st;
}

Product::Sort_Type Product::Base::get_sorted_choice() {
  return sorted_choice;
}

bool Product::Base::is_sorted_choice() {
  switch (sorted_choice) {
    case Product::STANDARD:
    case Product::MULTI:
    case Product::NULLARY_SORTER:
    case Product::NULLARY_COMPERATOR:
    case Product::NULLARY_COMPERATOR_SORTER:
        return true;
    default:
        return false;
  }
  return false;
}

// if the choice function return type is a LIST, but in fact only 1 element
// is returned the LIST will be reduced to BASE
void Product::Base::reduce_return_type() {
  for (hashtable<std::string, Fn_Def*>::iterator i =
       algebra_->choice_fns.begin(); i != algebra_->choice_fns.end(); ++i) {
    Fn_Def *fn = i->second;
    if (fn->choice_mode().number == 1) {
      fn->reduce_return_type();
    }
  }
}

// looks for all choice function that don't need LIST as return type,
// because only 1 Element is returned
void Product::Single::eliminate_lists() {
  if (eliminate_lists_computed)
    return;

  reduce_return_type();

  eliminate_lists_computed = Bool(true);
}

// looks for all choice function that don't need LIST as return type,
// because only 1 Element is returned
void Product::Two::eliminate_lists() {
  if (eliminate_lists_computed)
    return;

  reduce_return_type();
  l->eliminate_lists();
  r->eliminate_lists();

  eliminate_lists_computed = Bool(true);
}

bool Product::Single::init() {
  return true;
}

// l, r are subproducts, so init them as well and then join the grammar
bool Product::Two::init() {
  bool x = true;
  bool b = l->init();
  x = x && b;
  b = r->init();
  x = x && b;
  if (x)
    algebra_ = new Algebra(*l->algebra(), *r->algebra());
  else
    algebra_ = new Algebra();
  return x;
}

bool Product::Times::init() {
  bool x = Two::init();

  // FIXME
  if (l->is(OVERLAY))
    return true;

  // iterate over left algebra choice functions
  for (hashtable<std::string, Fn_Def*>::iterator i =
       l->algebra()->choice_fns.begin();
       i != l->algebra()->choice_fns.end(); ++i) {
    // choice function in left algebra
    Fn_Def *fn_l = i->second;

    // choice function in right algebra
    // i->first guarantees same named choice function is for all  algebras if
    // multiple choice functions exist
    hashtable<std::string, Fn_Def*>::iterator j =
      r->algebra()->choice_fns.find(i->first);
    assert(j != r->algebra()->choice_fns.end());

    // choice function in joined algebra
    // i->first guarantees same named choice function is for all  algebras if
    // multiple choice functions exist
    Fn_Def *fn_r = j->second;
    hashtable<std::string, Fn_Def*>::iterator k =
      algebra_->choice_fns.find(i->first);
    assert(k != algebra_->choice_fns.end());
    Fn_Def *fn = k->second;


    // set the mode for the joined algebra
    if (fn_l->choice_mode() == Mode::SYNOPTIC) {
      // FIXME error or not
      Log::instance()->warning(location,
          "Left hand side is synoptic. Usually this violates Bellman's "
          "Principle. Only useful in special cases of k-backtracking.");
      /* FIXME
      x = x && false;
      continue;
       */
    }
    if (fn_l->choice_mode() == Mode::PRETTY &&
        !(fn_r->choice_mode() == Mode::PRETTY ||
          fn_r->choice_mode() == Mode::CLASSIFY)) {
      fn->choice_mode().set(Mode::PRETTY);
      fn->choice_mode().set(Yield::Poly(Yield::UP));
      x = x && true;
      continue;
    }
    if (fn_r->choice_mode() == Mode::SYNOPTIC) {
      fn->choice_mode().set(Mode::SYNOPTIC);
      fn->choice_mode().set(fn_l->choice_mode().number);
      x = x && true;
      continue;
    }
    if (fn_l->choice_mode() == Mode::SCORING &&
        fn_r->choice_mode() != Mode::SCORING) {
      fn->choice_mode().set(Mode::SCORING);
      fn->choice_mode().set(Yield::Poly(Yield::UP));
      x = x && true;
      continue;
    }
    if (fn_l->choice_mode() == Mode::SCORING &&
        fn_r->choice_mode() == Mode::SCORING) {
      fn->choice_mode().set(Mode::SCORING);
      x = x && true;
      continue;
    }
    if ((fn_l->choice_mode() == Mode::PRETTY ||
         fn_l->choice_mode() == Mode::CLASSIFY) &&
        (fn_r->choice_mode() == Mode::PRETTY ||
         fn_r->choice_mode() == Mode::CLASSIFY)) {
      fn->choice_mode().set(Mode::PRETTY);
      fn->choice_mode().set(Yield::Poly(Yield::UP));
      x = x && true;
      continue;
    }
    if (fn_l->choice_mode() == Mode::KSCORING ||
        fn_r->choice_mode() == Mode::KSCORING) {
      fn->choice_mode().set(Mode::KSCORING);
      fn->choice_mode().set(Yield::Poly(Yield::UP));
      x = x && true;
      continue;
    }
    std::ostringstream o;
    o << "Unknown combination: " << fn_l->choice_mode() << " , "
      << fn_r->choice_mode() << " (defaulting to kscoring)";
    // FIXME error or not
    Log::instance()->warning(location, o.str());
    /* FIXME
    x = x && false;
    */
    fn->choice_mode().set(Mode::KSCORING);
    fn->choice_mode().set(Yield::Poly(Yield::UP));
  }
  return x;
}

bool Product::Klass::init() {
  bool x = Two::init();

  // iterate over left algebra choice functions
  for (hashtable<std::string, Fn_Def*>::iterator i =
       l->algebra()->choice_fns.begin();
       i != l->algebra()->choice_fns.end(); ++i) {
     // choice function in left algebra
    Fn_Def *fn_l = i->second;

    // choice function in right algebra
    hashtable<std::string, Fn_Def*>::iterator j =
      r->algebra()->choice_fns.find(i->first);
    assert(j != r->algebra()->choice_fns.end());
    Fn_Def *fn_r = j->second;

    // choice function in joined algebra
    hashtable<std::string, Fn_Def*>::iterator k =
      algebra_->choice_fns.find(i->first);
    assert(k != algebra_->choice_fns.end());
    Fn_Def *fn = k->second;

    // set the mode for the joined algebra
    if (fn_l->choice_mode() != Mode::CLASSIFY) {
      Log::instance()->error(location, "LHS is not a classifying algebra.");
      x = x && false;
      continue;
    }
    if (fn_r->choice_mode() != Mode::SCORING) {
      Log::instance()->error(location, "RHS is not a k-scoring algebra.");
      x = x && false;
      continue;
    }
    fn->set_mode(fn_r->choice_mode());
    x = x && true;
  }
  return x;
}

bool Product::Cartesian::init() {
  bool x = Two::init();

  // iterate over left algebra choice functions
  for (hashtable<std::string, Fn_Def*>::iterator i =
       l->algebra()->choice_fns.begin();
       i != l->algebra()->choice_fns.end(); ++i) {
    // choice function in left algebra
    Fn_Def *fn_l = i->second;

    // choice function in right algebra
    hashtable<std::string, Fn_Def*>::iterator j =
      r->algebra()->choice_fns.find(i->first);
    assert(j != r->algebra()->choice_fns.end());
    Fn_Def *fn_r = j->second;

    // choice function in joined algebra
    hashtable<std::string, Fn_Def*>::iterator k =
      algebra_->choice_fns.find(i->first);
    assert(k != algebra_->choice_fns.end());
    Fn_Def *fn = k->second;

    // set the mode for the joined algebra

    if (fn_l->choice_mode().number > 1) {
      Log::instance()->error(location, "LHS has more than one solution.");
      x = x && false;
      continue;
    }
    if (fn_r->choice_mode().number > 1) {
      Log::instance()->error(location, "RHS has more than one solution.");
      x = x && false;
      continue;
    }
    if (fn_l->choice_mode() == Mode::SYNOPTIC ||
        fn_r->choice_mode() == Mode::SYNOPTIC)
      fn->choice_mode().set(Mode::SYNOPTIC);
    else
      fn->choice_mode().set(Mode::SCORING);
    fn->choice_mode().set(Yield::Poly(1));
  }
  return x;
}

bool Product::Pareto::init() {
  bool x = Two::init();


  // iterate over left algebra choice functions
  for (hashtable<std::string, Fn_Def*>::iterator i =
       l->algebra()->choice_fns.begin();
       i != l->algebra()->choice_fns.end(); ++i) {
    // choice function in left algebra
    Fn_Def *fn_l = i->second;

    // choice function in right algebra
    // i->first guarantees same named choice function is for all  algebras
    // if multiple choice functions exist
    hashtable<std::string, Fn_Def*>::iterator j =
      r->algebra()->choice_fns.find(i->first);
    assert(j != r->algebra()->choice_fns.end());

    // choice function in joined algebra
    // i->first guarantees same named choice function is for all
    // algebras if multiple choice functions exist
    Fn_Def *fn_r = j->second;
    hashtable<std::string, Fn_Def*>::iterator k =
      algebra_->choice_fns.find(i->first);
    assert(k != algebra_->choice_fns.end());
    Fn_Def *fn = k->second;


    // set the mode for the joined algebra

    // filter out synoptic products, since they produce objects not
    // found directly in search space
    if (fn_l->choice_mode() == Mode::SYNOPTIC) {
        Log::instance()->error(
          location, "LHS Cannot be synoptic for Pareto Products.");
        x = x && false;
        continue;
    }
    if (fn_r->choice_mode() == Mode::SYNOPTIC) {
        Log::instance()->error(
          location, "RHS Cannot be synoptic for Pareto Products.");
        x = x && false;
        continue;
    }

    // warn if choice functions are not scoring, ideally they should
    // only yield one result each
    if (fn_l->choice_mode() != Mode::SCORING ||
       fn_r->choice_mode() != Mode::SCORING) {
        Log::instance()->warning(
          location,
          "!! (Ignore for option --multi-dim-pareto) !! For Pareto product, "
          "choice functions should yield only one result. Only the first "
          "element of the result list will be used.");
    }

    fn->choice_mode().set(Mode::KSCORING);
    fn->choice_mode().set(Yield::Poly(Yield::UP));
  }
  return x;
}

void Product::Pareto::set_pareto_type(int i) {
  switch (i) {
    case 1:
        pareto_type = Product::Pareto::Sort;
        break;
    case 2:
        pareto_type = Product::Pareto::ISort;
        break;
    case 3:
        pareto_type = Product::Pareto::MultiDimOpt;
        break;
    case 4:
        pareto_type = Product::Pareto::NoSortDomOpt;
        break;
    default:
        pareto_type = Product::Pareto::NoSort;
        break;
  }
}


// set the target_name of all contained functions by adding p as suffix to
// the function name
void Product::Single::init_fn_suffix(const std::string &p) {
  algebra_->init_fn_suffix(p);
  fn_suffix = p;
}

// set the target_name of all contained functions by adding p as suffix to the
// function name
void Product::Two::init_fn_suffix(const std::string &p) {
  algebra_->init_fn_suffix(p);
  fn_suffix = p;
  l->init_fn_suffix(p + "_l");
  r->init_fn_suffix(p + "_r");
}

void Product::Base::install_choice_filter() {
  if (!filter_)
    return;
  algebra_->install_choice_filter(*filter_);
}

void Product::Single::codegen() {
  algebra_->codegen();
  install_choice_filter();
}

void Product::Two::codegen() {
  l->codegen();
  r->codegen();
  algebra_->codegen(*this);
  install_choice_filter();
}

void Product::Single::print_code(Printer::Base &s) {
  algebra_->print_code(s);
}

void Product::Two::print_code(Printer::Base &s) {
  algebra_->print_code(s);
  l->print_code(s);
  r->print_code(s);
}

unsigned int Product::Single::width() {
  return 1;
}

unsigned int Product::Two::width() {
  return l->width() + r->width();
}

Algebra *Product::Single::nth_algebra(unsigned int &n) {
  if (n) {
    n--;
    return NULL;
  }
  return algebra_;
}

Algebra *Product::Two::nth_algebra(unsigned int &n) {
  Algebra *a = l->nth_algebra(n);
  Algebra *b = r->nth_algebra(n);
  if (a)
    return a;
  if (b)
    return b;
  return NULL;
}


bool Product::Base::contains_only_times() {
  return false;
}

bool Product::Single::contains_only_times() {
  return true;
}

bool Product::Times::contains_only_times() {
  return l->contains_only_times() && r->contains_only_times();
}

bool Product::Nop::contains_only_times() {
  return l->contains_only_times() && r->contains_only_times();
}

Product::Base *Product::Base::left_most() {
  std::abort();
  return 0;
}

Product::Base *Product::Base::right_most() {
  std::abort();
  return 0;
}

Product::Base *Product::Single::left_most() {
  return this;
}

Product::Base *Product::Single::right_most() {
  return this;
}

Product::Base *Product::Two::left_most() {
  return l->left_most();
}

Product::Base *Product::Two::right_most() {
  return r->right_most();
}

Product::Base *Product::Base::optimize_shuffle_products() {
  return this;
}

Product::Base *Product::Two::optimize_shuffle_products() {
  l = l->optimize_shuffle_products();
  r = r->optimize_shuffle_products();
  return this;
}


// make impossible product combinations a NOP and remove this original product
Product::Base *Product::Times::optimize_shuffle_products() {
  l = l->optimize_shuffle_products();
  r = r->optimize_shuffle_products();

  bool x = true;
  // loop over left algebra
  for (hashtable<std::string, Fn_Def*>::iterator i =
       l->algebra()->choice_fns.begin();
       i != l->algebra()->choice_fns.end(); ++i) {
    // left algebra choice function
    Fn_Def *fn_l = i->second;

    // right algebra choice function (i->first = same name if
    // multiple choice functions exist)
    hashtable<std::string, Fn_Def*>::iterator j =
      r->algebra()->choice_fns.find(i->first);
    assert(j != r->algebra()->choice_fns.end());
    Fn_Def *fn_r = j->second;

    if ((fn_l->choice_mode() == Mode::PRETTY
      && fn_r->choice_mode() == Mode::PRETTY)

      || (fn_l->choice_mode() == Mode::PRETTY
      && fn_r->choice_mode() == Mode::SCORING)

      || (fn_l->choice_mode() == Mode::PRETTY
      && fn_r->choice_mode() == Mode::SYNOPTIC
      && fn_r->choice_fn_type() == Expr::Fn_Call::SUM)

      || (fn_l->choice_mode() == Mode::CLASSIFY
      && fn_r->choice_mode() == Mode::PRETTY
      )

      || (fn_l->choice_mode() == Mode::PRETTY
      && fn_r->choice_mode() == Mode::CLASSIFY
      )

      || (fn_l->choice_mode() == Mode::CLASSIFY
      && fn_r->choice_mode() == Mode::CLASSIFY
      )

      ) {
      x = x && true;
    } else {
      x = false;
      break;
    }
  }

  if (x) {
    Base *t = new Product::Nop(*this);
    delete this;
    return t;
  }
  return this;
}

Mode & Product::Two::left_mode(const std::string &s) {
  hashtable<std::string, Fn_Def*>::iterator i =
    l->algebra()->choice_fns.find(s);
  assert(i != l->algebra()->choice_fns.end());
  return i->second->choice_mode();
}

Expr::Fn_Call::Builtin Product::Two::left_choice_fn_type(
  const std::string &s) const {
  hashtable<std::string, Fn_Def*>::iterator i =
    l->algebra()->choice_fns.find(s);
  assert(i != l->algebra()->choice_fns.end());
  return i->second->choice_fn_type();
}

Fn_Def* Product::Two::left_choice_function(const std::string &s) {
  hashtable<std::string, Fn_Def*>::iterator i =
    l->algebra()->choice_fns.find(s);
  assert(i != l->algebra()->choice_fns.end());
  return i->second;
}

Mode & Product::Two::right_mode(const std::string &s) {
  hashtable<std::string, Fn_Def*>::iterator i =
    r->algebra()->choice_fns.find(s);
  assert(i != r->algebra()->choice_fns.end());
  return i->second->choice_mode();
}

Expr::Fn_Call::Builtin Product::Two::right_choice_fn_type(
  const std::string &s) const {
  hashtable<std::string, Fn_Def*>::iterator i =
    r->algebra()->choice_fns.find(s);
  assert(i != r->algebra()->choice_fns.end());
  return i->second->choice_fn_type();
}

Fn_Def* Product::Two::right_choice_function(const std::string &s) {
  hashtable<std::string, Fn_Def*>::iterator i =
    l->algebra()->choice_fns.find(s);
  assert(i != r->algebra()->choice_fns.end());
  return i->second;
}

Product::Nop::Nop(Times &times) : Two(NOP, times.left(), times.right()) {
  set_filter(times.filter());

  // loop over joined algebra and set mode and yield for all
  algebra_ = new Algebra(*l->algebra(), *r->algebra());
  for (hashtable<std::string, Fn_Def*>::iterator i =
       algebra_->choice_fns.begin();
       i != algebra_->choice_fns.end(); ++i) {
    Fn_Def *fn = i->second;
    fn->choice_mode().set(Mode::PRETTY);
    fn->choice_mode().set(Yield::Poly(Yield::UP));
  }
}

// add function found in algebra by name to list l
void Product::Base::collect_fns(
  std::list<Fn_Def*> &l, const std::string &name) {
  assert(algebra_);
  Fn_Def *fn = algebra_->fn_def(name);
  l.push_back(fn);
}

// add function found in left and right algebra by name to list list
void Product::Two::collect_fns(
  std::list<Fn_Def*> &list, const std::string &name) {
  Base::collect_fns(list, name);
  l->collect_fns(list, name);
  r->collect_fns(list, name);
}


bool Product::Overlay::contains_only_times() {
  return l->contains_only_times() && r->contains_only_times();
}


// overlay product uses different algebras for forward (algebra_)
// and backtracing (bt_score_algebra_))
bool Product::Overlay::init() {
  bool x = true;
  bool b = l->init();
  x = x && b;
  b = r->init();
  x = x && b;
  algebra_ = l->algebra();
  bt_score_algebra_ = r->algebra();
  return x;
}

// returns the backtrace algebra
// for overlay product this is different
// for all other it's the same
Algebra *Product::Base::bt_score_algebra() {
  if (bt_score_algebra_) {
    return bt_score_algebra_;
  } else {
    return algebra_;
  }
}

// for all types except Overlay backtrace is calculated using the normal product
Product::Base *Product::Base::bt_score_product() {
  return this;
}

// for Overlay the backtrace is calculated using the right product
Product::Base *Product::Overlay::bt_score_product() {
  return r;
}

// looks for all choice function that don't need LIST as return type,
// because only 1 Element is returned
void Product::Overlay::eliminate_lists() {
  l->eliminate_lists();
  r->eliminate_lists();
}

void Product::Overlay::codegen() {
  l->codegen();
  r->codegen();
}

void Product::Overlay::print_code(Printer::Base &s) {
  algebra_->print_code(s);
}

// set the target_name of all contained functions by adding p as suffix
// to the function name
void Product::Overlay::init_fn_suffix(const std::string &p) {
  algebra_->init_fn_suffix(p);
  fn_suffix = p;
}

bool Product::Base::contains(Type t) {
  return false;
}

bool Product::Two::contains(Type t) {
  return is(t) || l->contains(t) || r->contains(t);
}

void Product::Base::set_in_use(const Fn_Decl &f) {
  hashtable<std::string, Fn_Def*>::iterator i = algebra_->fns.find(*f.name);
  assert(i != algebra_->fns.end());
  i->second->set_in_use(f.in_use());
}

void Product::Two::set_in_use(const Fn_Decl &f) {
  Base::set_in_use(f);
  l->set_in_use(f);
  r->set_in_use(f);
}

bool Product::Takeone::init() {
  bool x = Two::init();
  // loop over left algebra choice functions
  for (hashtable<std::string, Fn_Def*>::iterator i =
       l->algebra()->choice_fns.begin();
       i != l->algebra()->choice_fns.end(); ++i) {
    // left algebra choice function
    Fn_Def *fn_l = i->second;

    // right algebra choice function (same named choice functions by
    // i->first, in cause multiple)
    hashtable<std::string, Fn_Def*>::iterator j =
      r->algebra()->choice_fns.find(i->first);
    assert(j != r->algebra()->choice_fns.end());
    Fn_Def *fn_r = j->second;

    // joined algebra choice function (same named choice functions by
    // i->first, in cause multiple)
    hashtable<std::string, Fn_Def*>::iterator k =
      algebra_->choice_fns.find(i->first);
    assert(k != algebra_->choice_fns.end());
    Fn_Def *fn = k->second;

    // set mode of joined algebra
    if (fn_l->choice_mode().number > 1) {
      Log::instance()->error(location, "LHS has more than one solution.");
      x = x && false;
      continue;
    }
    if (fn_r->choice_mode() == Mode::SYNOPTIC) {
      Log::instance()->error(location, "RHS is synoptic.");
      x = x && false;
      continue;
    }
    if (fn_l->choice_mode() == Mode::SYNOPTIC ||
        fn_r->choice_mode() == Mode::SYNOPTIC)
      fn->choice_mode().set(Mode::SYNOPTIC);
    else
      fn->choice_mode().set(Mode::SCORING);
    fn->choice_mode().set(Yield::Poly(1));
  }
  return x;
}

Product::Base *Product::Base::left() { assert(0); return 0; }
Product::Base *Product::Base::right() { assert(0); return 0; }

// vaccs print a string representation to an output stream
void Product::Base::init_vacc(Var_Acc::Base *src, Var_Acc::Base *dst) {
  assert(0); std::abort();
}

// vaccs print a string representation to an output stream
void Product::Single::init_vacc(Var_Acc::Base *src, Var_Acc::Base *dst) {
  src_vacc = src;
  dst_vacc = dst;
}

// vaccs print a string representation to an output stream
void Product::Two::init_vacc(Var_Acc::Base *src, Var_Acc::Base *dst) {
  src_vacc = src;
  dst_vacc = dst;
  l->init_vacc(new Var_Acc::Comp(src, new std::string("first")),
               new Var_Acc::Comp(dst, new std::string("first")) );
  r->init_vacc(new Var_Acc::Comp(src, new std::string("second")),
               new Var_Acc::Comp(dst, new std::string("second")) );
}

#include "filter.hh"
#include "statement/fn_call.hh"

void Product::Base::generate_filter_decl(
    std::list<Statement::Base*> &hash_code,
    std::list<Statement::Var_Decl*> &filters) const {
  if (filter_) {
    Statement::Var_Decl *filter_decl = new Statement::Var_Decl(
        new ::Type::External(filter_->name),
        *filter_->name + "_" + fn_suffix);
    Statement::Fn_Call *update = new Statement::Fn_Call(
      Statement::Fn_Call::UPDATE);

    update->add_arg(*filter_decl);
    update->add_arg(new Expr::Vacc(src_vacc));
    hash_code.push_back(update);
    filters.push_back(filter_decl);
  }
}

void Product::Base::generate_hash_decl(const Fn_Def &fn,
    std::list<Statement::Base*> &hash_code,
    std::list<Statement::Var_Decl*> &filters,
    std::list<Statement::Base*> &finalize_code,
    std::list<Statement::Base*> &init_code,
    std::list<Statement::Base*> &equal_score_code,
    std::list<Statement::Base*> &compare_code) const {
  assert(0);
  std::abort();
}

void Product::Single::generate_hash_decl(const Fn_Def &fn_def,
    std::list<Statement::Base*> &hash_code,
    std::list<Statement::Var_Decl*> &filters,
    std::list<Statement::Base*> &finalize_code,
    std::list<Statement::Base*> &init_code,
    std::list<Statement::Base*> &equal_score_code,
    std::list<Statement::Base*> &compare_code) const {
  Fn_Def *fn = algebra_->fn_def(*fn_def.name);
  if (fn->choice_mode() == Mode::CLASSIFY)
    return;

  generate_filter_decl(hash_code, filters);

  assert(src_vacc);
  assert(dst_vacc);
  Expr::Vacc *e = new Expr::Vacc(src_vacc);
  Expr::Vacc *f = new Expr::Vacc(dst_vacc);
  switch (fn->choice_fn_type()) {
    case Expr::Fn_Call::MINIMUM :
      {
      Expr::Base *cexpr = new Expr::Less(e, f);
      Statement::If *cond = new Statement::If(
        cexpr, new Statement::Var_Assign(dst_vacc, e));
      hash_code.push_back(cond);

      equal_score_code.push_back(new Statement::Return(new Expr::Eq(e, f)));
      compare_code.push_back(new Statement::Return(cexpr));
      }
      break;
    case Expr::Fn_Call::MAXIMUM :
      {
      Expr::Base *cexpr = new Expr::Greater(e, f);
      Statement::If *cond = new Statement::If(
        cexpr, new Statement::Var_Assign(dst_vacc, e));
      hash_code.push_back(cond);

      equal_score_code.push_back(new Statement::Return(new Expr::Eq(e, f)));
      compare_code.push_back(new Statement::Return(cexpr));
      }
      break;
    case Expr::Fn_Call::SUM:
      {
      Statement::Var_Assign *ass = new Statement::Var_Assign(dst_vacc, e);
      ass->set_op(Expr::PLUS);
      hash_code.push_back(ass);
      }
      break;
    case Expr::Fn_Call::EXPSUM:
      {
      Expr::Fn_Call *l = new Expr::Fn_Call(Expr::Fn_Call::EXP);
      l->add_arg(e);
      Statement::Var_Assign *ass = new Statement::Var_Assign(dst_vacc, l);
      ass->set_op(Expr::PLUS);
      hash_code.push_back(ass);

      Expr::Fn_Call *fn = new Expr::Fn_Call(Expr::Fn_Call::LOG);
      fn->add_arg(src_vacc);
      Statement::Var_Assign *fin = new Statement::Var_Assign(src_vacc, fn);
      finalize_code.push_back(fin);

      Expr::Fn_Call *ex = new Expr::Fn_Call(Expr::Fn_Call::EXP);
      ex->add_arg(e);
      Statement::Var_Assign *expass = new Statement::Var_Assign(dst_vacc, ex);
      init_code.push_back(expass);
      }
      break;
    case Expr::Fn_Call::EXP2SUM:
      {
      Expr::Fn_Call *l = new Expr::Fn_Call(Expr::Fn_Call::EXP2);
      l->add_arg(e);
      Statement::Var_Assign *ass = new Statement::Var_Assign(dst_vacc, l);
      ass->set_op(Expr::PLUS);
      hash_code.push_back(ass);

      Expr::Fn_Call *fn = new Expr::Fn_Call(Expr::Fn_Call::LOG2);
      fn->add_arg(src_vacc);
      Statement::Var_Assign *fin = new Statement::Var_Assign(src_vacc, fn);
      finalize_code.push_back(fin);

      Expr::Fn_Call *ex = new Expr::Fn_Call(Expr::Fn_Call::EXP2);
      ex->add_arg(e);
      Statement::Var_Assign *expass = new Statement::Var_Assign(dst_vacc, ex);
      init_code.push_back(expass);
      }
      break;
    case Expr::Fn_Call::BITSUM:
      {
      Expr::Fn_Call *l = new Expr::Fn_Call(Expr::Fn_Call::POW);
      l->add_arg(new Expr::Const(2.0));
      l->add_arg(e);
      Statement::Var_Assign *ass = new Statement::Var_Assign(dst_vacc, l);
      ass->set_op(Expr::PLUS);
      hash_code.push_back(ass);

      Expr::Fn_Call *f1 = new Expr::Fn_Call(Expr::Fn_Call::LOG);
      f1->add_arg(src_vacc);
      Expr::Fn_Call *f2 = new Expr::Fn_Call(Expr::Fn_Call::LOG);
      f2->add_arg(new Expr::Const(2.0));
      Statement::Var_Assign *fin = new Statement::Var_Assign(
        src_vacc, new Expr::Div(f1, f2));
      finalize_code.push_back(fin);

      Expr::Fn_Call *ex = new Expr::Fn_Call(Expr::Fn_Call::POW);
      ex->add_arg(new Expr::Const(2.0));
      ex->add_arg(e);
      Statement::Var_Assign *expass = new Statement::Var_Assign(dst_vacc, ex);
      init_code.push_back(expass);
      }
      break;
    default:
      std::cerr << "Type: " << fn->choice_fn_type() << '\n';
      assert(0);
      std::abort();
  }
}

void Product::Times::generate_hash_decl(const Fn_Def &fn,
    std::list<Statement::Base*> &hash_code,
    std::list<Statement::Var_Decl*> &filters,
    std::list<Statement::Base*> &finalize_code,
    std::list<Statement::Base*> &init_code,
    std::list<Statement::Base*> &equal_score_code,
    std::list<Statement::Base*> &compare_code) const {
  std::list<Statement::Base*> a, b;
  l->generate_hash_decl(fn, a, filters, finalize_code, init_code,
                        equal_score_code, compare_code);
  r->generate_hash_decl(fn, b, filters, finalize_code, init_code,
                        equal_score_code, compare_code);
  generate_filter_decl(hash_code, filters);

  if (a.empty()) {
    hash_code.insert(hash_code.end(),
        b.begin(), b.end());
    return;
  }

  Statement::If *cond = dynamic_cast<Statement::If*>(a.back());
  assert(cond);
  cond->then.insert(cond->then.end(), b.begin(), b.end());
  hash_code.insert(hash_code.end(), a.begin(), a.end());
}

void Product::Cartesian::generate_hash_decl(const Fn_Def &fn,
    std::list<Statement::Base*> &hash_code,
    std::list<Statement::Var_Decl*> &filters,
    std::list<Statement::Base*> &finalize_code,
    std::list<Statement::Base*> &init_code,
    std::list<Statement::Base*> &equal_score_code,
    std::list<Statement::Base*> &compare_code) const {
  std::list<Statement::Base*> a, b;
  l->generate_hash_decl(fn, a, filters, finalize_code, init_code,
                        equal_score_code, compare_code);
  r->generate_hash_decl(fn, b, filters, finalize_code, init_code,
                        equal_score_code, compare_code);
  generate_filter_decl(hash_code, filters);

  hash_code.insert(hash_code.end(), a.begin(), a.end());
  hash_code.insert(hash_code.end(), b.begin(), b.end());
}



void Product::Pareto::generate_hash_decl(const Fn_Def &fn,
    std::list<Statement::Base*> &hash_code,
    std::list<Statement::Var_Decl*> &filters,
    std::list<Statement::Base*> &finalize_code,
    std::list<Statement::Base*> &init_code,
    std::list<Statement::Base*> &equal_score_code,
    std::list<Statement::Base*> &compare_code) const {
  std::list<Statement::Base*> a, b;
  l->generate_hash_decl(fn, a, filters, finalize_code, init_code,
                        equal_score_code, compare_code);
  r->generate_hash_decl(fn, b, filters, finalize_code, init_code,
                        equal_score_code, compare_code);
  generate_filter_decl(hash_code, filters);

  // TODO(who?): Is this really the real insert condition? Testing needed.
  hash_code.insert(hash_code.end(), a.begin(), a.end());
  hash_code.insert(hash_code.end(), b.begin(), b.end());
}

bool Product::Base::left_is_classify() {
  unsigned int t = 0;
  Algebra *l = nth_algebra(t);
  for (hashtable<std::string, Fn_Def*>::const_iterator i =
         l->choice_fns.begin(); i != l->choice_fns.end(); ++i) {
    Fn_Def *f = i->second;
    if (f->choice_mode() != Mode::CLASSIFY)
      return false;
  }
  return true;
}

bool Product::Base::one_per_class() {
  Product::iterator i = Product::begin(this);
  for ( ; i != Product::end(); ++i)
    if ((*i)->is(Product::SINGLE)) {
      ++i;
      break;
    }
  for ( ; i != Product::end(); ++i) {
    Product::Single *p = dynamic_cast<Product::Single*>(*i);
    if (!p)
      continue;
    for (hashtable<std::string, Fn_Def*>::const_iterator j =
         (*i)->algebra()->choice_fns.begin();
         j != (*i)->algebra()->choice_fns.end(); ++j) {
      Fn_Def *f = j->second;
      if (f->choice_mode() != Mode::ONE)
        return false;
    }
  }
  return true;
}

Bool Product::Base::no_coopt_;
Bool Product::Base::no_coopt_class_;


Product::Base *Product::Single::replace_classified(bool &x) {
  return this;
}

Product::Base *Product::Two::replace_classified(bool &x) {
  l = l->replace_classified(x);
  r = r->replace_classified(x);
  return this;
}

Product::Base *Product::Klass::replace_classified(bool &x) {
  x = true;
  // new Product::Times(*dynamic_cast<Two*>(this));
  Times *t = new Product::Times(*this);
  return t->replace_classified(x);
}

Product::Times::Times(const Two &t) : Two(TIMES, t) {
}
Product::Times::Times(Base *a, Base *b, const Loc &lo) : Two(TIMES, lo, a, b) {
}

Product::Two::Two(Type t, const Two &x) : Base(t), l(x.l), r(x.r) {
}

bool Product::Single::contains_algebra(Algebra &a) {
  return this->algebra_ == &a;
}
bool Product::Two::contains_algebra(Algebra &a) {
  return l->contains_algebra(a) || r->contains_algebra(a);
}

std::string *Product::Single::get_component_accessor(Algebra &a) {
  return new std::string("");
}
std::string *Product::Two::get_component_accessor(Algebra &a) {
  std::string *acc;
  if (l->contains_algebra(a)) {
    acc = new std::string(".first" + *l->get_component_accessor(a));
  } else if (r->contains_algebra(a)) {
    acc = new std::string(".second" + *r->get_component_accessor(a));
  } else {
    acc = new std::string("");
  }
  return acc;
}
