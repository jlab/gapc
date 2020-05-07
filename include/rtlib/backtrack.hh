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

#ifndef RTLIB_BACKTRACK_HH
#define RTLIB_BACKTRACK_HH

#include <cassert>

// FIXME replace with more efficient version
#include <list>

#include <boost/intrusive_ptr.hpp>
using boost::intrusive_ptr;

template<typename Value>
class Eval_List {
  private:
    // FIXME more efficient version ...
    std::list<Value> list;
  public:
    size_t count;
    Eval_List()
      : count(0)
    {
    }
    typedef typename std::list<Value>::iterator iterator;
    iterator begin() { return list.begin(); }
    iterator end() { return list.end(); }
    void push_back(Value &v) { list.push_back(v); }

    template<typename O, typename T>
      void print(O &out, const T &v)
      {
        for (typename std::list<Value>::iterator i = list.begin();
             i != list.end(); ++i) {
          out << "( " << v << " , " << *i << " )\n";
        }
      }
};

template <typename Value, typename pos_int>
class Backtrace {
  private:
  public:
    size_t count;
    intrusive_ptr<Eval_List<Value> > evaluated;
    Backtrace()
      : count(0), evaluated(0)
    {}
    virtual ~Backtrace() { }
    virtual intrusive_ptr<Backtrace<Value, pos_int> > backtrack()
    {
      return intrusive_ptr<Backtrace<Value, pos_int> >(this);
    }

    virtual intrusive_ptr<Eval_List<Value> > eval() = 0;

    //virtual bool is_proxy() const { return false; }

    virtual void print(std::ostream &out) { assert(0); }

};


template <typename Value, typename pos_int>
class Backtrace_List : public virtual Backtrace<Value, pos_int> {
  private:
    // FIXME more efficient version
    std::list<intrusive_ptr<Backtrace<Value, pos_int> > > list;
  public:

    typedef typename std::list<intrusive_ptr<Backtrace<Value, pos_int> > >::iterator iterator;
    iterator begin() { return list.begin(); }
    iterator end() { return list.end(); }

    void push_back(intrusive_ptr<Backtrace<Value,pos_int> > x)
    {
      list.push_back(x);
    }
    
    intrusive_ptr<Eval_List<Value> > eval()
    {
      intrusive_ptr<Eval_List<Value> > l = new Eval_List<Value>();
      for (typename std::list<intrusive_ptr< Backtrace<Value, pos_int> > >::iterator i =
           list.begin();
           i != list.end(); ++i) {
        intrusive_ptr<Backtrace<Value, pos_int> > bt = *i;
        intrusive_ptr<Eval_List<Value> > elist = bt->eval();
        erase(bt);
#ifndef NDEBUG
        *i = 0;
#endif
        for (typename Eval_List<Value>::iterator j = elist->begin();
             j != elist->end();
             ++j) {
          l->push_back(*j);
        }
        erase(elist);
      }
      return l;
    }
};

/*
split
string eval()
{
  Eval_List answer;
  foreach l_bt in l  {
    foreach r_bt in r    {
      Eval_List l_elist = evaluate(l_bt);
      Eval_List r_elist = evaluate(r_bt);
      foreach l_elem in l_elist      {
        foreach r_elem in r_elist        {
          string ret = split(l_elem, r_elem);
          push_back( answer, ret);
        }

      }

      erase( l_elist);
      erase( r_elist);
    }

  }

  erase( l);
  erase( r);
  return answer;
}
*/

#include "list.hh"


template<typename S, typename T, typename pos_int, typename ref_int>
inline
intrusive_ptr<Backtrace<T, pos_int> > exe_bt
  (List_Ref<std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > >, ref_int>
     & list,
   bool allow_cooptimal)
{
  typedef List<std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > >, ref_int> list_t;
  intrusive_ptr<Backtrace_List<T, pos_int> > ret(new Backtrace_List<T, pos_int>());
  if (isEmpty(list)) {
    //assert(false);
    return ret;
  }
  list_t &l = list.ref();
  for (typename list_t::iterator i = l.begin();
       i != l.end(); ++i) {
    intrusive_ptr<Backtrace<T, pos_int> > sec = (*i).second;
    assert(sec);
    intrusive_ptr<Backtrace<T, pos_int> > x = sec->backtrack();
    if (x != sec)
      erase(sec);

    ret->push_back(x);
    if (!allow_cooptimal)
      break;
  }
  return ret;
}
template<typename S, typename T, typename pos_int, typename ref_int>
inline
intrusive_ptr<Backtrace<T, pos_int> > execute_backtrack
   (List_Ref<std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > >, ref_int>
      & list)
{
  return exe_bt(list, true);
}

template<typename S, typename T, typename pos_int, typename ref_int>
inline
intrusive_ptr<Backtrace<T, pos_int> > execute_backtrack_one
   (List_Ref<std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > >, ref_int>
      & list)
{
  return exe_bt(list, false);
}

template<typename S, typename T, typename pos_int>
inline
intrusive_ptr<Backtrace<T, pos_int>  > execute_backtrack
   (std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > > & tuple)
{
  intrusive_ptr<Backtrace_List<T, pos_int> > ret =
    new Backtrace_List<T, pos_int>();
  if (isEmpty(tuple)) {
    //assert(false);
    return ret;
  }
  intrusive_ptr<Backtrace<T, pos_int> > sec = tuple.second;
  assert(sec);
  intrusive_ptr<Backtrace<T, pos_int> > x = sec->backtrack();
  if (x != sec)
    erase(sec);

  ret->push_back(x);

  return ret;
}

template<typename S, typename T, typename pos_int>
inline
intrusive_ptr<Backtrace<T, pos_int>  > execute_backtrack_one
   (std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > > & tuple)
{
  return execute_backtrack(tuple);
}

template<typename Value, typename pos_int>
inline
intrusive_ptr<Eval_List<Value> > evaluate(intrusive_ptr<Backtrace<Value, pos_int> > bt)
{
  assert(bt);
  if (!bt->evaluated)
    bt->evaluated = bt->eval();
  return bt->evaluated;
}


template<typename Value>
inline
void erase(intrusive_ptr<Eval_List<Value> > &e)
{
  // FIXME
  e.reset();
}

template<typename Value, typename pos_int>
inline
void erase(intrusive_ptr<Backtrace<Value, pos_int> > /* * */ &bt)
{
  // FIXME
  bt.reset();

  /* FIXME
  delete bt;
  bt = 0;
  */
}

template<typename Value, typename pos_int>
inline
void intrusive_ptr_add_ref(Backtrace<Value, pos_int> *b)
{
  assert(b);
  b->count++;
}

template<typename Value, typename pos_int>
inline
void intrusive_ptr_release(Backtrace<Value, pos_int> *b)
{
  assert(b);
  b->count--;
  if (!b->count)
    delete b;
}

template<typename Value>
inline
void intrusive_ptr_add_ref(Eval_List<Value> *b)
{
  assert(b);
  b->count++;
}

template<typename Value>
inline
void intrusive_ptr_release(Eval_List<Value> *b)
{
  assert(b);
  b->count--;
  if (!b->count)
    delete b;
}

template<typename Value>
inline
void push_back(intrusive_ptr<Eval_List<Value> > e, Value &v)
{
  e->push_back(v);
}

template<class T, typename pos_int, typename D>
inline void push_back_max_subopt(List_Ref<T, pos_int> &x, T &e,
    D score,
    D delta)
{
  assert(!isEmpty(e));
  if (score - left_most(e) <= delta)
    x.ref().push_back(e);
}

template<class T, typename pos_int, typename D>
inline void push_back_min_subopt(List_Ref<T, pos_int> &x, T &e,
    D score,
    D delta)
{
  assert(!isEmpty(e));
  if (left_most(e)-score <= delta)
    x.ref().push_back(e);
}

template<class T, typename pos_int, typename D>
inline void append_min_subopt(List_Ref<T, pos_int> &x, List_Ref<T, pos_int> &e,
    D score,
    D delta)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  List<T, pos_int> &l = e.ref();
  for (typename List<T, pos_int>::iterator i = l.begin(); i != l.end(); ++i)
    push_back_min_subopt(x, *i, score, delta);
}

template<class T, typename pos_int, typename D>
inline void append_max_subopt(List_Ref<T, pos_int> &x, List_Ref<T, pos_int> &e,
    D score,
    D delta)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  List<T, pos_int> &l = e.ref();
  for (typename List<T, pos_int>::iterator i = l.begin(); i != l.end(); ++i)
    push_back_max_subopt(x, *i, score, delta);
}

// needed for --kbacktrack

template<typename score_type, typename Value, typename pos_int>
class Backtrace_Score : public virtual Backtrace<Value, pos_int>
{
  private:
    score_type score_;
#ifndef NDEBUG
    bool value_set;
#endif
  public:
#ifndef NDEBUG
    Backtrace_Score() : value_set(false) {}
#endif
    const score_type &score() const
    {
      assert(value_set);
      return score_;
    }
    void setScore(const score_type &s)
    {
#ifndef NDEBUG
      value_set = true;
#endif
      score_ = s;
    }

    void print(std::ostream &out)
    {
      assert(value_set);
      this->eval()->print(out, score_);
    }

};

// Hint:
//   --> Backtrace_List and Backtrace_score use public virtual inheritance now
template<typename score_type, typename Value, typename pos_int>
class Backtrace_List_Score
  : public Backtrace_List<Value, pos_int>,
    public Backtrace_Score<score_type, Value, pos_int>
{
};

template <typename score_type, typename Klass, typename Value, typename pos_int>
class Backtrace_NT_Back_Base
{
  protected:
    intrusive_ptr<Backtrace_List<Value, pos_int> > scores;

    Klass *klass;

    size_t count;

    virtual void backtrack() = 0;

  public:
    Backtrace_NT_Back_Base(Klass *klass_)
      : klass(klass_), count(0)
    {}
    virtual ~Backtrace_NT_Back_Base() {}

    intrusive_ptr<Backtrace<Value, pos_int> > backtrack(const score_type &score)
    {
      intrusive_ptr<Backtrace_List_Score<score_type, Value, pos_int> > ret;
      ret = new Backtrace_List_Score<score_type, Value, pos_int>();
      if (scores == 0)
        backtrack();
      for (typename Backtrace_List<Value, pos_int>::iterator i =
           scores->begin();
           i != scores->end(); ++i) {
        intrusive_ptr<Backtrace_Score<score_type, Value, pos_int> > bt =
          boost::dynamic_pointer_cast<
            Backtrace_Score<score_type, Value, pos_int> >(*i);
        assert(bt != 0);
        if (bt->score() == score)
          ret->push_back(bt);
      }
      ret->setScore(score);
      return ret;
    }

    void add_ref() { count++; }
    void rm_ref() { assert(count); count--; }
    bool unref() const { return count == 0; }

};

template <typename score_type, typename Klass, typename Value, typename pos_int>
inline
void intrusive_ptr_add_ref(
    Backtrace_NT_Back_Base<score_type, Klass, Value, pos_int> *b)
{
  assert(b);
  b->add_ref();
}

template <typename score_type, typename Klass, typename Value, typename pos_int>
inline
void intrusive_ptr_release(
    Backtrace_NT_Back_Base<score_type, Klass, Value, pos_int> *b)
{
  assert(b);
  b->rm_ref();
  if (b->unref())
    delete b;
}


template<typename score_type, typename Value, typename pos_int>
inline
void
set_value(std::pair<score_type, intrusive_ptr<Backtrace<Value, pos_int> > > &p)
{
  intrusive_ptr<Backtrace_Score<score_type, Value, pos_int> > bt
    = boost::dynamic_pointer_cast<Backtrace_Score<score_type, Value, pos_int> >
        (p.second);
  bt->setScore(p.first);
}


template<typename S, typename T, typename pos_int, typename ref_int>
inline
intrusive_ptr<Backtrace<T, pos_int> > exe_bt_k
  (List_Ref<std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > >, ref_int>
     & list,
   bool allow_cooptimal)
{
  typedef List<std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > >, ref_int>
    list_t;
  intrusive_ptr<Backtrace_List<T, pos_int> >
    ret(new Backtrace_List_Score<S, T, pos_int>());
  if (isEmpty(list)) {
    //assert(false);
    return ret;
  }
  list_t &l = list.ref();
  for (typename list_t::iterator i = l.begin();
       i != l.end(); ++i) {
    set_value( *i );
    intrusive_ptr<Backtrace<T, pos_int> > sec = (*i).second;
    assert(sec);
    ret->push_back(sec);
    if (!allow_cooptimal)
      break;

  }
  return ret;
}

template<typename S, typename T, typename pos_int, typename ref_int>
inline
intrusive_ptr<Backtrace<T, pos_int> > execute_backtrack_k
   (List_Ref<std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > >, ref_int>
      & list)
{
  return exe_bt_k(list, true);
}

template<typename S, typename T, typename pos_int>
inline
intrusive_ptr<Backtrace<T, pos_int> > execute_backtrack_k
   (std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > >
      & tuple)
{
  intrusive_ptr<Backtrace_List<T, pos_int> > ret =
    new Backtrace_List<T, pos_int>();
  if (isEmpty(tuple)) {
    //assert(false);
    return ret;
  }
  set_value(tuple);
  intrusive_ptr<Backtrace<T, pos_int> > sec = tuple.second;
  assert(sec);
  ret->push_back(sec);


  return ret;
}

template<typename S, typename T, typename pos_int, typename ref_int>
inline
intrusive_ptr<Backtrace<T, pos_int> > execute_backtrace_k_one
   (List_Ref<std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > >, ref_int>
      & list)
{
  return exe_bt_k(list, false);
}

template<typename S, typename T, typename pos_int>
inline
intrusive_ptr<Backtrace<T, pos_int> > execute_backtrace_k_one
   (std::pair<S, intrusive_ptr<Backtrace<T, pos_int> > >
      & tuple)
{
  return execute_backtrack_k(tuple);
}


// end --kbacktrack

#endif

