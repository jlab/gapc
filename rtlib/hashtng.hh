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

#ifndef RTLIB_HASHTNG_HH_
#define RTLIB_HASHTNG_HH_

// XXX remove
#include <iostream>
#include <ostream>
#include <cassert>
#include <vector>
#include <algorithm>
#include <utility>
#include <boost/cstdint.hpp>


#include "bitops.hh"
#include "move.hh"

#include "ref.hh"

#include "pool.hh"

#include "vector_sparse.hh"

#include "hash_stats.hh"

#if defined(CHECKPOINTING_INTEGRATED)
// serialization headers for the checkpointing of Hash_Ref objects
// (will be included in generated code through rtlib/adp.hh)

#include "boost/serialization/base_object.hpp"  // serialize base obj of class
#endif


using std::swap;



#ifndef HASH_INITIAL
#define HASH_INITIAL 16
#endif

#ifndef HASH_LOAD_FACTOR
#define HASH_LOAD_FACTOR 75
#endif

#ifndef HASH_SHRINK
#define HASH_SHRINK EnableShrink
#endif

#ifndef HASH_STAT
#define HASH_STAT NoStats
#endif


inline uint32_t hashable_value(int t) { return t; }

namespace Hash {


// FIXME
inline uint32_t hashable_value(size_t t) { return t; }


struct Multhash {
  uint32_t operator()(uint32_t kk, uint32_t m) const {
    uint32_t i = 32 - count_leading_zeroes(m) - 1;
    assert(m == size_to_next_power(m));
    uint32_t s = 2654435769;
    uint64_t k = kk;
    uint64_t t = s * k & 0x00000000FFFFFFFF;
    return t >> (32 - i);
  }
};

template <typename T>
struct Size2pow {
  T initial() const {
    // return 512;
    // return 4;
    // return 8;
    // return 32;
    // return 16;
    return HASH_INITIAL;
  }
  T expand(T size) const {
    return size * 2;
  }

  T filter(T size) const {
    return size_to_next_power(size);
  }
};

struct EnableShrink {
  enum { value = true };
};
struct DisableShrink {
  enum { value = false };
};

template <typename T, typename U = uint32_t>
struct Default_Inspector {
#if defined(CHECKPOINTING_INTEGRATED)
  /*
     while this default inspector doesn't contain any members,
     custom inspector objects are often used as template arguments
     for the Set class that gets wrapped in the Hash::Ref class;
     these custom inspectors can potentially contain members
     that need to be serialized, so this default
     inspector needs an empty serialize method so boost doesn't complain
  */
  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive & ar, const unsigned int version) {
  }
#endif
  T init(const T &x) const { return x; }
  U hash(const T &x) const {
    return hashable_value(x);
  }
  void update(T &dst, const T &src) const {
  }
  bool equal(const T &a, const T &b) const {
    return a == b;
  }
  bool filter() const { return false; }
  bool filter(const T &x) const { assert(0); return false; }
  void finalize(T &src) const {
  }

uint32_t k() const { assert(0); return 0; }
bool cutoff() const { return false; }
bool equal_score(const T &a, const T &b) const {
assert(0);;
return false;
}
struct compare {
bool operator()(const T &a, const T &b) const {
  assert(0);;
  return false;
}
};
};

template <typename T,
class Inspector = Default_Inspector<T>,
typename U = uint32_t,
typename Hash_Policy = Multhash,
template<typename> class Resize_Policy = Size2pow,
typename Shrink_Policy = HASH_SHRINK,
typename Stat_Policy = HASH_STAT,
unsigned int load_factor = HASH_LOAD_FACTOR>
class Set {
 private:
#if defined(CHECKPOINTING_INTEGRATED)
  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive & ar, const unsigned int version) {
    ar & array;
    ar & init;
    ar & used_;
    ar & inspector;
  }
#endif
    Vector_Sparse<T, U> array;
    std::vector<bool> init;
    U used_;
#ifndef NDEBUG
    bool finalized;
#endif

    Inspector inspector;
    Resize_Policy<U> resize_policy;
    Hash_Policy hash_policy;

    void rehash(U i) {
      assert(i > array.size());
      U size = resize_policy.filter(i);
      Vector_Sparse<T, U> a(size);
      swap(a, array);
      std::vector<bool> b(size);
      swap(init, b);
      used_ = 0;
      for (typename Vector_Sparse<T, U>::iterator i = a.begin();
           i != a.end(); ++i)
        add(*i, false);
    }
    bool loaded() const {
      unsigned int load = static_cast<double>(used_) /
        static_cast<double>(array.size()) * 100.0;
      return load >= load_factor;
    }
    void expand() {
      U i = resize_policy.expand(array.size()+1);
      rehash(i);
    }
    U hash(const U &index) const {
      return hash_policy(index, array.size());
    }
    bool insert(U index, const T &t, bool update) {
#ifndef NDEBUG
      U check = 0;
#endif
      for (U i = index; ; i = (i+1)%array.size()) {
        assert(check++ < array.size());
        if (init[i]) {
          if (inspector.equal(array(i), t)) {
            assert(update);
            inspector.update(array(i), t);
            return false;
          }
        } else {
          init[i] = true;
          if (update)
            array.init(i, inspector.init(t));
          else
            array.init(i, t);
          return true;
        }
      }
      assert(0);
    }

    void add(const T &t, bool update) {
      assert(!finalized);
      if (!array.size() || loaded())
        expand();
      U index = hash(inspector.hash(t));
      bool r = insert(index, t, update);
      if (r)
        ++used_;
    }


    Set(const Set&);
    Set &operator=(const Set&);

 public:
#ifdef CHECKPOINTING_INTEGRATED
    size_t vector_size() const  {
      return array.size();
    }
#endif
    U ref_count;
    Set()
      : used_(0),
#ifndef NDEBUG
        finalized(false),
#endif
        ref_count(1) {
    }
    void resize(U i) {
      if (i < array.size())
        return;
      rehash(i);
    }
    void add(const T &t) {
      add(t, true);
    }
    bool isEmpty() const { return !used_; }

    typedef typename Vector_Sparse<T, U>::iterator iterator;

    iterator begin() { assert(finalized); return array.begin(); }
    iterator end() { return array.end(); }

    void filter() {
      if (isEmpty())
        return;

      if (inspector.filter()) {
        Vector_Sparse<T, U> a(used_);
        swap(array, a);
        std::vector<bool> b(used_);
        swap(init, b);
        used_ = 0;
        for (typename Vector_Sparse<T, U>::iterator i = a.begin();
             i != a.end(); ++i)
          if (!inspector.filter(*i)) {
            array.init(used_, *i);
            init[used_] = true;
            ++used_;
          }
      }

      if (!inspector.cutoff() && Shrink_Policy::value) {
        Vector_Sparse<T, U> a(used_);
        swap(array, a);
        std::vector<bool> b(used_);
        swap(init, b);
        U j = 0;
        for (typename Vector_Sparse<T, U>::iterator i = a.begin();
            i != a.end(); ++i, ++j) {
          array.init(j, *i);
          init[j] = true;
        }
      }

      if (inspector.cutoff()) {
        assert(Shrink_Policy::value);
        assert(array.end()-array.begin() == used_);
        std::sort(array.begin(), array.end(), typename Inspector::compare());
        if (array.begin() != array.end()) {
          typename Vector_Sparse<T, U>::iterator last = array.begin();
          typename Vector_Sparse<T, U>::iterator i = array.begin();
          ++i;
          U uniques = 1;
          U newend = 1;
          U k = inspector.k();
          if (uniques < k) {
            for (; i != array.end(); ++last, ++i) {
              if (!inspector.equal_score(*last, *i))
                ++uniques;
              if (uniques > k)
                break;
              ++newend;
            }
          }
          used_ = newend;
          Vector_Sparse<T, U> a(used_);
          swap(array, a);
          std::vector<bool> b(used_);
          swap(init, b);
          U j = 0;
          for (typename Vector_Sparse<T, U>::iterator  x = a.begin();
               j < newend; ++x, ++j) {
            array.init(j, *x);
            init[j] = true;
          }
        }
      }
    }

    void finalize() {
#ifndef NDEBUG
      assert(!finalized);
      finalized = true;
#endif

      if (isEmpty())
        return;

      for (typename Vector_Sparse<T, U>::iterator i = array.begin();
          i != array.end(); ++i)
        inspector.finalize(*i);
    }

    void *operator new(size_t t) noexcept(false);
    void operator delete(void *b) noexcept(false);
};

#define SET_TEMPLATE_DECL \
class T, \
class I, \
typename U, \
typename Hash_Policy, \
template<typename> class Resize_Policy, \
typename Shrink_Policy, \
typename Stat_Policy, \
unsigned int load_factor
#define SET_TEMPLATE_ARGS \
T, I, U, Hash_Policy, Resize_Policy, Shrink_Policy, Stat_Policy, load_factor

template<SET_TEMPLATE_DECL>
  struct Set_Dummy {
    static Pool<Set<SET_TEMPLATE_ARGS> > pool;
  };

template<SET_TEMPLATE_DECL>
   Pool<Set<SET_TEMPLATE_ARGS> >
     Set_Dummy<SET_TEMPLATE_ARGS>::pool;

template<SET_TEMPLATE_DECL>
void *Set<SET_TEMPLATE_ARGS>::operator new(size_t t) noexcept(false) {
  assert(sizeof(Set<SET_TEMPLATE_ARGS>) == t);
  Set<SET_TEMPLATE_ARGS> *r = Set_Dummy<SET_TEMPLATE_ARGS>::pool.malloc();
  return r;
}

template<SET_TEMPLATE_DECL>
void Set<SET_TEMPLATE_ARGS>::operator delete(void *b) noexcept(false) {
  if (!b)
    return;
  Set_Dummy<SET_TEMPLATE_ARGS>::pool.free(
      static_cast<Set<SET_TEMPLATE_ARGS>*>(b));
}

template<class T, class I>
class Ref : public ::Ref::Lazy<Set<T, I> > {
 private:
#if defined(CHECKPOINTING_INTEGRATED)
  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive & ar, const unsigned int version) {
    // serialize the base object
    // (basically just a wrapper around boost::shared_ptr)
    ar &
    boost::serialization::base_object<::Ref::Lazy<Set<T, I>>>(*this);
  }
#endif

 public:
};

#undef SET_TEMPLATE_DECL
#undef SET_TEMPLATE_ARGS

}  // namespace Hash

#include "empty.hh"

template<class T, class I>
inline void hash_filter(Hash::Ref<T, I> &x) {
  x->filter();
}

template<class T>
inline void finalize(T &x) {
}

template<class T, class I>
inline void finalize(Hash::Ref<T, I> &x) {
  x->finalize();
}

template<class T, class I>
inline void push_back(Hash::Ref<T, I> &x, const T &e) {
  assert(is_not_empty(e));
  x->add(e);
}

template<class T, class I>
inline void append(Hash::Ref<T, I> &x, Hash::Ref<T, I> &e) {
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  for (typename Hash::Ref<T, I>::iterator i = e->begin(); i != e->end(); ++i)
    x->add(*i);
}

template<class T, class I, typename Iterator>
inline void append_filter(Hash::Ref<T, I> &x, std::pair<Iterator, Iterator> i) {
  for (Iterator a = i.first; a != i.second; ++a)
    push_back(x, *a);
  hash_filter(x);
}

template<class T, class I>
inline void empty(Hash::Ref<T, I> &x) {
}

template<class T, class I>
inline bool isEmpty(const Hash::Ref<T, I> &x) {
  return !x.l || x.const_ref().isEmpty();
}

template<class T, class I>
inline void erase(Hash::Ref<T, I> &x) {
}

template<class T, class U>
inline void update_filter(T &x, const U &a) {
  x.update(a);
}

template<class T, class I>
inline
std::ostream &operator<<(std::ostream &out, Hash::Ref<T, I> &x) {
  if (isEmpty(x))
    return out;
  typename Hash::Ref<T, I>::Type &h = x.ref();
  for (typename Hash::Ref<T, I>::iterator i = h.begin(); i != h.end(); ++i)
    out << *i << '\n';
  return out;
}


#undef HASH_INITIAL
#undef HASH_LOAD_FACTOR
#undef HASH_SHRINK
#undef HASH_STAT

#endif  // RTLIB_HASHTNG_HH_
