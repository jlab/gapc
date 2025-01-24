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

#ifndef RTLIB_ROPE_HH_
#define RTLIB_ROPE_HH_

#include <cassert>
#include <iostream>
#include <algorithm>
#include <cstring>
#include <utility>
#include <fstream>
#include <sstream>

#include <string>
#include <boost/cstdint.hpp>
#include <boost/algorithm/string/replace.hpp>

#include "../rtlib/cstr.h"
#include "bitops.hh"

#include "pool.hh"

namespace rope {
class Ref_Count {
 private:
  uint32_t i;

 public:
  enum { ENABLED = 1, block_size = 60 };

  Ref_Count()
    : i(1) {
  }
  void operator++() {
    ++i;
  }
  void operator--() {
    assert(i > 0);
    --i;
  }
  bool operator==(uint32_t x) const { return i == x; }
};

class No_Ref_Count {
 private:
 public:
  enum { ENABLED = 0, block_size = 64 };
  void operator++() {
  }
  void operator--() {
  }
  bool operator==(uint32_t x) const { assert(!x); return true; }
};

template<typename Refcount = Ref_Count>
class Block {
 public:
    enum { block_size = Refcount::block_size };

 private:
    // enum { RL , LR } Dir;
    // Dir dir;
#ifdef CHECKPOINTING_INTEGRATED
  friend class boost::serialization::access;
  template <class Archive>
  void serialize(Archive &ar, const unsigned int version) {
      ar & pos;
      ar & next;
      for (unsigned char i = 0; i < pos; i++) {
        ar & array[i];
      }
  }
#endif

    Block<Refcount> &operator=(const Block<Refcount> &r);
    Block(const Block<Refcount> &r);

 public:
    unsigned char pos;
    Block<Refcount> *next;
    unsigned char array[block_size];

 public:
    Refcount refcount;
    Block()
      :  // dir(LR),
      pos(0), next(0) {
    }
    ~Block() {
      assert(refcount == 0);
    }

    unsigned char free_space() const { return block_size-pos; }
    unsigned char size() const { return pos; }

    bool right_available(unsigned char x) const {
      return x <= free_space();
    }

    Block<Refcount> *extend_right() {
      assert(!next);
      next = new Block<Refcount>();
      return next;
    }

    void append(char c) {
      assert(right_available(1));
      array[pos++] = c;
    }

    Block<Refcount> *append(Block<Refcount> *o) {
      assert(!next);
      if (free_space() < o->size()) {
        std::memcpy(array+pos, o->array, free_space());
        next = new Block<Refcount>();
        std::memcpy(next->array, o->array+free_space(),
            o->pos - free_space());
        next->pos = o->pos - free_space();
        pos = block_size;
        return next;
      } else {
        std::memcpy(array+pos, o->array, o->pos);
        pos += o->pos;
        return this;
      }
    }

    const char *append(const char *x, uint32_t &len) {
#ifndef NDEBUG
      assert(len <= std::strlen(x));
#endif
      if (free_space() < len) {
        std::memcpy(array+pos, x, free_space());
        len -= free_space();
        x += free_space();
        pos = block_size;
        return x;
      } else {
        std::memcpy(array+pos, x, len);
        pos += len;
        len = 0;
        return 0;
      }
    }

    void append(char x, uint32_t &len) {
      if (free_space() < len) {
        std::memset(array+pos, x, free_space());
        len -= free_space();
        pos = block_size;
      } else {
        std::memset(array+pos, x, len);
        pos += len;
        len = 0;
      }
    }

    void *operator new(size_t t) noexcept(false);
    void operator delete(void *b)noexcept(false);
};

template<typename T> class Readonly {
};

template<>
class Readonly<Ref_Count> {
 private:
#ifdef CHECKPOINTING_INTEGRATED
    friend class boost::serialization::access;
    template <class Archive>
    void serialize(Archive &ar, const unsigned int version) {
      ar & pos;
    }
#endif
    unsigned char pos;

 public:
    Readonly()
      : pos(0) {
    }
    explicit Readonly(bool b)
      : pos(0) {
      assert(!b);
    }
    Readonly<Ref_Count> &operator=(bool b) {
      assert(!b);
      pos = 0;
      return *this;
    }
    Readonly<Ref_Count> &operator=(unsigned char p) {
      pos = p;
      return *this;
    }
    bool operator==(bool b) const { return b == static_cast<bool>(pos); }
    unsigned char operator()() const { return pos; }
};

template<>
class Readonly<No_Ref_Count> {
 private:
 public:
    Readonly() {
    }
    explicit Readonly(bool b) {
      assert(!b);
    }
    Readonly<No_Ref_Count> &operator=(bool b) {
      return *this;
    }
    Readonly<No_Ref_Count> &operator=(unsigned char p) {
      return *this;
    }
    bool operator==(bool b) const { return false; }
    unsigned char operator()() const { assert(false); return 0; }
};

template<typename Refcount = Ref_Count>
class Ref {
 public:
    static Pool<Block<Refcount> > pool;

 private:
#ifdef CHECKPOINTING_INTEGRATED
    friend class boost::serialization::access;
    template <class Archive>
    void serialize(Archive &ar, const unsigned int version) {
      ar & first;
      ar & last;
      ar & empty_;
      ar & readonly;
    }
#endif
    Block<Refcount> *first;
    Block<Refcount> *last;
    bool empty_;
    Readonly<Refcount> readonly;

    void del() {
      if (first) {
        --first->refcount;
        if (first->refcount == 0) {
          Block<Refcount>* x = first->next;
          delete first;
          while (x) {
            assert(x->refcount == 1);
            --x->refcount;
            Block<Refcount>* t = x;
            x = x->next;
            delete t;
          }
        }
      }
      readonly = false;
      first = last = 0;
      empty_ = false;
    }

    Block<Refcount> *copy_blocks(Block<Refcount>* dest, Block<Refcount>* src) {
      Block<Refcount>* x = dest;
      Block<Refcount>* y = src;
      while (y) {
        assert(x->refcount == 1);
        x->pos = y->pos;
        std::memcpy(x->array, y->array, y->pos);
        y = y->next;
        if (y && !x->next)
          x->next = new Block<Refcount>();
        x = x->next;
      }
      if (!x) {
        assert(dest);
        x = dest;
      }
      return x;
    }

    Ref &copy(const Ref<Refcount> &r) {
      if (r.empty_) {
        del();
        empty_ = true;
        return *this;
      }
      if (!first && !r.first)
        return *this;
      if (first && !r.first) {
        del();
        return *this;
      }
      del();
      if (Refcount::ENABLED) {
        assert(r.last->pos);
        readonly = r.last->pos;
        first = r.first;
        last = r.last;
        ++first->refcount;
      } else {
        last = copy_blocks(first, r.first);
      }
      return *this;
    }

    void right_alloc(unsigned char l) {
      assert(l == 1);
      empty_ = false;
      if (readonly == true) {
        Block<Refcount> *tfirst = new Block<Refcount>();
        Block<Refcount> *tlast = copy_blocks(tfirst, first);
        del();
        first = tfirst;
        last = tlast;
        return;
      }
      if (!first) {
        first = last = new Block<Refcount>();
        return;
      }

      if (!last->right_available(l))
        last = last->extend_right();
    }

 public:
    Ref()
      : first(0), last(0), empty_(false), readonly(false) {
    }

    Ref(const Ref<Refcount> &r)
      : first(0), last(0), empty_(false), readonly(false) {
      copy(r);
    }
    ~Ref() {
      del();
    }

    Ref &operator=(const Ref<Refcount> &r) {
      del();
      return copy(r);
    }

    void move(Ref<Refcount> &o) {
      del();
      first = o.first;
      last = o.last;
      empty_ = o.empty_;
      readonly = o.readonly;
      o.first = o.last = 0;
      o.empty_ = false;
      o.readonly = false;
    }

    void swap(Ref<Refcount> &o) {
      using std::swap;
      swap(first, o.first);
      swap(last, o.last);
      swap(readonly, o.readonly);
      swap(empty_, o.empty_);
    }

    void append(char c) {
      right_alloc(1);
      assert(last);
      last->append(c);
    }

    void append(const Ref<Refcount> &o) {
      if (!o.first)
        return;
      right_alloc(1);
      Block<Refcount>* x = last;
      Block<Refcount>* y = o.first;
      while (y) {
        x = x->append(y);
        y = y->next;
      }
      last = x;
    }

    void append(const char *s, uint32_t len) {
      if (!len)
        return;
      right_alloc(1);
      while (s) {
        if (!last->right_available(1))
          last = last->extend_right();
        s = last->append(s, len);
      }
    }

    void append(int j) {
      char s[12];
      unsigned char len;
      char *x = int_to_str(s, &len, j);
      assert(len);
      append(x, len);
    }

    void append(char c, uint32_t len) {
      if (!len)
        return;
      right_alloc(1);
      while (len) {
        if (!last->right_available(1))
          last = last->extend_right();
        last->append(c, len);
      }
    }

    void append(const char *s) {
      append(s, std::strlen(s));
    }

    explicit Ref(const char *s)
      : first(0), last(0), empty_(false), readonly(false) {
      assert(s);
      if (s && *s)
        append(s);
    }

    /*
       need to overload put methods, because operator<< needs to be
       overloaded so it doesn't interfere with boost's operator<<
       overload for the serialization of an archive
    */
    void put(std::ofstream &o) const {
      if (readonly == true) {
        Block<Refcount>* i = first;
        while (i) {
          unsigned char z = 0;
          if (i == last) {
            z = readonly();
          } else {
            z = i->size();
            assert(z == Block<Refcount>::block_size);
          }
          for (unsigned char j = 0; j < z; ++j)
            o << i->array[j];
          i = i->next;
        }
      } else {
        Block<Refcount>* i = first;
        while (i) {
          for (unsigned char j = 0; j < i->size(); ++j)
            o << i->array[j];
          i = i->next;
        }
      }
    }

    void put(std::stringstream &o) const {
      if (readonly == true) {
        Block<Refcount>* i = first;
        while (i) {
          unsigned char z = 0;
          if (i == last) {
            z = readonly();
          } else {
            z = i->size();
            assert(z == Block<Refcount>::block_size);
          }
          for (unsigned char j = 0; j < z; ++j)
            o << i->array[j];
          i = i->next;
        }
      } else {
        Block<Refcount>* i = first;
        while (i) {
          for (unsigned char j = 0; j < i->size(); ++j)
            o << i->array[j];
          i = i->next;
        }
      }
    }

    void put(std::ostream &o) const {
      if (readonly == true) {
        Block<Refcount>* i = first;
        while (i) {
          unsigned char z = 0;
          if (i == last) {
            z = readonly();
          } else {
            z = i->size();
            assert(z == Block<Refcount>::block_size);
          }
          for (unsigned char j = 0; j < z; ++j)
            o << i->array[j];
          i = i->next;
        }
      } else {
        Block<Refcount>* i = first;
        while (i) {
          for (unsigned char j = 0; j < i->size(); ++j)
            o << i->array[j];
          i = i->next;
        }
      }
    }

    class Const_Iterator {
     protected:
        friend class Ref<Refcount>;
        Ref<Refcount> &ref;
        Block<Refcount> *i;
        unsigned char j, z;

     private:
        void init() {
          if (ref.readonly == true) {
            i = ref.first;
            if (i) {
              z = 0;
              if (i == ref.last) {
                z = ref.readonly();
              } else {
                z = i->size();
                assert(z == Block<Refcount>::block_size);
              }
            }
          } else {
            i = ref.first;
          }
        }

     protected:
        Const_Iterator(
          Ref<Refcount> &r) : ref(r), i(0), j(0), z(0) { init(); }
        Const_Iterator(
          Ref<Refcount> &r, Ref<Refcount> &rr) : ref(r), i(0), j(0), z(0) { }

     public:
        unsigned char &operator*() { assert(i); return i->array[j]; }
        Const_Iterator &operator++() {
          if (ref.readonly == true) {
            if (i == ref.last) {
              z = ref.readonly();
            } else {
              z = i->size();
              assert(z == Block<Refcount>::block_size);
            }
            ++j;
            if (j >= z) {
              i = i->next;
              j = 0;
            }
          } else {
            ++j;
            if (j >= i->size()) {
              i = i->next;
              j = 0;
            }
          }
          return *this;
        }
        bool operator==(const Const_Iterator &other) const {
          assert(ref == other.ref); return i == other.i && j == other.j;
        }
        bool operator!=(const Const_Iterator &other) const {
          return !(*this == other);
        }
    };

    class Iterator : public Const_Iterator {
     private:
        friend class Ref<Refcount>;

     protected:
        explicit Iterator(Ref<Refcount> &r) : Const_Iterator(r) {}
        Iterator(Ref<Refcount> &r, Ref<Refcount> &rr) : Const_Iterator(r, r) {
        }

     public:
        unsigned char operator*() const {
          assert(this->i); return this->i->array[this->j];
        }
    };

    typedef Iterator iterator;
    iterator begin() { return Iterator(*this); }
    iterator end() { return Iterator(*this, *this); }
    typedef Const_Iterator const_iterator;
    const_iterator begin() const { return Const_Iterator(*this); }
    const_iterator end() const { return Const_Iterator(*this, *this); }

    size_t size() const {
      size_t r = 0;
      if (readonly == true) {
        Block<Refcount>* i = first;
        while (i) {
          unsigned char z = 0;
          if (i == last) {
            z = readonly();
          } else {
            z = i->size();
            assert(z == Block<Refcount>::block_size);
          }
          r += z;
          i = i->next;
        }
      } else {
        Block<Refcount>* i = first;
        while (i) {
          r += i->size();
          i = i->next;
        }
      }
      return r;
    }

    void empty() {
      empty_ = true;
    }

    bool isEmpty() const { return empty_; }

    bool operator==(const Ref<Refcount> &o) const {
      Block<Refcount>* a = first;
      Block<Refcount>* b = o.first;
      while (a && b) {
        if (a->pos != b->pos)
          return false;
        if (std::memcmp(a->array, b->array, a->pos))
          return false;
        a = a->next;
        b = b->next;
      }
      return a == b;
    }

    bool operator!=(const Ref<Refcount> &o) const {
      return !(*this == o);
    }

    bool operator<(const Ref<Refcount> &o) const {
      Block<Refcount>* a = first;
      Block<Refcount>* b = o.first;
      while (a && b) {
        if (a->pos != b->pos)
          return a->pos < b->pos;
        int t = std::memcmp(a->array, b->array, a->pos);
        if (t < 0)
          return true;
        if (t > 0)
          return false;
        a = a->next;
        b = b->next;
      }
      if (a)
        return false;
      if (b)
        return true;
      return false;
    }

    uint32_t hashable_value() const {
      hash_to_uint32::djb hash_fn;
      uint32_t hash = hash_fn.initial();

      if (readonly == true) {
        Block<Refcount>* i = first;
        while (i) {
          unsigned char z = 0;
          if (i == last) {
            z = readonly();
          } else {
            z = i->size();
            assert(z == Block<Refcount>::block_size);
          }
          for (unsigned char j = 0; j < z; ++j)
            hash_fn.next(hash, static_cast<char>(i->array[j]));
          i = i->next;
        }
      } else {
        Block<Refcount>* i = first;
        while (i) {
          for (unsigned char j = 0; j < i->size(); ++j)
            hash_fn.next(hash, static_cast<char>(i->array[j]));
          i = i->next;
        }
      }

      return hash;
    }

    char front() const {
      assert(!isEmpty());
      return *first->array;
    }
};

template <typename Refcount>
inline std::ofstream &operator<<(std::ofstream &o, const Ref<Refcount>& r) {
  r.put(o);
  return o;
}

template <typename Refcount>
inline std::stringstream &operator<<(std::stringstream &o,
const Ref<Refcount>& r) {
  r.put(o);
  return o;
}

template <typename Refcount>
inline std::ostream &operator<<(std::ostream &o, const Ref<Refcount>& r) {
  r.put(o);
  return o;
}

}  // namespace rope

typedef rope::Ref<rope::Ref_Count> Rope;


template<typename X>
Pool<rope::Block<X> > rope::Ref<X>::pool;

template<typename X>
void *rope::Block<X>::operator new(size_t t) noexcept(false) {
  assert(t == sizeof(Block<X>));
  Block<X> *r = rope::Ref<X>::pool.malloc();
  return r;
}

template<typename X>
void rope::Block<X>::operator delete(void *b) noexcept(false) {
  if (!b)
    return;
  rope::Ref<X>::pool.free(static_cast<Block<X>*>(b));
}

template<class T, typename X>
inline void append(rope::Ref<X> &str, const T &x) {
  str.append(x);
}

/* accepts a string and escapes all containing characters to be compatible
 * to LaTeX math mode. Used for tikZ generations.
 */
inline std::string latex(const std::string in) {
  std::string out = std::string(in);

  /* https://www.cespedes.org/blog/85/how-to-escape-latex-special-characters
   * note that we are in math mode, which might use different rules than
   * LaTeX's "normal" text mode. */
  boost::replace_all(out, "\\", "\\backslash");
  boost::replace_all(out, "#", "\\#");
  boost::replace_all(out, "$", "\\$");
  boost::replace_all(out, "%", "\\%");
  boost::replace_all(out, "&", "\\&");
  boost::replace_all(out, "_", "\\_");
  boost::replace_all(out, "{", "\\{");
  boost::replace_all(out, "}", "\\}");
  boost::replace_all(out, "^", "\\hat{\\ }");
  boost::replace_all(out, "~", "\\tilde{\\ }");

  return out;
}
template<typename X>
inline void append_latex(rope::Ref<X> &str, char &c) {
  std::string in(1, c);
  std::string out = latex(in);
  str.append(out.c_str(), out.size());
}
template<typename X>
inline void append_latex(rope::Ref<X> &str, const Subsequence &s) {
  for (typename Subsequence::const_iterator i = s.begin(); i != s.end(); ++i) {
    char ichar = (*i);
    append_latex(str, ichar);
  }
}
template<typename X>
inline void append_latex(rope::Ref<X> &str, const char *s, int slen) {
  for (int i = 0; i < slen; ++i) {
    char ichar = (s[i]);
    append_latex(str, ichar);
  }
}

template<class T, typename X>
inline void append(rope::Ref<X> &str, char c, T i) {
  assert(i >= 0);
  str.append(c, static_cast<uint32_t>(i));
}

template<typename X>
inline void append(rope::Ref<X> &str, const char *c, int i) {
  str.append(c, i);
}

template<typename X>
inline void append(rope::Ref<X> &str, const char *c) {
  str.append(c);
}

template<typename X>
inline void append(rope::Ref<X> &str, unsigned int i) {
  str.append(static_cast<int>(i));
}

template<typename X>
inline void append(rope::Ref<X> &str, double i) {
  std::ostringstream o;
  o << i;
  str.append(o.str().c_str(), o.str().size());
}

template<typename X>
inline bool operator!=(const rope::Ref<X> &str, const char *s) {
  return str != Rope(s);
}

template<typename X>
inline bool operator==(const rope::Ref<X> &str, const char *s) {
  return str == Rope(s);
}

template<typename X>
inline Rope operator+=(rope::Ref<X> &str, const Rope &i) {
  Rope res;
  append(res, str);
  append(res, i);
  return res;
}


// Stefan Janssen: returns the first character of the rope, if not empty. For
// the empty case it returns the fallback character 0.
template<typename X>
inline char front(const rope::Ref<X> &str, char r = 0) {
  if (str.size() <= 0) {
    return r;
  } else {
    return str.front();
  }
}

// Stefan Janssen: returns the last character of the rope, if not empty. For the
// empty case it returns the fallback character 0.
// FIXME: I am not happy with the iteration through the whole rope, but since I
// don't fully understand the ADT for rope I'm not sure if there is a better
// solution.
template<typename X>
inline char back(const rope::Ref<X> &str, char r = 0) {
  rope::Ref<X> &x = const_cast<rope::Ref<X>&>(str);
  typename rope::Ref<X>::iterator it = x.begin();
  if (str.size() <= 0) {
    return r;
  } else {
    for (unsigned int i = 0; i < str.size()-1; i++) {
      ++it;
    }
    return *it;
  }
}

// Stefan Janssen: returns everything but the first character of the rope, if
// not empty and contains more than one letter. Otherwise it returns the empty
// rope.
// FIXME: I am not happy with the iteration through the whole rope, but since
// I don't fully understand the ADT for rope I'm not sure if there is a better
// solution.
template<typename X>
inline Rope tail(const rope::Ref<X> &str) {
  rope::Ref<X> &x = const_cast<rope::Ref<X>&>(str);
  typename rope::Ref<X>::iterator it = x.begin();
  Rope res;
  if (str.size() < 2) {
    res.empty();
  } else {
    ++it;
    for (unsigned int i = 1; i < str.size(); ++i) {
      append(res, static_cast<char>(*it));
      ++it;
    }
  }
  return res;
}


template<typename X>
inline size_t size(const rope::Ref<X> &str) {
  return str.size();
}

inline
Rope operator+(const Rope &a, const Rope &b) {
  Rope r;
  append(r, a);
  append(r, b);
  return r;
}

inline
Rope operator+(const Rope &a, char b) {
  Rope r;
  append(r, a);
  append(r, b);
  return r;
}

inline
Rope operator+(const Rope &a, const char *b) {
  Rope r;
  append(r, a);
  append(r, Rope(b));
  return r;
}

inline
Rope operator+(char a, const Rope &b) {
  Rope r;
  append(r, a);
  append(r, b);
  return r;
}

inline
Rope operator+(const char *a, const Rope &b) {
  Rope r;
  append(r, Rope(a));
  append(r, b);
  return r;
}



// FIXME gcc Bug?!?
namespace Hash {

template<typename X>
inline uint32_t hashable_value(const rope::Ref<X> &str) {
  return str.hashable_value();
}

}

template<typename X>
inline uint32_t hashable_value(const rope::Ref<X> &str) {
  return str.hashable_value();
}

template <typename X>
inline
void move(rope::Ref<X> &a, rope::Ref<X> &b) {
  a.move(b);
}


namespace std {

template <>
inline
void swap<rope::Ref<rope::Ref_Count> >(
rope::Ref<rope::Ref_Count> &a, rope::Ref<rope::Ref_Count> &b) {
  a.swap(b);
}


}  // namespace std



#endif  // RTLIB_ROPE_HH_
