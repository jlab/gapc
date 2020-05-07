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


#ifndef SHAPE_HH
#define SHAPE_HH

#include "multipool.hh"


#include <cassert>
#include <cstring>
#include <algorithm>

#include <boost/cstdint.hpp>

//remove
#include <bitset>
#include <iostream>

#ifndef HAVE_EFFICIENT_FFS
  #ifdef __GNUC__
    #define HAVE_EFFICIENT_FFS 1
  #endif
#endif


#include "shape_alph.hh"
#include "bitops.hh"

template <typename T, typename Size, typename alphset = ShapeAlph<T, Size> >
class Fiber {
  private:
    enum { bits = sizeof(T)*8, char_width = alphset::char_width,
      chars = bits/char_width };

    static MultiPool<T> pool;

    T *alloc(Size n)
    {
      assert(n);
      // alloced memory must be zeroed!
      return pool.malloc(n);
    }

    void dealloc(T *t, Size n)
    {
      assert(t);
      assert(n);
      pool.free(t, n);
    }

    Size length() const
    {
      Size l = 1;
      for (T *a = array; *a & T(-1)  >> bits-char_width; ++a)
        ++l;
      return l;
    }

    void copy(T *t, T *s, Size l)
    {
      std::memcpy(t, s, l*sizeof(T));
    }

    static T null_elem;

    bool is_null_elem() const
    {
      return array == &null_elem;
    }

  public:
    T *array;

    Fiber() : array(&null_elem)
    {
    }

    Fiber(const Fiber<T, Size, alphset> &other)
    {
      if (other.isEmpty())
        array = 0;
      else if (other.is_null_elem())
        array = &null_elem;
      else {
        array = alloc(other.length());
        copy(array, other.array, other.length());
      }
    }

    Fiber &operator=(const Fiber<T, Size, alphset> &other)
    {
      if (!(isEmpty() || is_null_elem()))
        dealloc(array, length());
      if (other.isEmpty())
        array = 0;
      else if (other.is_null_elem())
        array = &null_elem;
      else {
        array = alloc(other.length());
        copy(array, other.array, other.length());
      }
      return *this;
    }

    ~Fiber()
    {
      if (!(isEmpty() || is_null_elem()))
        dealloc(array, length());
    }

    bool operator==(char c) const
    {
      if (isEmpty())
        return false;
      if (is_null_elem())
        return false;
      if (char_length() != 1)
        return false;
      alphset alph;
      return c == alph.to_char(*array, bits-char_width);
    }

    bool operator!=(char c) const
    {
      return !(*this == c);
    }

    bool operator==(const Fiber<T, Size, alphset> &other) const
    {
      if (isEmpty() && other.isEmpty())
        return true;
      if (isEmpty() || other.isEmpty())
        return false;
      if (is_null_elem() && other.is_null_elem())
        return true;
      if (is_null_elem() || other.is_null_elem())
        return false;
      if (length() != other.length())
        return false;
      return !std::memcmp(array, other.array, length()*sizeof(T));
    }

    bool operator!=(const Fiber<T, Size, alphset> &other) const
    {
      return !(*this == other);
    }

    bool operator<(const Fiber<T, Size, alphset> &other) const
    {
      assert(!isEmpty() && !other.isEmpty());
      if (is_null_elem() && other.is_null_elem())
        return false;
      if (is_null_elem())
        return true;
      if (other.is_null_elem())
        return false;
      Size a = length();
      Size b = other.length();
      Size m = std::min(a, b);
      return std::memcmp(array, other.array, m*sizeof(T)) < 0;
    }

  private:

    size_t char_length() const
    {
      T *t = array;
      Size l = 0;
      for (;;) {
        if (*t & T(-1) >> bits-char_width) {
          ++t;
          l+=chars;
        } else
          break;
      }
      Size a = first_pos(*t);
      l+= (bits - a - 1) / char_width;
      return l;
    }


    T *last(T *a) const
    {
      for (; *a & T(-1) >> bits-char_width; ++a)
        ;
      return a;
    }

    Size first_pos(T a) const
    {
#if !HAVE_EFFICIENT_FFS
      if (a & T(-1) >> bits-char_width)
        return 0;
      for (unsigned int i = 2*char_width; i <= sizeof(T) * 8; i+=char_width)
        if (a & T(-1) >> bits-i)
          return i-(char_width+1);
      return sizeof(T)*8 - 1;
#else
      Size x = find_first_set(a);
      if (!x)
        return sizeof(T)*8 - 1;
      if (x <= char_width)
        return 0;
      Size ret = (x-1) - ((x-1)%char_width) - 1;
      assert(ret < 8*sizeof(T));
      return ret;
#endif
    }

    void lazy()
    {
      if (isEmpty() || is_null_elem())
        array = alloc(1);
    }

  public:

    void append(char x)
    {
      lazy();
      T *t = last(array);
      if (*t & T(-1) >> bits-2*char_width) {
        Size l = t-array;
        T *x = alloc(l + 1 + 1);
        copy(x, array, l + 1);
        dealloc(array, l + 1);
        array = x;
        t = array + l;
      }
      Size a = first_pos(*t);
      alphset()(*t, x, a);
    }

    Fiber(char c)
     : array(&null_elem)
    {
      append(c);
    }

    Fiber(const char *s)
     : array(&null_elem)
    {
      assert(s);
      assert(*s && s[1] && !s[2]);
      append(*s);
      append(s[1]);
    }

    class Iterator {
      private:
        T *a;
        Size i;
        char c;
      public:
        Iterator(T *t)
          : a(t), i(bits), c(0)
        {
          alphset alph;
          if (!a)
            return;
          c = alph.to_char(*a, i-char_width);
          assert(c);
        }
        Iterator &operator++()
        {
          alphset alph;
          i -= char_width;
          if (i && (c = alph.to_char(*a, i-char_width)))
            return *this;

          if (*a & T(-1) >> bits-char_width) {
            i = bits;
            ++a;
            c = alph.to_char(*a, i-char_width);
            if (!c)
              a = 0;
          } else
            a = 0;
          return *this;
        }
        char operator*() const
        {
          return c;
        }
        bool operator==(const Iterator &itr) const
        {
          return !a ? a == itr.a : (a == itr.a && i == itr.i);
        }
        bool operator!=(const Iterator &itr) const
        {
          return !(*this == itr);
        }
    };

    typedef Iterator iterator;

    iterator begin() const { return iterator(is_null_elem() ? 0 : array); }
    iterator end() const { return iterator(0); }

    class Reverse_Iterator {
      private:
        T *a;
        T *beg;
        Size i;
      public:
        Reverse_Iterator()
          : a(0), beg(0), i(0)
        {
        }
        Reverse_Iterator(T *t)
          : a(t), beg(0), i(bits+char_width)
        {
        }
        Reverse_Iterator(T *t, Size u, T *b)
          : a(t), beg(b), i(u)
        {
        }
        Reverse_Iterator &operator++()
        {
          assert(i<=bits+char_width);
          i += char_width;
          if (i>bits) {
            if (beg<a) {
              a--;
              i=char_width;
            }
          }
          return *this;
        }
        char operator*() const
        {
          alphset alph;
          return alph.to_char(*a, i-char_width);
        }
        void drop()
        {
          *a &= T(-1) << i;
        }
        void set(char c)
        {
          assert(i<=bits);
          assert(i>=char_width);

          alphset alph;
          //std::cout << "\tXX i: " << size_t(i) << std::endl;
          T mask = 0;
          if (i != 4)
            mask = T(-1) >> bits-(i-char_width);
          if (i != bits)
            mask |= T(-1) << i;

          *a &= mask;
          alph(*a, c, i-1);

          return;
        }
        bool operator==(const Reverse_Iterator &itr) const
        {
          return !a ? a == itr.a : (a == itr.a && i == itr.i);
        }
        bool operator!=(const Reverse_Iterator &itr) const
        {
          return !(*this == itr);
        }
    };

    typedef Reverse_Iterator reverse_iterator;

    reverse_iterator rbegin() const
    {
      if (is_null_elem() || isEmpty())
        return reverse_iterator();
      T *l = last(array);
      Size p = first_pos(*l);
      if (p == bits-1) {
        assert(array < l);
        l--;
        p = first_pos(*l);
        assert(p!=bits-1);
      }
      //std::cerr << "rbegin: " << int(p) << std::endl;
      if (p)
        p = p + char_width + 1;
      else
        p = char_width;
      return reverse_iterator(l, p, array);
    }
    reverse_iterator rend() const
    {
      if (is_null_elem() || isEmpty())
        return reverse_iterator();
      return reverse_iterator(array);
    }


  private:

    void app(T *& dst, T *src) const
    {
      Size x = first_pos(*dst);
      Size y = first_pos(*src);
      if ((x+1)/char_width < chars-(y+1)/char_width) {
      //if (x < y) {
        *dst |= *src >> bits-1-x;
        ++dst;
        *dst |= *src << 1+x;
      } else {
        *dst |= *src >> bits-1-x;
      }
      if (*dst & T(-1)  >> bits-char_width)
        ++dst;
    }

  public:
    void append(const Fiber<T, Size, alphset> &other)
    {
      if (other.is_null_elem())
        return;
      lazy();
      assert(this != &other);
      assert(array != other.array);
      size_t m = char_length();
      size_t n = other.char_length();
      if (chars - m%chars <= n) {
        Size rest = n-(chars-m%chars);
        Size add = rest/chars + 1;//(rest%chars ? 1 : 0);
        //if (!add)
        //  add = 1;
        Size old = m/chars + 1 ;//(m%chars ? 1 : 0);
        T *t = alloc(old + add);
        copy(t, array, m/chars + (m%chars ? 1 : 0));
        dealloc(array, old);
        array = t;
      }
      T *a = array;
      for (; *a & T(-1)  >> bits-char_width; ++a)
        ;
      for (T *t = other.array;;) {
        app(a, t);
        if (*t & T(-1)  >> bits-char_width)
          ++t;
        else
          break;
      }
    }

    void print()
    {
      assert(!isEmpty());
      std::cerr << "========" << std::endl;
      T *t = array;
      for (;;) {
        for (Size i = 1; i<=bits; i++) {
          if (*t & T(1) << bits-i)
            std::cerr << 1;
          else
            std::cerr << 0;
        }
        if (*t & T(-1)  >> bits-char_width) {
          ++t;
          std::cerr << std::endl;
        }
        else
          break;
        std::cerr << "--------" << std::endl;
      }
      std::cerr << std::endl << "========" << std::endl;
    }

    void put(std::ostream &o) const
    {
      for (iterator i = begin(); i!=end(); ++i)
        o << *i;
      return;


      if (is_null_elem())
        return;
      assert(!isEmpty());
      alphset alph;
      T *t = array;
      for (;;) {
        for (Size i = bits; i>0; i-=char_width) {
          char c = alph.to_char(*t, i-char_width);
          if (c)
            o << c;
          else
            break;
        }
        if (*t & T(-1)  >> bits-char_width)
          ++t;
        else
          break;
      }
    }

    void empty()
    {
      if (!(isEmpty() || is_null_elem()))
        dealloc(array, length());
      array = 0;
    }

    bool isEmpty() const
    {
      return !array;
    }

#ifndef SHAPE_HASH
#define SHAPE_HASH djb
#endif

    typedef hash_to_uint32::SHAPE_HASH Hash_Fn;

    // for different string hash fns see:
    // http://www.cse.yorku.ca/~oz/hash.html
    uint32_t hashable_value() const
    {
      Hash_Fn hash_fn;
      assert(!isEmpty());
      T *t = array;
      uint32_t hash = hash_fn.initial();
      for (;;) {
        hash_fn.next(hash, *t);
        if (*t & T(-1)  >> bits-char_width)
          ++t;
        else
          break;
      }
      return hash;
    }

    void swap(Fiber<T, Size, alphset> &other)
    {
      T *t = array;
      array = other.array;
      other.array = t;
    }

    void move(Fiber<T, Size, alphset> &other)
    {
      if (!(isEmpty() || is_null_elem()))
        dealloc(array, length());
      array = other.array;
      other.array = 0;
    }

    size_t size() const { return char_length(); }

};

template <typename T, typename Size, typename alphset>
MultiPool<T> Fiber<T, Size, alphset>::pool;

template <typename T, typename Size, typename alphset>
T Fiber<T, Size, alphset>::null_elem(0);

#ifdef __APPLE__
  // work around weird Mac OS X type ambiguity problems
  // cf. http://stackoverflow.com/questions/11603818/why-is-there-ambiguity-between-uint32-t-and-uint64-t-when-using-size-t-on-mac-os

  #ifdef __x86_64__
    typedef Fiber<uint64_t, unsigned char> Shape;
  #else
    typedef Fiber<uint32_t, unsigned char> Shape;
  #endif
#else
  typedef Fiber<size_t, unsigned char> Shape;
#endif


template <typename T, typename Size, typename alphset >
inline
std::ostream &operator<<(std::ostream &o, const Fiber<T, Size, alphset> &f)
{
  f.put(o);
  return o;
}

template <typename T, typename Size, typename alphset >
inline void append(Fiber<T, Size, alphset> &s,
    const Fiber<T, Size, alphset> &x)
{
  s.append(x);
}

template <typename T, typename Size, typename alphset >
inline void append(Fiber<T, Size, alphset> &s, char c)
{
  s.append(c);
}

template <typename T, typename Size, typename alphset >
inline void append(Fiber<T, Size, alphset> &s, const char *c, int i)
{
  assert(i == 2);
  s.append(*c);
  s.append(*(c+1));
}


template <typename T, typename Size, typename alphset >
inline
Fiber<T, Size, alphset> operator+(const Fiber<T, Size, alphset> &a,
    const Fiber<T, Size, alphset> &b)
{
  Fiber<T, Size, alphset> r;
  append(r, a);
  append(r, b);
  return r;
}

template <typename T, typename Size, typename alphset >
inline
Fiber<T, Size, alphset> operator+(const Fiber<T, Size, alphset> &a,
    char b)
{
  Fiber<T, Size, alphset> r;
  append(r, a);
  append(r, b);
  return r;
}

template <typename T, typename Size, typename alphset >
inline
Fiber<T, Size, alphset> operator+(char a,
    const Fiber<T, Size, alphset> &b)
{
  Fiber<T, Size, alphset> r;
  append(r, a);
  append(r, b);
  return r;
}

template <typename T, typename Size, typename alphset >
inline
Fiber<T, Size, alphset> operator+(const Fiber<T, Size, alphset> &a,
    const char *b)
{
  Fiber<T, Size, alphset> r;
  append(r, a);
  append(r, b, std::strlen(b));
  return r;
}

template <typename T, typename Size, typename alphset >
inline
Fiber<T, Size, alphset> operator+(const char *a,
    const Fiber<T, Size, alphset> &b)
{
  Fiber<T, Size, alphset> r;
  append(r, a, std::strlen(a));
  append(r, b);
  return r;
}


template <typename T, typename Size, typename alphset >
inline void empty(Fiber<T, Size, alphset> &s)
{
  s.empty();
}

template <typename T, typename Size, typename alphset >
inline bool isEmpty(const Fiber<T, Size, alphset> &s)
{
  return s.isEmpty();
}

template <typename T, typename Size, typename alphset >
inline uint32_t hashable_value(const Fiber<T, Size, alphset> &shape)
{
  return shape.hashable_value();
}

template <typename T, typename Size, typename alphset >
inline
void swap(Fiber<T, Size, alphset> &a, Fiber<T, Size, alphset> &b)
{
  a.swap(b);
}

template <typename T, typename Size, typename alphset >
inline
void move(Fiber<T, Size, alphset> &a, Fiber<T, Size, alphset> &b)
{
  a.move(b);
}

template <typename T, typename Size, typename alphset >
inline
Fiber<T, Size, alphset>
push_after_front(Fiber<T, Size, alphset> &a, char c, char d)
{
  typedef Fiber<T, Size, alphset> X;
  X ret;
  typename X::iterator i = a.begin();
  for (; i != a.end(); ++i)
    if (*i == c)
      ret.append(*i);
    else
      break;
  if (i == a.begin()) {
    ret.append(d);
    ret.append(a);
    return ret;
  }
  ret.append(d);
  for (; i!=a.end(); ++i)
    ret.append(*i);
  return ret;
}

template <typename T, typename Size, typename alphset >
inline
Fiber<T, Size, alphset> &
push_before_back(Fiber<T, Size, alphset> &a, char c, char d)
{
  typedef Fiber<T, Size, alphset> X;
  typename X::reverse_iterator i = a.rbegin();
  typename X::reverse_iterator j = a.rbegin();
  if (i == a.rend()) {
    a.append(d);
    return a;
  }
  if (*i != c) {
    a.append(d);
    return a;
  }
  ++i;
  if (i == a.rend()) {
    j.set(d);
    a.append(c);
    return a;
  }
  for (; i != a.rend(); ++i, ++j) {
    if (*i != c) {
      j.set(d);
      a.append(c);
      return a;
    }
  }
  j.set(d);
  a.append(c);
  //assert(false);
  return a;
}

template <typename T, typename Size, typename alphset >
inline
char
front(Fiber<T, Size, alphset> &a, char r = 0)
{
  typedef Fiber<T, Size, alphset> X;
  typename X::iterator i = a.begin();
  if (i == a.end())
    return r;
  return *i;

}

template <typename T, typename Size, typename alphset >
inline
Fiber<T, Size, alphset>
tail(Fiber<T, Size, alphset> &a)
{
  typedef Fiber<T, Size, alphset> X;
  X x;
  typename X::iterator i = a.begin();
  if (i != a.end())
    ++i;
  for ( ; i != a.end(); ++i)
    x.append(*i);
  return x;
}

template <typename T, typename Size, typename alphset >
inline
char
back(Fiber<T, Size, alphset> &a, char r = 0)
{
  typedef Fiber<T, Size, alphset> X;
  typename X::reverse_iterator i = a.rbegin();
  if (i == a.rend())
    return r;
  return *i;
}

#endif

