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


#ifndef SEQUENCE
#define SEQUENCE

#include <cstring>
#include <cassert>

#include <sstream>
#include <vector>

#include <stdexcept>

template<typename alphabet = char>
struct Copier {
  std::pair<alphabet*, size_t> copy(const char *x, size_t l) const
  {
    alphabet *r = new char[l];
    std::memcpy(r, x, l);
    return std::make_pair(r, l);
  }
  unsigned rows() const { return 1; }
  char *row(char *seq, unsigned x) { return seq; }
  const char *row(char *seq, unsigned x) const { return seq; }
};

#include <cstdlib>
#include <errno.h>
template<>
struct Copier<double> {
  std::pair<double*, size_t> copy(const char *x, size_t l) const
  {
    std::stringstream s;
    s << x;
    std::vector<double> v;
    for (;;) {
      // to parse reliable NaNs etc.
      std::string str;
      s >> str;
      errno = 0;
      double d = std::strtod(str.c_str(), 0);
      if (errno)
        break;
      if (s.bad() || s.fail())
        break;
      v.push_back(d);
      if (s.eof())
        break;
    }
    double *arr = new double[v.size()];
    double *t = arr;
    for (std::vector<double>::iterator i = v.begin(); i != v.end(); ++i, ++t)
      *t = *i;
    return std::make_pair(arr, v.size());
  }
};

template<>
struct Copier<float> {
  std::pair<float*, size_t> copy(const char *x, size_t l) const
  {
    std::stringstream s;
    s << x;
    std::vector<float> v;
    for (;;) {
      // to parse reliable NaNs etc.
      std::string str;
      s >> str;
      errno = 0;
      float d = std::strtod(str.c_str(), 0);
      if (errno)
        break;
      if (s.bad() || s.fail())
        break;
      v.push_back(d);
      if (s.eof())
        break;
    }
    float *arr = new float[v.size()];
    float *t = arr;
    for (std::vector<float>::iterator i = v.begin(); i != v.end(); ++i, ++t)
      *t = *i;
    return std::make_pair(arr, v.size());
  }
};

template<>
struct Copier<int> {
  std::pair<int*, size_t> copy(const char *x, size_t l) const
  {
    std::stringstream s;
    s << x;
    std::vector<int> v;
    for (;;) {
      // to parse reliable NaNs etc.
      std::string str;
      s >> str;
      errno = 0;
      int d = std::strtol(str.c_str(), 0, 0);
      if (errno)
        throw std::runtime_error("Int convert error.");
      if (s.bad() || s.fail())
        throw std::runtime_error("Int input error.");
      v.push_back(d);
      if (s.eof())
        break;
    }
    int *arr = new int[v.size()];
    int *t = arr;
    for (std::vector<int>::iterator i = v.begin(); i != v.end(); ++i, ++t)
      *t = *i;
    return std::make_pair(arr, v.size());
  }
};


class M_Char {
  public:
    typedef char alphabet;
    typedef unsigned pos_type;
  private:
    alphabet *begin;
  public:
    M_Char()
      : begin(0)
    {
    }
    M_Char(alphabet *x)
      : begin(x)
    {
      assert(x);
    }
    alphabet &column(pos_type x)
    {
      return *(begin + x);
    }
    const alphabet &column(pos_type x) const
    {
      return *(begin + x);
    }
};

template<>
struct Copier<M_Char> {
  public:
    typedef M_Char alphabet;
    typedef char alphabet2;
    typedef unsigned pos_type;
  private:
    alphabet2 *seq;
    alphabet2 *src;
    pos_type rows_, row_size_;
    Copier(const Copier &c);
    Copier &operator=(const Copier &c);
  public:
    Copier()
      : seq(0), src(0), rows_(0), row_size_(0)
    {
    }
    ~Copier()
    {
      delete[] seq;
      delete[] src;
    }
    std::pair<alphabet*, size_t> copy(const char *x, size_t l) // const
    {
      Copier<char> c;
      std::pair<char*, size_t> u = c.copy(x, l);
      src = u.first;

      rows_ = 1;
      row_size_ = 0;
      size_t a = 0;
      for (size_t i = 0; i<l; ++i, ++a) {
        if (x[i] == '#') {
          ++rows_;
          if (!row_size_) {
            row_size_ = a;
            a = 0;
          } else {
            if (row_size_ != a-1) {
              throw std::length_error("Row sizes mismatch.");
            }
            a = 0;
          }
        }
      }
      //assert(row_size_);
      if (!row_size_ && a)
        row_size_ = a;
      if (l>0 && x[l-1] == '#')
        --rows_;
      alphabet *r = new M_Char[row_size_];
      seq = new alphabet2[row_size_ * rows_];
      size_t j = 0;
      for (size_t i = 0; i<row_size_; ++i) {
        r[i] = M_Char(seq+j);
        for (size_t k = 0; k<rows_; ++k) {
          seq[j++] = x[i + k * (row_size_+1)];
        }
      }
      return std::make_pair(r, row_size_);
    }
    pos_type rows() const { return rows_; }
    alphabet2 *row(alphabet *t, pos_type x)
    {
      assert(src);
      assert(x<rows_);
      pos_type off = x * (row_size_+1);
      return src + off;
    }
    const alphabet2 *row(alphabet *t, pos_type x) const
    {
      assert(src);
      assert(x<rows_);
      pos_type off = x * (row_size_+1);
      return src + off;
    }
};

template<typename alphabet = char, typename pos_type = unsigned int>
class Basic_Sequence {
  private:
    Copier<alphabet> copier; // to let Copier cleanup shared storage
  public:
    alphabet *seq;
    pos_type n;

    void copy(const char *s, pos_type l)
    {
      delete[] seq;

      std::pair<alphabet*, size_t> p = copier.copy(s, l);
      seq = p.first;
      n = p.second;
    }
  public:
    typedef alphabet alphabet_type;
    typedef char alphabet2;
    Basic_Sequence(alphabet *s, pos_type l)
      : seq(0)
    {
      copy(s, l);
    }
    Basic_Sequence(alphabet *s)
      : seq(0)
    {
      n = std::strlen(s);
      copy(s, n);
    }
    Basic_Sequence()
      : seq(0), n(0) {}
    Basic_Sequence(const Basic_Sequence &o)
      : seq(0)
    {
      copy(o.seq, o.n);
    }
    ~Basic_Sequence()
    {
      delete[] seq;
    }
    Basic_Sequence &operator=(const Basic_Sequence &o)
    {
      copy(o.seq, o.n);
      return *this;
    }

    alphabet &operator[](pos_type x) 
    {
      assert(x < n);
      return seq[x];
    }
    const alphabet &operator[](pos_type x) const
    {
      assert(x < n);
      return seq[x];
    }

    pos_type size() const { return n; }

    typedef alphabet * iterator;

    alphabet * begin() { return seq; }
    alphabet * end() { return seq + n; }

    pos_type rows() const
    {
      return copier.rows();
    }
    alphabet2 *row(pos_type row)
    {
      assert(row < copier.rows());
      return copier.row(seq, row);
    }
    const alphabet2 *row(pos_type row) const
    {
      assert(row < copier.rows());
      return copier.row(seq, row);
    }
};

typedef Basic_Sequence<> Sequence;

inline char lower_case(char c)
{
  if (c == '_')
    return c;
  if (c == '+')
	return c;
  if (c < 'a')
    return c+('a'-'A');
  else
    return c;
}

//template<typename alphabet, typename pos_type>
inline const char &column(const M_Char /*<alphabet, pos_type>*/ &c, /*pos_type*/ unsigned l)
{
  return c.column(l);
}

#include <cctype>

template<typename alphabet, typename pos_type>
inline void char_to_upper(Basic_Sequence<alphabet, pos_type> &seq)
{
  for (typename Basic_Sequence<alphabet, pos_type>::iterator i = seq.begin();
       i != seq.end(); ++i)
    *i = std::toupper(*i);
}

#endif
