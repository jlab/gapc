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

#ifndef SUBSEQUENCE_HH
#define SUBSEQUENCE_HH

#include "sequence.hh"

template<typename alphabet = char, typename pos_type = unsigned int>
class Basic_Subsequence {
  public:
    const Basic_Sequence<alphabet, pos_type> *seq;
    pos_type i;
    pos_type j;

    Basic_Subsequence() : seq(NULL), i(0), j(0) {}

    Basic_Subsequence(const Basic_Sequence<alphabet, pos_type> &s,
        pos_type a, pos_type b)
      : seq(&s), i(a), j(b)
    {
    }

    alphabet &front()
    {
      assert(seq);
      return (*seq)[i];
    }
    const alphabet &front() const
    {
      assert(seq);
      return (*seq)[i];
    }

    alphabet &back()
    {
      assert(seq);
      return (*seq)[j-1];
    }

    void empty()
    {
      seq = NULL;
    }

    bool isEmpty() const
    {
      return !seq;
    }

    pos_type size() const
    {
      return j-i;
    }

    alphabet operator[](pos_type x) const
    {
      //assert(x < j);
      //assert(x >= i);
      assert(seq);
      return (*seq)[x];
    }

    typedef alphabet* iterator;
    typedef const alphabet* const_iterator;
    iterator begin() { assert(seq); return seq->seq+i; }
    iterator end() { assert(seq); return seq->seq+j; }
    const_iterator begin() const { assert(seq); return seq->seq+i; }
    const_iterator end() const { assert(seq); return seq->seq+j; }

};

typedef Basic_Subsequence<> Subsequence;

template<typename alphabet, typename pos_type>
inline pos_type size(const Basic_Subsequence<alphabet, pos_type> &sub)
{
  return sub.size();
}

template<typename alphabet, typename pos_type>
inline pos_type seq_size(const Basic_Subsequence<alphabet, pos_type> &sub)
{
  assert(sub.seq);
  return sub.seq->size();
}

template<typename alphabet, typename pos_type>
inline alphabet seq_char(const Basic_Subsequence<alphabet, pos_type> &sub, pos_type i)
{
  return sub.seq->seq[i];
}

template<typename alphabet, typename pos_type>
inline alphabet seq_char(const Basic_Subsequence<alphabet, pos_type> &sub, int i)
{
  return seq_char(sub, pos_type(i));
}

template<typename alphabet, typename pos_type>
inline const alphabet &front(const Basic_Subsequence<alphabet, pos_type> &sub)
{
  return sub.front();
}

template<typename alphabet, typename pos_type>
inline pos_type rows(const Basic_Subsequence<alphabet, pos_type> &sub)
{
  return sub.seq->rows();
}

#include <ostream>

template<typename alphabet, typename pos_type>
inline
std::ostream &operator<<(std::ostream &s, const Basic_Subsequence<alphabet, pos_type> &seq)
{
  s << '<' << seq.i << ", " << seq.j << '>';
  return s;
}

#include "rope.hh"

template<typename X, typename alphabet, typename pos_type>
inline void append(rope::Ref<X> &str, const Basic_Subsequence<alphabet, pos_type> &sub)
{
  typename rope::Ref<X> t;
  t.append('<');
  t.append(int(sub.i));
  t.append(',');
  t.append(int(sub.j));
  t.append('>');
  str.append(t);
}

template<typename X, typename alphabet, typename pos_type>
inline void append_deep(rope::Ref<X> &str, const Basic_Subsequence<alphabet, pos_type> &sub)
{
  for (typename Basic_Subsequence<alphabet, pos_type>::const_iterator i = sub.begin();
      i != sub.end(); ++i)
    str.append(*i);
}

#endif
