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

#ifndef HASHLIST_HH
#define HASHLIST_HH

#include "ref.hh"
#include "list.hh"

// TODO gapc should automatically select this, unless filtering is detected

#include <cstdlib>

template<class T, class I>
class Hash_List_Back {
  private:
    typedef Hash::Set<T, I, uint32_t,
            Hash::Multhash, Hash::Size2pow, Hash::RecordPositions> Hash_t;

    I inspector;
    // FIXME directly use *list && destructor;
    List_Ref<T> list;
    unsigned int size;

    Hash_List_Back(const Hash_List_Back&);
    Hash_List_Back &operator=(const Hash_List_Back &);
  public:
    uint32_t ref_count;

    Hash_List_Back()
      : size(0), ref_count(1)
    {
    }
    void filter()
    {
      if (inspector.filter()) {
        std::abort();
      } else {
        Hash_t &set = Singleton<Hash_t>::ref();
        set.hint(4096);
        for (typename List<T>::iterator i = list->begin(); i != list->end(); ++i)
          set.add(*i);
        List_Ref<T> l;
        size = 0;
        for (typename Hash_t::discard_iterator i = set.discard_begin();
            i!=set.discard_end(); ++i) {
          move(l.ref().push_back_ref(), *i);
          ++size;
        }
        set.reset();
        list = l;
      }
    }
    void add(const T &t)
    {
      list->push_back(t);
      ++size;
    }

    //bool isEmpty() const { return !list.l || list.const_ref().isEmpty(); }
    bool isEmpty() const { return ::isEmpty(list); }

    typedef typename List<T>::iterator iterator;

    iterator begin() const { return list.const_ref().begin(); }
    iterator end() const { return list.const_ref().end(); }
};

template<class T, class I>
class Hash_List : public ::Ref::Lazy<Hash_List_Back<T, I> > {
  private:
  public:
};


#include "empty.hh"

template<class T, class I>
inline void hash_filter(Hash_List<T, I> &x)
{
  x->filter();
}

template<class T, class I>
inline void push_back(Hash_List<T, I> &x, const T &e)
{
  assert(is_not_empty(e));
  x->add(e);
}

template<class T, class I>
inline void append(Hash_List<T, I> &x, Hash_List<T, I> &e)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  for (typename Hash_List<T, I>::iterator i = e->begin(); i != e->end(); ++i)
    x->add(*i);
}

template<class T, class I, typename Iterator>
inline void append_filter(Hash_List<T, I> &x, std::pair<Iterator, Iterator> i)
{
  for (Iterator a = i.first; a != i.second; ++a)
    push_back(x, *a);
  hash_filter(x);
}

template<class T, class I>
inline void empty(Hash_List<T, I> &x)
{
}

template<class T, class I>
inline bool isEmpty(const Hash_List<T, I> &x)
{
  return !x.l || x.const_ref().isEmpty();
}

template<class T, class I>
inline void erase(Hash_List<T, I> &x)
{
}

template<class T, class I>
inline
std::ostream &operator<<(std::ostream &out, Hash_List<T, I> &x)
{
  if (isEmpty(x))
    return out;
  typename Hash_List<T, I>::Type &h = x.ref();
  for (typename Hash_List<T, I>::iterator i = h.begin(); i != h.end(); ++i)
    out << *i << '\n';
  return out;
}

#endif
