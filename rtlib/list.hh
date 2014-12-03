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


#ifndef LIST_HH
#define LIST_HH

#include "empty.hh"
#include "erase.hh"

// FIXME profile this
#include "pool.hh"

// tr1 has it
#include <boost/cstdint.hpp>

#include <new>
#include <cassert>
#include <vector>

#include <iterator>
#include <algorithm>

//FIXME
//#include <iostream>

template<class T, typename pos_int>
struct List_Dummy;

template<class T, typename pos_int>
class List_Ref;

template <class T, typename pos_int = unsigned char>
class List {
  private:
    friend class List_Dummy<T, pos_int>;


    class Block {
      private:
        Block(const Block &);
        Block &operator=(const Block&);

      public:
        enum { size = 8 };
        T array[size];
        pos_int pos;
        Block *next;
        Block()
          : pos(0), next(NULL)
        {
        }

        ~Block()
        {
        }

        Block *push_back(const T &e)
        {
          if (pos == size) {
            if (!next)
              next = new Block();
            next->pos = 0;
            next->push_back(e);
            return next;
          } else {
            array[pos++] = e;
            return this;
          }
        }

        T & push_back_ref(Block *& last)
        {
          if (pos == size) {
            if (!next)
              next = new Block();
            next->pos = 0;
            return next->push_back_ref(last);
          } else {
            if (last != this)
              last = this;
            pos++;
            return array[pos-1];
          }
        }

        void *operator new(size_t t) throw (std::bad_alloc);

        void operator delete(void *b) throw ();
    };


    List &operator=(const List&);
    List(const List&);

    Block *first;
    Block *last;

    void del()
    {
      Block *t = first;
      while (t) {
        Block *x = t;
        t = t->next;
        delete x;
      }
    }

  public:
    unsigned int ref_count;

    List()
      : ref_count(1)
    {
      first = last = new Block();
    }

    ~List()
    {
      del();
    }

    void push_back(const T &e)
    {
      Block *t = last->push_back(e);
      // FIXME is always assignment faster?
      if (last != t)
        last = t;
    }

    T & push_back_ref()
    {
      return last->push_back_ref(last);
    }

    void append(List<T, pos_int> &l)
    {
      // FIXME optimize for larger lists ...
      for (typename List<T, pos_int>::iterator i = l.begin(); i != l.end(); ++i)
        push_back(*i);
    }

    T & front()
    {
      assert(!isEmpty());
      return first->array[0];
    }

    void clear()
    {
      first->pos = 0;
      last = first;
    }

    size_t size()
    {
      size_t r = 0;
      for (iterator i = begin(); i != end(); ++i)
        r++;
      return r;
    }

    bool isEmpty () const { return first->pos == 0 && last == first; }

    void *operator new(size_t t) throw (std::bad_alloc);

    void operator delete(void *b) throw ();

    class Iterator {
      //protected:
      public:
        Block *current;
        pos_int i;
      public:
      typedef T value_type;
      typedef size_t difference_type;
      typedef std::forward_iterator_tag iterator_category;
      typedef T* pointer;
      typedef T& reference;

        Iterator(Block *b)
          : current(b), i(0)
        {
          if (!current->pos && current->next)
            current = current->next;
        }
        Iterator(Block *b, Block *c)
        {
          if (b->next) {
            current = b->next;
            i = 0;
          } else {
            current = b;
            i = b->pos;
          }
        }

        Iterator &operator++()
        {
          assert(i<current->pos);
          ++i;
          if (i == current->pos && current->next) {
              current = current->next;
              i = 0;
          }
          return *this;
        }

        T &operator*()
        {
          assert(i<current->pos);
          return current->array[i];
        }

        const T &operator*() const
        {
          assert(i<current->pos);
          return current->array[i];
        } 

        bool operator==(const Iterator &itr) const
        {
          return i == itr.i && current == itr.current;
        }

        bool operator!=(const Iterator &itr) const
        {
          return !(*this == itr);
        }

    };
    typedef Iterator iterator;

    class RandomAccessIterator  {
      private:
        std::vector<Block*> *blocks;
        size_t x;
      public:

      typedef T value_type;
      typedef size_t difference_type;
      typedef std::random_access_iterator_tag iterator_category;
      typedef T* pointer;
      typedef T& reference;

        RandomAccessIterator(std::vector<Block*> *v)
          : blocks(v), x(0)
        {
        }

        RandomAccessIterator(std::vector<Block*> *v, bool b)
          : blocks(v)
        {
          assert(v->size());
          x = (v->size()-1)*v->front()->size + v->back()->pos;
        }

        RandomAccessIterator(std::vector<Block*> *v, size_t a)
          : blocks(v), x(a)
        {
        }

        RandomAccessIterator() : blocks(NULL), x(0) {}

        void operator++()
        {
          x++;
        }

        T &operator[](size_t i)
        {
          size_t t = i / blocks->front()->size;
          size_t r = i % blocks->front()->size;
          assert(t<blocks->size());
          assert(r<(*blocks)[t]->pos);
          return (*blocks)[t]->array[r];
        }
/*
        RandomAccessIterator operator-(const RandomAccessIterator &other) const
        {
          return RandomAccessIterator(blocks, x-other.x);
        }*/

        difference_type operator-(const RandomAccessIterator &other) const
        {
          return x-other.x;
        }

        RandomAccessIterator operator-(size_t a) const
        {
          return RandomAccessIterator(blocks, x-a);
        }

        difference_type operator+(const RandomAccessIterator &other) const
        {
          return x+other.x;
        }

        RandomAccessIterator operator+(size_t a) const
        {
          return RandomAccessIterator(blocks, x+a);
        }

        RandomAccessIterator operator*(size_t a) const
        {
          return RandomAccessIterator(blocks, x*a);
        }


        T &operator*() { return (*this)[x]; }

        bool operator==(const RandomAccessIterator &itr) const
        {
          return x == itr.x;
        }

        bool operator!=(const RandomAccessIterator &itr) const
        {
          return !(*this == itr);
        }

        bool operator==(size_t a) const
        {
          return x == a;
        }

        bool operator!=(size_t a) const
        {
          return !(x == a);
        }

        RandomAccessIterator &operator=(size_t a)
        {
          x = a;
          return *this;
        }

        RandomAccessIterator &operator>>=(int a)
        {
          x >>= a;
          return *this;
        }

        bool operator<(const RandomAccessIterator &itr) const
        {
          return x < itr.x;
        }

        bool operator<(size_t a) const
        {
          return x < a;
        }

        bool operator>(size_t a) const
        {
          return x > a;
        }

        RandomAccessIterator &operator--()
        {
          --x;
          return *this;
        }

        RandomAccessIterator operator--(int a)
        {
          size_t t = x;
          --x;
          return RandomAccessIterator(blocks, t);
        }

        RandomAccessIterator operator/(size_t a)
        {
          return RandomAccessIterator(blocks, x/a);
        }
    };

    typedef RandomAccessIterator ra_iterator;

    iterator begin() const { return iterator(first); }
    iterator end() const { return iterator(last, last); }

    void map_blocks(std::vector<Block*> &v)
    {
      Block *l = first;
      while (l) {
        v.push_back(l);
        l = l->pos != l->size ? NULL : l->next;
      }
    }

    ra_iterator ra_begin(std::vector<Block*> *v) 
    {
      return ra_iterator(v);
    }

    ra_iterator ra_end(std::vector<Block*> *v) const
    {
      return ra_iterator(v, true);
    }

    void unique()
    {
      std::vector<Block*> v;
      map_blocks(v);
      std::sort(ra_begin(&v), ra_end(&v));
      iterator new_end = std::unique(begin(), end());
      last = new_end.current;
      last->pos = new_end.i;
    }

};

template<class T, typename pos_int>
struct List_Dummy {
  static Pool<List<T, pos_int> > pool;
  static Pool<typename List<T, pos_int>::Block> block_pool;
};

template<class T, typename pos_int>
Pool<List<T, pos_int> > List_Dummy<T, pos_int>::pool;

template<class T, typename pos_int>
Pool<typename List<T, pos_int>::Block> List_Dummy<T, pos_int>::block_pool;


template<class T, typename pos_int>
void *List<T, pos_int>::operator new(size_t t) throw (std::bad_alloc)
{
  assert(sizeof(List) == t);
  List<T, pos_int> *r = List_Dummy<T, pos_int>::pool.malloc();
  return r;
}

template<class T, typename pos_int>
void List<T, pos_int>::operator delete(void *b) throw ()
{
  if (!b)
    return;
  List_Dummy<T, pos_int>::pool.free(static_cast<List<T, pos_int>*>(b));
}

template<class T, typename pos_int>
void *List<T, pos_int>::Block::operator new(size_t t) throw (std::bad_alloc)
{
  assert(t == sizeof(Block));
  Block *r = List_Dummy<T, pos_int>::block_pool.malloc();
  return r;
}


template<class T, typename pos_int>
void List<T, pos_int>::Block::operator delete(void *b) throw ()
{
  if (!b)
    return;
  List_Dummy<T, pos_int>::block_pool.free(static_cast<Block*>(b));
}

#include <ostream>
#include "output.hh"

template<class T, typename pos_int>
inline
std::ostream &operator<<(std::ostream &out, const List<T, pos_int> &list)
{
  for (typename List<T, pos_int>::iterator i = list.begin(); i != list.end(); ++i)
    out << *i << '\n';
  return out;
}

#include "ref.hh"

template<class T, typename pos_int = unsigned char>
class List_Ref : public ::Ref::Lazy<List<T, pos_int> >
{
};

template<class T, typename pos_int>
inline
std::ostream &operator<<(std::ostream &out, const List_Ref<T, pos_int> &list)
{
  if (list.l)
    out << list.const_ref();
  return out;
}



template<class T, typename pos_int>
inline void empty(List_Ref<T, pos_int> &x)
{
  // empty as explicit initialization which is not needed with lists
  // could delete computed answers in previous alternatives ...
  //if (!isEmpty(x))
  //  x.ref().clear();
}

template<class T, typename pos_int>
inline bool isEmpty(const List_Ref<T, pos_int> &x)
{
  return !x.l || x.const_ref().isEmpty();
}

template<class T, typename pos_int>
inline void erase(List_Ref<T, pos_int> &x)
{
}

template<class T, typename pos_int>
inline void push_back(List_Ref<T, pos_int> &x, const T &e)
{
  assert(is_not_empty(e));
  x.ref().push_back(e);
}

template<class T, typename pos_int>
inline void append(List_Ref<T, pos_int> &x, List_Ref<T, pos_int> &e)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  x.ref().append(e.ref());
}

#include "algebra.hh"

template<class T, typename pos_int>
inline List_Ref<T, pos_int> unique(List_Ref<T, pos_int> &x)
{
  typedef typename List_Ref<T, pos_int>::iterator itr;
  std::pair<itr, itr> p = std::make_pair(x->begin(), x->end());
  return unique(p);
}


#endif
