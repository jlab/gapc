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

#ifndef TREE_LIST_HH
#define TREE_LIST_HH

#include "ref.hh"
#include "pool.hh"
#include <cassert>

template <typename T>
class Node {
  private:
    Node(const Node&);
    Node &operator=(const Node&);
  public:
  T payload;
  Node *next, *left, *right;

  Node(const T &x)
    : payload(x), next(0), left(0), right(0)
  {
  }

  void *operator new(size_t t) throw (std::bad_alloc);

  void operator delete(void *b) throw ();
};


template <typename T>
struct Node_Dummy {
  static Pool<Node<T> > pool;
};

#ifdef GAPC_MOD_TRANSLATION_UNIT
template <typename T>
Pool<Node<T> > Node_Dummy<T>::pool;

#endif

template <typename T>
void * Node<T>::operator new(size_t t) throw (std::bad_alloc)
{
  assert(sizeof(Node<T>) == t);
  Node<T> *r = Node_Dummy<T>::pool.malloc();
  return r;
}

template <typename T>
void Node<T>::operator delete(void *b) throw ()
{
  if (!b)
    return;
  Node_Dummy<T>::pool.free(static_cast<Node<T>*>(b));
}


template <typename T, typename I>
class Tree_List_Back {
  private:
    I inspector;

    typedef Node<T> node_t;

    node_t *head, *last;

    Tree_List_Back(const Tree_List_Back &);
    Tree_List_Back &operator=(const Tree_List_Back &);
  public:
    uint32_t ref_count;
    Tree_List_Back()
      : head(0), last(0), ref_count(1)
    {
    }
    ~Tree_List_Back()
    {
      node_t *t = head;
      while (t) {
        node_t *x = t;
        t = t->next;
        delete x;
      }
    }
    void add(const T &x)
    {
      if (!head) {
        head = new node_t(x);
        last = head;
        return;
      }
      node_t *t = head;
      for (;;) {
        if (inspector.less(x, t->payload)) {
          if (!t->left) {
            t->left = new node_t(x);
            last->next = t->left;
            last = t->left;
            break;
          } else {
            t = t->left;
          }
          continue;
        } 

        if (inspector.equal(t->payload, x)) {
          inspector.update(t->payload, x);
          break;
        }
        
        {
          if (!t->right) {
            t->right = new node_t(x);
            last->next = t->right;
            last = t->right;
            break;
          } else {
            t = t->right;
          }
          continue;
        }


      }
    }

    class Iterator {
      private:
        node_t *i;
      public:
        Iterator(node_t *x)
          : i(x) {}
        Iterator()
          : i(0) {}

        Iterator &operator++()
        {
          assert(i);
          i = i->next;
          return *this;
        }
        bool operator==(const Iterator &o) const
        {
          return i == o.i;
        }
        bool operator!=(const Iterator &o) const
        {
          return !(*this == o);
        }
        T &operator*() { return i->payload; }
    };

    typedef Iterator iterator;

    iterator begin() { return Iterator(head); }
    iterator end() { return Iterator(); }

    bool isEmpty() const { return !head; }

    void filter()
    {
      if (!inspector.filter())
        return;
      while (head) {
        if (inspector.filter(head->payload)) {
          node_t *x = head->next;
          delete head;
          head = x;
        } else
          break;
      }
      if (!head)
        return;
      node_t *prev = head, *t = head->next;
      while (t) {
        if (inspector.filter(t->payload)) {
          prev->next = t->next;
          node_t *x = t;
          t = t->next;
          delete x;
        } else {
          prev = t;
          t = t->next;
        }
      }
    }

};

template<class T, class I>
class Tree_List : public ::Ref::Lazy<Tree_List_Back<T, I> > {
  private:
  public:
};


#include "empty.hh"

template<class T, class I>
inline void hash_filter(Tree_List<T, I> &x)
{
  x->filter();
}

template<class T, class I>
inline void push_back(Tree_List<T, I> &x, const T &e)
{
  assert(is_not_empty(e));
  x->add(e);
}

template<class T, class I>
inline void append(Tree_List<T, I> &x, Tree_List<T, I> &e)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  for (typename Tree_List<T, I>::iterator i = e->begin(); i != e->end(); ++i)
    x->add(*i);
}

template<class T, class I, typename Iterator>
inline void append_filter(Tree_List<T, I> &x, std::pair<Iterator, Iterator> i)
{
  for (Iterator a = i.first; a != i.second; ++a)
    push_back(x, *a);
  hash_filter(x);
}

template<class T, class I>
inline void empty(Tree_List<T, I> &x)
{
}

template<class T, class I>
inline bool isEmpty(const Tree_List<T, I> &x)
{
  return !x.l || x.const_ref().isEmpty();
}

template<class T, class I>
inline void erase(Tree_List<T, I> &x)
{
}

template<class T, class I>
inline
std::ostream &operator<<(std::ostream &out, Tree_List<T, I> &x)
{
  if (isEmpty(x))
    return out;
  typename Tree_List<T, I>::Type &h = x.ref();
  for (typename Tree_List<T, I>::iterator i = h.begin(); i != h.end(); ++i)
    out << *i << '\n';
  return out;
}


#endif
