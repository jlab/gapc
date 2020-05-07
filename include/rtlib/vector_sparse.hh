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


#ifndef VECTOR_SPARSE_HH
#define VECTOR_SPARSE_HH

#include <cstdlib>

#include <vector>

#include <iterator>

using std::swap;

template <typename T, typename U = size_t>
class Stapel {
  private:
    T *array;
    U top_, size_;
    
    Stapel(const Stapel &);
    Stapel &operator=(const Stapel &);
  public:
    Stapel()
      : array(0), top_(0), size_(0)
    {
    }
    Stapel(U i)
      : array(0), top_(0), size_(0)
    {
      resize(i);
    }
    ~Stapel()
    {
      std::free(array);
    }
    void swapper(Stapel<T, U> &o)
    {
      swap(array, o.array);
      swap(top_, o.top_);
      swap(size_, o.size_);
    }
    void resize(U i)
    {
      if (i <= size_)
        return;
      if (!array) {
        array = static_cast<T*>(std::malloc(sizeof(T) * i));
        size_ = i;
      } else {
        T *t = static_cast<T*>(std::realloc(array, sizeof(T) * i));
        assert(t);
        if (!t)
          std::abort();
        array = t;
        size_ = i;
      }
    }
    void push(const T &t)
    {
      assert(top_ < size_);
      array[top_++] = t;
    }
    T &pop()
    {
      assert(top_);
      return array[top_--];
    }
    T &top()
    {
      assert(top_);
      return array[top_-1];
    }

    typedef T* iterator;

  
    iterator begin() { return array; }
    iterator end() { return array + top_; }

    typedef std::reverse_iterator<iterator> reverse_iterator;

    reverse_iterator rbegin()
    {
      return reverse_iterator(end());
    }
    reverse_iterator rend()
    {
      return reverse_iterator(begin());
    }
};

template <typename T, typename U>
  inline
  void swap(Stapel<T, U> &a, Stapel<T, U> &b)
  {
    a.swapper(b);
  }



template <typename T, typename U = size_t>
class Vector_Sparse {
  private:
    T *array;
    U size_;
    Stapel<U> stack;
#ifndef NDEBUG
    std::vector<bool> init_;
#endif
    Vector_Sparse(const Vector_Sparse &);
    Vector_Sparse &operator=(const Vector_Sparse &);
  public:
    Vector_Sparse()
      : array(0), size_(0)
    {}
    Vector_Sparse(U i)
      : array(0), size_(0)
    {
      resize(i);
    }
    ~Vector_Sparse()
    {
      for (typename Stapel<U>::iterator i = stack.begin(); i != stack.end(); ++i)
        array[*i].~T();
      std::free(array);
    }

    void swapper(Vector_Sparse<T, U> &o)
    {
      swap(array, o.array);
      swap(size_, o.size_);
      swap(stack, o.stack);
#ifndef NDEBUG
      swap(init_, o.init_);
#endif
    }

    T &operator()(U i)
    {
      assert(i<size_);
      assert(init_[i]);
      return array[i];
    }
    const T &operator()(U i) const
    {
      assert(i<size_);
      assert(init_[i]);
      return array[i];
    }

    void init(U i, const T &t)
    {
      assert(i<size_);
      assert(!init_[i]);
#ifndef NDEBUG
      init_[i] = true;
#endif
      new (array+i) T(t);
      stack.push(i);
    }

    void operator()(U i, const T &t)
    {
      assert(i<size_);
      assert(init_[i]);
      array[i] = t;
    }

    void resize(U i)
    {
      stack.resize(i);
#ifndef NDEBUG
      init_.resize(i);
#endif
      if (i <= size_)
        return;
      if (!array) {
        array = static_cast<T*>(std::malloc(sizeof(T) * i));
        size_ = i;
      } else {
        T *t = static_cast<T*>(std::realloc(array, sizeof(T) * i));
        assert(t);
        if (!t)
          std::abort();
        array = t;
        size_ = i;
      }
    }

    U size() const { return size_; }

    class Iterator {
      private:
        friend class Vector_Sparse;
        typedef typename Stapel<U>::iterator itr;
        itr i;
        Vector_Sparse<T, U> &v;
      protected:
        Iterator(Vector_Sparse<T, U> &a, itr x)
          : v(a)
        {
          i = x;
        }
      public:
        typedef T value_type;
        typedef std::random_access_iterator_tag iterator_category;
        typedef U difference_type;
        typedef T* pointer;
        typedef T& reference;

        difference_type operator-(const Iterator &other) const { assert(i>other.i); return i-other.i; }
        Iterator operator+(U a) const { return Iterator(v, i+a); }
        Iterator operator-(U a) const { return Iterator(v, i-a); }
        Iterator &operator=(const Iterator &other) { assert(&v == &other.v); i = other.i; return *this; }
        Iterator &operator--() { --i; return *this;}
        Iterator operator--(int) { Iterator r(*this); --i; return r;}
        bool operator<(const Iterator &other) const { return i < other.i; }

        bool operator==(const Iterator &o) const { assert(&v == &o.v); return i == o.i; }
        bool operator!=(const Iterator &o) const { return !(*this == o); }
        T &operator*() { return v(*i); }
        Iterator &operator++() { ++i; return *this; }
    };

    typedef Iterator iterator;


    iterator begin()
    {
      return iterator(*this, stack.begin());
    }
    iterator end()
    {
      return iterator(*this, stack.end());
    }

};

template <typename T, typename U>
  inline
  void swap(Vector_Sparse<T, U> &a, Vector_Sparse<T, U> &b)
  {
    a.swapper(b);
  }


/*
namespace std {
  template<typename T, typename U>
    struct iterator_traits<typename Vector_Sparse<T,U>::iterator>
      : public std::iterator<, T>
    {
    };
}
*/

#endif
