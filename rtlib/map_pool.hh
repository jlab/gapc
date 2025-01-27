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




#ifndef RTLIB_MAP_POOL_HH_
#define RTLIB_MAP_POOL_HH_

#if defined(__APPLE__) && defined(__MACH__)
  #define _DARWIN_C_SOURCE
#endif


#include <sys/mman.h>
#include <vector>
#include <cassert>

#include <cstdlib>
#include <cstdio>

#ifdef USE_BOOST_POOL
  #include <boost/pool/pool.hpp>
#endif

// work thesis:
//   - space usage competitive with rtlib/pool.hh + POOL_DEBUG
//     (i.e. system malloc)
//     (which is for 150,adpf_nonfold,notes_cart 90 MB less)
//
//     -> definitely: even less than the malloc version
//         -> 40 % better than boost pool
//
//   - less run-time overhead than boost pool?
//
//     -> no

namespace Map {
struct MapMapper {
  void *map(size_t l) const {
    void *ret = mmap(0, l, PROT_READ | PROT_WRITE, MAP_PRIVATE
// http://predef.sourceforge.net/preos.html#sec20
#if defined(__APPLE__) && defined(__MACH__)
        | MAP_ANON
#else
        | MAP_ANONYMOUS
#endif
        , -1, 0);
    if (ret == MAP_FAILED) {
      std::perror(0);
      std::abort();
    }
    return ret;
  }
  void unmap(void *x, size_t l) const {
    int ret = munmap(x, l);
    if (ret == -1) {
      std::perror(0);
      std::abort();
    }
  }
};

struct MallocMapper {
  void *map(size_t l) const {
    void *ret = std::malloc(l);
    assert(ret);
    return ret;
  }
  void unmap(void *x, size_t l) const {
    std::free(x);
  }
};

template <typename Type>
class Entry {
 private:
    typedef Entry<Type> entry_t;
    entry_t *next_;
    // payload ...

 public:
    Entry()
      : next_(0) {
    }
    entry_t *next() { return next_; }
    void set_next(entry_t *n) { next_ = n; }
    Type *payload() { return reinterpret_cast<Type*>(&next_ + 1); }
};

template <typename Type, size_t size,
#ifdef NO_MMAP
typename Mapper = MallocMapper
#else
typename Mapper = MapMapper
#endif
>
class Block {
 private:
    typedef Entry<Type> entry_t;

    Mapper mapper;

    // don't call constructors ...
#ifdef POOL_DEBUG
    void **array;
#else
    void *array;
#endif
    size_t used;
    size_t multiplicity;

    size_t entry_size() const {
      return sizeof(entry_t*) + sizeof(Type) * multiplicity;
    }
    void init() {
#ifdef POOL_DEBUG
      array = static_cast<void**>(std::malloc(sizeof(void*)*size));
      for (size_t i = 0; i < size; ++i)
        array[i] = static_cast<void*>(std::malloc(entry_size()));
#else
      array = mapper.map(size*entry_size());
#endif
    }

 public:
    Block()
      : used(0),
        multiplicity(1) {
      init();
    }
    explicit Block(size_t c)
      : used(0),
        multiplicity(c) {
      init();
    }
    ~Block() {
#ifdef POOL_DEBUG
      for (size_t i = 0; i < size; ++i)
        std::free(array[i]);
      std::free(array);
#else
      mapper.unmap(array, size*entry_size());
#endif
    }
    bool is_full() const { return used == size; }

    entry_t *malloc() {
      assert(!is_full());
      entry_t *ret;
#ifdef POOL_DEBUG
      ret = static_cast<entry_t*>(array[used++]);
#else
      ret = reinterpret_cast<entry_t*>(static_cast<unsigned char*>(array)
              + entry_size()*used);
      used++;
#endif
      return new (ret) entry_t();
    }
};

#ifdef USE_BOOST_POOL
// just for comparison purposes


template <typename Type = unsigned char, size_t Block_Size = 23>
class Pool {
 private:
    size_t count;
    size_t multiplicity;
    boost::pool<> *pool;

 public:
    Pool()
      : count(0),
        multiplicity(1) {
      pool = new boost::pool<>(sizeof(Type));
    }
    explicit Pool(size_t m)
      : count(0),
        multiplicity(m) {
      pool = new boost::pool<>(sizeof(Type)*multiplicity);
    }
    Type *malloc() {
      count++;
      return static_cast<Type*>(pool->malloc());
    }
    void free(Type *t) {
      assert(t);
      count--;
      pool->free(t);
    }
};

#else

template <typename Type = unsigned char,
#ifdef POOL_DEBUG
size_t Block_Size = 23
#else
size_t Block_Size = 100 * 1024 * 1024 / (
sizeof(Entry<Type>*) + sizeof(Type) )
#endif
>
class Pool {
 private:
    typedef Entry<Type> entry_t;
    typedef Block<Type, Block_Size> block_t;

    std::vector<block_t*> blocks;
    entry_t *head;
#ifndef NDEBUG
    size_t count;
#endif

    size_t multiplicity;

    entry_t *to_entry(Type *t) const {
      size_t *x = reinterpret_cast<size_t*>(t);
      return reinterpret_cast<entry_t*>(x-1);
    }
    void init() {
      blocks.push_back(new block_t(multiplicity));
      head = blocks.back()->malloc();
      assert(head);
    }

 public:
    Pool()
      : head(0),
#ifndef NDEBUG
        count(0),
#endif
        multiplicity(1) {
      init();
    }

    explicit Pool(size_t c)
      : head(0),
#ifndef NDEBUG
        count(0),
#endif
        multiplicity(c) {
      init();
    }

    ~Pool() {
      for (typename std::vector<block_t*>::iterator i = blocks.begin();
          i != blocks.end(); ++i)
        delete *i;
      assert(!count);
    }

    Type *malloc() {
      assert(head);
#ifndef NDEBUG
      count++;
#endif
      entry_t *ret = head;
      entry_t *t = head->next();
      if (t) {
        head = t;
      } else {
        if (blocks.back()->is_full())
          blocks.push_back(new block_t(multiplicity));
        head = blocks.back()->malloc();
        assert(head);
      }
      return ret->payload();
    }

    void free(Type *t) {
      assert(t);
#ifndef NDEBUG
      count--;
#endif
      entry_t *ret = to_entry(t);
      ret->set_next(head);
      head = ret;
    }
};
#endif


}  // namespace Map

#endif  // RTLIB_MAP_POOL_HH_
