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


#ifndef RTLIB_STRING_HH_
#define RTLIB_STRING_HH_

#include <new>
#include <cassert>
#include <cstring>
#include <utility>
#include <ostream>
#include <stack>
#include <vector>
#include <cstdint>

// tr1 has it
#include <boost/cstdint.hpp>

// FIXME profile this
#include "pool.hh"

#include "subsequence.hh"

#include "../rtlib/cstr.h"

class String {
 private:
#if defined(CHECKPOINTING_INTEGRATED)
    friend class boost::serialization::access;

    template<class Archive>
    void serialize(Archive & ar, const unsigned int version) {
      ar & block;
      ar & readonly;
      ar & empty_;
      ar & block_as_int;
    }
#endif

 public:
    class Block {
     private:
#if defined(CHECKPOINTING_INTEGRATED)
        friend class boost::serialization::access;

        template<class Archive>
        void serialize(Archive & ar, const unsigned int version) {
          ar & pos;
          for (unsigned char i = 0; i < pos; i++) {
            ar & array[i];
          }
       }
#endif
        void del(Block *b) {
          assert(b->ref_count);
          b->dec_ref();
          if (!b->ref_count)
            delete b;
        }

        Block &operator=(const Block &b);

     public:
      uint32_t ref_count;

      // total size of object should be 64 bytes
      // (refcount: 4 bytes, pos: 1 byte, array: 59 bytes -> 64 bytes total)
      enum { size = 59 };
      enum { SEQ_END, SEQ, LINK, REP };

      unsigned char pos;
      unsigned char array[size];  // FIXME size template param?

      Block *get_link(unsigned char k) {
        assert(k > 0);
        assert(array[k-1] == LINK);
        Block *b = NULL;
        for (unsigned char j = 0; j < sizeof(Block*); ++j)
          ((unsigned char*)&b)[j] = array[k++];
        return b;
      }

      class Iterator {
       private:
          Block *b;
          unsigned char i;

       public:
          explicit Iterator(Block *block) : b(block), i(0) {
            this->operator++();
          }

          Iterator(Block *block, Block *other) : b(block), i(b->pos+1) {}

          Iterator &operator++() {
            for (; i < b->pos; i++)
              switch (b->array[i]) {
                case LINK :
                  i++;
                  i += sizeof(Block*);
                  assert(i <= b->pos);
                  return *this;
                  break;
                case SEQ :
                  for (; b->array[i] != SEQ_END; i++) {
                    {}
                    assert(i < b->pos);
                  }
                  break;
                case REP :
                  i++;
                  i += sizeof(uint32_t);
                  break;
                default :
                  assert(false);
              }
            i++;
            assert(b->pos+1 == i);
            return *this;
          }

          Block *operator*() {
            unsigned char k = i - sizeof(Block*);
            Block *r = b->get_link(k);
            return r;
          }

          bool operator==(const Iterator &itr) const {
            return b == itr.b && i == itr.i;
          }

          bool operator!=(const Iterator &itr) const {
            return !(*this == itr);
          }
      };

      typedef Iterator iterator;

      iterator begin() { return iterator(this); }
      iterator end() { return iterator(this, this); }

      Block() : ref_count(1), pos(0) {}

      Block(const Block &b) : ref_count(1), pos(b.pos) {
        for (unsigned char i = 0; i < pos; ++i)
          array[i] = b.array[i];
        for (iterator i = begin(); i != end(); ++i)
          (*i)->inc_ref();
      }

      ~Block() {
        for (iterator i = begin(); i != end(); ++i)
          del(*i);
      }

      void inc_ref() { ref_count++; }
      void dec_ref() { assert(ref_count > 0); ref_count--; }

      void append(char c) {
        assert(pos + 3 <= size);
        assert((unsigned char)c > LINK);
        array[pos++] = SEQ;
        array[pos++] = c;
        array[pos++] = SEQ_END;
      }

      void append(const char *s, unsigned char length) {
        assert(pos + length + 2 <= size);
        if (!length)
          return;
        array[pos++] = SEQ;
        for (unsigned char i = 0; i < length; ++i) {
          assert((unsigned char) (s[i]) > LINK);
          array[pos++] = s[i];
        }
        array[pos++] = SEQ_END;
      }

      void append(int j) {
        char s[12];
        unsigned char len;
        char *x = int_to_str(s, &len, j);
        assert(len);
        append(x, len);
      }

      void append(Block *b) {
        b->inc_ref();
        assert(pos + 1 + sizeof(Block*) <= size);
        array[pos++] = LINK;
        for (unsigned char i = 0; i < sizeof(Block*); ++i) {
          array[pos++] = ((unsigned char*)&b)[i];
        }
      }

      void append(char c, uint32_t l) {
        assert(pos + 1 + sizeof(uint32_t)  <= size);
        array[pos++] = REP;
        array[pos++] = c;

        array[pos++] = ((unsigned char *) &l)[0];
        array[pos++] = ((unsigned char *) &l)[1];
        array[pos++] = ((unsigned char *) &l)[2];
        array[pos++] = ((unsigned char *) &l)[3];
      }

      void *operator new(size_t t) noexcept(false);

      void operator delete(void *b) noexcept(false);

      void put(std::ostream &s) const;
    };

 private:
    static Pool<Block> pool;

    Block *block;
    bool readonly;
    bool empty_;

    void del() {
      if (!block) {
        return;
      }
      assert(block->ref_count);
      block->dec_ref();
      if (!block->ref_count) {
        delete block;
        block = NULL;
#if defined(CHECKPOINTING_INTEGRATED)
        block_as_int = 0;
#endif
      }
    }

    void check_copy() {
      if (readonly) {
        Block *t = new Block(*block);
        del();
        block = t;
#if defined(CHECKPOINTING_INTEGRATED)
        block_as_int = reinterpret_cast<uintptr_t>(block);
#endif
      }
    }

 public:
#if defined(CHECKPOINTING_INTEGRATED)
    // store block ptr as decimal number so
    // links can be restored after deserialization
    uintptr_t block_as_int;
#endif

    String() : block(NULL), readonly(false), empty_(false)
#if defined(CHECKPOINTING_INTEGRATED)
               , block_as_int(0)
#endif
    {}

    Block *get_block() const {
      return block;
    }

    void lazy_alloc() {
      if (!block)
        block = new Block();
#if defined(CHECKPOINTING_INTEGRATED)
        block_as_int = reinterpret_cast<uintptr_t>(block);
#endif
    }

    String(const String &s) {
      block = s.block;
      if (block) {
        block->inc_ref();
        readonly = true;
      } else {
        readonly = false;
      }
      empty_ = s.empty_;
#if defined(CHECKPOINTING_INTEGRATED)
      block_as_int = reinterpret_cast<uintptr_t>(block);
#endif
    }

    ~String() {
      del();
    }

    void append(char c) {
      lazy_alloc();
      empty_ = false;
      check_copy();
      block->append(c);
    }

    void append(const char *c, unsigned char l) {
      lazy_alloc();
      empty_ = false;
      check_copy();
      block->append(c, l);
    }

    void append(const String &s) {
      if (!s.block)
        return;
      lazy_alloc();
      empty_ = false;
      check_copy();
      block->append(s.block);
    }

    void append(int i) {
      lazy_alloc();
      empty_ = false;
      check_copy();
      block->append(i);
    }

    void append(char c, uint32_t l) {
      lazy_alloc();
      empty_ = false;
      check_copy();
      block->append(c, l);
    }

    void empty() {
      empty_ = true;
    }

    bool isEmpty() const {
      return empty_;
    }

    void put(std::ostream &s) const {
      if (block)
        block->put(s);
    }

    class Iterator {
     private:
        std::stack<std::pair<Block*, unsigned char> > stack;
        unsigned char i;
        Block *block;
        bool end;
        bool in_seq;

        bool in_rep;
        uint32_t rep;

        void fwd() {
          while (true) {
            assert(i <= block->pos);
            if (i == block->pos) {
              if (stack.empty()) {
                end = true;
                return;
              } else {
                block = stack.top().first;
                i = stack.top().second;
                stack.pop();
                continue;
              }
            }
            if (in_seq) {
              if (block->array[i] == Block::SEQ_END) {
                ++i;
                in_seq = false;
              } else {
                return;
              }
            } else {
              if (in_rep) {
                if (!rep) {
                  i++;
                  i += sizeof(uint32_t);
                  in_rep = false;
                } else {
                  rep--;
                  return;
                }
              } else {
                switch (block->array[i]) {
                  case Block::SEQ:
                    ++i;
                    in_seq = true;
                    break;
                  case Block::LINK: {
                    ++i;
                    Block *t = block->get_link(i);
                    i += sizeof(Block*);
                    stack.push(std::pair<Block*, unsigned char>(block, i));
                    block = t;
                    i = 0;
                                    }
                    break;
                  case Block::REP:
                    ++i;
                    ((unsigned char*) &rep)[0] = block->array[i+1];
                    ((unsigned char*) &rep)[1] = block->array[i+2];
                    ((unsigned char*) &rep)[2] = block->array[i+3];
                    ((unsigned char*) &rep)[3] = block->array[i+4];
                    in_rep = true;
                    break;
                  default:
                    assert(false);
                }
              }
            }
          }
        }

     public:
        explicit Iterator(Block *b)
          : i(0), block(b), end(false), in_seq(false), in_rep(false), rep(0) {
          if (!b)
            end = true;
          else
            fwd();
        }
        explicit Iterator(bool b) : i(0), block(NULL), end(true), in_seq(false),
          in_rep(false), rep(0) {}

        void operator++() {
          if (!in_rep)
            ++i;
          fwd();
        }

        char operator*() const {
          return block->array[i];
        }

        bool operator==(const Iterator &itr) const {
          if (end)
            if (itr.end)
              return true;
          return i == itr.i && block == itr.block;
        }

        bool operator!=(const Iterator &itr) const {
          return !(*this == itr);
        }
    };

    typedef Iterator iterator;

    iterator begin() const { return Iterator(block); }
    // FIXME does the compiler optimize this away
    // or is an end() object constructed every iterator?
    // alternative: use static end ...
    iterator end() const { return Iterator(true); }

    bool operator==(const String &s) const {
      iterator j = s.begin();
      iterator i = begin();
      for (; i != end() && j != s.end(); ++i, ++j)
        if (*i != *j)
          return false;
      return !(i != end() || j != end());
    }

    bool operator!=(const String &s) const {
      return !(*this == s);
    }

    String &operator=(const String &s) {
      Block *t = s.block;
      if (t)
        t->inc_ref();
      del();
      block = t;
      if (block)
        readonly = true;
      else
        readonly = false;
      empty_ = s.empty_;
#if defined(CHECKPOINTING_INTEGRATED)
      block_as_int = reinterpret_cast<uintptr_t>(block);
#endif
      return *this;
    }

    bool operator<(const String &s) const {
      iterator j = s.begin();
      iterator i = begin();
      for (; i != end() && j != s.end(); ++i, ++j)
        if (*i < *j)
          return true;
        else if (*j < *i)
          return false;
      if (i != end())
        return false;
      if (j != s.end())
        return true;
      return false;
    }

    bool operator>(const String &s) const {
      iterator j = s.begin();
      iterator i = begin();
      for (; i != end() && j != s.end(); ++i, ++j)
        if (*i > *j)
          return true;
        else if (*j > *i)
          return false;
      if (i != end())
        return true;
      if (j != s.end())
        return false;
      return false;
    }
};

std::ostream &operator<<(std::ostream &s, const String &str);


template<class T>
inline void append(String &str, const T &x) {
  str.append(x);
}

template<class T>
inline void append(String &str, char c, T i) {
  str.append(c, static_cast<uint32_t>(i));
}

inline void append(String &str, const char *c, int i) {
  str.append(c, i);
}

template<typename alphabet, typename pos_type>
inline void append(
  String &str, const Basic_Subsequence<alphabet, pos_type> &sub) {
  String t;
  t.append('<');
  t.append(static_cast<int>(sub.i));
  t.append(',');
  t.append(static_cast<int>(sub.j));
  t.append('>');
  str.append(t);
}

#include "bitops.hh"

inline uint32_t hashable_value(const String &str) {
  hash_to_uint32::djb hash_fn;
  uint32_t hash = hash_fn.initial();
  for (String::iterator i = str.begin(); i != str.end(); ++i)
    hash_fn.next(hash, *i);
  return hash;
}

#endif  // RTLIB_STRING_HH_
