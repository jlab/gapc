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


#include "string.hh"

// FIXME instead of object pool interface use singleton pool interface
// like in list?
Pool<String::Block> String::pool;

void *String::Block::operator new(size_t t) throw (std::bad_alloc)
{
  assert(t == sizeof(Block));
  Block *r = pool.malloc();
  return r;
}

void String::Block::operator delete(void *b) throw ()
{
  if (!b)
    return;
  pool.free(static_cast<Block*>(b));
}

void String::Block::put(std::ostream &s) const
{ 
  unsigned char i;
  for (i = 0; i<pos; ) {
    switch (array[i]) {
      case LINK :
        ++i;
        Block *b;
        for (unsigned char j = 0; j < sizeof(Block*); ++j)
          ((unsigned char*)&b)[j] = array[i++];
        b->put(s);
        break;
      case SEQ :
        ++i;
        for (; array[i] != SEQ_END; ++i)
          s << ((char) array[i]);
        ++i;
        break;
      case REP :
        ++i;
        uint32_t a;
        ( (unsigned char*) &a)[0] = array[i+1];
        ( (unsigned char*) &a)[1] = array[i+2];
        ( (unsigned char*) &a)[2] = array[i+3];
        ( (unsigned char*) &a)[3] = array[i+4];
        for (uint32_t b = 0; b < a; b++)
          s << (char) array[i];
        i++;
        i += sizeof(uint32_t);
        break;
      default:
        assert(false);
    }
  }
  assert(i == pos || i == pos+1);
}

std::ostream &operator<<(std::ostream &s, const String &str)
{
  str.put(s);
  return s;
}

