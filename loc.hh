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


#ifndef LOC_HH
#define LOC_HH

#include "location.hh"

#include <string>
#include <cstdio>

#define LINE_SIZE 160
#define CHECK_EXIT(A) if ((A)<0) { std::fprintf(stderr, "%s:%d", __FILE__, __LINE__); std::perror(""); std::abort(); }

class Loc : public yy::location {

  private:
    long off;
    std::FILE *file;

  public:
    // FIXME needed?
    Loc();
    Loc(const yy::location &l);

    long offset() const { return off; }
    void setOffset(long o) { off = o; }
    void set_file(std::FILE* f);
    std::FILE *file_ptr() const { return file; }

    std::string line() const;

};

inline const Loc operator+ (const Loc& begin, const Loc& end)
{
  const yy::location &b = (begin);
  const yy::location &e = (end);
  yy::location r = b + e;
  Loc l(r);
  l.setOffset(begin.offset());
  l.set_file(begin.file_ptr());
  return l;
}


#endif
