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

//  #include "lexer.h"

#include "loc.hh"

#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <string>

#include <iostream>
#include <cassert>

Loc::Loc() : yy::location(), off(0), file(0) {}
Loc::Loc(const yy::location &l) : yy::location(l), off(0), file(0) {
}

std::string Loc::line() const {
  assert(file);
  int r;
  char *buffer = new char[LINE_SIZE];
  int64_t fpos = std::ftell(file); CHECK_EXIT(fpos);
  r = std::fseek(file, off, SEEK_SET); CHECK_EXIT(r);
  char *t = std::fgets(buffer, LINE_SIZE, file);
  if (!t) {
    if (std::feof(file)) {
      return std::string("<EOF>");
    }
    std::perror("fgets");
    std::exit(1);
  }
  // XXX wtf?!?
  if (std::strlen(buffer))
    buffer[std::strlen(buffer)-1] = 0;
  r = std::fseek(file, fpos, SEEK_SET); CHECK_EXIT(r);
  std::string s(buffer);
  delete[] buffer;
  return s;
}

void Loc::set_file(std::FILE* f) {
  //  assert(f);
  file = f;
}
