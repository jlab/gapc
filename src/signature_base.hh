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

#ifndef SRC_SIGNATURE_BASE_HH_
#define SRC_SIGNATURE_BASE_HH_

#include <string>
#include "loc.hh"

class Fn_Decl;

class Signature_Base {
 public:
  std::string *name;
  Loc location;
  Signature_Base() : name(NULL) {}
  Signature_Base(std::string *n, const Loc &l) : name(n), location(l) {}
  explicit Signature_Base(std::string *n) : name(n) {}
  virtual ~Signature_Base();

  virtual Fn_Decl* decl(const std::string &s) = 0;
};

#endif  // SRC_SIGNATURE_BASE_HH_
