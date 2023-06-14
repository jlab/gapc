/* {{{

    This file is part of gapc (GAPC - Grammars, Algebras, Products - Compiler;
      a system to compile algebraic dynamic programming programs)

    Copyright (C) 2011-2023  Stefan Janssen
         email: stefan.m.janssen@gmail.com or stefan.janssen@computational.bio.uni-giessen.de

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

#ifndef SRC_OUTSIDE_GRAMMAR_TRANSFORMATION_HH_
#define SRC_OUTSIDE_GRAMMAR_TRANSFORMATION_HH_

#include <vector>
#include <list>
#include <string>
#include <set>

#include "grammar_transformation.hh"
#include "../signature.hh"
#include "../instance.hh"
#include "../grammar.hh"
#include "../ast.hh"
#include "../fn_def.hh"
#include "../visitor.hh"
#include "../type/multi.hh"
#include "../fn_arg.hh"

static const char * const OUTSIDE_NT_PREFIX = "outside_";
static const char * const OUTSIDE_ALL = "ALL";

// check if a type (used in Signature or Algebra) belongs to a terminal parser
// or a non-terminal parser
bool is_terminal_type(Type::Base*);

/* recursively resolve Alt::Blocks (which are short hand notations for
 * alternative production rules) for a non-terminal. An example would be:
 *   struct = cadd(dangle, incl({struct | weak}))
 * which shall be resolved into
 *   struct = cadd(dangle, incl(struct)) | cadd(dangle, incl(weak))
 * note that
 *  a) we do not know the "level" of the Alt::Block use
 *  b) we can have Alt::Block within Alt::Block */
void resolve_blocks(Symbol::NT *nt);

#endif  // SRC_OUTSIDE_GRAMMAR_TRANSFORMATION_HH_
