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


#ifndef TRACKS_VISITOR_HH
#define TRACKS_VISITOR_HH

#include "visitor.hh"

#include <stddef.h>
#include <utility>
#include <string>

#include "hashtable.hh"

#include "symbol_fwd.hh"

class Grammar;

class Tracks_Visitor : public Visitor {
  private:
    bool skip;
    size_t local_tracks, old_tracks;
    size_t current_single_track;

    typedef std::pair<std::string, size_t> key_t;
    hashtable<key_t, Symbol::NT*> duplicates;

    Grammar &grammar;
    Symbol::NT *different_track(Symbol::NT *nt, size_t track_pos);

  public:
    bool again;
    bool error;

    Tracks_Visitor(Grammar &g);

    void visit(Symbol::NT &n);

    void visit(Alt::Base &a);
    void visit_begin(Alt::Simple &a);
    void visit(Alt::Link &a);
    void visit(Alt::Multi &a);
    void visit_itr(Alt::Multi &a);
    void visit_end(Alt::Multi &a);

    void visit(Fn_Arg::Base &a);

};

#endif
