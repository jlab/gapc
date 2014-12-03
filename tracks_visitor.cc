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

#include "tracks_visitor.hh"

#include <sstream>
#include "log.hh"

#include "symbol.hh"
#include "alt.hh"
#include "fn_arg.hh"

Tracks_Visitor::Tracks_Visitor(Grammar &g)
  : skip(false), local_tracks(0), old_tracks(0),
    current_single_track(0),
    grammar(g),
    again(false), error(false)
{
}

void Tracks_Visitor::visit(Symbol::NT &n)
{
  local_tracks = n.tracks();
  current_single_track = n.track_pos();
  if (!local_tracks) {
    again = true;
    skip = true;
    return;
  }
  skip = false;
}

void Tracks_Visitor::visit(Fn_Arg::Base &a)
{
  a.set_tracks(local_tracks);
}

void Tracks_Visitor::visit(Alt::Base &a)
{
  a.set_tracks(local_tracks, current_single_track);
}

void Tracks_Visitor::visit_begin(Alt::Simple &a)
{
  a.set_tracks(local_tracks, current_single_track);

  if (a.has_index_overlay()) {
    a.index_overlay.front()->traverse(*this);
  }
}

Symbol::NT *Tracks_Visitor::different_track(Symbol::NT *nt, size_t track_pos)
{
  key_t key(*nt->orig_name, track_pos);
  hashtable<key_t, Symbol::NT*>::iterator i = duplicates.find(key);
  if (i != duplicates.end()) {
    return i->second;;
  }
  Symbol::NT *x = nt->clone(track_pos);
  duplicates[key] = x;
  grammar.add_nt_later(x);
  again = true;
  return x;
}

void Tracks_Visitor::visit(Alt::Link &a)
{
  if (skip)
    return;
  size_t x = a.nt->tracks();
  if (x && x != local_tracks) {
    error = true;
    std::ostringstream o, m;
    o << "Multi-Track mis-match: Caller has " << local_tracks << " tracks";
    Log::instance()->error(a.location, o.str());
    if (a.nt->is(Symbol::TERMINAL)) {
      Log::instance()->error("Terminal parsers operate on one track.");
      return;
    }
    m  << "Callee has " << x << " tracks";
    Log::instance()->error(a.nt->location, m.str());
    return;
  }
  if (a.nt->is(Symbol::NONTERMINAL)) {
    if (a.nt->tracks() && a.nt->track_pos() != current_single_track) {
      Symbol::NT *nt = different_track(dynamic_cast<Symbol::NT*>(a.nt),
          current_single_track);
      a.nt = nt;
      a.name = nt->name;
    } else {
      key_t key(*a.nt->orig_name, current_single_track);
      hashtable<key_t, Symbol::NT*>::iterator i = duplicates.find(key);
      if (i == duplicates.end())
        duplicates[key] = dynamic_cast<Symbol::NT*>(a.nt);
    }
    a.nt->set_tracks(local_tracks, current_single_track);
  }
}

void Tracks_Visitor::visit(Alt::Multi &a)
{
  if (skip)
    return;
  if (local_tracks != a.tracks() || a.tracks() == 1) {
    error = true;
    std::ostringstream o;
    o << "Incompatible number of tracks: " << local_tracks << " vs. " << a.tracks();
    Log::instance()->error(a.location, o.str());
  }
  old_tracks = local_tracks;
  local_tracks = 1;

  current_single_track = 0;
}

void Tracks_Visitor::visit_itr(Alt::Multi &a)
{
  if (skip)
    return;
  current_single_track++;
}

void Tracks_Visitor::visit_end(Alt::Multi &a)
{
  if (skip)
    return;
  local_tracks = old_tracks;
  current_single_track = 0;
}


