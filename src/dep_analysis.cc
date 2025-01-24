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

#include <utility>
#include <algorithm>
#include <vector>
#include <list>
#include <string>

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>

#include "dep_analysis.hh"
#include "symbol.hh"


Dep_Analysis::Dep_Analysis(const hashtable<std::string, Symbol::Base*> &s) :
  symbols(s) {
  size_t a = 0;
  int_map.resize(s.size());
  for (hashtable<std::string, Symbol::Base*>::const_iterator i = s.begin();
       i != s.end(); ++i) {
    Symbol::NT* nt = dynamic_cast<Symbol::NT*>(i->second);
    if (!nt)
      continue;
    int_map[a] = nt;
    nt_map[nt] = a;
    a++;
  }
  int_map.resize(nt_map.size());
}


typedef boost::adjacency_list< boost::vecS, boost::vecS, boost::directedS,
        boost::property<boost::vertex_color_t, boost::default_color_type> >
        Graph;
typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;
typedef std::pair<size_t, size_t> Edge;
typedef std::list<Vertex> TOrdering;

void Dep_Analysis::sort() {
  std::vector<std::string> debug_out;

  std::vector<Edge> edges;
  for (hashtable<std::string, Symbol::Base*>::const_iterator
       i = symbols.begin();
       i != symbols.end(); ++i) {
    Symbol::NT* nt = dynamic_cast<Symbol::NT*>(i->second);
    if (!nt)
      continue;
    std::list<Symbol::NT*> list;
    nt->collect_lr_deps(list);
    hashtable<Symbol::NT*, size_t>::iterator a = nt_map.find(nt);
    assert(a != nt_map.end());
    size_t x = a->second;

    std::string debug_from(*a->first->name + " -> ");

    for (std::list<Symbol::NT*>::iterator j = list.begin(); j != list.end();
         ++j) {
      hashtable<Symbol::NT*, size_t>::iterator a = nt_map.find(*j);
      assert(a != nt_map.end());
      size_t y = a->second;
      edges.push_back(std::make_pair(x, y));

      debug_out.push_back(debug_from + *a->first->name);
    }
  }

  std::sort(debug_out.begin(), debug_out.end());
  if (Log::instance()->is_debug()) {
    Log::o() << '\n';
    for (std::vector<std::string>::iterator i = debug_out.begin();
         i != debug_out.end(); ++i)
      Log::o() << *i << '\n';
  }

  Graph g(edges.begin(), edges.end(), int_map.size());
  TOrdering res;
  boost::topological_sort(g, std::back_inserter(res));
  for (TOrdering::iterator j = res.begin(); j != res.end(); ++j) {
    ordering.push_back(int_map[*j]);
  }
}
