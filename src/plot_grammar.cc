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

#include "alt.hh"
#include "symbol.hh"
#include "expr.hh"
#include "const.hh"
#include "fn_arg.hh"
#include "fn_def.hh"

static const char *COLOR_OVERLAY = "#cc5555";
static const char *COLOR_INDICES = "#555555";
static const char *COLOR_FILTER = "magenta";
static const char *COLOR_TYPE = "orange";
static const char *COLOR_TERMINAL = "blue";
static const char *COLOR_ALGFCT = "green";
static const char *COLOR_NONTERMINAL = "black";
static const char *COLOR_BLOCK = "gray";
static const char *COLOR_EVALFCT = "purple";

static const bool HIDEINVISIBLE = true;

std::string make_insivible(bool singlearg) {
  if (!HIDEINVISIBLE) {
    return "";
  } else {
    if (singlearg) {
      return "style=\"invis\"";
    } else {
      return "style=\"invis\", ";
    }
  }
}

// the following functions produce graphViz code to represent the grammar
void Alt::Link::to_dot_overlayindices(std::ostream &out, bool is_left_index) {
  out << "<td><font point-size='8' color='" << COLOR_OVERLAY << "'><b>";
  if (is_left_index) {
    this->indices.front()->put(out);
  } else {
    this->indices.back()->put(out);
  }
  out << "</b></font></td>";
}
// prints left or right indices of a parser to out stream.
// used as a helper for to_dot functions
void to_dot_indices(std::vector<Expr::Base*> indices, std::ostream &out) {
  out << "<td><font point-size='8' color='" << COLOR_INDICES << "'>";
  for (std::vector<Expr::Base*>::const_iterator track = indices.begin();
       track != indices.end(); ++track) {
    // assert(*track != NULL);
    if (*track == NULL) {
      out << "NULL";
    } else {
      (*track)->put(out);
    }
    if (std::next(track) != indices.end()) {
      out << "<br/>";
    }
  }
  out << "</font></td>";
}

// adds further lines (one per track) to indicate yield sizes of
// grammar components
void to_dot_multiys(Yield::Multi m_ys, std::ostream &out) {
  // one additional line per track for minimum and maximum yield size
  unsigned int track = 0;
  for (Yield::Multi::iterator ys = m_ys.begin(); ys != m_ys.end();
       ++ys, ++track) {
    out << "<tr>";
    out << "<td colspan=\"3\">yield size";
    if (m_ys.tracks() > 1) {
      out << " (track " << track << ")";
    }
    out << ": " << *ys << "</td>";
    out << "</tr>";
  }
}
void to_dot_filternameargs(Filter *filter, std::ostream &out) {
  out << *filter->name;
  for (std::list<Expr::Base*>::const_iterator arg = filter->args.begin();
       arg != filter->args.end(); ++arg) {
    if (arg == filter->args.begin()) {
      out << "(";
    }
    Expr::Const *c = dynamic_cast<Expr::Const*>(*arg);
    if (c && c->base->is(Const::STRING)) {
      out << "\\\"";
      (*c).put_noquote(out);
      out << "\\\"";
    } else {
      (*arg)->put(out);
    }
    if (std::next(arg) != filter->args.end()) {
      out << ", ";
    } else {
      out << ")";
    }
  }
}
unsigned int Alt::Base::to_dot_semanticfilters(unsigned int *nodeID,
    unsigned int thisID, std::ostream &out,
    std::vector<unsigned int> *childIDs) {
  unsigned int deepest_nodeID = 0;
  // add syntactic filters
  for (std::list<Filter*>::const_iterator filter = this->filters.begin();
       filter != this->filters.end(); ++filter) {
    unsigned int childID = (unsigned int)((*nodeID)++);
    deepest_nodeID = childID;
    out << "    node_" << childID << " [ label=\"";
    to_dot_filternameargs(*filter, out);
    out << "\" , fontcolor=\"" << COLOR_FILTER << "\" , shape=none ];\n";
    if (childIDs) {
      for (std::vector<unsigned int>::const_iterator i = childIDs->begin();
           i != childIDs->end(); ++i) {
        out << "node_" << *i << " -> node_" << childID
            << " [ arrowhead=none, color=\"" << COLOR_FILTER << "\" ];\n";
      }
    } else {
      out << "    node_" << thisID << " -> node_" << childID
          << " [ arrowhead=none, color=\"" << COLOR_FILTER << "\" ];\n";
    }
  }
  // Multiple filter can be added to each track.
  // Unfortunately, filters are stored in a list per track, containing
  // lists of filter. However, we want to draw one node per set of
  // filters, i.e. filters at same position on all tracks. Therefore,
  // we need to know the max number of filters across tracks and
  // then add one node per position each containing all tracks.
  unsigned int max_multifilter_number = 0;
  for (std::vector<std::list<Filter*> >::iterator
       track = this->multi_filter.begin();
       track != this->multi_filter.end(); ++track) {
    if ((*track).size() > max_multifilter_number) {
      max_multifilter_number = (*track).size();
    }
  }
  for (unsigned int filterpos = 0; filterpos < max_multifilter_number;
       ++filterpos) {
    unsigned int childID = (unsigned int)((*nodeID)++);
    deepest_nodeID = childID;
    out << "    node_" << childID << " [ label=<<table border='0'>";
    for (std::vector<std::list<Filter*> >::iterator
         i = this->multi_filter.begin();
         i != this->multi_filter.end(); ++i) {
      out << "<tr><td>";
      std::list<Filter*>::const_iterator filter = (*i).begin();
      // advance to filter position
      for (unsigned int i = 0; i < filterpos; ++i, ++filter) {}
      if (filter != (*i).end()) {
        to_dot_filternameargs(*filter, out);
      } else {
        out << "-";
      }
      out << "</td></tr>";
    }
    out << "</table>>, fontcolor=\"" << COLOR_FILTER << "\", shape=none ];\n";
    out << "node_" << thisID << " -> node_" << childID
        << " [ arrowhead=none, color=\"" << COLOR_FILTER << "\" ];\n";
  }
  return deepest_nodeID;
}
unsigned int* Alt::Base::to_dot(unsigned int *nodeID, std::ostream &out,
         int plot_grammar) {
  unsigned int max_depth = 1;
  unsigned int thisID = (unsigned int)((*nodeID)++);
  unsigned int deepest_nodeID = thisID;
  out << "    node_" << thisID << " [ label=<<table border='0'><tr>";
  Alt::Link *link = dynamic_cast<Alt::Link*>(this);
  if (plot_grammar > 1) {
    if (link && (link->is_explicit() == true)) {
      // indices have been given via index hack in source file:
      link->to_dot_overlayindices(out, true);
    } else {
      to_dot_indices(this->left_indices, out);
    }
  }
  Alt::Simple *simple = dynamic_cast<Alt::Simple*>(this);
  if (simple) {
    out << "<td>";
    if (simple->has_index_overlay()) {
      out << ".[ ";
    }
    out << *simple->name;
    simple->ntparas_to_dot(out);
    if (simple->has_index_overlay()) {
      out << " ].";
    }

    // terminal arguments e.g. CHAR('A')
    if (simple->is_terminal()) {
      for (std::list<Fn_Arg::Base*>::const_iterator arg = simple->args.begin();
           arg != simple->args.end(); ++arg) {
        if (arg == simple->args.begin()) {
          out << "(";
        }
        (*arg)->print(out);
        if (std::next(arg) != simple->args.end()) {
          out << ", ";
        } else {
          out << ")";
        }
      }
    }
  }
  if (link) {
    out << "<td>" << *link->name;
    link->ntparas_to_dot(out);
  }
  Alt::Block *block = dynamic_cast<Alt::Block*>(this);
  if (block) {
    out << "<td>a block";
  }
  if (plot_grammar > 2) {
    // if we want to also print out datatypes
    out << "<br/><font color='" << COLOR_TYPE << "'>";
    if (this->datatype == NULL) {
      out << "NULL";
    } else {
      this->datatype->to_dot(out);
    }
    out << "</font>";
  }
  out << "</td>";
  if (plot_grammar > 1) {
    if (link && (link->is_explicit() == true)) {
      // indices have been given via index hack in source file:
      link->to_dot_overlayindices(out, false);
    } else {
      to_dot_indices(this->right_indices, out);
    }
  }
  out << "</tr>";
  if (plot_grammar > 3) {
    to_dot_multiys(m_ys, out);
  }
  out << "</table>>, color=\"";
  if (simple) {
    if (simple->is_terminal()) {
      out << COLOR_TERMINAL;
    } else {
      out << COLOR_ALGFCT;
    }
  } else if (link) {
    Symbol::NT *nt = dynamic_cast<Symbol::NT*>(link->nt);
    if (nt) {
      out << COLOR_NONTERMINAL;
    } else {
      out << COLOR_TERMINAL;
    }
  } else if (block) {
    out << COLOR_BLOCK;
  } else {
    out << COLOR_NONTERMINAL;
  }
  out << "\" ";
  // indicate index hack via 8-sided polygon instead of circle
  if ((simple && simple->has_index_overlay()) ||
      (link && link->is_explicit())) {
    out << ", shape=\"polygon\", sides=8";
  }
  out << "];\n";

  // add syntactic filters
  unsigned int filter_nodeID = to_dot_semanticfilters(nodeID, thisID, out);
  if (filter_nodeID > 0) {
    // semantic filter can at most add one new level of nodes as all filters
    // will populate the same level. If no filter is present, no new layer
    // is added.
    max_depth++;
    deepest_nodeID = filter_nodeID;
  }

  unsigned int* res = (unsigned int*) malloc(3* sizeof(unsigned int*));
  res[0] = thisID;
  res[1] = max_depth;
  res[2] = deepest_nodeID;
  return res;
}
unsigned int* Alt::Simple::to_dot(unsigned int *nodeID, std::ostream &out,
         int plot_grammar) {
  unsigned int* res = Alt::Base::to_dot(nodeID, out, plot_grammar);
  unsigned int thisID = res[0];
  unsigned int max_depth = 0;
  unsigned int deepest_nodeID = res[2];
  for (std::list<Fn_Arg::Base*>::const_iterator arg = this->args.begin();
       arg != this->args.end(); ++arg) {
    Fn_Arg::Alt *argalt = dynamic_cast<Fn_Arg::Alt*>(*arg);
    if (argalt) {
      unsigned int* childID = argalt->alt_ref()->to_dot(nodeID, out,
              plot_grammar);
      out << "    node_" << thisID << " -> node_" << childID[0]
          << " [ arrowhead=none ";
      if (childID[1] > max_depth) {
        max_depth = childID[1];
        deepest_nodeID = childID[2];
      }
      Alt::Multi *multi = dynamic_cast<Alt::Multi*>(argalt->alt_ref());
      if (multi) {
        out << ", lhead=cluster_node_" << (childID[0]-1) << " ";
      }
      out << "];\n";
    }
  }
  if (res[1] < max_depth+1) {
    res[1] = max_depth+1;
  }
  res[2] = deepest_nodeID;
  return res;
}
unsigned int* Alt::Link::to_dot(unsigned int *nodeID, std::ostream &out,
        int plot_grammar) {
  return Alt::Base::to_dot(nodeID, out, plot_grammar);
}
unsigned int* Alt::Block::to_dot(unsigned int *nodeID, std::ostream &out,
        int plot_grammar) {
  unsigned int* res = Alt::Base::to_dot(nodeID, out, plot_grammar);
  unsigned int thisID = res[0];
  unsigned int max_depth = 0;
  unsigned int deepest_nodeID = res[2];
  if (res[1] > max_depth) {
    max_depth = res[1];
  }
  for (std::list<Alt::Base*>::const_iterator alt = this->alts.begin();
       alt != this->alts.end(); ++alt) {
    unsigned int* res2 = (*alt)->to_dot(nodeID, out, plot_grammar);
    if (res2[1]+1 > max_depth) {
        max_depth = res2[1]+1;
        deepest_nodeID = res2[2];
    }
    unsigned int childID = res2[0];
    out << "    node_" << thisID << " -> node_" << childID
        << " [ ];\n";
  }
  res[1] = max_depth;
  res[2] = deepest_nodeID;
  return res;
}
unsigned int* Alt::Multi::to_dot(unsigned int *nodeID, std::ostream &out,
        int plot_grammar) {
  unsigned int thisID = (unsigned int)((*nodeID)++);
  out << "subgraph cluster_node_" << thisID << " {\n";
  unsigned int lastID = 0;
  unsigned int max_depth = 0;
  unsigned int deepest_nodeID = 0;
  std::vector<unsigned int> *childIDs = new std::vector<unsigned int>();
  for (std::list<Alt::Base*>::const_iterator alt = this->list.begin();
       alt != this->list.end(); ++alt) {
    unsigned int* childID = (*alt)->to_dot(nodeID, out, plot_grammar);
    max_depth += childID[1];
    deepest_nodeID = childID[2];
    childIDs->push_back(childID[0]);
    if (lastID > 0) {
      out << "    node_" << lastID << " -> node_" << childID[0]
          << " [ " << make_insivible(true) << " ];\n";
    }
    lastID = childID[0];
  }
  // add syntactic filter and draw an edge to every track component
  unsigned int filter_nodeID = to_dot_semanticfilters(nodeID, thisID, out, childIDs);
  if (filter_nodeID > 0) {
    // semantic filter can at most add one new level of nodes as all filters
    // will populate the same level. If no filter is present, no new layer
    // is added.
    max_depth++;
    deepest_nodeID = filter_nodeID;
  }
  out << "};\n";

  unsigned int* res = (unsigned int*) malloc(3*sizeof(unsigned int*));
  res[0] = thisID+1;
  res[1] = max_depth;
  res[2] = deepest_nodeID;

  // return not the cluster ID but the id of the first element
  return res;
}

void Alt::Base::ntparas_to_dot(std::ostream &out) {
  throw LogError("Non-terminal parameter list cannot be plotted to graphViz!");
}
void Alt::Simple::ntparas_to_dot(std::ostream &out) {
  if (this->ntparas.size() > 0) {
    out << " (";
  }
  for (std::list<Expr::Base*>::const_iterator ip = this->ntparas.begin();
       ip != this->ntparas.end(); ++ip) {
    out << *(*ip);
    if (std::next(ip) != this->ntparas.end()) {
      out << ", ";
    }
  }
  if (this->ntparas.size() > 0) {
    out << ")";
  }
}
void Alt::Link::ntparas_to_dot(std::ostream &out) {
  if (this->ntparas.size() > 0) {
    out << " (";
  }
  for (std::list<Expr::Base*>::const_iterator ip = this->ntparas.begin();
       ip != this->ntparas.end(); ++ip) {
    out << *(*ip);
    if (std::next(ip) != this->ntparas.end()) {
      out << ", ";
    }
  }
  if (this->ntparas.size() > 0) {
    out << ")";
  }
}
// END functions produce graphViz code to represent the grammar


// the following functions produce graphViz code to represent the grammar
unsigned int Symbol::Base::to_dot(unsigned int *nodeID, std::ostream &out,
        bool is_rhs, Symbol::NT *axiom, int plot_grammar) {
  unsigned int thisID = (unsigned int)((*nodeID)++);
  out << "    node_" << thisID << " [ label=<<table border='0'><tr>";
  if (plot_grammar > 1) {
    to_dot_indices(this->left_indices, out);
    out << "<td>" << *this->name;
    if (plot_grammar > 2) {
      // if we want to also print out datatypes
      out << "<br/><font color='" << COLOR_TYPE << "'>";
      if (this->datatype == NULL) {
        out << "NULL";
      } else {
        this->datatype->to_dot(out);
      }
      out << "</font>";
    }
    out << "</td>";
    to_dot_indices(this->right_indices, out);
    out << "</tr>";
    if (plot_grammar > 3) {
      to_dot_multiys(this->m_ys, out);
    }
    if (plot_grammar > 4) {
      // print table dimensions ...
      Symbol::NT *nt = dynamic_cast<Symbol::NT*>(this);
      if (nt) {
        // .. if Symbol is Non-terminal
        out << "<tr><td colspan=\"3\">";
        for (std::vector<Table>::const_iterator t = nt->tables().begin();
             t != nt->tables().end(); ++t) {
          (*t).print(out);
          if (std::next(t) != nt->tables().end()) {
            out << " ";
          }
        }
        out << "</td></tr>";
      }
    }
    out << "</table>>";
  } else {
    out << "<td>" << *this->name << "</td></tr></table>>";
  }
  return thisID;
}
unsigned int Symbol::Terminal::to_dot(unsigned int *nodeID, std::ostream &out,
                                      bool is_rhs, Symbol::NT *axiom,
                                      int plot_grammar) {
  unsigned int thisID = Symbol::Base::to_dot(nodeID, out, is_rhs, axiom,
    plot_grammar);
  out << ", color=\"" << COLOR_TERMINAL << "\", fontcolor=\""
      << COLOR_TERMINAL << "\" ];\n";
  return thisID;
}
unsigned int Symbol::NT::to_dot(unsigned int *nodeID, std::ostream &out,
                                bool is_rhs, Symbol::NT *axiom,
                                int plot_grammar) {
  unsigned int thisID = Symbol::Base::to_dot(
    nodeID, out, is_rhs, axiom, plot_grammar);
  unsigned int anchorID = 0;
  unsigned int max_depth = 1;
  unsigned int deepest_nodeID = 0;
  unsigned int *res = (unsigned int *) malloc(2 * sizeof(int));
  // with "rank" we collect nodes that must be drawn topmost in a cluster
  std::string rank = "";
  out << ", color=\"" << COLOR_NONTERMINAL << "\"";
  if (!is_rhs) {
    // a non-terminal "calling" productions, i.e. on the left hand side
    out << ", shape=\"box\"";
    if (axiom && (this == axiom)) {
      out << ", penwidth=3";
    }
    if (!this->tabulated) {
      out << ", style=\"dotted\"";
    }
    out << " ];\n";

    unsigned int sepNodeID;
    if (this->alts.size() > 0) {
      sepNodeID = (unsigned int)((*nodeID)++);
      // add an invisible edge from lhs NT to --> node
      out << "    node_" << thisID << " -> node_" << sepNodeID
          << " [ " << make_insivible(false) << "weight=99 ];\n";
      // adding a separator node to draw the --> arrow from lhs NT
      // name to alternatives
      out << "    node_" << sepNodeID << " [ label=<<table border='0'><tr>"
          << "<td><font point-size='30'>&rarr;</font></td></tr></table>>,"
          << " shape=plaintext ];\n";
      // add an invisible edge from the --> node to the first
      // alternative production
      out << "    node_" << sepNodeID << " -> node_" << *nodeID
          << " [ " << make_insivible(true) << " ];\n";
      rank += "node_" + std::to_string(sepNodeID) + " ";
    }
    for (std::list<Alt::Base*>::const_iterator alt = this->alts.begin();
         alt != this->alts.end(); ++alt) {
      // drawing the alternative
      res = (*alt)->to_dot(nodeID, out, plot_grammar);
      unsigned int childID = res[0];
      if (res[1] > max_depth) {
        max_depth = res[1];
        deepest_nodeID = res[2];
      }

      rank += "node_" + std::to_string(childID) + " ";
      if (std::next(alt) != this->alts.end()) {
        // add a separator node to draw a | symbol between every
        // two alternatives. This is similar to the --> node, i.e. with
        // incoming and outgoing invisible edges
        sepNodeID = (unsigned int)((*nodeID)++);
        out << "    node_" << childID << " -> node_" << sepNodeID
            << " [ " << make_insivible(true) << " ];\n";
        out << "    node_" << sepNodeID << " [ label=<<table border='0'>"
            << "<tr><td><font point-size='30'>|</font></td></tr></table>>"
            << ", shape=plaintext ];\n";
        out << "    node_" << sepNodeID << " -> node_" << *nodeID
            << " [ " << make_insivible(true) << " ];\n";
        rank += "node_" + std::to_string(sepNodeID) + " ";
      }
    }

    // depth of the lhsNT to which other NT boxes will be oriented
    unsigned int lhsNT_depth = 1;

    // plot evaluation function
    unsigned int choiceID = thisID;
    if (this->eval_fn != NULL) {
      choiceID = (unsigned int)((*nodeID)++);
      out << "    node_" << choiceID << " [ label=<" << *this->eval_fn;
      if (plot_grammar > 2) {
        // if we want to also print out datatypes
        out << "<br/><font color='" << COLOR_TYPE << "'>";
        if (this->eval_decl == NULL) {
          out << "NULL";
        } else {
          this->eval_decl->return_type->to_dot(out);
        }
        out << "</font>";
      }
      out << ">, fontcolor=\"" << COLOR_EVALFCT << "\", shape=plain ];\n";
      out << "    node_" << thisID << " -> node_" << choiceID
          << " [ arrowhead=none, color=\""
          << COLOR_EVALFCT << "\" ];\n";
      // choice function will be located on depth+1, i.e. one less
      // invisible fake node necessary
      lhsNT_depth++;
    }

    /* In order to align each NT vertically below each other, we inject one
     * invisible "anchor" node on the same rank as the deepest node on the rhs.
     * We next connect the lhs NT node with this anchor on "west" = left ports
     * from south to north. Last, we ensure the invisible anchor and the deepest
     * node is on the same rank.
     * Note: it is important that the anchor node has a rectangular shape, we
     *       also want to make it as small as possible to not shift visible
     *       nodes unneccesarily far to the right*/
    if (max_depth > 1) {
      anchorID = (unsigned int)((*nodeID)++);
      out << "    node_" << anchorID << " [ " << make_insivible(false)
          << "shape=box, fixedsize=true, width=0.01, label=\"\" ];\n";
      out << "    node_" << thisID << ":sw -> node_" << anchorID << ":nw ["
          << make_insivible(false) << "weight=999 ];\n";
      out << "    { rank=same node_" << anchorID << " node_" << deepest_nodeID
          << "}\n";
    }

    out << "    { rank=same node_" << thisID << " " << rank << "}\n";
  }
  // if is_rhs = True, name will be drawn by alt::Base
  free(res);
  return anchorID;
}
// END functions produce graphViz code to represent the grammar

unsigned int Grammar::to_dot(unsigned int *nodeID, std::ostream &out,
        int plot_grammar) {
  int start_node;
  unsigned int i = 1;
  out << "digraph " << *this->name << " {\n";
  out << "compound = True;\n";
  out << "newrank = True;\n";
  out << "ordering = out;\n";
  for (std::list<Symbol::NT*>::const_iterator nt = this->nt_list.begin();
       nt != this->nt_list.end(); ++nt, ++i) {
    if (nt != this->nt_list.begin()) {
      // except for the first unit, we add an invisible node (anchor) and
      // invisible edges from the anchor to the lhs non-terminal node of the
      // next unit to enable vertical alignment
      out << "node_" << start_node << ":sw -> node_" << std::to_string(*nodeID)
          << ":nw [ " << make_insivible(true) << " ];\n";
    }
    // let's organize all nodes of a lhs non-terminal in one subgraph cluster
    // such that it can be plotted as one unit and these units are
    // vertically stacked, while elements in the unit are horizontally aligned
    out << "subgraph cluster_" << i << " {\n";
    start_node = (*nt)->to_dot(nodeID, out, false, this->axiom, plot_grammar);
    out << "}\n";
  }
  out << "}\n";
  return ((unsigned int)*nodeID);
}

// graphViz compatible text representation of datatype
void Type::Base::to_dot(std::ostream &out) {
  std::ostringstream dtype_stream;
  this->put(dtype_stream);
  std::string dtype = dtype_stream.str();
  replaceAll(dtype, std::string("<"), std::string("&lt;"));
  replaceAll(dtype, std::string(">"), std::string("&gt;"));
  out << dtype;
}
