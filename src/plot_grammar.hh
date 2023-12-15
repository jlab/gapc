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


#ifndef SRC_PLOT_GRAMMAR_HH_
#define SRC_PLOT_GRAMMAR_HH_


/* color definitions for grammar plots as well as tikZ candidate trees.
 * As tikZ's latex syntax requires an [HTML] parameter for color definitions,
 * we cannot use mixed encoding (names & hex), which works well for graphViz,
 * though.
 */

static const char * const COLOR_OVERLAY = "#cc5555";      // "brown"
static const char * const COLOR_INDICES = "#555555";      // "light gray"
static const char * const COLOR_FILTER = "#fc02fc";       // "magenta"
static const char * const COLOR_TYPE = "#fca604";         // "orange"
static const char * const COLOR_TERMINAL = "#0402fc";     // "blue"
static const char * const COLOR_ALGFCT = "#14fe14";       // "green"
static const char * const COLOR_NONTERMINAL = "#0c0a0c";  // "black";
static const char * const COLOR_BLOCK = "#c4c2c4";        // "gray";
static const char * const COLOR_EVALFCT = "#a42af4";      // "purple"


#endif /* SRC_PLOT_GRAMMAR_HH_ */
