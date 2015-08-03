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


#ifndef TERMINAL_HH
#define TERMINAL_HH


class Grammar;


/*
 * The Terminal namespace defines a method which adds all
 * predefined terminal parsers to the list of defined NTs
 * in the class Grammar. It is called from within the
 * constructor of that class.
 */
namespace Terminal {
	
	
	// Adds all predefined non-terminals to the list of non-terminals
	// of the given grammar instance.
	void add_predefined(Grammar &grammar);
	
	
}


#endif
