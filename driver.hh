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


#ifndef DRIVER_HH
#define DRIVER_HH

#include <string>
#include <vector>
#include <cstdio>

#include "location.hh"

#include "ast.hh"


class Driver {
	
	private:
	
		bool from_stdin;
		bool trace_lexer;
		bool trace_parser;
		bool fail_later;
		
		std::string *filename_;
		
		std::vector<std::string> includes;
		std::vector<std::FILE*> open_files;
		
		bool lexer_prepare(void);
		
		void file_close();
		bool file_open();
	
	
	public:
	
		bool parse();
		void parse_product(const std::string &s);
		void setStdin(bool b);
		
		void setFilename(const std::string &s);
		std::string *filename();
		
		bool is_failing() { return fail_later; }
		
		void error(const std::string &m);
		void error(const Loc & l, const std::string& m);
		
		void set_includes(const std::vector<std::string> &v);
		void push_buffer(const std::string &s);
		
		
		AST ast;
		
		Driver();
	
};


#endif
