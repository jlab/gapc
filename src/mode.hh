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

#ifndef MODE_HH
#define MODE_HH


#include <string>
#include <ostream>

#include "hashtable.hh"
#include "yieldsize.hh"


class Mode {
	
	public:
		
		enum Type { NONE, SYNOPTIC, PRETTY, CLASSIFY, SCORING, KSCORING };
		enum Number { ZERO, ONE, MANY };
		
		static const Number map_type_to_number[];
		struct pair { const char *a; Type b; };
		static const pair map_string_to_mode[];
		static hashtable<std::string, Type> table;
		static void init_table();
		
		Type type;
		Yield::Poly number;
		Mode() : type(NONE) {}
		//Mode (Type t) : type(t) {}
		
		void set(Type t);
		bool set(const std::string &n);
		void set(Yield::Poly p) { number = p; }
		
		bool is(Type t) {  return type == t; }
		
		
		std::ostream &put(std::ostream &s) const;
		
		bool operator==(const Mode &other) const {
			return type == other.type;
		}
		
		bool operator!=(const Mode &other) const {
			return !(*this == other);
		}
		
		bool operator==(Type t) const {
			return type == t;
		}
		bool operator!=(Type t) const {
			return !(*this == t);
		}
		
		bool operator==(Number n) const {
			return n == MANY ? number == Yield::UP : number == n;
		}
		bool operator!=(Number n) const { return !(*this == n); }
		
		
};


inline std::ostream &operator<<(std::ostream &s, const Mode &m)
{
	return m.put(s);
}


#endif
