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

#ifndef INPUT_HH
#define INPUT_HH

#include "loc.hh"

#include <string>
#include <vector>
#include <list>
#include <cassert>


class Input {
	
	public:
		
		// Enum of all modes of input the compiler provides.
		// Note that the last element of the enumeration
		// MODE_END is not a mode, but an end marker which
		// has the same value as there are useful modes,
		// provided that this element is the last element
		// in the enumeration! This property is used in
		// the implementation. Please keep in mind that
		// extending the list of modes must leave the MODE_END
		// element at the end of the enumeration.
		enum Mode { RAW, RNA, UPPER, MODE_END };
		
		
	private:
		
		// 
		std::vector<Mode> modes_;
		// Array of strings that represent all allowed
		// modes.
		static const char map[][6];
		Mode str_to_mode (const std::string &s, const Loc &l);
		
		
	public:
		
		Input() {}
		
		void set (const std::string &s, const Loc &l);
		void set (const std::list<std::string*> &s, const Loc &l);
		
		bool is (Mode m) const
		{
			assert (modes_.size() == 1);
			return modes_.front() == m;
		}
		
		Mode mode() const
		{
			assert (modes_.size() == 1);
			return modes_.front();
		}
		
		const std::vector<Mode> &modes() const { return modes_; }
		size_t tracks() const { return modes_.size() ? modes_.size() : 1; }
		
		void set_tracks (size_t t);
		
		static std::string* toString (Mode m);
		
		
};


#endif

