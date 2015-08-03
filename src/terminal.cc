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


#include "terminal.hh"

#include "loc.hh"
#include "symbol.hh"
#include "type.hh"
#include "yieldsize.hh"


void Terminal::add_predefined(Grammar &grammar)
{
	Loc l;
	std::string *s = new std::string("CHAR");
	Symbol::Terminal *t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(1, 1);
	t->set_data_type(new Type::Char());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	s = new std::string("CHAR_SEP");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(1, 1);
	t->set_data_type(new Type::Char());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	s = new std::string("INT");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(1,Yield::UP);
	t->set_data_type(new Type::Int());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	s = new std::string("STRING");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(0,Yield::UP);
	t->set_data_type(new Type::String());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	s = new std::string("ROPE");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(1,Yield::UP);
	t->set_data_type(new Type::External("Rope"));
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	s = new std::string("ROPE0");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(0,Yield::UP);
	t->set_data_type(new Type::External("Rope"));
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	s = new std::string("REGION");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(1,Yield::UP);
	// FIXME use Seq as parameter
	t->set_data_type(new Type::Subseq());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	// deprecated
	s = new std::string("UREGION");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(0,Yield::UP);
	t->set_data_type(new Type::Subseq());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	s = new std::string("REGION0");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(0,Yield::UP);
	t->set_data_type(new Type::Subseq());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	s = new std::string("BASE");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(1,1);
	t->set_data_type(new Type::Subseq());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	s = new std::string("LOC");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(0,0);
	t->set_data_type(new Type::Subseq());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	// FIXME return type bool
	s = new std::string("EMPTY");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(0,0);
	t->set_data_type(new Type::Void());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	s = new std::string("SEQ");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(0,Yield::UP);
	t->set_data_type(new Type::Int());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	s = new std::string("SEQ1");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(1,Yield::UP);
	t->set_data_type(new Type::Int());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
	
	// not a number terminal parser
	s = new std::string("NON");
	t = new Symbol::Terminal(s, l);
	t->writeable_ys().set(1,1);
	t->set_data_type(new Type::Char());
	t->setPredefinedTerminalParser (true);
	grammar.NTs[*(t->name)] = t;
}

