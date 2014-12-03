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


#include "list_visitor.hh"

#include "symbol.hh"
#include "alt.hh"
#include "fn_arg.hh"

#include <iostream>


void List_Visitor::visit(Symbol::Terminal &s)
{
  std::cerr << "#" << *s.name << " [" << s.list_size() << "]"
    << std::endl << std::endl;
}

void List_Visitor::visit(Symbol::NT &n)
{
  std::cerr << std::endl << std::endl;
  std::cerr << *n.name << " [" << n.list_size() << "] = ";
}

void List_Visitor::visit_itr(Symbol::NT &n)
{
  std::cerr << " | ";
}

void List_Visitor::visit_begin(Alt::Simple &a)
{
  std::cerr << "[" << a.list_size() << "]" << *a.name << "(";
}

void List_Visitor::visit_end(Alt::Simple &a)
{
  std::cerr << ")";
}

void List_Visitor::visit_itr(Alt::Simple &a)
{
  std::cerr << ", ";
}

void List_Visitor::visit(Alt::Link &a)
{
  std::cerr << "<" << a.list_size() << " " << *a.nt->name << ">";
}

void List_Visitor::visit_begin(Alt::Block &a)
{
  std::cerr << "[" << a.list_size() << "] { ";
}

void List_Visitor::visit_itr(Alt::Block &a)
{
  std::cerr << " | ";
}

void List_Visitor::visit_end(Alt::Block &a)
{
  std::cerr << " }";
}

void List_Visitor::visit(Fn_Arg::Const &f)
{
  std::cerr << "|" << f.list_size() << "|";
}

void List_Visitor::visit(Fn_Arg::Alt &f)
{
  std::cerr << "|" << f.list_size() << "|";
}


void List_Visitor::visit(Alt::Multi &a)
{
  std::cerr << "[" << a.list_size() << "] < ";
}

void List_Visitor::visit_itr(Alt::Multi &a)
{
  std::cerr << " , ";
}

void List_Visitor::visit_end(Alt::Multi &a)
{
  std::cerr << " >";
}



