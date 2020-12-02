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

#include <iostream>
#include "list_visitor.hh"

#include "symbol.hh"
#include "alt.hh"
#include "fn_arg.hh"


void List_Visitor::visit(Symbol::Terminal &s) {
  std::cout << "\n called visit(Symbol::Terminal &s) of class list_visitor \n";
  std::cerr << "#" << *s.name << " [" << s.list_size() << "]"
    << std::endl << std::endl;
}

void List_Visitor::visit(Symbol::NT &n) {
  std::cout << "\n called visit(Symbol::NT &n) of class list_visitor \n";
  std::cerr << std::endl << std::endl;
  std::cerr << *n.name << " [" << n.list_size() << "] = ";
}

void List_Visitor::visit_itr(Symbol::NT &n) {
  std::cout << "\n called visit_itr(Symbol::NT &n of class list_visitor \n";
  std::cerr << " | ";
}

void List_Visitor::visit_begin(Alt::Simple &a) {
  std::cout << "\n called visit_begin(Alt::Simple &a) of class list_visitor \n";
  std::cerr << "[" << a.list_size() << "]" << *a.name << "(";
}

void List_Visitor::visit_end(Alt::Simple &a) {
  std::cout << "\n called visit_end(Alt::Simple &a) of class list_visitor \n";
  std::cerr << ")";
}

void List_Visitor::visit_itr(Alt::Simple &a) {
  std::cout << "\n called visit_itr(Alt::Simple &a) of class list_visitor \n";
  std::cerr << ", ";
}

void List_Visitor::visit(Alt::Link &a) {
  std::cout << "\n called visit(Alt::Link &a) of class list_visitor \n";
  std::cerr << "<" << a.list_size() << " " << *a.nt->name << ">";
}

void List_Visitor::visit_begin(Alt::Block &a) {
  std::cout << "\n called visit_begin(Alt::Block &a) of class list_visitor \n";
  std::cerr << "[" << a.list_size() << "] { ";
}

void List_Visitor::visit_itr(Alt::Block &a) {
  std::cout << "\n called visit_itr(Alt::Block &a) of class list_visitor \n";
  std::cerr << " | ";
}

void List_Visitor::visit_end(Alt::Block &a) {
  std::cout << "\n called visit_end(Alt::Block &a) of class list_visitor \n";
  std::cerr << " }";
}

void List_Visitor::visit(Fn_Arg::Const &f) {
  std::cout << "\n called visit(Fn_Arg::Const &f) of class list_warn \n";
  std::cerr << "|" << f.list_size() << "|";
}

void List_Visitor::visit(Fn_Arg::Alt &f) {
  std::cout << "\n called visit(Fn_Arg::Alt &f) of class list_warn \n";
  std::cerr << "|" << f.list_size() << "|";
}


void List_Visitor::visit(Alt::Multi &a) {
  std::cout << "\n called visit(Alt::Multi &a) of class list_warn \n";
  std::cerr << "[" << a.list_size() << "] < ";
}

void List_Visitor::visit_itr(Alt::Multi &a) {
  std::cout << "\n called visit_itr(Alt::Multi &a) of class list_warn \n";
  std::cerr << " , ";
}

void List_Visitor::visit_end(Alt::Multi &a) {
  std::cout << "\n called visit_end(Alt::Multi &a) of class list_warn \n";
  std::cerr << " >";
}
