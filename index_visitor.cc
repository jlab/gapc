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


#include "index_visitor.hh"

#include "symbol.hh"
#include "alt.hh"
#include "fn_arg.hh"
#include "expr.hh"

#include <iostream>


void Index_Visitor::visit(Symbol::Terminal &s)
{
}

void Index_Visitor::visit(Symbol::NT &n)
{
  std::cerr << std::endl << std::endl;

  std::cerr << *n.name << " [ < ";

  assert(n.left_indices.size() == n.right_indices.size());
  assert(!n.left_indices.empty());
  std::vector<Expr::Base*>::iterator l = n.left_indices.begin();
  std::vector<Expr::Base*>::iterator r = n.right_indices.begin();
  if (!*l)
    std::cerr << "(NULL, NULL)";
  else
    std::cerr << "(" << **l  << ", " << **r << ")";
  ++l;
  ++r;
  for (; l != n.left_indices.end(); ++l, ++r)
    if (!*l)
      std::cerr << "(NULL, NULL)";
    else
      std::cerr << ", (" << **l  << ", " << **r << ")";

  std::cerr << " > ] = ";
}

void Index_Visitor::visit_end(Symbol::NT &n)
{
  std::cerr << std::endl;
}

void Index_Visitor::visit_itr(Symbol::NT &n)
{
  std::cerr << " | \n\t";
}

void Index_Visitor::visit_begin(Alt::Simple &a)
{
  a.put_indices(std::cerr);
}

void Index_Visitor::visit(Alt::Link &a)
{
  //std::cerr << "<" << a.list_size() << " " << *a.nt->name << ">";
}

void Index_Visitor::visit_begin(Alt::Block &a)
{
  std::cerr << " { ";
}

void Index_Visitor::visit_itr(Alt::Block &a)
{
  std::cerr << " | \n\t";
}

void Index_Visitor::visit_end(Alt::Block &a)
{
  std::cerr << " }";
}

void Index_Visitor::visit(Fn_Arg::Const &f)
{
  //std::cerr << "|" << f.list_size() << "|";
}

void Index_Visitor::visit(Fn_Arg::Alt &f)
{
  //std::cerr << "|" << f.list_size() << "|";
}
