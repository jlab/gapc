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


#ifndef VISITOR_HH
#define VISITOR_HH

#include "symbol_fwd.hh"
#include "alt_fwd.hh"
#include "fn_arg_fwd.hh"

class Visitor {

  public:
    virtual ~Visitor();

    virtual void visit(Symbol::Terminal &s);

    virtual void visit(Symbol::NT &n);
    virtual void visit_itr(Symbol::NT &n);
    virtual void visit_end(Symbol::NT &n);

    virtual void visit(Alt::Base &b);

    virtual void visit_begin(Alt::Simple &a);
    virtual void visit_end(Alt::Simple &a);
    virtual void visit_itr(Alt::Simple &a);
    virtual void visit(Alt::Link &a);
    virtual void visit_begin(Alt::Block &a);
    virtual void visit_itr(Alt::Block &a);
    virtual void visit_end(Alt::Block &a);
    virtual void visit(Alt::Multi &a);
    virtual void visit_itr(Alt::Multi &a);
    virtual void visit_end(Alt::Multi &a);

    virtual void visit(Fn_Arg::Base &f);

    virtual void visit(Fn_Arg::Const &f);
    virtual void visit(Fn_Arg::Alt &f);

};

#endif
