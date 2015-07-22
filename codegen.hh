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

#ifndef CODEGEN_HH
#define CODEGEN_HH

#include "type_fwd.hh"
#include "bool.hh"

#include <ostream>
#include <cassert>

class AST;

namespace Code {


  class Mode {
    public:
      enum Type { UNGER, CYK };
      enum Bt_Type { FORWARD, BACKTRACK, SUBOPT };
    private:
      Type type;
      Bt_Type bt_type;

      bool allow_cooptimal_;
      bool keep_equal_; // used for ADP specialization
      bool kscoring_;

      Bool subopt_buddy_;
      Bool marker_;

      Bool sample_;
    public:
      Mode()
        : type(UNGER), bt_type(FORWARD),
          allow_cooptimal_(true), keep_equal_(true),
          kscoring_(false)
      {
      }
      Mode(Type t, Bt_Type b)
        : type(t), bt_type(b),
          allow_cooptimal_(true), keep_equal_(true),
          kscoring_(false)
      {
      }
      void operator=(Type t) { type = t; }
      //Type operator()() const { return type; }
      bool operator==(Type t) const { return type == t; }
      bool operator!=(Type t) const { return !(*this == t); }

      void operator=(Bt_Type t) { bt_type = t; }
      bool operator==(Bt_Type t) const { return bt_type == t; }
      bool operator!=(Bt_Type t) const { return !(*this == t); }

      void set_cooptimal(bool t) { allow_cooptimal_ = t; }
      bool cooptimal() const { return allow_cooptimal_; }

      void set_keep_cooptimal(bool t) { keep_equal_ = t; }
      bool keep_cooptimal() const { return keep_equal_; }
      
      void set_kscoring(bool t) { kscoring_ = t; }
      bool kscoring() const { return kscoring_; }

      void put(std::ostream &o) const
      {
        o << "Type: ";
        switch (type) {
          case UNGER : o << "unger"; break;
          case CYK : o << "cyk"; break;
        }
        o << " Bt_Type: ";
        switch (bt_type) {
          case FORWARD : o << "forward"; break;
          case BACKTRACK : o << "backtrack"; break;
          case SUBOPT : o << "subopt"; break;
        }
        o << '\n';
      }

      bool subopt_buddy() const { return subopt_buddy_; }
      void set_subopt_buddy() { subopt_buddy_ = true; }

      bool marker() const { return marker_; }
      void set_marker() { marker_ = true; }

      bool sample() const { return sample_; }
      void set_sample(bool b) { sample_ = b; }
  };

  inline std::ostream &operator<<(std::ostream &o, const Mode &m)
  {
    m.put(o);
    return o;
  }

  class Gen {
    private:
      Type::Base *return_type_;
    public:
      Gen() : return_type_(0) { }
      Gen(AST &ast);
      const Type::Base *return_type() const
      {
        assert(return_type_); return return_type_;
      }
  };

}

#endif
