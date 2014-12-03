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

#ifndef TYPE_BACKTRACE_HH
#define TYPE_BACKTRACE_HH

#include "base.hh"

#include "../bool.hh"

namespace Type {

  class Backtrace : public Base {
    private:
      std::string *name_;
      ::Type::Base *pos_type_;
      ::Type::Base *value_type_;
      ::Type::Base *score_type_;
    public:
      enum Derive { NONE, FN, FN_USE, NT, FN_SPEC,
                    NT_BACKEND, NT_FRONTEND };
    private:
      Derive d;
      ::Bool body_context_;
    public:
      MAKE_CLONE(Backtrace);
      Backtrace()
        : Base(BACKTRACE), name_(0), pos_type_(0), value_type_(0),
          score_type_(0), d(NONE)
      {
      }
      Backtrace(std::string *n)
        : Base(BACKTRACE), name_(n), pos_type_(0), value_type_(0),
          score_type_(0), d(FN)
      {
      }
      Backtrace(::Type::Base *v, ::Type::Base *p, std::string *n)
        : Base(BACKTRACE), name_(n), pos_type_(p), value_type_(v),
          score_type_(0), d(FN_USE)
      {
      }
      Backtrace(std::string *n, ::Type::Base *p, ::Type::Base *v)
        : Base(BACKTRACE), name_(n), pos_type_(p), value_type_(v),
          score_type_(0), d(NT)
      {
      }

      Backtrace(::Type::Base *p, ::Type::Base *v)
        : Base(BACKTRACE), name_(0), pos_type_(p), value_type_(v),
          score_type_(0), d(FN_SPEC)
      {
      }

      Backtrace(std::string *n, ::Type::Base *p, ::Type::Base *v,
          ::Type::Base *s)
        : Base(BACKTRACE), name_(n), pos_type_(p), value_type_(v),
          score_type_(s),
          d(NT_BACKEND)
      {
      }

      Backtrace(Backtrace *bt)
        : Base(BACKTRACE), name_(bt->name_), pos_type_(bt->pos_type_),
          value_type_(bt->value_type_), score_type_(bt->score_type_),
          d(NT_FRONTEND)
      {
      }

      std::ostream & put(std::ostream &s) const;
      void print(Printer::Base &s) const;

      Derive subclass() const { return d; }
      const std::string *name() const { return name_; }
      const ::Type::Base *pos_type() const { return pos_type_; }
      const ::Type::Base *value_type() const { return value_type_; }

      Base *component();

      void set_body_context() { body_context_ = true; }

      bool body_context() const { return body_context_; }

  };

  class Backtrace_List : public Base {
    private:
    public:
      MAKE_CLONE(Backtrace_List);
      Backtrace_List()
        : Base(BACKTRACE_LIST)
      {
      }

      std::ostream & put(std::ostream &s) const;
      void print(Printer::Base &s) const;
  };

  class Eval_List : public Base {
    private:
    public:
      MAKE_CLONE(Eval_List);
      ::Type::Base *of;
      Eval_List()
        : Base(EVAL_LIST), of(0)
      {
      }

      std::ostream & put(std::ostream &s) const;
      void print(Printer::Base &s) const;
  };
}

#endif
