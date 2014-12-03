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


#ifndef TABLE_HH
#define TABLE_HH

#include "yieldsize.hh"

class Table {
  public:
    enum Dim { NONE, CONSTANT, LINEAR, QUADRATIC };
    enum Sticky { NO_INDEX, LEFT, RIGHT };
  private:
    Dim dim;
    bool const_bounded;
    Sticky sticky_;
  public:
    Yield::Poly up;
  private:

    Yield::Size left_rest_;
    Yield::Size right_rest_;
  public:
    Table() : dim(NONE), const_bounded(false), sticky_(NO_INDEX)  {}

    bool bounded() const { return const_bounded; }
    void set_bounded(bool b) { const_bounded = b; }
    void set_bounded(const Yield::Poly &p) {
      if (p == Yield::UP)
        set_bounded(false);
      else
        set_bounded(true);
      up = p;
    }

    Dim type() const { return dim; }

    const Yield::Poly & high() const { return up; }

    Table &operator|=(const Dim &d) {
      if (dim < d) {
        // FIXME  should be tested in codegen
        // to generate DIAG_LINEAR tables ...
        // if (const_bounded && d == QUADRATIC)
        //  dim = LINEAR;
        //else
          dim = d;
      }
      return *this;
    }

    Table &operator|=(const Table &t) {
      *this |= t.type();
      return *this;
    }

    void set_sticky(Sticky s);
    Sticky sticky() const { return sticky_; }

    const Yield::Size & left_rest() const { return left_rest_; }
    const Yield::Size & right_rest() const { return right_rest_; }
    void set_left_rest(const Yield::Size &a) { left_rest_ /= a; }
    void set_right_rest(const Yield::Size &a) { right_rest_ /= a; }

    bool is_cyk_left() const
    {
      return dim == LINEAR && sticky_ == LEFT && left_rest_.high() == 0;
    }
    bool is_cyk_right() const
    {
      return dim == LINEAR && sticky_ == RIGHT && right_rest_.high() == 0;
    }
    bool delete_left_index() const;
    bool delete_right_index() const;

    bool is_cyk_const() const
    {
      return dim == CONSTANT &&
        left_rest_.high() == 0 && right_rest_.high() == 0;
    }

    std::ostream &put(std::ostream &s) const {
      s << '(';
      switch (dim) {
        case NONE : s << "none"; 
                    break;
        case CONSTANT : s << "const";
                    break;
        case LINEAR : s << "linear";
                    break;
        case QUADRATIC : s << "quadratic";
                    break;
      }
      s << ')';
      return s;
    }
    void print(std::ostream &s) const;
    void print_short(std::ostream &s, const std::string &name) const;

};

inline std::ostream &operator<<(std::ostream &s, const Table &p)
{
  return p.put(s);
}

#endif
