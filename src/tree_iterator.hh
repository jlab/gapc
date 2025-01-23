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


#ifndef SRC_TREE_ITERATOR_HH_
#define SRC_TREE_ITERATOR_HH_

#include <stack>
#include <cassert>


namespace Tree {

template <typename S, class T> inline S *left(T &t) {
  return t.left();
}

template <typename S, class T> inline S *right(T &t) {
  return t.right();
}

template <class Base, class Two>
class Iterator {
 private:
    std::stack<Base*> stack;

 public:
    explicit Iterator(Base *expr) {
      stack.push(expr);
    }

    Iterator() {
    }

    Iterator<Base, Two> &operator++() {
      assert(!stack.empty());
      Base *expr = stack.top();
      stack.pop();
      if (Two *t = dynamic_cast<Two*>(expr)) {
        stack.push(right<Base>(*t));
        stack.push(left<Base>(*t));
      }
      return *this;
    }

    Base *operator*() {
      return stack.top();
    }

    bool operator==(const Iterator<Base, Two> &itr) const {
      assert(itr.stack.empty());
      return stack.empty();
    }

    bool operator!=(const Iterator<Base, Two> &itr) const {
      return !(*this == itr);
    }
};

}  // namespace Tree

#endif  // SRC_TREE_ITERATOR_HH_
