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

#ifndef REF_HH
#define REF_HH


namespace Ref {

  template<class T>
    class Lazy 
    {
      public:
        T *l;
      protected:
        void del()
        {
          if (!l)
            return;
          assert(l->ref_count);
          l->ref_count--;
          if (!l->ref_count)
            delete l;
        }
        void lazy()
        {
          if (!l)
            l = new T();
        }
        void copy(const Lazy &r)
        {
          l = r.l;
          if (l)
            l->ref_count ++;
        }
      public:
        typedef T Type;

        typedef typename T::iterator iterator;

        Lazy()
          : l(0)
        {
        }

        Lazy(const Lazy &r)
        {
          copy(r);
        }

        ~Lazy()
        {
          del();
        }

        Lazy &operator=(const Lazy &r)
        {
          del();
          copy(r);
          return *this;
        }

        T *operator->() { lazy(); return l; }
        T &ref() { lazy(); return *l; }
        //const List<T, pos_int> &ref() const { assert(l);  return *l; } 
        const T &const_ref() const { assert(l);  return *l; } 
    };

}

#endif
