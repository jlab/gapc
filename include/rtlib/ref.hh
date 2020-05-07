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

#include <boost/shared_ptr.hpp>

namespace Ref {

  template<class T>
    class Lazy 
    {
      public:
      
      boost::shared_ptr<T> l;
      
      protected:

        void lazy()
        {
          if (!l.use_count()) l.reset(new T());
        }
        
        void copy(const Lazy &r)
        {
          l = r.l;
        }
      public:
          
        typedef T Type;

        typedef typename T::iterator iterator;

        Lazy()
        {
        }

        Lazy(const Lazy &r)
        {
          copy(r);
        }

        ~Lazy()
        {
          
        }

        Lazy &operator=(const Lazy &r)
        {
          copy(r);
          return *this;
        }

        T *operator->() { lazy(); return l.get(); }
        
        T &ref() { lazy(); return *l.get(); }
        

        const T &const_ref() const { assert(l.use_count());  return *l.get(); } 
    };

}

#endif
