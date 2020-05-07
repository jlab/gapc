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


#ifndef PARETO_YUKISH_HH
#define	PARETO_YUKISH_HH


#if __cplusplus >= 201103L
#define _MOVE(__val) std::move(__val)
#define _MOVE_RANGE(__it1,__it2,__in) std::move(__it1,__it2,__in)
#else
#define _MOVE(__val) (__val)
#define _MOVE_RANGE(__it1,__it2,__in) std::copy(__it1,__it2,__in)
#endif

#include "list.hh"
#include <iostream>
#include <deque>
#include <iterator>
#include <algorithm>

#include <boost/shared_ptr.hpp>


template <class T>
class y_list : public std::deque<T*> {};

template <class T>
class y_in_list : public std::deque<T> {};

template<class T, typename Iterator>
struct y_split_it {
    public:
       y_split_it() {} 
       y_split_it(int depth, bool unsplittable, Iterator begin, Iterator end) {
           this->depth = depth;
           this->begin = begin;
           this->end = end;
           this->unsplittable = unsplittable;
       }
       int depth;
       bool unsplittable;
       Iterator begin;
       Iterator end;       
 };
 
 
template<class T>
struct y_split {
    public:
       y_split() {} 
       y_split(int depth, bool unsplittable, typename y_list<T>::iterator begin, typename y_list<T>::iterator end) {
           this->depth = depth;
           this->begin = begin;
           this->end = end;
           this->unsplittable = unsplittable;
       }
       int depth;
       bool unsplittable;
       typename y_list<T>::iterator begin;
       typename y_list<T>::iterator end;       
 };

 template<class T>
 struct y_split_p {
     public:
        y_split_p() {}
        y_split_p(int depth) {
            this->depth = depth;
        }
        int depth;
        y_list<T> list;
 };

template <typename T>
class Deleter {
public:
  Deleter(T* pointer) {
	l.reset(pointer);
  }
  Deleter(const Deleter& deleter) {
    Deleter* d = const_cast<Deleter*>(&deleter);
    l = d->l;
  }

  boost::shared_ptr<T> l;

  T operator*()
  {
      return *l.get();
  }

  T* operator->(){
     return l.get();
  }
};
 
template<class T, typename Compare>
bool y_dominates(const T & c1, const T & c2, Compare &c, int dim) {
    return y_dominates(c1, c2, c, 1, dim);
}

template<class T, typename Compare>
bool y_dominates(const T & c1, const T & c2, Compare &c, int s, int dim) {
   
    for (int i=s; i<=dim; i++) {
  
        if (c(c1, c2, i) < 0) {
            return false;
        }
    }
    
    return true;
}

template<class T, typename Compare>
struct y_sorter {
    public:     
     y_sorter(int s, int dim, Compare &c) {
         this->s = s;
         this->dim = dim;
         this->c = c;
     }
     int s, dim;
     Compare c;
     
     bool operator () (T *c1, T  *c2)
     {
         for (int i=s; i<=dim; i++) {
             
             int sort = c(*c1, *c2, i);
             if (sort == 0) {
                 continue;
             }
             return sort > 0;
         }
         return false;
     }
 };
 
 
// sorts the given subset of the list, starting at dimension s
template<class T, typename Compare>
void y_sortList(typename y_list<T>::iterator begin, typename y_list<T>::iterator end, Compare &c,  int s,  int dim) {
  
  y_sorter<T, Compare> sortob = y_sorter<T, Compare>(s,dim,c);  
  std::sort(begin, end, sortob);
}


//adds y to x
template<class T>
void y_join_deque( y_list<T> &x, y_list<T> &y) {
    _MOVE_RANGE(y.begin(), y.end(), std::back_inserter(x));
}

/////-------------------------- Split Marry -------------------------------

template<class T, typename Compare>
typename y_in_list<y_split<T> >::iterator y_sortedSplitMarry_inner1(y_in_list<y_split<T> > &splits, typename y_in_list<y_split<T> >::iterator in , y_split<T> ob, Compare &c, int d, int dim, T &median) {
    
    if (ob.begin == ob.end) {
        return in;
    }
    
    typename y_list<T>::iterator mid = ob.begin;
    std::advance(mid, std::distance(ob.begin, ob.end) / 2);
    
    // move over elements having same value as mid
    typename y_list<T>::iterator store = mid;
    for(; store != ob.end && c(**store, **mid, d) == 0; store++)  {
    }
    
    // special case when all elements are the same (weird case, I know)
    // make unsplittable
    bool unsplittable = false;
    if (store == ob.end) {
        unsplittable = true;
        typename y_list<T>::iterator last =  store;
        last--;
        median = **last;
    } else {
        median = **store;
    }
    
    
    in = splits.insert(in , y_split<T>(ob.depth+1, unsplittable, store, ob.end));
    in++;
    in = splits.insert(in , y_split<T>(ob.depth+1, unsplittable, ob.begin, store));
    return in;
}

template<class T, typename Compare>
typename y_in_list<y_split<T> >::iterator y_sortedSplitMarry_inner2(y_in_list<y_split<T> > &splits, typename y_in_list<y_split<T> >::iterator in , y_split<T> ob, Compare &c, int d, int dim, const T &median) {
    
    if (ob.begin == ob.end) {
        return in;
    }
    
    typename y_list<T>::iterator store = ob.begin;
    // move over elements until median
    for(; store != ob.end && c(median, **store, d) <= 0; store++)  {
    }
    
    // special case when all elements are the same (weird case, I know)
    // make unsplittable
    bool unsplittable = false;
    if (store == ob.end) {
        unsplittable = true;
    }
    
    in = splits.insert(in , y_split<T>(ob.depth+1, unsplittable, store, ob.end));
    in++;
    in = splits.insert(in , y_split<T>(ob.depth+1, unsplittable, ob.begin, store));
    return in;
}

template<class T, typename Compare>
void y_sortDoubleSplit(y_in_list<y_split<T> > &splits_x, y_in_list<y_split<T> > &splits_y,
         y_list<T> &x , typename y_list<T>::iterator y_begin, typename y_list<T>::iterator y_end, Compare &c, int s, int dim, int blocksize) {
    
    
    // add first lists to splits
    splits_x.push_back( y_split<T>(1, false, x.begin(), x.end() ) );
    splits_y.push_back( y_split<T>(1, false, y_begin, y_end ) );
    
    bool continue_split = true;
    while (continue_split) {
          continue_split = false;
          
          typename y_in_list<y_split<T> >::iterator s_x = splits_x.begin();
          typename y_in_list<y_split<T> >::iterator s_y = splits_y.begin();
          
          for( ; s_x != splits_x.end() && s_y != splits_y.end(); s_x++, s_y++) {
              
              if ( std::distance(s_x->begin, s_x->end) > blocksize && std::distance(s_y->begin, s_y->end) > blocksize
                      && !s_x->unsplittable && !s_y->unsplittable) {
                  
                  y_split<T> ob_y = *s_y;
                  y_split<T> ob_x = *s_x;
                  
                  s_x = splits_x.erase(s_x);
                  s_y = splits_y.erase(s_y);
                  
                  T median;
                  s_x = y_sortedSplitMarry_inner1(splits_x, s_x , ob_x, c,  s, dim, median);
                  s_y = y_sortedSplitMarry_inner2(splits_y, s_y , ob_y, c,  s, dim, median);
                  continue_split = true;
              } 
          }
    }
    
}

/////-------------------------- Marry -------------------------------

template<class T, typename Compare>
bool y_marry2d_comperator(const T &c1, const T  &c2, Compare &c, int s, int dim)
{
    for (int i=s; i<=dim; i++) {

         int sort = c(c1, c2, i);
         if (sort == 0) {
             continue;
         }
         return sort > 0;
     }
     return true;
}

template<class T, typename Compare>
void y_marry2d( y_list<T> &answers ,  y_list<T> &x, typename y_list<T>::iterator y_begin, typename y_list<T>::iterator y_end , Compare &c, int s, int dim) {
    
    if(y_begin == y_end) { // handle empty case, nothing to dominate
        y_join_deque(answers, x);
        return;
    }
    
    typename y_list<T>::iterator s_x = x.begin();
    typename y_list<T>::iterator s_y = y_begin;
    typename y_list<T>::iterator ref = y_begin;
    
    // first get all x bigger than first y
    while(s_x != x.end()) {
        if (c(**s_x, **s_y, s) <= 0 ) {
            break;
        }
       
        answers.push_back(_MOVE(*s_x));
        s_x++;
    }
    
    // now first s_y is always reference point :)
    while(s_x != x.end()) {
        
        typename y_list<T>::iterator snext_y = s_y;
        snext_y++;
        
        // test if next y or next x
        if( snext_y != y_end && y_marry2d_comperator(**snext_y, **s_x, c, s, dim) ) {
           // add y, because y better
            s_y++;
            
            if( c(**s_y, **ref, dim) >= 0) {
               ref = s_y;
           } 
        } else {
           // x is next
           if( c(**s_x, **ref, dim) > 0) {
               answers.push_back(_MOVE(*s_x));
           } 
           s_x++; 
        }
    }
    
}

template<class T, typename Compare>
void y_marryBrute(y_list<T>  &answers, const y_split<T> &x, const y_split<T> &y, Compare &c, int s, int dim) {

    for(typename y_list<T>::iterator el_x = x.begin; el_x != x.end; ++el_x) {
   
        bool add = true;
        for(typename y_list<T>::iterator el_y = y.begin; el_y != y.end; ++el_y) {

          if (y_dominates( **el_y, **el_x , c, s, dim) ) {
               add = false;
               break;
           }
        }

        if (add) {
             answers.push_back(_MOVE(*el_x));
        } 
    }
    
}


template<class T, typename Compare>
void y_marry(y_list<T> &answers , y_list<T> &x, typename y_list<T>::iterator y_begin, typename y_list<T>::iterator y_end , Compare &c, int s, int dim, int blocksize) {
    
    // x and y need to be sorted for all cases
    y_sortList<T, Compare>(x.begin(), x.end(), c, s, dim);
    y_sortList<T, Compare>(y_begin, y_end, c, s, dim);
    
    if (dim - s == 1) {
        y_marry2d(answers, x, y_begin, y_end, c, s, dim);
        return;
    }
    
    // flattened trees to store splits
    y_in_list<y_split<T> > raw_splits_x;
    y_in_list<y_split<T> > raw_splits_y;
    y_sortDoubleSplit(raw_splits_x, raw_splits_y, x, y_begin, y_end, c, s, dim, blocksize);
    
    // apply brute solving to all on base level
    typename y_in_list<y_split<T> >::iterator s_x = raw_splits_x.begin();
    typename y_in_list<y_split<T> >::iterator s_y = raw_splits_y.begin();
    
    y_in_list<Deleter<y_split_p<T> > >  splits_x;
    
    for(; s_x != raw_splits_x.end() && s_y != raw_splits_y.end(); ++s_x, ++s_y) {
        
        y_split_p<T> * tmp = new y_split_p<T>(s_x->depth);
        y_marryBrute(tmp->list, *s_x, *s_y, c, s, dim);
        
        splits_x.push_back(tmp);
    }
        
    // join up bottom up, x can bee seen as already married
    while(splits_x.size() > 1) {
        
        typename y_in_list<Deleter<y_split_p<T> > > ::iterator s_x = splits_x.begin();
        typename y_in_list<y_split<T> >::iterator s_y = raw_splits_y.begin();
        
        for(; s_x != splits_x.end() && s_y != raw_splits_y.end(); ++s_x, ++s_y) { 
              
             typename y_in_list<Deleter<y_split_p<T> > > ::iterator snext_x = s_x;
             snext_x++;
             typename y_in_list<y_split<T> >::iterator snext_y = s_y;
             snext_y++;
            
                  
            if (snext_x == splits_x.end() || snext_y == raw_splits_y.end()) {
                break;
            }
            
            if((*s_x)->depth == (*snext_x)->depth) { 
             
                y_split_p<T> * tmp = new y_split_p<T>((*s_x)->depth-1);
                
                y_marry(tmp->list, (*s_x)->list, snext_y->begin, snext_y->end, c, s+1, dim, blocksize);
                y_join_deque(tmp->list, (*snext_x)->list);
                
                s_x = splits_x.erase(s_x,s_x+2);
                s_x = splits_x.insert(s_x,tmp);
                
                y_split<T> ytmp = *s_y;
                y_split<T> ynexttmp = *snext_y;
                        
                s_y = raw_splits_y.erase(s_y,s_y+2);
                s_y = raw_splits_y.insert(s_y, y_split<T>((*s_x)->depth, false, ynexttmp.begin, ytmp.end) );
                
                break;
            }
        }
        
    }

    y_join_deque(answers, splits_x.front()->list);
}

template<class T, typename Compare>
void y_marry(y_list<T> &answers , y_list<T> &x, y_list<T> &y, Compare &c, int s, int dim, int blocksize) {
    y_marry(answers, x, y.begin(), y.end(), c, s, dim, blocksize);
}

/////-------------------------- Split DC -------------------------------

template<class T, typename Iterator, typename Compare>
typename y_in_list<y_split_it<T, Iterator> >::iterator y_sortedSplit_inner( y_in_list<y_split_it<T, Iterator> > &splits, typename y_in_list<y_split_it<T, Iterator> >::iterator in ,y_split_it<T, Iterator> ob, Compare &c, int d, int dim) {
    
    if (ob.begin == ob.end) {
        return in;
    }
    
    Iterator mid = ob.begin;
    std::advance(mid, std::distance(ob.begin, ob.end) / 2);
    
    // move over elements having same value as mid
    Iterator store = mid;
    for(; store != ob.end && c(*store, *mid, d) == 0; ++store)  {
    }
    
    // special case when all elements are the same (weird case, I know)
    // make unsplittable
    bool unsplittable = false;
    if (store == ob.end) {
        unsplittable = true;
    }
   
    in = splits.insert(in , y_split_it<T, Iterator>(ob.depth+1, unsplittable, store, ob.end));
    in++;
    in = splits.insert(in , y_split_it<T, Iterator>(ob.depth+1, unsplittable, ob.begin, store));
    return in;
}


template<class T, typename Iterator, typename Compare>
void y_sortedSplit( y_in_list<y_split_it<T, Iterator> > &splits, Iterator begin, Iterator end, Compare &c, int blocksize, int dim) {
    
    // add first list to split
    splits.push_back( y_split_it<T, Iterator>(1, false, begin, end) );
    
    bool continue_split = true;
    while (continue_split) {
          continue_split = false;
          
          for(typename y_in_list<y_split_it<T, Iterator> >::iterator s = splits.begin()
                  ; s != splits.end(); s++) {
              
              if ( std::distance((*s).begin, (*s).end) > blocksize && !(*s).unsplittable) {
                  
                  y_split_it<T, Iterator> ob = *s;
                  s = splits.erase(s);

                  s = y_sortedSplit_inner(splits, s, ob, c,  1, dim);
                  continue_split = true;
              } 
          }
    }
    
}


/////-------------------------- Brute Solve SC -------------------------------

template<class T, typename Iterator, typename Compare>
void y_bruteSolveSC(y_list<T> &answers, Iterator begin, Iterator end, Compare &c, int dim) {
     
    // n^2 adding
   Iterator ref = begin;
   if (ref != end) { 
        answers.push_back(&(*ref));
        ++ref;
   }
   for(; ref != end; ++ref) {
       bool add = true;
    
       for( typename y_list<T>::iterator ans = answers.begin(); ans != answers.end();ans++) {

           if (y_dominates( **ans, *ref , c, 2, dim)) {
               add = false;
               break;
           }
       }
      
       if (add) {
            answers.push_back(&(*ref));
       } 
   }
}

/////--------------------------  Solve DC -------------------------------


template<class T, typename Iterator, typename Compare>
 void y_recSolveDC(y_list<T> &answers ,Iterator begin, Iterator end, Compare &c, int dim, int blocksize) {
    
    // split up the problem until blocksize is reached
    
    // flattened tree to store splits
    y_in_list<y_split_it<T, Iterator> > raw_splits;
    y_sortedSplit(raw_splits, begin, end, c, blocksize, dim);

    y_in_list<Deleter<y_split_p<T> > >  splits;
    // apply brute solving to all
    for(typename y_in_list<y_split_it<T, Iterator> >::iterator s = raw_splits.begin()
                  ; s != raw_splits.end(); ++s) {
        
        y_split_p<T>* tmp = new y_split_p<T>(s->depth);
        y_bruteSolveSC(tmp->list ,s->begin, s->end, c, dim);
        splits.push_back(tmp);
    }
    
    while(splits.size() > 1) {
             
            for(typename y_in_list<Deleter<y_split_p<T> > >::iterator s = splits.begin()
                  ; s != splits.end(); s++) {
                  
                  typename y_in_list<Deleter<y_split_p<T> > >::iterator snext = s;
                  snext++;
                  
                  if (snext == splits.end()) {
                      break;
                  }
                  
                  // is the level matching?
                  if((*s)->depth == (*snext)->depth) {
                      
                      y_split_p<T>* tmp = new y_split_p<T>((*s)->depth-1);
                      y_marry(tmp->list, (*s)->list, (*snext)->list, c, 2, dim, blocksize);
                      
                      y_join_deque(tmp->list, (*snext)->list);

                      s = splits.erase(s,s+2);

                      s = splits.insert(s,tmp);
                      break;
                  } 
            }
    }

    answers.insert(answers.end(), splits.front()->list.begin(), splits.front()->list.end());
}

/////-------------------------- Main -------------------------------

template<class T, typename Iterator, typename Compare>
void pareto_yukish(List_Ref<T> &ret_answers, Iterator begin, Iterator end, Compare &c, int dim, int blocksize)
{
    
   // solve it
   y_list<T> answers;
   y_recSolveDC(answers, begin, end, c, dim, blocksize);


   //   create response list
   for( typename y_list<T>::iterator ans = answers.begin(); ans != answers.end();++ans) {
       push_back( ret_answers, **ans);
   }
   
}

#endif	/* PARETO_YUKISH_HH */

