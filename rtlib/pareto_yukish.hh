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


#include "list.hh"
#include <iostream>
#include <deque>
#include <iterator>
#include <algorithm>

#include <boost/shared_ptr.hpp>

template<class T>
struct y_split {
    public:
       y_split() {} 
       y_split(int depth, bool unsplittable, typename std::deque<T*>::iterator begin, typename std::deque<T*>::iterator end) {
           this->depth = depth;
           this->begin = begin;
           this->end = end;
           this->unsplittable = unsplittable;
       }
       int depth;
       bool unsplittable;
       typename std::deque<T*>::iterator begin;
       typename std::deque<T*>::iterator end;       
 };

 template<class T>
 struct y_split_p {
     public:
        y_split_p() {}
        y_split_p(int depth) {
            this->depth = depth;
        }
        int depth;
        std::deque<T*> list;
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

template<class T>
bool y_dominates(T c1, T c2, int (*c)(T, T, int), int dim) {
    return y_dominates(c1, c2, c, 1, dim);
}

template<class T>
bool y_dominates(T c1, T c2, int (*c)(T, T, int), int s, int dim) {

    bool dominates = true;    
    for (int i=1; i<=dim; i++) {
        
        if (c(c1, c2, i) < 0) {
            dominates = false;
        }
    }
    
    return dominates;
}

template<class T>
struct y_sorter {
    public:     
     y_sorter(int s, int dim, int (*c)(T, T, int)) {
         this->s = s;
         this->dim = dim;
         this->c = c;
     }
     int s, dim;
     int (*c)(T, T, int);
     
     bool operator () (const T *c1, const T  *c2)
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
template<class T>
void y_sortList(typename std::deque<T*>::iterator begin, typename std::deque<T*>::iterator end, int (*c)(T, T, int),  int s,  int dim) {
  
  y_sorter<T> sortob = y_sorter<T>(s,dim,c);  
  std::sort(begin, end, sortob);
}

//adds y to x
template<class T>
void y_join_deque(std::deque<T*> &x, std::deque<T*> &y) {
    
    for(typename std::deque<T*>::iterator el = y.begin() ; el !=y.end(); el++ ) {
        x.push_back(*el);
    }
    
}

/////-------------------------- Splitting -------------------------------

// median of medians algorithm, compare at dimension d
template<class T>
T* y_medianOfMedians(typename std::deque<T*>::iterator begin, typename std::deque<T*>::iterator end, int (*c)(T, T, int), int d) {
   
    std::deque<T*> *oldlist = NULL;
    
    while(true) {
        
        std::deque<T*> *medList = new std::deque<T*>();
        
        typename std::deque<T*>::iterator el = begin;
        while(el != end) {

           if(std::distance(el, end) < 5) {
               y_sortList(el, end, c, d, d);
               std::advance(el, std::distance(el, end) / 2);
               medList->push_back(*el);

               break;
           } else {
               typename std::deque<T*>::iterator s = el;
               typename std::deque<T*>::iterator mid = el;
               std::advance(mid, 2);
               std::advance(el, 5);
               y_sortList(s, el, c, d, d);
               medList->push_back(*mid);
           }
        }
        
        if (medList->size() <= 5) {
            
            y_sortList(medList->begin(), medList->end(), c, d, d);
            T* m = medList->at(medList->size()/2);
            
            if (oldlist) {
                 delete oldlist;
            }
            delete medList;
            
            return m ;
        } 
        
        if (oldlist) {
            delete oldlist;
        }
        oldlist = medList;
        begin = medList->begin();
        end = medList->end();
    }
}



template<class T>
typename std::deque<y_split<T> >::iterator y_medianOfMedianSplit( std::deque<y_split<T> > &splits, typename std::deque<y_split<T> >::iterator in ,y_split<T> ob, int (*c)(T, T, int), int d, int dim) {
    
    if (ob.begin == ob.end) {
        return in;
    }
    
    T* median = y_medianOfMedians(ob.begin, ob.end, c, d);

    typename std::deque<T*>::iterator store = ob.begin;
  
    for(typename std::deque<T*>::iterator s = ob.begin; s != ob.end; s++)  {
        
        if ( c(**s, *median, d) <= 0 ) {
            std::swap(*s, *store);
            store++;
        }
        
    }
    
    // special case when all elements are the same (weird case, I know)
    // make unsplittable
    bool unsplittable = false;
    if (store == ob.end) {
        unsplittable = true;
    }
    
    in = splits.insert(in , y_split<T>(ob.depth+1, unsplittable, ob.begin, store));
    in++;
    in = splits.insert(in , y_split<T>(ob.depth+1, unsplittable, store, ob.end));
    return in;
}

template<class T>
typename std::deque<y_split<T> >::iterator y_medianMarrySplit(std::deque<y_split<T> > &splits, typename std::deque<y_split<T> >::iterator in ,y_split<T> ob, int (*c)(T, T, int), int d, int dim, T* median) {
    
    typename std::deque<T*>::iterator store = ob.begin;
  
    for(typename std::deque<T*>::iterator s = ob.begin; s != ob.end; s++)  {
        
       
        if ( c(**s, *median, d) <= 0 ) {
            std::swap(*s, *store);
            store++;
        }
    }
    
    // special case when all elements are the same (weird case, I know)
    // make unsplittable
    bool unsplittable = false;
    if (store == ob.end) {
        unsplittable = true;
    }
    
    in = splits.insert(in , y_split<T>(ob.depth+1, unsplittable, ob.begin, store));
    in++;
    in = splits.insert(in , y_split<T>(ob.depth+1, unsplittable, store, ob.end));
    return in;
}


template<class T>
void y_medianSplit( std::deque<y_split<T> > &splits, std::deque<T*> &references, int (*c)(T, T, int), int blocksize, int dim) {
    
    // add first list to split
    splits.push_back( y_split<T>(1, false, references.begin(), references.end() ) );
    
    bool continue_split = true;
    while (continue_split) {
          continue_split = false;
          
          for(typename std::deque<y_split<T> >::iterator s = splits.begin()
                  ; s != splits.end(); s++) {
              
              if ( std::distance((*s).begin, (*s).end) > blocksize && !(*s).unsplittable) {
                  
                  y_split<T> ob = *s;
                  s = splits.erase(s);

                  s = y_medianOfMedianSplit(splits, s, ob, c,  1, dim);
                  continue_split = true;
              } 
          }
    }
    
}

/////-------------------------- Marry Splitting -------------------------------


template<class T>
void y_medianDoubleSplit( std::deque<y_split<T> > &splits_x, std::deque<y_split<T> > &splits_y,
        std::deque<T*> &x , typename std::deque<T*>::iterator y_begin, typename std::deque<T*>::iterator y_end, int (*c)(T, T, int), int s, int dim, int blocksize) {
    
    
    // add first lists to splits
    splits_x.push_back( y_split<T>(1, false, x.begin(), x.end() ) );
    splits_y.push_back( y_split<T>(1, false, y_begin, y_end ) );
    
    bool continue_split = true;
    while (continue_split) {
          continue_split = false;
          
          typename std::deque<y_split<T> >::iterator s_x = splits_x.begin();
          typename std::deque<y_split<T> >::iterator s_y = splits_y.begin();
          
          for( ; s_x != splits_x.end() && s_y != splits_y.end(); s_x++, s_y++) {
              
              if ( std::distance(s_x->begin, s_x->end) > blocksize && std::distance(s_y->begin, s_y->end) > blocksize
                      && !s_x->unsplittable && !s_y->unsplittable) {
                  
                  y_split<T> ob_y = *s_y;
                  y_split<T> ob_x = *s_x;
                  
                  s_x = splits_x.erase(s_x);
                  s_y = splits_y.erase(s_y);
                  
                  T* median = y_medianOfMedians(ob_x.begin, ob_x.end, c, s);

                  s_x= y_medianMarrySplit(splits_x, s_x , ob_x, c,  s, dim, median);
                  s_y = y_medianMarrySplit(splits_y, s_y , ob_y, c,  s, dim, median);
                  continue_split = true;
              } 
          }
    }
    
}

/////-------------------------- Marry -------------------------------


template<class T>
void y_marryBrute(std::deque<T*> &answers, y_split<T> x, y_split<T> y, int (*c)(T, T, int), int s, int dim) {


    for(typename std::deque<T*>::iterator el_x = x.begin; el_x != x.end; ++el_x) {
   
        bool add = true;
        for(typename std::deque<T*>::iterator el_y = y.begin; el_y != y.end; ++el_y) {

          if (y_dominates( **el_y, **el_x , c, s, dim) ) {
               add = false;
               break;
           }
        }

        if (add) {
             answers.push_back(*el_x);
        } 
    }
    
}

template<class T>
bool y_marry2d_comperator( T *c1, T  *c2, int (*c)(T, T, int), int s, int dim)
{
    for (int i=s; i<=dim; i++) {

         int sort = c(*c1, *c2, i);
         if (sort == 0) {
             continue;
         }
         return sort > 0;
     }
     return true;
}

template<class T>
void y_marry2d(std::deque<T*> &answers , std::deque<T*> &x, typename std::deque<T*>::iterator y_begin, typename std::deque<T*>::iterator y_end , int (*c)(T, T, int), int s, int dim) {
    
    // first sort x and y
    y_sortList(x.begin(), x.end(), c, s, dim);
    y_sortList(y_begin, y_end, c, s, dim);
    
    if(y_begin == y_end) { // handle empty case, nothing to dominate
        for( typename std::deque<T*>::iterator s_x = x.begin(); s_x != x.end();s_x++ ) {
            answers.push_back(*s_x);
        }
        return;
    }
    
    
    typename std::deque<T*>::iterator s_x = x.begin();
    typename std::deque<T*>::iterator s_y = y_begin;
    typename std::deque<T*>::iterator ref = y_begin;
    
    // first get all x bigger than first y
    while(s_x != x.end()) {
        if (c(**s_x, **s_y, s) <= 0 ) {
            break;
        }
        
        answers.push_back(*s_x);
        s_x++;
    }
    
    // now first s_y is always reference point :)
    while(s_x != x.end()) {
        
        typename std::deque<T*>::iterator snext_y = s_y;
        snext_y++;
        
        // test if next y or next x
        if( snext_y != y_end && y_marry2d_comperator(*snext_y, *s_x, c, s, dim) ) {
           // add y, because y better
            s_y++;
            
            if( c(**s_y, **ref, dim) >= 0) {
               ref = s_y;
           } 
            
        } else {
           // x is next
           if( c(**s_x, **ref, dim) > 0) {
               answers.push_back(*s_x);
           } 
           s_x++; 
        }
    }
    
}


template<class T>
void y_marry(std::deque<T*> &answers , std::deque<T*> &x, typename std::deque<T*>::iterator y_begin, typename std::deque<T*>::iterator y_end , int (*c)(T, T, int), int s, int dim, int blocksize) {
    
    if (dim - s == 1) {
        y_marry2d(answers, x, y_begin, y_end, c, s, dim);
        return;
    }
    
    // flattened trees to store splits
    std::deque<y_split<T> > raw_splits_x;
    std::deque<y_split<T> > raw_splits_y;
    y_medianDoubleSplit(raw_splits_x, raw_splits_y, x, y_begin, y_end, c, s, dim, blocksize);
    
    // apply brute solving to all on base level
    typename std::deque<y_split<T> >::iterator s_x = raw_splits_x.begin();
    typename std::deque<y_split<T> >::iterator s_y = raw_splits_y.begin();
    
    typename std::deque<Deleter<y_split_p<T> > >  splits_x;
    
    for(; s_x != raw_splits_x.end() && s_y != raw_splits_y.end(); ++s_x, ++s_y) {
        
        y_split_p<T> * tmp = new y_split_p<T>(s_x->depth);
        y_marryBrute(tmp->list, *s_x, *s_y, c, s, dim);
        
        splits_x.push_back(tmp);
    }
        
    // join up bottom up, x can bee seen as already married
    while(splits_x.size() > 1) {
        
        typename std::deque<Deleter<y_split_p<T> > > ::iterator s_x = splits_x.begin();
        typename std::deque<y_split<T> >::iterator s_y = raw_splits_y.begin();
        
        for(; s_x != splits_x.end() && s_y != raw_splits_y.end(); ++s_x, ++s_y) { 
              
             typename std::deque<Deleter<y_split_p<T> > > ::iterator snext_x = s_x;
             snext_x++;
             typename std::deque<y_split<T> >::iterator snext_y = s_y;
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
                s_y = raw_splits_y.insert(s_y, y_split<T>((*s_x)->depth, false, ytmp.begin, ynexttmp.end) );
                
                break;
            }
        }
        
    }

    y_join_deque(answers, splits_x.front()->list);
}

template<class T>
void y_marry(std::deque<T*> &answers , std::deque<T*> &x, std::deque<T*> &y, int (*c)(T, T, int), int s, int dim, int blocksize) {
    y_marry(answers, x, y.begin(), y.end(), c, s, dim, blocksize);
}


/////-------------------------- SC -------------------------------

template<class T>
void y_bruteSolveSC(std::deque<T*> &answers,typename std::deque<T*>::iterator begin, typename std::deque<T*>::iterator end, int (*c)(T, T, int), int dim, bool unsorted) {
     
    // first sort the list if not already done so
    if (unsorted) {
        y_sortList(begin,end, c, 1, dim);
    }
    
    // n^2 adding
   typename std::deque<T*>::iterator ref = begin;
   if (ref != end) { 
       
        answers.push_back(*ref);
        ++ref;
   }
   for(; ref != end; ++ref) {
       bool add = true;
    
       for( typename std::deque<T*>::iterator ans = answers.begin(); ans != answers.end();ans++) {

           if (y_dominates( **ans, **ref , c, 2, dim)) {
               add = false;
               break;
           }
       }
      
       if (add) {
            answers.push_back(*ref);
       } 
   }
    
}


template<class T>
typename std::deque<T*> y_recSolveDC(std::deque<T*> &references, int (*c)(T, T, int), int dim, int blocksize) {
    
    // split up the problem until blocksize is reached
    
    // flattened tree to store splits
    std::deque<y_split<T> > raw_splits;
    y_medianSplit(raw_splits, references, c, blocksize, dim);

    std::deque<Deleter<y_split_p<T> > >  splits;
    // apply brute solving to all
    for(typename std::deque<y_split<T> >::iterator s = raw_splits.begin()
                  ; s != raw_splits.end(); ++s) {
        
        y_split_p<T>* tmp = new y_split_p<T>(s->depth);
        y_bruteSolveSC(tmp->list ,s->begin, s->end, c, dim, true);
        splits.push_back(tmp);
    }
    
    while(splits.size() > 1) {
             
            for(typename std::deque<Deleter<y_split_p<T> > >::iterator s = splits.begin()
                  ; s != splits.end(); s++) {
                  
                  typename std::deque<Deleter<y_split_p<T> > >::iterator snext = s;
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

    return splits.front()->list;
}

/////-------------------------- Main -------------------------------

template<class T, typename Iterator>
 List_Ref<T> pareto_yukish(Iterator begin, Iterator end, int (*c)(T, T, int), int dim, int blocksize)
{

    // write all to new reference list
   std::deque<T*> references;
   for (Iterator tupel = begin; tupel != end; ++tupel) {

       references.push_back(&(*tupel)); 
   }

   // solve it
   std::deque<T*> answers = y_recSolveDC(references, c, dim, blocksize);

   // create new tuples in response list
   List_Ref<T> ret_answers;
   for( typename std::deque<T*>::iterator ans = answers.begin(); ans != answers.end();++ans) {
       push_back( ret_answers, **ans);
   }

   return ret_answers;
}

#endif	/* PARETO_YUKISH_HH */

