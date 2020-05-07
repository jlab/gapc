/* 
 * File:   pareto_0_nosort_step.hh
 * Author: gatter
 *
 * Created on June 29, 2015, 2:57 PM
 */

#ifndef PARETO_3_YUKISH_STEP_HH
#define	PARETO_3_YUKISH_STEP_HH


#if __cplusplus >= 201103L
#define _MOVE(__val) std::move(__val)
#define _MOVE_RANGE(__it1,__it2,__in) std::move(__it1,__it2,__in)
#else
#define _MOVE(__val) (__val)
#define _MOVE_RANGE(__it1,__it2,__in) std::copy(__it1,__it2,__in)
#endif

template <class T>
class yp_list : public std::deque<T*> {};

template <class T>
class yp_in_list : public std::deque<T> {};

template<class T, typename Iterator>
struct yp_split_it {
    public:
       yp_split_it() {} 
       yp_split_it(int depth, bool unsplittable, Iterator begin, Iterator end) {
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
struct yp_split {
    public:
       yp_split() {} 
       yp_split(int depth, bool unsplittable, typename yp_list<T>::iterator begin, typename yp_list<T>::iterator end) {
           this->depth = depth;
           this->begin = begin;
           this->end = end;
           this->unsplittable = unsplittable;
       }
       int depth;
       bool unsplittable;
       typename yp_list<T>::iterator begin;
       typename yp_list<T>::iterator end;       
 };

 template<class T>
 struct yp_split_p {
     public:
        yp_split_p() {}
        yp_split_p(int depth) {
            this->depth = depth;
        }
        int depth;
        yp_list<T> list;
 };
 
 template<class T>
 struct yp_split_p_raw {
     public:
        yp_split_p_raw() {}
        yp_list<T> list;
 };

template <typename T>
class yp_deleter {
public:
  yp_deleter(T* pointer) {
	l.reset(pointer);
  }
  yp_deleter(const yp_deleter& deleter) {
    yp_deleter* d = const_cast<yp_deleter*>(&deleter);
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
bool yp_dominates(const T & c1, const T & c2, Compare &c, int dim) {
    return yp_dominates(c1, c2, c, 1, dim);
}

template<class T, typename Compare>
bool yp_dominates(const T & c1, const T & c2, Compare &c, int s, int dim) {
   
    for (int i=s; i<=dim; i++) {
  
        if (c(c1, c2, i) < 0) {
            return false;
        }
    }
    
    return true;
}

template<class T, typename Compare>
bool yp_co_dominates(const T & c1, const T & c2, Compare &c, int s, int dim) {
   
    bool co = true;
    for (int i=s; i<=dim; i++) {
  
        const int res = c(c1, c2, i);
        switch(res) {
            case -1:
                return false;
            case 0:
                break;
            case 1:
                co = false;
                
        }
    }
    
    return !co;
}

template<class T, typename Compare>
struct yp_sorter {
    public:     
     yp_sorter(int s, int dim, Compare &c) {
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
 
 
 template<class T, typename Sorter>
struct yp_fullsorter {
    public:     
     yp_fullsorter(Sorter &c) {
         this->c = c;
     }

     Sorter c;
     
     bool operator () (T *c1, T  *c2)
     {
        return c(*c1, *c2);
     }
 };
 
// sorts the given subset of the list, starting at dimension s
template<class T, typename Compare, typename Sorter>
void yp_sortList(typename yp_list<T>::iterator begin, typename yp_list<T>::iterator end, Compare &c, Sorter &sort,  const int s,  const int dim) {
  
   if (s==1)  {
        yp_fullsorter<T, Sorter> sortob = yp_fullsorter<T, Sorter>(sort);
        std::sort(begin, end, sortob);
   } else {
        yp_sorter<T, Compare> sortob = yp_sorter<T, Compare>(s,dim,c);  
        std::sort(begin, end, sortob);
   }
}


//adds y to x
template<class T>
void yp_join_deque( yp_list<T> &x, yp_list<T> &y) {
    _MOVE_RANGE(y.begin(), y.end(), std::back_inserter(x));
}

//adds y to x
template<class T>
void yp_join_deque( yp_list<T> &x, typename yp_list<T>::iterator &y_begin, typename yp_list<T>::iterator &y_end) {
    _MOVE_RANGE(y_begin, y_end, std::back_inserter(x));
}


/////-------------------------- Split Marry -------------------------------

template<class T, typename Compare>
typename yp_in_list<yp_split<T> >::iterator yp_sortedSplitMarryp_inner1(yp_in_list<yp_split<T> > &splits, typename yp_in_list<yp_split<T> >::iterator in , yp_split<T> ob, Compare &c, int d, int dim, T &median) {
    
    if (ob.begin == ob.end) {
        return in;
    }
    
    typename yp_list<T>::iterator mid = ob.begin;
    std::advance(mid, std::distance(ob.begin, ob.end) / 2);
    
    // move over elements having same value as mid
    typename yp_list<T>::iterator store = mid;
    for(; store != ob.end && c(**store, **mid, d) == 0; store++)  {
    }
    
    // special case when all elements are the same (weird case, I know)
    // make unsplittable
    bool unsplittable = false;
    if (store == ob.end) {
        unsplittable = true;
        typename yp_list<T>::iterator last =  store;
        last--;
        median = **last;
    } else {
        median = **store;
    }
    
    
    in = splits.insert(in , yp_split<T>(ob.depth+1, unsplittable, store, ob.end));
    in++;
    in = splits.insert(in , yp_split<T>(ob.depth+1, unsplittable, ob.begin, store));
    return in;
}

template<class T, typename Compare>
typename yp_in_list<yp_split<T> >::iterator yp_sortedSplitMarryp_inner2(yp_in_list<yp_split<T> > &splits, typename yp_in_list<yp_split<T> >::iterator in , yp_split<T> ob, Compare &c, int d, int dim, const T &median) {
    
    if (ob.begin == ob.end) {
        return in;
    }
    
    typename yp_list<T>::iterator store = ob.begin;
    // move over elements until median
    for(; store != ob.end && c(median, **store, d) <= 0; store++)  {
    }
    
    // special case when all elements are the same (weird case, I know)
    // make unsplittable
    bool unsplittable = false;
    if (store == ob.end) {
        unsplittable = true;
    }
    
    in = splits.insert(in , yp_split<T>(ob.depth+1, unsplittable, store, ob.end));
    in++;
    in = splits.insert(in , yp_split<T>(ob.depth+1, unsplittable, ob.begin, store));
    return in;
}


template<class T, typename Compare>
typename yp_in_list<yp_split<T> >::iterator yp_sortedSplitMarryp_inner3(yp_in_list<yp_split<T> > &splits, typename yp_in_list<yp_split<T> >::iterator in , yp_split<T> ob, Compare &c, int d, int dim, const T &median) {
    
    if (ob.begin == ob.end) {
        return in;
    }
    
    typename yp_list<T>::iterator store = ob.begin;
    // move over elements until median
    for(; store != ob.end && c(median, **store, d) < 0; store++)  {
    }
    
    // special case when all elements are the same (weird case, I know)
    // make unsplittable
    bool unsplittable = false;
    if (store == ob.end) {
        unsplittable = true;
    }
    
    in = splits.insert(in , yp_split<T>(ob.depth+1, unsplittable, store, ob.end));
    in++;
    in = splits.insert(in , yp_split<T>(ob.depth+1, unsplittable, ob.begin, store));
    return in;
}

template<class T, typename Compare>
void yp_sortDoubleSplit(yp_in_list<yp_split<T> > &splits_x, yp_in_list<yp_split<T> > &splits_y,
         typename yp_list<T>::iterator x_begin, typename yp_list<T>::iterator x_end,
        typename yp_list<T>::iterator y_begin, typename yp_list<T>::iterator y_end,
        Compare &c, int s, int dim, int blocksize, const bool equal_to_same) {
    
    
    // add first lists to splits
    splits_x.push_back( yp_split<T>(1, false, x_begin, x_end ) );
    splits_y.push_back( yp_split<T>(1, false, y_begin, y_end ) );
    
    bool continue_split = true;
    while (continue_split) {
          continue_split = false;
          
          typename yp_in_list<yp_split<T> >::iterator s_x = splits_x.begin();
          typename yp_in_list<yp_split<T> >::iterator s_y = splits_y.begin();
          
          for( ; s_x != splits_x.end() && s_y != splits_y.end(); s_x++, s_y++) {
              
              if ( std::distance(s_x->begin, s_x->end) > blocksize && std::distance(s_y->begin, s_y->end) > blocksize
                      && !s_x->unsplittable && !s_y->unsplittable) {
                  
                  yp_split<T> ob_y = *s_y;
                  yp_split<T> ob_x = *s_x;
                  
                  s_x = splits_x.erase(s_x);
                  s_y = splits_y.erase(s_y);
                  
                  T median;
                  s_x = yp_sortedSplitMarryp_inner1(splits_x, s_x , ob_x, c,  s, dim, median);

                  if (equal_to_same && !s_x->unsplittable) {
                       s_y = yp_sortedSplitMarryp_inner3(splits_y, s_y , ob_y, c,  s, dim, median);
                  } else {
                       s_y = yp_sortedSplitMarryp_inner2(splits_y, s_y , ob_y, c,  s, dim, median);
                  }
                  continue_split = true;
              } 
          }
    }
    
}

/////-------------------------- Marry -------------------------------


template<class T, typename Compare>
bool yp_marry2d_comperator(const T &c1, const T  &c2, Compare &c, int s, int dim)
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
void yp_marry2d( yp_list<T> &answers, typename yp_list<T>::iterator x_begin, typename yp_list<T>::iterator x_end,
        typename yp_list<T>::iterator y_begin, typename yp_list<T>::iterator y_end , Compare &c, int s, int dim) {
    
    if(y_begin == y_end) { // handle empty case, nothing to dominate
        yp_join_deque(answers, x_begin, x_end);
        return;
    }
    
    typename yp_list<T>::iterator s_x = x_begin;
    typename yp_list<T>::iterator s_y = y_begin;
    typename yp_list<T>::iterator ref = y_begin;
    
    // first get all x bigger than first y
    while(s_x != x_end) {
        if (c(**s_x, **s_y, s) <= 0 ) {
            break;
        }
       
        answers.push_back(_MOVE(*s_x));
        s_x++;
    }
    
    // now first s_y is always reference point :)
    while(s_x != x_end) {
        
        typename yp_list<T>::iterator snext_y = s_y;
        snext_y++;
        
        // test if next y or next x
        if( snext_y != y_end && yp_marry2d_comperator(**snext_y, **s_x, c, s, dim) ) {
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
void yp_marryBrute(yp_list<T>  &answers, const yp_split<T> &x, const yp_split<T> &y, Compare &c, int s, int dim, const bool keep_equal) {

    for(typename yp_list<T>::iterator el_x = x.begin; el_x != x.end; ++el_x) {
   
        bool add = true;
        for(typename yp_list<T>::iterator el_y = y.begin; el_y != y.end; ++el_y) {

          if(keep_equal) {
                if (yp_co_dominates( **el_y, **el_x , c, s, dim) ) {
                    add = false;
                    break;
                }
          } else {
                if (yp_dominates( **el_y, **el_x , c, s, dim) ) {
                    add = false;
                    break;
                }
          }
        }

        if (add) {
             answers.push_back(_MOVE(*el_x));
        } 
    }
    
}


template<class T, typename Compare>
void yp_marryBrute(yp_list<T>  &answers, const yp_split<T> &x, yp_list<T> &y, Compare &c, int s, int dim, const bool keep_equal) {

    for(typename yp_list<T>::iterator el_x = x.begin; el_x != x.end; ++el_x) {
   
        bool add = true;
        for(typename yp_list<T>::iterator el_y = y.begin(); el_y != y.end(); ++el_y) {

            if(keep_equal) {
                  if (yp_co_dominates( **el_y, **el_x , c, s, dim) ) {
                      add = false;
                      break;
                  }
            } else {
                  if (yp_dominates( **el_y, **el_x , c, s, dim) ) {
                      add = false;
                      break;
                  }
           }
        }

        if (add) {
             answers.push_back(_MOVE(*el_x));
        } 
    }
    
}


//------------- dominating marry ------------------


template<class T, typename Compare, typename Sorter>
void yp_marry_base(yp_list<T> &answers , yp_list<T> &x, typename yp_list<T>::iterator y_begin, typename yp_list<T>::iterator y_end , Compare &c, Sorter &sort, int s, int dim, int blocksize) {
    
    // x and y need to be sorted for all cases
    yp_sortList<T, Compare, Sorter>(x.begin(), x.end(), c, sort, s, dim);
    yp_sortList<T, Compare, Sorter>(y_begin, y_end, c, sort, s, dim);
    
    if (dim - s == 1) {
        yp_marry2d(answers, x.begin(), x.end(), y_begin, y_end, c, s, dim);
        return;
    }
    
    // flattened trees to store splits
    yp_in_list<yp_split<T> > raw_splits_x;
    yp_in_list<yp_split<T> > raw_splits_y;
    yp_sortDoubleSplit(raw_splits_x, raw_splits_y, x.begin(), x.end(), y_begin, y_end, c, s, dim, blocksize, false);
    
    // apply brute solving to all on base level
    typename yp_in_list<yp_split<T> >::iterator s_x = raw_splits_x.begin();
    typename yp_in_list<yp_split<T> >::iterator s_y = raw_splits_y.begin();
    
    yp_in_list<yp_deleter<yp_split_p<T> > >  splits_x;
    
    for(; s_x != raw_splits_x.end() && s_y != raw_splits_y.end(); ++s_x, ++s_y) {
        
        yp_split_p<T> * tmp = new yp_split_p<T>(s_x->depth);
        yp_marryBrute(tmp->list, *s_x, *s_y, c, s, dim, false);
        
        splits_x.push_back(tmp);
    }
        
    // join up bottom up, x can bee seen as already married
    while(splits_x.size() > 1) {
        
        typename yp_in_list<yp_deleter<yp_split_p<T> > > ::iterator s_x = splits_x.begin();
        typename yp_in_list<yp_split<T> >::iterator s_y = raw_splits_y.begin();
        
        for(; s_x != splits_x.end() && s_y != raw_splits_y.end(); ++s_x, ++s_y) { 
              
             typename yp_in_list<yp_deleter<yp_split_p<T> > > ::iterator snext_x = s_x;
             snext_x++;
             typename yp_in_list<yp_split<T> >::iterator snext_y = s_y;
             snext_y++;
            
                  
            if (snext_x == splits_x.end() || snext_y == raw_splits_y.end()) {
                break;
            }
            
            if((*s_x)->depth == (*snext_x)->depth) { 
             
                yp_split_p<T> * tmp = new yp_split_p<T>((*s_x)->depth-1);
                
                yp_marry_base(tmp->list, (*s_x)->list, snext_y->begin, snext_y->end, c, sort, s+1, dim, blocksize);
                yp_join_deque(tmp->list, (*snext_x)->list);
                
                s_x = splits_x.erase(s_x,s_x+2);
                s_x = splits_x.insert(s_x,tmp);
                
                yp_split<T> ytmp = *s_y;
                yp_split<T> ynexttmp = *snext_y;
                        
                s_y = raw_splits_y.erase(s_y,s_y+2);
                s_y = raw_splits_y.insert(s_y, yp_split<T>((*s_x)->depth, false, ynexttmp.begin, ytmp.end) );
                
                break;
            }
        }
        
    }

    yp_join_deque(answers, splits_x.front()->list);
}

template<class T, typename Compare, typename Sorter>
void yp_marry_base(yp_list<T> &answers , yp_list<T> &x, yp_list<T> &y, Compare &c, Sorter &sort, int s, int dim, int blocksize) {
    yp_marry_base(answers, x, y.begin(), y.end(), c, sort, s, dim, blocksize);
}


// ------------------- co marry ------------------------------

template<class T, typename Compare, typename Sorter>
void yp_marry( yp_list<T> &answers_x,  yp_list<T> &answers_y,
        typename yp_list<T>::iterator x_begin, typename yp_list<T>::iterator x_end,
        typename yp_list<T>::iterator y_begin, typename yp_list<T>::iterator y_end,
        Compare &c, Sorter &sort, int dim, int blocksize, const bool keep_equal) {
    
    // x and y need to be sorted for all cases
    yp_sortList<T, Compare, Sorter>(x_begin, x_end, c, sort, 1, dim);
    yp_sortList<T, Compare, Sorter>(y_begin, y_end, c, sort, 1, dim);
    
    // flattened trees to store splits
    yp_in_list<yp_split<T> > raw_splits_x;
    yp_in_list<yp_split<T> > raw_splits_y;
    yp_sortDoubleSplit(raw_splits_x, raw_splits_y, x_begin, x_end, y_begin, y_end, c, 1, dim, blocksize, true);
    
    // apply brute solving to all on base level
    typename yp_in_list<yp_split<T> >::iterator s_x = raw_splits_x.begin();
    typename yp_in_list<yp_split<T> >::iterator s_y = raw_splits_y.begin();
    
    yp_in_list<yp_deleter<yp_split_p<T> > >  splits_x;
    yp_in_list<yp_deleter<yp_split_p_raw<T> > >  splits_y;
    
    for(; s_x != raw_splits_x.end() && s_y != raw_splits_y.end(); ++s_x, ++s_y) {
        
        // y to x
        yp_split_p<T> * tmp = new yp_split_p<T>(s_x->depth);
        yp_marryBrute(tmp->list, *s_x, *s_y, c, 1, dim, keep_equal);
        splits_x.push_back(tmp);
        
        // x to y
        yp_split_p_raw<T> * tmp2 = new yp_split_p_raw<T>();
        yp_marryBrute(tmp2->list, *s_y, tmp->list , c, 1, dim, keep_equal);
        splits_y.push_back(tmp2);
    }
        
    // join up bottom up
    while(splits_x.size() > 1) {
        
        
        typename yp_in_list<yp_deleter<yp_split_p<T> > > ::iterator s_x = splits_x.begin();
        typename yp_in_list<yp_deleter<yp_split_p_raw<T> > >::iterator s_y = splits_y.begin();
        
        for(; s_x != splits_x.end() && s_y != splits_y.end(); ++s_x, ++s_y) { 
              
             typename yp_in_list<yp_deleter<yp_split_p<T> > > ::iterator snext_x = s_x;
             snext_x++;
             typename yp_in_list<yp_deleter<yp_split_p_raw<T> > >::iterator snext_y = s_y;
             snext_y++;
              
            if (snext_x == splits_x.end() || snext_y == splits_y.end()) {
                break;
            }
            
            if((*s_x)->depth == (*snext_x)->depth) { 
             
                // y to x
                yp_split_p<T> * tmp = new yp_split_p<T>((*s_x)->depth-1);
                
                yp_marry_base(tmp->list, (*s_x)->list, (*snext_y)->list.begin(), (*snext_y)->list.end(), c, sort, 1, dim, blocksize);
                yp_join_deque(tmp->list, (*snext_x)->list);
                
                // x to y
                yp_split_p_raw<T> * tmp2 = new yp_split_p_raw<T>();
                
                yp_marry_base(tmp2->list, (*s_y)->list, (*snext_x)->list.begin(), (*snext_x)->list.end(), c, sort, 1, dim, blocksize);
                yp_join_deque(tmp2->list, (*snext_y)->list);
                
                // insert elements
                s_x = splits_x.erase(s_x,s_x+2);
                s_x = splits_x.insert(s_x,tmp);
                s_y = splits_y.erase(s_y,s_y+2);
                s_y = splits_y.insert(s_y,tmp2);
                                
                break;
            }
        }
        
    }

    yp_join_deque(answers_x, splits_x.front()->list);
    yp_join_deque(answers_y, splits_y.front()->list);
    
}

template<class T, typename Compare, typename Sorter>
inline void yp_marry(yp_list<T> &answers_x, yp_list<T> &answers_y , yp_list<T> &x, yp_list<T> &y, Compare &c, Sorter &sort, int dim, int blocksize, const bool keep_equal) {
    yp_marry(answers_x, answers_y, x.begin(), x.end(), y.begin(), y.end(), c, sort, dim, blocksize, keep_equal);
}


// append with no sort Pareto
template<class T, typename Compare, typename Sorter>
inline void append(List_Ref<T> &answers, T &in, Compare &c, Sorter &s, const bool keep_equal) {
    
    const int dim = c.dim;
    bool add = true;
    for (typename List_Ref<T>::iterator answer = answers.ref().begin(); answer!=answers.ref().end(); ){
      
      bool less = false;  
      bool better = false;
      for (int i = 1; i<= dim; ++i) {
          int res = c(*answer, in, i);
          switch(res) {
              case 1:
                  better = true;
                  break;
              case -1:
                  less = true;
                  break;
              default:
                  break;
          }
          
          if (better && less) {
              break;
          }
      }
      if (better && less) { // no domination
          ++answer;
      } else if (better || (!better && !less && !keep_equal) ) { // answer is always better or equal or all values equal
          add = false;
          break;
      } else if (less){ // less && !better
          // remove from answer list
          answer = erase_element(answers, answer);
      } else {
          ++answer;
      }
    }
    
    if (add == true)
    {
      answers.ref().push_back(_MOVE(in));
    } 
    
}


extern const int yukish_cutoff;

// append with no sort Pareto
template<class T, typename Compare, typename Sorter>
inline void append(List_Ref<T> &answers, List_Ref<T> &inserts, Compare &c, Sorter &s, const bool keep_equal)
{

  // basic security tests  
  if (isEmpty(inserts)) {
    return;
  }
  
  if (isEmpty(answers)) {
       _MOVE_RANGE(inserts.ref().begin(), inserts.ref().end(), std::back_inserter(answers.ref()));
      return;
  } 
  
  if (inserts.ref().size() == 1) {
      append(answers, inserts.ref().front(), c, s, keep_equal);
      return;
  }
  
  yp_list<T> copy_y;
  yp_list<T> copy_x;
  
  for (typename List_Ref<T>::iterator it = answers.ref().begin(); it!=answers.ref().end(); ++it){
      copy_x.push_back(&(*it));
  }
  for (typename List_Ref<T>::iterator it = inserts.ref().begin(); it!=inserts.ref().end(); ++it){
      copy_y.push_back(&(*it));
  }
  
  yp_list<T> answers_x;
  yp_list<T> answers_y;
  
  yp_marry(answers_x, answers_y, copy_x, copy_y, c, s, c.dim, yukish_cutoff, keep_equal);
  
  List_Ref<T> newans;
  for( typename yp_list<T>::iterator ans = answers_x.begin(); ans != answers_x.end();++ans) {
       push_back( newans, _MOVE(**ans));
  }
  for( typename yp_list<T>::iterator ans = answers_y.begin(); ans != answers_y.end();++ans) {
       push_back( newans, _MOVE(**ans));
  }
  
  answers = newans;
  
}


#endif	/* PARETO_3_YUKISH_STEP_HH */

