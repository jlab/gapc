/* 
 * File:   pareto_1_sorted_step.hh
 * Author: gatter
 *
 * Created on July 6, 2015, 10:31 AM
 */

#ifndef PARETO_1_SORTED_STEP_HH
#define	PARETO_1_SORTED_STEP_HH


#if __cplusplus >= 201103L
#define _MOVE(__val) std::move(__val)
#define _MOVE_RANGE(__it1,__it2,__in) std::move(__it1,__it2,__in)
#else
#define _MOVE(__val) (__val)
#define _MOVE_RANGE(__it1,__it2,__in) std::copy(__it1,__it2,__in)
#endif



// specialized algorithm for 2 dimensions
template<class T, typename Compare>
inline void join_2d_drop(T &ref, typename List_Ref<T>::iterator &it, typename List_Ref<T>::iterator end, Compare &c, const bool keep_equal) {
    
    if (keep_equal) {
        while( it!= end && c(ref, *it, 2) > 0) {
            ++it;
        }
    } else {
        while( it!= end && c(ref, *it, 2) >= 0) {
            ++it;
        }
    }
    
}

// specialized algorithm for 2 dimensions
template<class T, typename Compare>
inline void join_2d_step(List_Ref<T> &answers, List_Ref<T> &inserts, Compare &c, const bool keep_equal) {
    
    typename List_Ref<T>::iterator a_it = answers.ref().begin();
    typename List_Ref<T>::iterator i_it = inserts.ref().begin();
    
    // new list
    List_Ref<T> new_list;
    
    // ended by return
    while(true) {
        
        // end conditions lists are empty
        if (i_it == inserts.ref().end()) {
            for(;a_it != answers.ref().end();++a_it) {
                new_list.ref().push_back(_MOVE(*a_it));
            }
            break;
        }
        if (a_it == answers.ref().end() ) {
            for(;i_it != inserts.ref().end();++i_it) {
                new_list.ref().push_back(_MOVE(*i_it));
            }
            break;
        }
        
        int c1 = c(*a_it, *i_it, 1);
        int c2 = c(*a_it, *i_it, 2);

        switch(c1) {
            case -1:
                switch (c2) {
                    case -1:
                        ++a_it;
                        join_2d_drop(*i_it, a_it, answers.ref().end(), c, keep_equal);
                        break;
                    case 0:
                        ++a_it;
                        break;
                    case 1:
                        break;
                }
                new_list.ref().push_back(_MOVE(*i_it));
                ++i_it;
                break;
            case 0:
                switch (c2) {
                    case -1:
                        ++a_it;
                        join_2d_drop(*i_it, a_it, answers.ref().end(), c, keep_equal);
                        new_list.ref().push_back(_MOVE(*i_it));
                        ++i_it;
                        break;
                    case 0:
                        new_list.ref().push_back(_MOVE(*a_it));
                        if (keep_equal) {
                             new_list.ref().push_back(_MOVE(*i_it));
                        }
                        ++a_it;
                        ++i_it;
                        break;
                    case 1:
                         ++i_it;
                        join_2d_drop(*a_it, i_it, inserts.ref().end(), c, keep_equal);
                        new_list.ref().push_back(_MOVE(*a_it));
                        ++a_it;
                        break;
                }
                break; 
            case 1: 
                switch (c2) {
                    case -1:
                        break;
                    case 0:
                        ++i_it;
                        break;
                    case 1:
                        ++i_it;
                        join_2d_drop(*a_it, i_it, inserts.ref().end(), c, keep_equal);
                        break;
                }
                new_list.ref().push_back(_MOVE(*a_it));
                ++a_it;
                break;     
        }
    }
    
    answers = _MOVE(new_list);
}


// generalized algorithm for >2 dimensions
template<class T, typename Compare, typename Sorter>
inline void join_all_step(List_Ref<T> &answers, List_Ref<T> &inserts, Compare &c, Sorter &s, const bool keep_equal)
{

    typename List_Ref<T>::iterator a_it = answers.ref().begin();
    typename List_Ref<T>::iterator i_it = inserts.ref().begin();
    
    // new list
    List_Ref<T> new_list;
    
    while(a_it != answers.ref().end() || i_it != inserts.ref().end()) {

         // from which list to add next
         typename List_Ref<T>::iterator next;
         if (i_it == inserts.ref().end() || (a_it != answers.ref().end() && s(*a_it, *i_it))) {
             next = a_it;
             ++a_it;
         } else {
             next = i_it;
             ++i_it;
         }
        
         // pareto list is empty, 
         if (new_list.ref().empty()) {
             new_list.ref().push_back(_MOVE(*next));
             continue;
         }
         
         
         if (keep_equal) {
             // test if element is the same as last inserted
             bool equal = true;
             for(int i=1; i <= c.dim; ++i ) {
                 if( c( *next, new_list.ref().back(), i) != 0) {
                     equal = false;
                     break;
                 }
             }
             if (equal) {
                 new_list.ref().push_back(_MOVE(*next));
                 continue;
             }
         }
         
         // move through answers so far
         bool add = true;
         for(typename List_Ref<T>::iterator n = new_list.ref().begin(); n != new_list.ref().end(); ++n ) {
             
             bool dominates = true;
             for(int i=2; i <= c.dim; ++i ) {
                 if( c( *next, *n, i) > 0) {
                     dominates = false;
                     break;
                 }
             }
             if (dominates) {
                 add = false;
                 break;
             }
             
         }

         if (add) {
             new_list.ref().push_back(_MOVE(*next));
         }
    }
    
    answers = _MOVE(new_list);
}


// only insert one element, 2D
template<class T, typename Compare, typename Sorter>
inline void join_insert_one_2d(List_Ref<T> &answers, T &insert, Compare &c, Sorter &s, const bool keep_equal)
{
    
    // find insert position
    typename List_Ref<T>::iterator a_it = answers.ref().begin();
    
    while(a_it != answers.ref().end() && s(*a_it, insert)){
        ++a_it;
    }
    
    int comp = c(*a_it, insert, 2);
    
    
    if (keep_equal) {
        
        
        if (comp < 0) {
            a_it = answers.ref().insert(a_it, insert);
        } else if (comp == 0 && c(*a_it, insert, 1)==0) { // special equal inserts
            a_it = answers.ref().insert(a_it, insert);
        }
        
        ++a_it;

        while( a_it != answers.ref().end() && c(*a_it, insert, 1) == 0 ) {
            int o = c(*a_it, insert, 2);
            if ( o < 0 || (o == 0 && c(*a_it, insert, 1) != 0)) { // keep equal!
                a_it = answers.ref().erase(a_it);
            } 
        }
    } else {
        if (comp < 0) {
            a_it = answers.ref().insert(a_it, insert);
        }
        ++a_it;

        while( a_it != answers.ref().end() && c(*a_it, insert, 1) == 0 ) {
            if (c(*a_it, insert, 2)  <= 0) {
                a_it = answers.ref().erase(a_it);
            }
        }
    }
    
}

// only insert one element, 3D+
template<class T, typename Compare, typename Sorter>
inline void join_insert_one_all(List_Ref<T> &answers, T &insert, Compare &c, Sorter &s, const bool keep_equal)
{
    
    List_Ref<T> inserts;
    inserts.ref().push_back(insert);
    
    join_all_step(answers, inserts, c, s, keep_equal);
    
}

// append with sorted Pareto
template<class T, typename Compare, typename Sorter>
inline void append(List_Ref<T> &answers, T &insert, Compare &c, Sorter &s, const bool keep_equal)
{
        
    if (isEmpty(answers)) {
      answers.ref().push_back(insert);
      return;
    }
    
    // assume answers and inserts are sorted
    if (c.dim == 2) {
        join_insert_one_2d(answers, insert, c, s, keep_equal);
    } else {
        join_insert_one_all(answers, insert, c, s, keep_equal);
    }
     
}

// append with sorted Pareto
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
  
  assert(&answers.ref() != &inserts.ref());
  
  // assume answers and inserts are sorted
  if (c.dim == 2) {
      join_2d_step(answers, inserts, c, keep_equal);
  } else {
      join_all_step(answers, inserts, c, s, keep_equal);
  }

}


#endif	/* PARETO_1_SORTED_STEP_HH */

