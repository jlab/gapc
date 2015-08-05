/* 
 * File:   pareto_1_sorted_block.hh
 * Author: gatter
 *
 * Created on July 6, 2015, 10:31 AM
 */

#ifndef PARETO_1_SORTED_BLOCK_HH
#define	PARETO_1_SORTED_BLOCK_HH


#if __cplusplus >= 201103L
#define _MOVE(__val) std::move(__val)
#define _MOVE_RANGE(__it1,__it2,__in) std::move(__it1,__it2,__in)
#else
#define _MOVE(__val) (__val)
#define _MOVE_RANGE(__it1,__it2,__in) std::copy(__it1,__it2,__in)
#endif



// ---------------- 2D JOIN  ---------------

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
inline void join_2d_step(typename List_Ref<T>::iterator &a_begin, typename List_Ref<T>::iterator &a_end,
       typename List_Ref<T>::iterator &i_begin, typename List_Ref<T>::iterator &i_end, 
        List_Ref<T> &res, Compare &c, const bool keep_equal) {
    
    typename List_Ref<T>::iterator a_it = a_begin;
    typename List_Ref<T>::iterator i_it = i_begin;
    
    // ended by return
    while(true) {
        
        // end conditions lists are empty
        if (i_it == i_end) {
            for(;a_it != a_end;++a_it) {
                res.ref().push_back(_MOVE(*a_it));
            }
            break;
        }
        if (a_it == a_end ) {
            for(;i_it != i_end;++i_it) {
                res.ref().push_back(_MOVE(*i_it));
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
                        join_2d_drop(*i_it, a_it, a_end, c, keep_equal);
                        break;
                    case 0:
                        ++a_it;
                        break;
                    case 1:
                        break;
                }
                res.ref().push_back(_MOVE(*i_it));
                ++i_it;
                break;
            case 0:
                switch (c2) {
                    case -1:
                        ++a_it;
                        join_2d_drop(*i_it, a_it, a_end, c, keep_equal);
                        res.ref().push_back(_MOVE(*i_it));
                        ++i_it;
                        break;
                    case 0:
                        res.ref().push_back(_MOVE(*a_it));
                        if (keep_equal) {
                             res.ref().push_back(_MOVE(*i_it));
                        }
                        ++a_it;
                        ++i_it;
                        break;
                    case 1:
                         ++i_it;
                        join_2d_drop(*a_it, i_it, i_end, c, keep_equal);
                        res.ref().push_back(_MOVE(*a_it));
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
                        join_2d_drop(*a_it, i_it, i_end, c, keep_equal);
                        break;
                }
                res.ref().push_back(_MOVE(*a_it));
                ++a_it;
                break;     
        }
    }
    
}


template<class T, typename Compare>
inline void join_marked_multi_to_two_2D(List_Ref<T> &x, List_Ref<int> &markers, Compare &c, const bool keep_equal) {
    
    if (markers.ref().size()<= 1) { // already sorted
        return;
    }

    std::deque<List_Ref<T> > merges;
    
    typename List_Ref<T>::iterator s1_start, middle, s2_end;
       
    typename List_Ref<int>::iterator p1 = markers.ref().begin();
    typename List_Ref<int>::iterator p2 = markers.ref().begin();
    typename List_Ref<int>::iterator x_end = markers.ref().end();
    p2++;
    
    s1_start = x.ref().begin();
    
    for(; p2 != x_end && p1 != x_end ; p1+=2, p2+=2) {

        // get iterator
        middle = x.ref().begin();
        std::advance(middle, *p1);

        s2_end = x.ref().begin();
        std::advance(s2_end, *p2);
        
        // do the actual join
        List_Ref<T> e;
        merges.push_back(e);
        join_2d_step(s1_start, middle, middle, s2_end, merges.back(), c, keep_equal);
        
        s1_start = s2_end;
    }

    if (p1 != x_end) {
        typename List_Ref<T>::iterator end = x.ref().begin();
        std::advance(end, *p1);

        List_Ref<T> e;
        merges.push_back(e);
        _MOVE_RANGE(s1_start, end, std::back_inserter(merges.back().ref()));
    }
    
    
    while (merges.size() > 1) {
        
            std::deque<List_Ref<T> > new_merges;
        
            typename List_Ref<List_Ref<T> >::iterator m1 = merges.begin();
            typename List_Ref<List_Ref<T> >::iterator m2 = merges.begin();
            typename List_Ref<List_Ref<T> >::iterator m_end = merges.end();
            ++m2;
            
            for(; m2 != m_end && m1 != m_end ; m1+=2, m2+=2) {

                // do the actual join
                List_Ref<T> e;
                new_merges.push_back(e);
                
                typename List_Ref<T>::iterator s1_s = m1->ref().begin();
                typename List_Ref<T>::iterator s1_e = m1->ref().end();
                typename List_Ref<T>::iterator s2_s = m2->ref().begin();
                typename List_Ref<T>::iterator s2_e = m2->ref().end();

                join_2d_step(s1_s , s1_e, s2_s , s2_e, new_merges.back(), c, keep_equal);

            }

            if (m1 != m_end) {
                new_merges.push_back(_MOVE(*m1));
            }
            
            merges = _MOVE(new_merges);
    }
   
    x = _MOVE(merges.front());
}

// ---------------- 3D+ JOIN  ---------------


// generalized algorithm for >2 dimensions

template<class T, typename Compare, typename Sorter>
inline void join_all_step(typename List_Ref<T>::iterator &a_begin, typename List_Ref<T>::iterator &a_end,
       typename List_Ref<T>::iterator &i_begin, typename List_Ref<T>::iterator &i_end, 
        List_Ref<T> &new_list, Compare &c, Sorter &s, const bool keep_equal) {


    typename List_Ref<T>::iterator a_it = a_begin;
    typename List_Ref<T>::iterator i_it = i_begin;
    
    
    while(a_it != a_end || i_it != i_end) {

         // from which list to add next
         typename List_Ref<T>::iterator next;
         if (i_it == i_end || (a_it != a_end && s(*a_it, *i_it))) {
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
    
}


template<class T, typename Compare, typename Sorter>
inline void join_marked_multi_to_two_all(List_Ref<T> &x, List_Ref<int> &markers, Compare &c, Sorter &s, const bool keep_equal) {
    
    if (markers.ref().size()<= 1) { // already sorted
        return;
    }

    std::deque<List_Ref<T> > merges;
    
    typename List_Ref<T>::iterator s1_start, middle, s2_end;
       
    typename List_Ref<int>::iterator p1 = markers.ref().begin();
    typename List_Ref<int>::iterator p2 = markers.ref().begin();
    typename List_Ref<int>::iterator x_end = markers.ref().end();
    p2++;
    
    s1_start = x.ref().begin();
    
    for(; p2 != x_end && p1 != x_end ; p1+=2, p2+=2) {

        // get iterator
        middle = x.ref().begin();
        std::advance(middle, *p1);

        s2_end = x.ref().begin();
        std::advance(s2_end, *p2);
        
        // do the actual join
        List_Ref<T> e;
        merges.push_back(e);
        join_all_step(s1_start, middle, middle, s2_end, merges.back(), c, s, keep_equal);
        
        s1_start = s2_end;
    }

    if (p1 != x_end) {
        typename List_Ref<T>::iterator end = x.ref().begin();
        std::advance(end, *p1);

        List_Ref<T> e;
        merges.push_back(e);
        _MOVE_RANGE(s1_start, end, std::back_inserter(merges.back().ref()));
    }
    
    
    while (merges.size() > 1) {
        
            std::deque<List_Ref<T> > new_merges;
        
            typename List_Ref<List_Ref<T> >::iterator m1 = merges.begin();
            typename List_Ref<List_Ref<T> >::iterator m2 = merges.begin();
            typename List_Ref<List_Ref<T> >::iterator m_end = merges.end();
            ++m2;
            
            for(; m2 != m_end && m1 != m_end ; m1+=2, m2+=2) {

                // do the actual join
                List_Ref<T> e;
                new_merges.push_back(e);
                
                typename List_Ref<T>::iterator s1_s = m1->ref().begin();
                typename List_Ref<T>::iterator s1_e = m1->ref().end();
                typename List_Ref<T>::iterator s2_s = m2->ref().begin();
                typename List_Ref<T>::iterator s2_e = m2->ref().end();

                join_all_step(s1_s , s1_e, s2_s , s2_e, new_merges.back(), c, s, keep_equal);

            }

            if (m1 != m_end) {
                new_merges.push_back(_MOVE(*m1));
            }
            
            merges = _MOVE(new_merges);
    }
   
    x = _MOVE(merges.front());
}


// ---------------- SORTED 2D  ---------------

template<class T, typename Compare, typename Sorter>
inline void join_sorted_2d(List_Ref<T> &x,Compare &c, Sorter &s, const bool keep_equal) {
    
    // full sorting
    std::sort(x.ref().begin(), x.ref().end(), s);
    
    List_Ref<T> new_list;
    
    // lex
    for ( typename List_Ref<T>::iterator e = x.ref().begin(); e != x.ref().end(); ++e) {
        
        if(isEmpty(new_list)) {
            new_list.ref().push_back(_MOVE(*e));
            continue;
        }
        
        
        if (keep_equal) {
            int o = c(*e, new_list.ref().back(), 2);
            switch (o) {
                case 1:
                    new_list.ref().push_back(_MOVE(*e));
                    break;
                case 0:
                    if (c(*e, new_list.ref().back(), 2) == 0) { // add fully equal
                        new_list.ref().push_back(_MOVE(*e));
                    }
                    break;
            }
        } else {
            if(c(*e, new_list.ref().back(), 2) > 0) {
                new_list.ref().push_back(_MOVE(*e));
            }
        }
    }
    
    x = _MOVE(new_list);
}


// ---------------- SORTED 3D+  ---------------

template<class T, typename Compare, typename Sorter>
inline void join_sorted_all(List_Ref<T> &x,Compare &c, Sorter &s, const bool keep_equal) {
    
    // full sorting
    std::sort(x.ref().begin(), x.ref().end(), s);
    
    List_Ref<T> new_list;
    
    // lex n² 
    for ( typename List_Ref<T>::iterator e = x.ref().begin(); e != x.ref().end(); ++e) {
        
        if(isEmpty(new_list)) {
            new_list.ref().push_back(_MOVE(*e));
            continue;
        }
        
        if (keep_equal) {
             // test if element is the same as last inserted
             bool equal = true;
             for(int i=1; i <= c.dim; ++i ) {
                 if( c( *e, new_list.ref().back(), i) != 0) {
                     equal = false;
                     break;
                 }
             }
             if (equal) {
                 new_list.ref().push_back(_MOVE(*e));
                 continue;
             }
         }
        
        bool add = true;
        for(typename List_Ref<T>::iterator n = new_list.ref().begin(); n != new_list.ref().end(); ++n) {
            
            bool dominates = true;
             for(int i=2; i <= c.dim; ++i ) {
                 if( c( *e, *n, i) > 0) {
                     dominates = false;
                     break;
                 }
             }
             if (dominates) {
                 add = false;
                 break;
             }
        }
        
        if(add) {
            new_list.ref().push_back(_MOVE(*e));
        }
    }
    
    x = _MOVE(new_list);
}

// ---------------- MARK  ---------------

template<class T>
inline void mark_position(List_Ref<T> &x, List_Ref<int> &markers) {
    int s = x.ref().size();
    if (s!=0 && (markers.ref().empty() || s != markers.ref().back()) )
        markers.ref().push_back(s);
}

#include <boost/timer.hpp>

// ---------------- MAIN  ---------------

template<class T, typename Compare, typename Sorter>
inline void join_marked(List_Ref<T> &x, List_Ref<int> &markers, Compare &c, Sorter &s, const bool keep_equal)
{
    if (markers.ref().size() <= 1) {
        return;
    }
    
    if(c.dim == 2) {
          join_marked_multi_to_two_2D(x, markers, c, keep_equal);
//        join_sorted_2d(x,c,s, keep_equal);
        
//        List_Ref<T > l1;	
//        List_Ref<int> ends1;
//        std::copy(x.ref().begin(), x.ref().end(), std::back_inserter(l1.ref()));
//        std::copy(markers.ref().begin(), markers.ref().end(), std::back_inserter(ends1.ref()));
//
//        List_Ref<T > l2;	
//        List_Ref<int> ends2;
//        std::copy(x.ref().begin(), x.ref().end(), std::back_inserter(l2.ref()));
//        std::copy(markers.ref().begin(), markers.ref().end(), std::back_inserter(ends2.ref()));
//        
//        boost::timer t;
//        join_marked_multi_to_two_2D(l1, ends1, c, keep_equal);
//        double t1 = t.elapsed();
//
//        boost::timer u;
//        join_sorted_2d(l2, c, s, keep_equal);
//        double t2 = u.elapsed();
//        
//        std::cerr << x.ref().size() << " " << markers.ref().size() 
//            << " M " << t1 << " " << l1.ref().size()
//            << " S " << t2 << " " << l2.ref().size();
//        if (t1 == t2) {
//            std::cerr << " ms" << std::endl;
//        } else if (t1 > t2){
//            std::cerr << " s" << std::endl;
//        } else {
//            std::cerr << " m" << std::endl;
//        }
//        
//        join_sorted_2d(x,c,s, keep_equal);
              
    } else {
          join_marked_multi_to_two_all(x, markers, c, s, keep_equal);
//        join_sorted_all(x,c,s, keep_equal);
        
        
//        List_Ref<T > l1;	
//        List_Ref<int> ends1;
//        std::copy(x.ref().begin(), x.ref().end(), std::back_inserter(l1.ref()));
//        std::copy(markers.ref().begin(), markers.ref().end(), std::back_inserter(ends1.ref()));
//
//        List_Ref<T > l2;	
//        List_Ref<int> ends2;
//        std::copy(x.ref().begin(), x.ref().end(), std::back_inserter(l2.ref()));
//        std::copy(markers.ref().begin(), markers.ref().end(), std::back_inserter(ends2.ref()));
//        
//        boost::timer t;
//        join_marked_multi_to_two_all(l1, ends1, c, s, keep_equal);
//        double t1 = t.elapsed();
//
//        boost::timer u;
//        join_sorted_all(l2, c, s, keep_equal);
//        double t2 = u.elapsed();
//        
//        std::cerr << x.ref().size() << " " << markers.ref().size() 
//            << " M " << t1 << " " << l1.ref().size()
//            << " S " << t2 << " " << l2.ref().size();
//        if (t1 == t2) {
//            std::cerr << " ms" << std::endl;
//        } else if (t1 > t2){
//            std::cerr << " s" << std::endl;
//        } else {
//            std::cerr << " m" << std::endl;
//        }
//        
//        join_sorted_all(x,c,s, keep_equal);
        
    }
}


#endif	/* PARETO_1_SORTED_BLOCK_HH */

