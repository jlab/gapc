/* 
 * File:   pareto_0_nosort_block.hh
 * Author: gatter
 *
 * Created on July 21, 2015, 12:33 PM
 */

#ifndef PARETO_0_NOSORT_BLOCK_HH
#define	PARETO_0_NOSORT_BLOCK_HH

#if __cplusplus >= 201103L
#define _MOVE(__val) std::move(__val)
#define _MOVE_RANGE(__it1,__it2,__in) std::move(__it1,__it2,__in)
#else
#define _MOVE(__val) (__val)
#define _MOVE_RANGE(__it1,__it2,__in) std::copy(__it1,__it2,__in)
#endif


template<class T>
inline void mark_position(List_Ref<T> &x, List_Ref<int> &markers) {
    int s = x.ref().size();
    if (s!=0 && (markers.ref().empty() || s != markers.ref().back()) )
        markers.ref().push_back(s);
}


// append with no sort Pareto
template<class T, typename Compare>
inline void join_step(List_Ref<T> &answers, typename List_Ref<T>::iterator &i_begin,
        typename List_Ref<T>::iterator &i_end, Compare &c, const bool keep_equal)
{
    
  // basic security tests  
  if (i_begin == i_end)
    return;
  
  if (isEmpty(answers)) {
       _MOVE_RANGE(i_begin, i_end, std::back_inserter(answers.ref()));
      return;
  }
  
  // do the real work
  const int dim = c.dim;
  
  for(typename List_Ref<T>::iterator in = i_begin; in != i_end; in++) {
      
    bool add = true;
    for (typename List_Ref<T>::iterator answer = answers.ref().begin(); answer!=answers.ref().end(); ){
     
      bool less = false;  
      bool better = false;
      for (int i = 1; i<= dim; ++i) {
          int res = c(*answer, *in, i);
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
      answers.ref().push_back(_MOVE(*in));
    } 
  }
  
}


template<class T, typename Compare>
inline void join_marked_multi_to_two_all(List_Ref<T> &x, List_Ref<int> &markers, Compare &c, const bool keep_equal) {
    
    std::deque<List_Ref<T> > merges;
    
    typename List_Ref<T>::iterator s1_start, middle, s2_end;
       
    typename List_Ref<int>::iterator p1 = markers.ref().begin();
    typename List_Ref<int>::iterator p2 = markers.ref().begin();
    typename List_Ref<int>::iterator x_end = markers.ref().end();
    p2++;
    
    s1_start = x.ref().begin();
    int start = 0;
    
    for(; p2 != x_end && p1 != x_end ; p1+=2, p2+=2) {

        // get iterator
        middle = x.ref().begin();
        std::advance(middle, *p1);

        s2_end = x.ref().begin();
        std::advance(s2_end, *p2);
        
        // do the actual join
        List_Ref<T> e;
        merges.push_back(e);
        
        if ( (*p1-start) > (*p2 - *p1) ) { // first list longer than second
            
            _MOVE_RANGE(s1_start, middle, std::back_inserter(merges.back().ref()));
            join_step(merges.back(), middle, s2_end, c, keep_equal);
        } else {
            _MOVE_RANGE(middle, s2_end, std::back_inserter(merges.back().ref()));
            join_step(merges.back(), s1_start, middle, c, keep_equal);
        }
        
        s1_start = s2_end;
        start = *p2;
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
                typename List_Ref<T>::iterator s2_s = m2->ref().begin();
                typename List_Ref<T>::iterator s2_e = m2->ref().end();

                join_step(*m1, s2_s, s2_e, c, keep_equal);
                new_merges.push_back(_MOVE(*m1));
            }

            if (m1 != m_end) {
                new_merges.push_back(_MOVE(*m1));
            }
            
            merges = _MOVE(new_merges);
    }
   
    x = _MOVE(merges.front());
}


template<class T, typename Compare>
inline void join_marked(List_Ref<T> &x, List_Ref<int> &markers, Compare &c, const bool keep_equal)
{
    if (markers.ref().size() <= 1) {
        return;
    }
    
    join_marked_multi_to_two_all(x, markers, c, keep_equal);
    
}

#endif	/* PARETO_0_NOSORT_BLOCK_HH */

