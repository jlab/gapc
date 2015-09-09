/* 
 * File:   pareto_dom_sort.hh
 * Author: gatter
 *
 * Created on September 9, 2015, 9:09 AM
 */

#ifndef PARETO_DOM_SORT_HH
#define	PARETO_DOM_SORT_HH


template<class T, typename Iterator, typename Compare>
void pareto_domination_sort(List_Ref<T> &answers, Iterator begin, Iterator end, Compare &c)
{
   
    if(begin==end) {
        return;
    }
    
//   std::cout << "IN --------" << std::endl;
//   for (Iterator i = begin; i!=end; ++i){
//       std::cout << *i << std::endl;
//   } 
//    
//   std::cout << "--------" << std::endl; 
    
   const int dim = c.dim;
  
  Iterator m1 = begin;
  std::advance(m1, std::distance(begin, end) / 2);
   
  Iterator m2 = m1;
  
  bool left = true;
  bool right = true;
  
  if(m2 == begin) {
      left = false;
  }
  
  while(left || right) {
       
    if(left) {
        
//        std::cout << "LEFT" << std::endl;

        Iterator m = m2;
        --m;
        
        bool add = true;
        for (typename List_Ref<T>::iterator answer = answers.ref().begin(); answer!=answers.ref().end(); ){

          bool less = false;  
          bool better = false;
          for (int i = 1; i<= dim; ++i) {
              int res = c(*answer, *m, i);
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
          } else if (better) { // answer is always better or equal or all values equal
              add = false;
              break;
          } else { // less && !better
              // remove from answer list
              answer = erase_element(answers, answer);
          }
        }

        if (add == true)
        {
          answers.ref().push_back(*m);
        }
        
        --m2;
        if (m2 == begin) {
            left = false;
        }
        
    }
    
    if(right) {  
        
//        std::cout << "RIGHT" << std::endl;
        
        bool add = true;
        for (typename List_Ref<T>::iterator answer = answers.ref().begin(); answer!=answers.ref().end(); ){

          bool less = false;  
          bool better = false;
          for (int i = 1; i<= dim; ++i) {
              int res = c(*answer, *m1, i);
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
          } else if (better) { // answer is always better or equal or all values equal
              add = false;
              break;
          } else { // less && !better
              // remove from answer list
              answer = erase_element(answers, answer);
          }
        }

        if (add == true)
        {
          answers.ref().push_back(*m1);
        }
        
        ++m1;
        if (m1 == end) {
            right = false;
        }
    }

    
  }    

   
}




#endif	/* PARETO_DOM_SORT_HH */

