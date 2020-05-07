/* 
 * File:   sort_block.hh
 * Author: gatter
 *
 * Created on June 18, 2015, 4:13 PM
 */

#ifndef SORT_BLOCK_HH
#define	SORT_BLOCK_HH

#include "../list.hh"


#include <boost/shared_ptr.hpp>

#include <math.h>

#include <deque>
#include <queue> 
#include <iostream>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include <boost/timer.hpp>


#if __cplusplus >= 201103L
#define _MOVE(__val) std::move(__val)
#define _MOVE_BACKWARD(_Tp, _Up, _Vp) std::move_backward(_Tp, _Up, _Vp)
#else
#define _MOVE(__val) (__val)
#define _MOVE_BACKWARD(_Tp, _Up, _Vp) std::copy_backward(_Tp, _Up, _Vp)
#endif


// ---------------- JOIN SORT ----------------

template<class T, typename Compare>
inline void join_sort(List_Ref<T> &l, List_Ref<int> &markers, Compare &c) {
    
    	// new list
	List_Ref<T> l2;

	// init array
	typename List_Ref<T>::iterator *itrs = new typename List_Ref<T>::iterator[markers.ref().size()+1];
	typename List_Ref<T>::iterator *ends = new typename List_Ref<T>::iterator[markers.ref().size()+1];

	itrs[0] = l.ref().begin();

	int i = 1;
	for(typename List_Ref<int>::iterator pos = markers.ref().begin(); pos != markers.ref().end(); pos++) {
		typename List_Ref<T>::iterator itr = l.ref().begin();
		std::advance(itr, *pos);
		itrs[i] = itr;
		ends[i-1] = itr;
		i++;
	}
	ends[i-1] = l.ref().end();

	int length = i;

	// do the sorting, yay

	while(true) { // killed by break
		int min_index = -1;
		T min;
		
		for(int j = 0; j < length; j++) {
			if(itrs[j] != ends[j]) {

				if (min_index == -1) {
					min_index = j;
					min = *itrs[j];
					continue;
				}

				if (c(*itrs[j], min)) {
					min_index = j;
					min = *itrs[j];
				}
			}
		}
		
		// exit condition
		if ( min_index == -1) {
			break;
		}

		l2.ref().push_back(_MOVE(*itrs[min_index]));
		itrs[min_index]++;
	}
	
	l = _MOVE(l2);

        delete[] itrs;
        delete[] ends;
}


// ---------------- MULTI ----------------

template<class T, typename Compare>
struct join_marked_multi_comp {

	join_marked_multi_comp(Compare &c) 
		: c(c) {}

        Compare c;
        
	public:
		bool operator () (T c1, T c2) {
			return c(c1, c2);
		}
};


template<class T, typename Compare>
inline void join_marked_multi(List_Ref<T> &l, List_Ref<int> &markers, Compare &c) {

        // first element of each list
    	typename List_Ref<T>::reverse_iterator *starts = new typename List_Ref<T>::reverse_iterator[markers.ref().size()+1];
        // last element of each list, modified during run!
	typename List_Ref<T>::reverse_iterator *ends = new typename List_Ref<T>::reverse_iterator[markers.ref().size()+1];
        
	starts[0] = l.ref().rend();
	int i = 1;
	for(typename List_Ref<int>::iterator pos = markers.ref().begin(); pos != --markers.ref().end(); pos++) {
		typename List_Ref<T>::iterator itr = l.ref().begin();
		std::advance(itr, *pos);
                
		starts[i] = typename List_Ref<T>::reverse_iterator(itr);
		ends[i-1] = typename List_Ref<T>::reverse_iterator(itr);
		i++;
	}
	ends[i-1] = l.ref().rbegin();
        
        // init temp queue
        join_marked_multi_comp<T, Compare> comp2 = join_marked_multi_comp<T, Compare>(c);
	std::vector<T > internal;
	std::priority_queue< T , std::vector<T >, join_marked_multi_comp<T, Compare> > temp_queue = std::priority_queue< T , std::vector<T >, join_marked_multi_comp<T, Compare> >( comp2, internal);
        
        // running iterator to go through list from right to left
        typename List_Ref<T>::reverse_iterator run_itr = l.ref().rbegin();
        // index of the list the running iterator is currently in
        int run_index = i - 1;

	// do the sorting, yay
	while(true) { // killed by break
		int worst_index = -1;
		    
                // only check entries that have not been completely processed yet
		for(int j = 0; j < run_index + 1 ; j++) {
                    
                        // it's possible that lists have already been full processed below the running index
			if(starts[j] != ends[j]) { 
                            
                                // initial condition
				if (worst_index == -1) {
					worst_index = j;
					continue;
				}

                                // compare to get real worst
				if (c(*ends[worst_index], *ends[j])) {
					worst_index = j;
				}
			}
		}

		// do we even still have sublists to insert?
		if ( worst_index == -1) {
                    
                    // exit condition, queue is also empty
                    // if not we still need to move back the elements still in temp_queue
                    while (!temp_queue.empty()) {
                        
                        // move over first queue element
                        *run_itr  = _MOVE(temp_queue.top());
                        temp_queue.pop();
                        
                        // move left in list (reverse iterator!)
                        ++run_itr;
                    }
                    
                    delete[] starts;
                    delete[] ends;
                    return;
		}
                
                
                // so we DO have an index to the worst element
                // now compare it to queue
                if (temp_queue.empty() ||  c(temp_queue.top() , *ends[worst_index]) ) {

                    // no elements in temp_queue or worst_index also worse than queue
                    // worst_index has to be moved to current position
                    // current position has to be queued
                    
                    if ( run_itr !=  ends[worst_index] ) { // no moving if itr matches worst element

                        if (ends[run_index] == run_itr) { 
                            temp_queue.push( _MOVE(*run_itr) );
                             // move end to the left in list (reverse iterator!)
                            ends[run_index]++;
                        }
                        
                        *run_itr = _MOVE(*ends[worst_index]);
                    }
                    
                     
                    // move end to the left in list (reverse iterator!)
                    ends[worst_index]++;

                } else {

                    // element in temp_queue is worst element
                    // insert element into current position
                    // save potential element at this position
                    
                    if (ends[run_index] == run_itr) { 
                        
                        // we are at a position still pointed at, save value
                        temp_queue.push( _MOVE(*run_itr) );
                        
                        // move end to the left in list (reverse iterator!)
                        ends[run_index]++;
                    }
                    
                    *run_itr = _MOVE(temp_queue.top());
                    temp_queue.pop();
                    
                }
                
                // move left in list (reverse iterator!)
                run_itr++;
                
                if (run_itr == starts[run_index]) { // did we pass a list border?
                    run_index--; // if 
                }
                
                if (run_itr == l.ref().rend()) {
                    delete[] starts;
                    delete[] ends;
                    return;
                }

	}
}


// ---------------- TWO with FRONT EXTRA ----------------

template<class T, typename Compare>
inline void join_marked_two_alt(typename List_Ref<T>::iterator start, typename List_Ref<T>::iterator middle,
        typename List_Ref<T>::iterator end, Compare &c) {

        // init temp queue
	std::deque<T> temp_queue;
	           
        
        // move elements smaller than first list in block first
        // we need forward iterator for that
        typename List_Ref<T>::iterator ml = middle;
        
        while (c( *ml, *start)) { // while the start of the right list is smaller than the start of the left
            temp_queue.push_back(_MOVE(*ml));
            
            ml++; // move middle to right
            if (ml == end) { // all elements are smaller than first list!
                _MOVE_BACKWARD(start, middle, ml);
                _MOVE_BACKWARD(temp_queue.begin(), temp_queue.end(), start+(temp_queue.size()));
                return;
            }
        }
        
         typename List_Ref<T>::iterator nstart = start+temp_queue.size();
        
         if (ml != middle) {
            _MOVE_BACKWARD(start, middle, ml);
            _MOVE_BACKWARD(temp_queue.begin(), temp_queue.end(), nstart);
         }

        // running iterator for left sublist only
        typename List_Ref<T>::reverse_iterator cl = typename List_Ref<T>::reverse_iterator(ml);
        
        // end condition for reverse cl 
        typename List_Ref<T>::reverse_iterator rstart = typename List_Ref<T>::reverse_iterator(nstart);
        
        // running iterator to go through whole list from right to left
        typename List_Ref<T>::reverse_iterator run_itr = typename List_Ref<T>::reverse_iterator(end);
        
        // the new created middle
        typename List_Ref<T>::reverse_iterator rmiddle = typename List_Ref<T>::reverse_iterator(ml);
        
        temp_queue.clear();
        
        // find first disagreement
        while (rmiddle!=run_itr && c(*cl, *run_itr)) { // left better than right
                ++run_itr; // one to the left (reverse iterator)
        }
        
        if (run_itr == rmiddle) {
            // already sorted here :)
            return;
        }
        
        // left worse than running (right)
        // get left to end, save running (right) to queue
        temp_queue.push_back(_MOVE(*run_itr));
        *run_itr = _MOVE(*cl);

        // from here on temp_queue is never empty before the end!
        
        ++run_itr; // one to the left (reverse iterator)
        ++cl; // one to the left (reverse iterator)

        if (cl == rstart) { // end condition, cl no longer interesting
            while (run_itr != rmiddle) {
                temp_queue.push_back(_MOVE(*run_itr));
                *run_itr = _MOVE(temp_queue.front());
                temp_queue.pop_front();

                ++run_itr;
            }
            *run_itr = _MOVE(temp_queue.front());
            temp_queue.pop_front();
            return;
        }
        
        // run until middle is reached
        // or start has reached it's end by break
        while (run_itr != rmiddle) {

            // find worst element
            // temp_queue.front() is ALWAYS guaranteed to be worse than run_itr
            if ( c(*cl, temp_queue.front() ) ) {
                // cl better than temp_queue.front() ==> temp_queue.front() is worst

                temp_queue.push_back(_MOVE(*run_itr));
                *run_itr = _MOVE(temp_queue.front());
                temp_queue.pop_front();
                
                ++run_itr; // one to the left (reverse iterator)
                // cl is not changed
            } else {
                // temp_queue.front() is better than cl ==> cl is worst

                temp_queue.push_back(_MOVE(*run_itr));
                *run_itr = _MOVE(*cl);
                
                ++run_itr; // one to the left (reverse iterator)
                ++cl; // one to the left (reverse iterator)
                
                if (cl == rstart) { // end condition, cl no longer interesting
                    while (run_itr != rmiddle) {
                        temp_queue.push_back(_MOVE(*run_itr));
                        *run_itr = _MOVE(temp_queue.front());
                        temp_queue.pop_front();

                        ++run_itr;
                    }
                    break;
                }
            }
        }
        
        while ( cl != rstart && !temp_queue.empty()) {
            
            // find worst element
            // run_itr in invalidated area
            if ( c(*cl, temp_queue.front() ) ) {
                // cl better than temp_queue.front() ==> temp_queue.front() is worst
                
                *run_itr = _MOVE(temp_queue.front());
                temp_queue.pop_front();
                
                ++run_itr; // one to the left (reverse iterator)
                // cl is not changed
            } else {
                // temp_queue.front() is better than cl ==> cl is worst
                
                *run_itr = _MOVE(*cl);
                
                ++run_itr; // one to the left (reverse iterator)
                ++cl; // one to the left (reverse iterator)
                
            }
            
        }
        
        // now write out the remaining elements in temp_queue
        for(typename std::deque<T>::iterator i = temp_queue.begin(); i!= temp_queue.end(); ++i) {
            *run_itr = _MOVE(*i);
            ++run_itr; // one to the left (reverse iterator)
        }

}

template<class T, typename Compare>
inline void join_marked_multi_to_two_merge_array_alt(List_Ref<T> &x, List_Ref<int> &markers, Compare &c) {
    
    if (markers.ref().size()<= 1) { // already sorted
        return;
    }
    
    int length = markers.ref().size() / 2 + markers.ref().size() % 2;
    typename List_Ref<T>::iterator *starts = new typename List_Ref<T>::iterator[length];
    typename List_Ref<T>::iterator *middles = new typename List_Ref<T>::iterator[length];
    typename List_Ref<T>::iterator *ends = new typename List_Ref<T>::iterator[length];
    
    
    typename List_Ref<int>::iterator p1 = markers.ref().begin();
    typename List_Ref<int>::iterator p2 = markers.ref().begin();
    typename List_Ref<int>::iterator x_end = markers.ref().end();
    p2++;
    
    int i=0;
    
    starts[0] = x.ref().begin();
    
    for(; p2 != x_end && p1 != x_end ; p1+=2, p2+=2) {

        middles[i] = x.ref().begin();
        std::advance(middles[i], *p1);

        ends[i] = x.ref().begin();
        std::advance(ends[i], *p2);
        
        ++i;
        
        if ( i < length) {
            starts[i] = ends[i-1];
        }
    }

    if (p1 != x_end) {
        typename List_Ref<T>::iterator end = x.ref().begin();
        std::advance(end, *p1);
        middles[i] = end;
        ends[i] = end;
    }
    
    int merges_left = length;

    while (merges_left > 1) {

        int new_merges_count = 0;

        int j = 0;
        for (j = 0; j+1 <  merges_left; j+=2) {
            
            join_marked_two_alt<T>(starts[j], middles[j], ends[j], c);
            
            // execute p2
            if (middles[j+1] != ends[j+1]) {

                join_marked_two_alt<T>(starts[j+1], middles[j+1], ends[j+1], c);
            }
 
            starts[new_merges_count] = starts[j];
            middles[new_merges_count] = ends[j];
            ends[new_merges_count] = ends[j+1];
            new_merges_count++;
            
        }
        
        if (j < merges_left) {
            starts[new_merges_count] = starts[j];
            middles[new_merges_count] = middles[j];
            ends[new_merges_count] = ends[j];
            new_merges_count++;
        }
        
        merges_left = new_merges_count;
    }
    
    join_marked_two_alt<T>(starts[0], middles[0], ends[0], c);
    
    delete[] starts;
    delete[] middles;
    delete[] ends;
    
}


// ---------------- TWO without in-place ---------------

template<class T, typename Compare>
inline void join_marked_two(typename List_Ref<T>::reverse_iterator start, typename List_Ref<T>::reverse_iterator middle,
        typename List_Ref<T>::reverse_iterator end, Compare &c) {
        
        // init temp queue
	std::deque<T> temp_queue;
	   
      // running iterator for left sublist only
        typename List_Ref<T>::reverse_iterator cl = middle;
        // running iterator to go through whole list from right to left
        typename List_Ref<T>::reverse_iterator run_itr = end;
        
        // find first disagreement
        while (middle!=run_itr && c(*cl, *run_itr)) { // left better than right
                ++run_itr; // one to the left (reverse iterator)
        }
        
        if (run_itr == middle) {
            // already sorted here :)
            return;
        }
        
        // left worse than running (right)
        // get left to end, save running (right) to queue
        temp_queue.push_back(_MOVE(*run_itr));
        *run_itr = _MOVE(*cl);

        // from here on temp_queue is never empty before the end!
        
        ++run_itr; // one to the left (reverse iterator)
        ++cl; // one to the left (reverse iterator)

        if (cl == start) { // end condition, cl no longer interesting
            while (run_itr != middle) {
                temp_queue.push_back(_MOVE(*run_itr));
                *run_itr = _MOVE(temp_queue.front());
                temp_queue.pop_front();

                ++run_itr;
            }
            *run_itr = _MOVE(temp_queue.front());
            temp_queue.pop_front();
            return;
        }
        
        // run until middle is reached
        // or start has reached it's end by break
        while (run_itr != middle) {

            // find worst element
            // temp_queue.front() is ALWAYS guaranteed to be worse than run_itr
            if ( c(*cl, temp_queue.front() ) ) {
                // cl better than temp_queue.front() ==> temp_queue.front() is worst

                temp_queue.push_back(_MOVE(*run_itr));
                *run_itr = _MOVE(temp_queue.front());
                temp_queue.pop_front();
                
                ++run_itr; // one to the left (reverse iterator)
                // cl is not changed
            } else {
                // temp_queue.front() is better than cl ==> cl is worst

                temp_queue.push_back(_MOVE(*run_itr));
                *run_itr = _MOVE(*cl);
                
                ++run_itr; // one to the left (reverse iterator)
                ++cl; // one to the left (reverse iterator)
                
                if (cl == start) { // end condition, cl no longer interesting
                    while (run_itr != middle) {
                        temp_queue.push_back(_MOVE(*run_itr));
                        *run_itr = _MOVE(temp_queue.front());
                        temp_queue.pop_front();

                        ++run_itr;
                    }
                    break;
                }
            }
        }
        
        while ( cl != start && !temp_queue.empty()) {
            
            // find worst element
            // run_itr in invalidated area
            
            if ( c(*cl, temp_queue.front() ) ) {
                // cl better than temp_queue.front() ==> temp_queue.front() is worst
                
                *run_itr = _MOVE(temp_queue.front());
                temp_queue.pop_front();
                
                ++run_itr; // one to the left (reverse iterator)
                // cl is not changed
            } else {
                // temp_queue.front() is better than cl ==> cl is worst
                
                *run_itr = _MOVE(*cl);
                
                ++run_itr; // one to the left (reverse iterator)
                ++cl; // one to the left (reverse iterator)
                
            }
            
        }
        
        // now write out the remaining elements in temp_queue
        for(typename std::deque<T>::iterator i = temp_queue.begin(); i!= temp_queue.end(); ++i) {
            *run_itr = _MOVE(*i);
            ++run_itr; // one to the left (reverse iterator)
        }

}


template<class T, typename Compare>
inline void join_marked_multi_to_two_merge_array(List_Ref<T> &x, List_Ref<int> &markers, Compare &c) {
    
    if (markers.ref().size()<= 1) { // already sorted
        return;
    }
    
    int length = markers.ref().size() / 2 + markers.ref().size() % 2;
    typename List_Ref<T>::iterator *starts = new typename List_Ref<T>::iterator[length];
    typename List_Ref<T>::iterator *middles = new typename List_Ref<T>::iterator[length];
    typename List_Ref<T>::iterator *ends = new typename List_Ref<T>::iterator[length];
    
    
    typename List_Ref<int>::iterator p1 = markers.ref().begin();
    typename List_Ref<int>::iterator p2 = markers.ref().begin();
    typename List_Ref<int>::iterator x_end = markers.ref().end();
    p2++;
    
    int i=0;
    
    starts[0] = x.ref().begin();
    
    for(; p2 != x_end && p1 != x_end ; p1+=2, p2+=2) {

        middles[i] = x.ref().begin();
        std::advance(middles[i], *p1);

        ends[i] = x.ref().begin();
        std::advance(ends[i], *p2);
        
        ++i;
        
        if ( i < length) {
            starts[i] = ends[i-1];
        }
    }

    if (p1 != x_end) {
        typename List_Ref<T>::iterator end = x.ref().begin();
        std::advance(end, *p1);
        middles[i] = end;
        ends[i] = end;
    }
    
    int merges_left = length;

    while (merges_left > 1) {

        int new_merges_count = 0;

        int j = 0;
        for (j = 0; j+1 <  merges_left; j+=2) {
            
            join_marked_two<T>(typename List_Ref<T>::reverse_iterator(starts[j]),
                    typename List_Ref<T>::reverse_iterator(middles[j]),
                    typename List_Ref<T>::reverse_iterator(ends[j]), c);
            
            
            // execute p2
            if (middles[j+1] != ends[j+1]) {

                join_marked_two<T>(typename List_Ref<T>::reverse_iterator(starts[j+1]),
                    typename List_Ref<T>::reverse_iterator(middles[j+1]),
                    typename List_Ref<T>::reverse_iterator(ends[j+1]), c);
            }
 
            starts[new_merges_count] = starts[j];
            middles[new_merges_count] = ends[j];
            ends[new_merges_count] = ends[j+1];
            new_merges_count++;
            
        }
        
        if (j < merges_left) {
            starts[new_merges_count] = starts[j];
            middles[new_merges_count] = middles[j];
            ends[new_merges_count] = ends[j];
            new_merges_count++;
        }
        
        merges_left = new_merges_count;
    }
    
    join_marked_two<T>(typename List_Ref<T>::reverse_iterator(starts[0]),
                    typename List_Ref<T>::reverse_iterator(middles[0]),
                    typename List_Ref<T>::reverse_iterator(ends[0]), c);
    
    delete[] starts;
    delete[] middles;
    delete[] ends;
    
}

// ---------------- TWO with c++ in-place ---------------

template<class T, typename Compare>
inline void join_marked_multi_to_two_merge_array_c(List_Ref<T> &x, List_Ref<int> &markers, Compare &c) {
    
    if (markers.ref().size()<= 1) { // already sorted
        return;
    }
    
    int length = markers.ref().size() / 2 + markers.ref().size() % 2;
    typename List_Ref<T>::iterator *starts = new typename List_Ref<T>::iterator[length];
    typename List_Ref<T>::iterator *middles = new typename List_Ref<T>::iterator[length];
    typename List_Ref<T>::iterator *ends = new typename List_Ref<T>::iterator[length];
    
    
    typename List_Ref<int>::iterator p1 = markers.ref().begin();
    typename List_Ref<int>::iterator p2 = markers.ref().begin();
    typename List_Ref<int>::iterator x_end = markers.ref().end();
    p2++;
    
    int i=0;
    
    starts[0] = x.ref().begin();
    
    for(; p2 != x_end && p1 != x_end ; p1+=2, p2+=2) {

        middles[i] = x.ref().begin();
        std::advance(middles[i], *p1);

        ends[i] = x.ref().begin();
        std::advance(ends[i], *p2);
        
        ++i;
        
        if ( i < length) {
            starts[i] = ends[i-1];
        }
    }

    if (p1 != x_end) {
        typename List_Ref<T>::iterator end = x.ref().begin();
        std::advance(end, *p1);
        middles[i] = end;
        ends[i] = end;
    }
    
    int merges_left = length;

    while (merges_left > 1) {

        int new_merges_count = 0;

        int j = 0;
        for (j = 0; j+1 <  merges_left; j+=2) {
            
            
            // execute 1
            std::inplace_merge(starts[j], middles[j], ends[j], c);
            
            
            // execute 2
            if (middles[j+1] != ends[j+1]) {
               std::inplace_merge(starts[j+1], middles[j+1], ends[j+1], c);
            }
 
            starts[new_merges_count] = starts[j];
            middles[new_merges_count] = ends[j];
            ends[new_merges_count] = ends[j+1];
            new_merges_count++;
            
        }
        
        if (j < merges_left) {
            starts[new_merges_count] = starts[j];
            middles[new_merges_count] = middles[j];
            ends[new_merges_count] = ends[j];
            new_merges_count++;
        }
        
        merges_left = new_merges_count;
    }
    
    std::inplace_merge(starts[0], middles[0], ends[0], c);
    
    delete[] starts;
    delete[] middles;
    delete[] ends;
   
}

// ---------------- Join Queue  ---------------

template<class T, typename Compare>
struct s_int_comp {

	s_int_comp(typename std::deque<T>::iterator *itrs, Compare &c) 
		: itrs(itrs), c(c) {}

	typename std::deque<T>::iterator *itrs;
        Compare c;

	public:
		bool operator () (int c1, int c2) {
			return !c(*itrs[c1], *itrs[c2]);
		}
};


template<class T, typename Compare>
void join_sort_queue(List_Ref<T> &l, List_Ref<int> &markers, Compare &c) {
	
	// new list
	List_Ref<T> l2;

	// init array
	typename List_Ref<T>::iterator *itrs = new typename List_Ref<T>::iterator[markers.ref().size()+1];
	typename List_Ref<T>::iterator *ends = new typename List_Ref<T>::iterator[markers.ref().size()+1];

	s_int_comp<T, Compare> comp = s_int_comp<T, Compare>(itrs, c);

	// init array
	std::vector<int > internal;
	internal.reserve(markers.ref().size()+1);
	std::priority_queue< int , std::vector<int >, s_int_comp<T, Compare> > pq = std::priority_queue< int , std::vector<int >, s_int_comp<T, Compare> >( comp, internal);

	// init priority queue
	itrs[0] = l.ref().begin();

	int i = 1;
	for(typename List_Ref<int>::iterator pos = markers.ref().begin(); pos != markers.ref().end(); pos++) {
		typename List_Ref<T>::iterator itr = l.ref().begin();
		std::advance(itr, *pos);

		ends[i-1] = itr;

		if (itrs[i-1] != ends[i-1]) {
			pq.push(i-1);
		}

		itrs[i] = itr;		
		i++;
	}
	ends[i-1] = l.ref().end();
	if (itrs[i-1] != ends[i-1]) {
		pq.push(i-1);
	}

	// do the sorting, yay
	while(!pq.empty()) { 
		
		int it = pq.top();
		pq.pop();

		l2.ref().push_back(_MOVE(*itrs[it]));
		itrs[it]++;

		if (itrs[it] != ends[it]) {
			pq.push(it);
		}
	}
	
	l = _MOVE(l2);	
        
        delete[] itrs;
        delete[] ends;
}

// ---------------- Full Sort  ---------------

template<class T, typename Compare>
inline void full_sort(List_Ref<T> &x, Compare &c) {
    //s_sorter<T> sortob = s_sorter<T>(dim,c);
    std::sort(x.ref().begin(), x.ref().end(), c);
}


// ---------------- MARK  ---------------

template<class T>
inline void mark_position(List_Ref<T> &x, List_Ref<int> &markers) {
    int s = x.ref().size();
    if (s!=0 && (markers.ref().empty() || s != markers.ref().back()) )
        markers.ref().push_back(s);
}

// ---------------- LOG  ---------------

extern const float switch_base;

inline float fast_log2 (float val)
{
  // assert (val > 0);

   int * const  exp_ptr = reinterpret_cast <int *> (&val);
   int          x = *exp_ptr;
   const int    log_2 = ((x >> 23) & 255) - 128;
   x &= ~(255 << 23);
   x += 127 << 23;
   *exp_ptr = x;

   return (val + log_2);
}

inline float fast_log (const float &val)
{
   return (fast_log2 (val) * 1.07991428f);
} 


// ---------------- MAIN  ---------------

template<class T, typename Compare>
inline void join_marked(List_Ref<T> &x, List_Ref<int> &markers, Compare &c)
{
    if (markers.ref().size() <= 1) {
        return;
    }
    
    // create copies
    
//    List_Ref<T > l1;	
//    List_Ref<int> ends1;
//    std::copy(x.ref().begin(), x.ref().end(), std::back_inserter(l1.ref()));
//    std::copy(markers.ref().begin(), markers.ref().end(), std::back_inserter(ends1.ref()));
//    
//    List_Ref<T > l2;	
//    List_Ref<int> ends2;
//    std::copy(x.ref().begin(), x.ref().end(), std::back_inserter(l2.ref()));
//    std::copy(markers.ref().begin(), markers.ref().end(), std::back_inserter(ends2.ref()));
//
//    List_Ref<T > l3;	
//    List_Ref<int> ends3;
//    std::copy(x.ref().begin(), x.ref().end(), std::back_inserter(l3.ref()));
//    std::copy(markers.ref().begin(), markers.ref().end(), std::back_inserter(ends3.ref()));
//    
//    List_Ref<T > l4;	
//    List_Ref<int> ends4;
//    std::copy(x.ref().begin(), x.ref().end(), std::back_inserter(l4.ref()));
//    std::copy(markers.ref().begin(), markers.ref().end(), std::back_inserter(ends4.ref()));   
//    
//    List_Ref<T > l5;	
//    List_Ref<int> ends5;
//    std::copy(x.ref().begin(), x.ref().end(), std::back_inserter(l5.ref()));
//    std::copy(markers.ref().begin(), markers.ref().end(), std::back_inserter(ends5.ref()));    
//    
//    List_Ref<T > l6;	
//    List_Ref<int> ends6;
//    std::copy(x.ref().begin(), x.ref().end(), std::back_inserter(l6.ref()));
//    std::copy(markers.ref().begin(), markers.ref().end(), std::back_inserter(ends6.ref()));    
//    
//    List_Ref<T > l7;	
//    List_Ref<int> ends7;
//    std::copy(x.ref().begin(), x.ref().end(), std::back_inserter(l7.ref()));
//    std::copy(markers.ref().begin(), markers.ref().end(), std::back_inserter(ends7.ref()));       
//    
//    
//    boost::timer t;
//    join_marked_multi(l1, ends1, c);
//    double t1 = t.elapsed();
//
//    boost::timer u;
//    join_sort(l2, ends2, c);
//    double t2 = u.elapsed();
//
//    boost::timer z;
//    join_sort_queue(l7, ends7, c);
//    double t7 = z.elapsed();
//    
//    boost::timer v;
//    join_marked_multi_to_two_merge_array(l3, ends3, c);
//    double t3 = v.elapsed(); 
//
//    boost::timer w;
//    join_marked_multi_to_two_merge_array_alt(l4, ends4, c);
//    double t4 = w.elapsed();
//
//    boost::timer x2;
//    join_marked_multi_to_two_merge_array_c(l5, ends5, c);
//    double t5 = x2.elapsed();
//
//    boost::timer y;
//    full_sort(l6, c);
//    double t6 = y.elapsed();
//
//    
//     std::cerr << l1.ref().size() << " " << ends1.ref().size() 
//            << " MULTI " << t1  
//            << " JOIN " << t2 
//            << " JOIND " << t7 
//            << " MERGEA " << t3 
//            << " MERGEB " << t4  
//            << " MERGEC " << t5 
//            << " SORT " << t6                    
//            << " ";
//
//    double min = std::min(t1, std::min(t2, std::min(t3, std::min(t4, std::min(t5, std::min(t6, t7))))));
//
//    if(t1 == min) std::cerr << "m";
//    if(t2 == min) std::cerr << "j";
//    if(t7 == min) std::cerr << "d";
//    if(t3 == min) std::cerr << "a";
//    if(t4 == min) std::cerr << "b";
//    if(t5 == min) std::cerr << "c";
//    if(t6 == min) std::cerr << "s";
//    std::cerr << std::endl;
//
//
//    if (! std::is_sorted(l1.ref().begin(),l1.ref().end(), c)) std::cerr << " FAIL 1" << std::endl;
//    if (! std::is_sorted(l2.ref().begin(),l2.ref().end(), c)) std::cerr << " FAIL 2" << std::endl;
//    if (! std::is_sorted(l3.ref().begin(),l3.ref().end(), c)) std::cerr << " FAIL 3" << std::endl;
//    if (! std::is_sorted(l4.ref().begin(),l4.ref().end(), c)) std::cerr << " FAIL 4" << std::endl;
//    if (! std::is_sorted(l5.ref().begin(),l5.ref().end(), c)) std::cerr << " FAIL 5" << std::endl;
//    if (! std::is_sorted(l6.ref().begin(),l6.ref().end(), c)) std::cerr << " FAIL 6" << std::endl;
//    if (! std::is_sorted(l7.ref().begin(),l7.ref().end(), c)) std::cerr << " FAIL 7" << std::endl;
    

      join_marked_multi_to_two_merge_array_c(x, markers, c);

}


#endif	/* SORT_BLOCK_HH */

