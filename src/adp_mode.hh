

#ifndef ADP_MODE_HH
#define	ADP_MODE_HH

namespace ADP_Mode {
    
    enum Adp_Specialization { STANDARD, SORTED_STEP, SORTED_BLOCK, PARETO_EAGER_STEP, PARETO_EAGER_BLOCK };
    
    enum Adp_Join { EMPTY, SORTER, COMPERATOR, SORTER_COMPERATOR};
    
    extern bool is_step(Adp_Specialization s);
    
    extern bool is_coopt_param(Adp_Specialization s);
    
    enum Rtlib_Header { NONE, PARETO_NOSORT_STEP, PARETO_NOSORT_BLOCK,
        PARETO_SORT_STEP, PARETO_SORT_BLOCK, PARETO_YUK_STEP, PARETO_YUK_BLOCK,
        SORT_BLOCK, SORT_STEP};
}


#endif	/* ADP_MODE_HH */

