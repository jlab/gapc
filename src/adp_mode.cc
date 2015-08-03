
#include "adp_mode.hh"

bool ADP_Mode::is_step(Adp_Specialization s) {
    return s == SORTED_STEP || s == PARETO_EAGER_STEP;
}

bool ADP_Mode::is_coopt_param(Adp_Specialization s) {
    return s == PARETO_EAGER_STEP || s == PARETO_EAGER_BLOCK;
} 