#ifndef VIENNA_RNA_PACKAGE_CONSTRAINTS_SOFT_H
#define VIENNA_RNA_PACKAGE_CONSTRAINTS_SOFT_H

/**
 *  @file     constraints/soft.h
 *  @ingroup  soft_constraints
 *  @brief    Functions and data structures for secondary structure soft constraints
 */


/**
 *
 *  @addtogroup soft_constraints
 *  @brief      Functions and data structures for secondary structure soft constraints
 *
 *  Soft-constraints are used to change position specific contributions
 *  in the recursions by adding bonuses/penalties in form of pseudo free energies
 *  to certain loop configurations.
 *
 */


/** @brief Typename for the soft constraints data structure #vrna_sc_s
 *  @ingroup  soft_constraints
 */
typedef struct  vrna_sc_s vrna_sc_t;

#include <ViennaRNA/datastructures/basic.h>
#include <ViennaRNA/fold_compound.h>
#include <ViennaRNA/constraints/basic.h>

/**
 * @brief Callback to retrieve pseudo energy contribution for soft constraint feature
 *
 * @ingroup soft_constraints
 *
 * This is the prototype for callback functions used by the folding recursions to evaluate generic
 * soft constraints. The first four parameters passed indicate the delimiting nucleotide positions
 * of the decomposition, and the parameter @p denotes the decomposition step. The last parameter
 * @p data is the auxiliary data structure associated to the hard constraints via vrna_sc_add_data(),
 * or NULL if no auxiliary data was added.
 *
 * @callback
 * @parblock
 * This callback enables one to add (pseudo-)energy contributions to individual decompositions
 * of the secondary structure.
 * @endparblock
 *
 * @see #VRNA_DECOMP_PAIR_HP, #VRNA_DECOMP_PAIR_IL, #VRNA_DECOMP_PAIR_ML, #VRNA_DECOMP_ML_ML_ML,
 *      #VRNA_DECOMP_ML_STEM, #VRNA_DECOMP_ML_ML, #VRNA_DECOMP_ML_UP, #VRNA_DECOMP_ML_ML_STEM,
 *      #VRNA_DECOMP_ML_COAXIAL, #VRNA_DECOMP_EXT_EXT, #VRNA_DECOMP_EXT_UP, #VRNA_DECOMP_EXT_STEM,
 *      #VRNA_DECOMP_EXT_EXT_EXT, #VRNA_DECOMP_EXT_STEM_EXT, #VRNA_DECOMP_EXT_EXT_STEM,
 *      #VRNA_DECOMP_EXT_EXT_STEM1, vrna_sc_add_f(), vrna_sc_add_exp_f(), vrna_sc_add_bt(),
 *      vrna_sc_add_data()
 *
 * @param i         Left (5') delimiter position of substructure
 * @param j         Right (3') delimiter position of substructure
 * @param k         Left delimiter of decomposition
 * @param l         Right delimiter of decomposition
 * @param d         Decomposition step indicator
 * @param data      Auxiliary data
 * @return          Pseudo energy contribution in deka-kalories per mol
 */
typedef int (vrna_callback_sc_energy)(int           i,
                                      int           j,
                                      int           k,
                                      int           l,
                                      unsigned char d,
                                      void          *data);

/**
 * @brief Callback to retrieve pseudo energy contribution as Boltzmann Factors for soft constraint feature
 *
 * @ingroup soft_constraints
 *
 * This is the prototype for callback functions used by the partition function recursions to evaluate generic
 * soft constraints. The first four parameters passed indicate the delimiting nucleotide positions
 * of the decomposition, and the parameter @p denotes the decomposition step. The last parameter
 * @p data is the auxiliary data structure associated to the hard constraints via vrna_sc_add_data(),
 * or NULL if no auxiliary data was added.
 *
 * @callback
 * @parblock
 * This callback enables one to add (pseudo-)energy contributions to individual decompositions
 * of the secondary structure (Partition function variant, i.e. contributions must be returned as Boltzmann factors).
 * @endparblock
 *
 * @see #VRNA_DECOMP_PAIR_HP, #VRNA_DECOMP_PAIR_IL, #VRNA_DECOMP_PAIR_ML, #VRNA_DECOMP_ML_ML_ML,
 *      #VRNA_DECOMP_ML_STEM, #VRNA_DECOMP_ML_ML, #VRNA_DECOMP_ML_UP, #VRNA_DECOMP_ML_ML_STEM,
 *      #VRNA_DECOMP_ML_COAXIAL, #VRNA_DECOMP_EXT_EXT, #VRNA_DECOMP_EXT_UP, #VRNA_DECOMP_EXT_STEM,
 *      #VRNA_DECOMP_EXT_EXT_EXT, #VRNA_DECOMP_EXT_STEM_EXT, #VRNA_DECOMP_EXT_EXT_STEM,
 *      #VRNA_DECOMP_EXT_EXT_STEM1, vrna_sc_add_exp_f(), vrna_sc_add_f(), vrna_sc_add_bt(),
 *      vrna_sc_add_data()
 *
 * @param i         Left (5') delimiter position of substructure
 * @param j         Right (3') delimiter position of substructure
 * @param k         Left delimiter of decomposition
 * @param l         Right delimiter of decomposition
 * @param d         Decomposition step indicator
 * @param data      Auxiliary data
 * @return          Pseudo energy contribution in deka-kalories per mol
 */
typedef FLT_OR_DBL (vrna_callback_sc_exp_energy)(int            i,
                                                 int            j,
                                                 int            k,
                                                 int            l,
                                                 unsigned char  d,
                                                 void           *data);

/**
 * @brief Callback to retrieve auxiliary base pairs for soft constraint feature
 *
 * @ingroup soft_constraints
 *
 * @callback
 * @parblock
 * This callback enables one to add auxiliary base pairs in the backtracking steps
 * of hairpin- and interior loops.
 * @endparblock
 *
 * @see #VRNA_DECOMP_PAIR_HP, #VRNA_DECOMP_PAIR_IL, #VRNA_DECOMP_PAIR_ML, #VRNA_DECOMP_ML_ML_ML,
 *      #VRNA_DECOMP_ML_STEM, #VRNA_DECOMP_ML_ML, #VRNA_DECOMP_ML_UP, #VRNA_DECOMP_ML_ML_STEM,
 *      #VRNA_DECOMP_ML_COAXIAL, #VRNA_DECOMP_EXT_EXT, #VRNA_DECOMP_EXT_UP, #VRNA_DECOMP_EXT_STEM,
 *      #VRNA_DECOMP_EXT_EXT_EXT, #VRNA_DECOMP_EXT_STEM_EXT, #VRNA_DECOMP_EXT_EXT_STEM,
 *      #VRNA_DECOMP_EXT_EXT_STEM1, vrna_sc_add_bt(), vrna_sc_add_f(), vrna_sc_add_exp_f(),
 *      vrna_sc_add_data()
 *
 * @param i         Left (5') delimiter position of substructure
 * @param j         Right (3') delimiter position of substructure
 * @param k         Left delimiter of decomposition
 * @param l         Right delimiter of decomposition
 * @param d         Decomposition step indicator
 * @param data      Auxiliary data
 * @return          List of additional base pairs
 */
typedef vrna_basepair_t *(vrna_callback_sc_backtrack)(int           i,
                                                      int           j,
                                                      int           k,
                                                      int           l,
                                                      unsigned char d,
                                                      void          *data);


/**
 *  @brief  The type of a soft constraint
 */
typedef enum {
  VRNA_SC_DEFAULT,  /**<  @brief  Default Soft Constraints */
  VRNA_SC_WINDOW    /**<  @brief  Soft Constraints suitable for local structure prediction using
                     *    window approach.
                     *    @see    vrna_mfe_window(), vrna_mfe_window_zscore(), pfl_fold()
                     */
} vrna_sc_type_e;


/**
 *  @brief  A base pair constraint
 */
typedef struct {
  unsigned int interval_start;
  unsigned int interval_end;
  int          e;
} vrna_sc_bp_storage_t;


/**
 *  @brief  The soft constraints data structure
 *
 *  @ingroup soft_constraints
 */
struct vrna_sc_s {
  const vrna_sc_type_e  type;
  unsigned int          n;

  unsigned char         state;

  int                   **energy_up;      /**<  @brief Energy contribution for stretches of unpaired nucleotides */
  FLT_OR_DBL            **exp_energy_up;  /**<  @brief Boltzmann Factors of the energy contributions for unpaired sequence stretches */

  int                   *up_storage;      /**<  @brief  Storage container for energy contributions per unpaired nucleotide */
  vrna_sc_bp_storage_t  **bp_storage;     /**<  @brief  Storage container for energy contributions per base pair */

#ifndef VRNA_DISABLE_C11_FEATURES
  /* C11 support for unnamed unions/structs */
  union {
    struct {
#endif
      int *energy_bp;                           /**<  @brief Energy contribution for base pairs */
      FLT_OR_DBL *exp_energy_bp;                /**<  @brief Boltzmann Factors of the energy contribution for base pairs */
#ifndef VRNA_DISABLE_C11_FEATURES
  /* C11 support for unnamed unions/structs */
    };
    struct {
#endif
      int         **energy_bp_local;                    /**<  @brief Energy contribution for base pairs (sliding window approach) */
      FLT_OR_DBL  **exp_energy_bp_local;                /**<  @brief Boltzmann Factors of the energy contribution for base pairs (sliding window approach) */
#ifndef VRNA_DISABLE_C11_FEATURES
  /* C11 support for unnamed unions/structs */
    };
  };
#endif

  int                         *energy_stack;      /**<  @brief Pseudo Energy contribution per base pair involved in a stack */
  FLT_OR_DBL                  *exp_energy_stack;  /**<  @brief Boltzmann weighted pseudo energy contribution per nucleotide involved in a stack */

  /* generic soft contraints below */
  vrna_callback_sc_energy     *f;     /**<  @brief  A function pointer used for pseudo
                                       *            energy contribution in MFE calculations
                                       *    @see    vrna_sc_add_f()
                                       */

  vrna_callback_sc_backtrack  *bt;    /**<  @brief  A function pointer used to obtain backtraced
                                       *            base pairs in loop regions that were altered
                                       *            by soft constrained pseudo energy contributions
                                       *    @see    vrna_sc_add_bt()
                                       */

  vrna_callback_sc_exp_energy *exp_f; /**<  @brief  A function pointer used for pseudo energy
                                       *            contribution boltzmann factors in PF
                                       *            calculations
                                       *    @see    vrna_sc_add_exp_f()
                                       */

  void                        *data;  /**<  @brief  A pointer to the data object provided for
                                       *            for pseudo energy contribution functions of the
                                       *            generic soft constraints feature
                                       */
  vrna_callback_free_auxdata  *free_data;
};

/**
 *  @brief Initialize an empty soft constraints data structure within a #vrna_fold_compound_t
 *
 *  This function adds a proper soft constraints data structure
 *  to the #vrna_fold_compound_t data structure.
 *  If soft constraints already exist within the fold compound, they are removed.
 *
 *  \note Accepts vrna_fold_compound_t of type #VRNA_FC_TYPE_SINGLE and #VRNA_FC_TYPE_COMPARATIVE
 *
 *  @ingroup  soft_constraints
 *
 *  @see  vrna_sc_set_bp(), vrna_sc_set_up(), vrna_sc_add_SHAPE_deigan(),
 *        vrna_sc_add_SHAPE_zarringhalam(), vrna_sc_remove(), vrna_sc_add_f(),
 *        vrna_sc_add_exp_f(), vrna_sc_add_pre(), vrna_sc_add_post()
 *  @param  vc  The #vrna_fold_compound_t where an empty soft constraint feature is to be added to
 */
void vrna_sc_init(vrna_fold_compound_t *vc);


void
vrna_sc_prepare(vrna_fold_compound_t  *vc,
                unsigned int          options);


int
vrna_sc_update(vrna_fold_compound_t *vc,
               unsigned int         i,
               unsigned int         options);


/**
 *  @brief  Set soft constraints for paired nucleotides
 *
 *  @note     This function replaces any pre-exisitng soft constraints with the ones supplied
 *            in @p constraints.
 *
 *  @ingroup  soft_constraints
 *
 *  @see      vrna_sc_add_bp(), vrna_sc_set_up(), vrna_sc_add_up()
 *
 *  @param  vc          The #vrna_fold_compound_t the soft constraints are associated with
 *  @param  constraints A two-dimensional array of pseudo free energies in @f$ kcal / mol @f$
 *  @param  options     The options flag indicating how/where to store the soft constraints
 *  @return             Non-zero on successful application of the constraint, 0 otherwise.
 */
int
vrna_sc_set_bp(vrna_fold_compound_t  *vc,
               const FLT_OR_DBL      **constraints,
               unsigned int          options);


/**
 *  @brief  Add soft constraints for paired nucleotides
 *
 *  @ingroup  soft_constraints
 *
 *  @see      vrna_sc_set_bp(), vrna_sc_set_up(), vrna_sc_add_up()
 *
 *  @param  vc          The #vrna_fold_compound_t the soft constraints are associated with
 *  @param  i           The 5' position of the base pair the soft constraint is added for
 *  @param  j           The 3' position of the base pair the soft constraint is added for
 *  @param  energy      The free energy (soft-constraint) in @f$ kcal / mol @f$
 *  @param  options     The options flag indicating how/where to store the soft constraints
 *  @return             Non-zero on successful application of the constraint, 0 otherwise.
 */
int
vrna_sc_add_bp(vrna_fold_compound_t  *vc,
               int                   i,
               int                   j,
               FLT_OR_DBL            energy,
               unsigned int          options);


/**
 *  @brief  Set soft constraints for unpaired nucleotides
 *
 *  @note     This function replaces any pre-exisitng soft constraints with the ones supplied
 *            in @p constraints.
 *
 *  @ingroup  soft_constraints
 *
 *  @see      vrna_sc_add_up(), vrna_sc_set_bp(), vrna_sc_add_bp()
 *
 *  @param  vc          The #vrna_fold_compound_t the soft constraints are associated with
 *  @param  constraints A vector of pseudo free energies in @f$ kcal / mol @f$
 *  @param  options     The options flag indicating how/where to store the soft constraints
 *  @return             Non-zero on successful application of the constraint, 0 otherwise.
 */
int
vrna_sc_set_up(vrna_fold_compound_t  *vc,
               const FLT_OR_DBL      *constraints,
               unsigned int          options);


/**
 *  @brief  Add soft constraints for unpaired nucleotides
 *
 *  @ingroup  soft_constraints
 *
 *  @see      vrna_sc_set_up(), vrna_sc_add_bp(), vrna_sc_set_bp()
 *
 *  @param  vc          The #vrna_fold_compound_t the soft constraints are associated with
 *  @param  i           The nucleotide position the soft constraint is added for
 *  @param  energy      The free energy (soft-constraint) in @f$ kcal / mol @f$
 *  @param  options     The options flag indicating how/where to store the soft constraints
 *  @return             Non-zero on successful application of the constraint, 0 otherwise.
 */
int
vrna_sc_add_up(vrna_fold_compound_t  *vc,
               int                   i,
               FLT_OR_DBL            energy,
               unsigned int          options);


int
vrna_sc_set_stack(vrna_fold_compound_t *vc,
                  const FLT_OR_DBL     *constraints,
                  unsigned int         options);


int
vrna_sc_set_stack_comparative(vrna_fold_compound_t  *fc,
                              const FLT_OR_DBL      **constraints,
                              unsigned int          options);


int
vrna_sc_add_stack(vrna_fold_compound_t *vc,
                  int                  i,
                  FLT_OR_DBL           energy,
                  unsigned int         options);


int
vrna_sc_add_stack_comparative(vrna_fold_compound_t  *fc,
                              int                   i,
                              const FLT_OR_DBL      *energies,
                              unsigned int          options);


/**
 *  @brief  Remove soft constraints from #vrna_fold_compound_t
 *
 *  @note Accepts vrna_fold_compound_t of type #VRNA_FC_TYPE_SINGLE and #VRNA_FC_TYPE_COMPARATIVE
 *
 *  @ingroup  soft_constraints
 *
 *  @param  vc  The #vrna_fold_compound_t possibly containing soft constraints
 */
void
vrna_sc_remove(vrna_fold_compound_t *vc);


/**
 *  @brief  Free memory occupied by a #vrna_sc_t data structure
 *
 *  @ingroup  soft_constraints
 *
 *  @param  sc  The data structure to free from memory
 */
void
vrna_sc_free(vrna_sc_t *sc);


/**
 *  @brief Add an auxiliary data structure for the generic soft constraints callback function
 *
 *  @ingroup soft_constraints
 *
 *  @see vrna_sc_add_f(), vrna_sc_add_exp_f(), vrna_sc_add_bt()
 *
 *  @param  vc        The fold compound the generic soft constraint function should be bound to
 *  @param  data      A pointer to the data structure that holds required data for function 'f'
 *  @param  free_data A pointer to a function that free's the memory occupied by @p data (Maybe NULL)
 *  @return           Non-zero on successful binding the data (and free-function), 0 otherwise
 */
int
vrna_sc_add_data(vrna_fold_compound_t       *vc,
                 void                       *data,
                 vrna_callback_free_auxdata *free_data);


int
vrna_sc_add_data_comparative(vrna_fold_compound_t       *vc,
                             void                       **data,
                             vrna_callback_free_auxdata **free_data);


/**
 *  @brief  Bind a function pointer for generic soft constraint feature (MFE version)
 *
 *  This function allows one to easily bind a function pointer and corresponding data structure
 *  to the soft constraint part #vrna_sc_t of the #vrna_fold_compound_t.
 *  The function for evaluating the generic soft constraint feature has to return
 *  a pseudo free energy @f$ \hat{E} @f$ in @f$ dacal/mol @f$, where @f$ 1 dacal/mol = 10 cal/mol @f$.
 *
 *  @ingroup soft_constraints
 *
 *  @see vrna_sc_add_data(), vrna_sc_add_bt(), vrna_sc_add_exp_f()
 *
 *  @param  vc    The fold compound the generic soft constraint function should be bound to
 *  @param  f     A pointer to the function that evaluates the generic soft constraint feature
 *  @return       Non-zero on successful binding the callback function, 0 otherwise
 */
int
vrna_sc_add_f(vrna_fold_compound_t    *vc,
              vrna_callback_sc_energy *f);


int
vrna_sc_add_f_comparative(vrna_fold_compound_t    *vc,
                          vrna_callback_sc_energy **f);


/**
 *  @brief  Bind a backtracking function pointer for generic soft constraint feature
 *
 *  This function allows one to easily bind a function pointer to the soft constraint part
 *  #vrna_sc_t of the #vrna_fold_compound_t.
 *  The provided function should be used for backtracking purposes in loop regions
 *  that were altered via the generic soft constraint feature. It has to return
 *  an array of #vrna_basepair_t data structures, were the last element in the list is indicated
 *  by a value of -1 in it's i position.
 *
 *  @ingroup soft_constraints
 *
 *  @see vrna_sc_add_data(), vrna_sc_add_f(), vrna_sc_add_exp_f()
 *
 *  @param  vc    The fold compound the generic soft constraint function should be bound to
 *  @param  f     A pointer to the function that returns additional base pairs
 *  @return       Non-zero on successful binding the callback function, 0 otherwise
 */
int
vrna_sc_add_bt(vrna_fold_compound_t       *vc,
               vrna_callback_sc_backtrack *f);


/**
 *  @brief  Bind a function pointer for generic soft constraint feature (PF version)
 *
 *  This function allows one to easily bind a function pointer and corresponding data structure
 *  to the soft constraint part #vrna_sc_t of the #vrna_fold_compound_t.
 *  The function for evaluating the generic soft constraint feature has to return
 *  a pseudo free energy @f$ \hat{E} @f$ as Boltzmann factor, i.e. @f$ exp(- \hat{E} / kT) @f$.
 *  The required unit for @f$ E @f$ is @f$ cal/mol @f$.
 *
 *  @ingroup soft_constraints
 *
 *  @see vrna_sc_add_bt(), vrna_sc_add_f(), vrna_sc_add_data()
 *
 *  @param  vc    The fold compound the generic soft constraint function should be bound to
 *  @param  exp_f A pointer to the function that evaluates the generic soft constraint feature
 *  @return       Non-zero on successful binding the callback function, 0 otherwise
 */
int
vrna_sc_add_exp_f(vrna_fold_compound_t        *vc,
                  vrna_callback_sc_exp_energy *exp_f);


int
vrna_sc_add_exp_f_comparative(vrna_fold_compound_t        *vc,
                              vrna_callback_sc_exp_energy **exp_f);


#endif
