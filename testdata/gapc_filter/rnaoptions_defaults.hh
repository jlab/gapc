#ifndef RNAOPTIONS_DEFAULTS_HH
#define RNAOPTIONS_DEFAULTS_HH

#ifdef WITH_RNAOPTIONS
	//use command line parameter options to define energy penalties for initializing pseudoknots, minimal length of kissing hairpin stems and the pKiss strategy
	#include "rnaoptions.hh"
	inline static int pkinit() { //initialization cost for opening a new pseudoknot. Default is 900.
		return gapc::Opts::getOpts()->energyPenaltyHtype;
	}
	inline static int pkissinit() { //initialization cost for opening a new kissing hairpin. Default is 1200.
		return gapc::Opts::getOpts()->energyPenaltyKtype;
	}
	inline static int minLengthKissingHairpinStems() { //minimal length of those two stems in a KH that form the hairpins, not the crossing stem of the kiss. Default is 2
		return gapc::Opts::getOpts()->minimalHelixLength;
	}
	inline static int maxPseudoknotSize() {
		return gapc::Opts::getOpts()->maximalPseudoknotSize;
	}
	inline static float lowProbabilityFilter() { //heuristically filtering out shapes with in very low initial probability. Default is 10^-6
		return gapc::Opts::getOpts()->lowProbabilityFilter;
	}
	inline static int shapelevel() {
		return gapc::Opts::getOpts()->shapelevel;
	}
	template<typename alphabet, typename pos_type, typename T>
	inline bool selectStrategy(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j, const char strategy) {
		return gapc::Opts::getOpts()->strategy == strategy;
	}
	template<typename alphabet, typename pos_type, typename T>
	inline bool allowLonelyBasepairs(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j, const bool isLonelyBP) {
		return gapc::Opts::getOpts()->allowLonelyBasepairs == isLonelyBP;
	}
	inline int getSuboptRange(int mfe) { //use command line parameter options to define the range of suboptimal answers, depending on MFE.
		int range = mfe + int(gapc::Opts::getOpts()->energydeviation_absolute*100);
		if (isnan(gapc::Opts::getOpts()->energydeviation_absolute)) {
			range = mfe * (100 - gapc::Opts::getOpts()->energydeviation_relative*(mfe < 0 ? 1 : -1))/100;
		}
		return range;
	}
	inline unsigned int getWindowSize() {
		return gapc::Opts::getOpts()->window_size;
	}
	inline unsigned int getWindowIncrement() {
		return gapc::Opts::getOpts()->window_increment;
	}
#else
	//if compiled with no special options to ask for energy penalties for initializing pseudoknots, minimal length of kissing hairpin stems and the pKiss strategy.
	inline static int pkinit() { //initialization cost for opening a new pseudoknot. Default is 900.
		return 900;
	}
	inline static int pkissinit() { //initialization cost for opening a new kissing hairpin. Default is 1200.
		return 1200;
	}
	inline static int minLengthKissingHairpinStems() { //minimal length of those two stems in a KH that form the hairpins, not the crossing stem of the kiss. Default is 2
		return 2;
	}
	inline static int maxPseudoknotSize() {
		return std::numeric_limits<int>::max();
	}
	inline static float lowProbabilityFilter() {
		return 0.000001;
	}
	inline static int shapelevel() {
		return 5;
	}
	template<typename alphabet, typename pos_type, typename T>
	inline bool selectStrategy(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j, const char strategy) {
		return 'A' == strategy;
	}
	template<typename alphabet, typename pos_type, typename T>
	inline bool allowLonelyBasepairs(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j, const bool isLonelyBP) {
		return false == isLonelyBP;
	}
	inline int getSuboptRange(int mfe) { //if compiled with no special options to ask for energy range, use 5% of MFE as a default.
		return mfe * (100 - 5*(mfe < 0 ? 1 : -1))/100;
	}
	inline unsigned int getWindowSize() {
		return 7;
	}
	inline unsigned int getWindowIncrement() {
		return 1;
	}
#endif
	
#endif //RNAOPTIONS_DEFAULTS_HH
