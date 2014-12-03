

#ifndef __NAMING_DOMAIN_HH__
#define __NAMING_DOMAIN_HH__


#include <map>

#include "naming_path.hh"


namespace Util {
	
	
	class NamingDomain {
		
		private:
			
			// The parent domain of this naming domain.
			NamingDomain* parentDomain;
			
			// Stores the mapping of all aliasses on any naming-path.
			std::map<std::string, std::string*> aliasMap;
			// Each alias name consists of the string "rule" and a
			// unique number (this one). Each naming-domain has its
			// own numbering cycle, because this variable is not a
			// static variable.
			static unsigned int nextNameNumber;
			
			
		public:
			
			NamingDomain();
			NamingDomain (NamingDomain* d);
			~NamingDomain();
			
			// Returns TRUE if the domain contains the name in
			// the given naming-path.
			bool containsName (std::string* name);
			bool containsName (std::string name);
			
			// Returns an alias for the given name. If this name
			// had no alias before, a new alias will be created. 
			std::string* getAlias (std::string* name);
			std::string* getAlias (std::string name);
			
			
	};
	
	
}


#endif	// ifndef __NAMING_DOMAIN_HH__

