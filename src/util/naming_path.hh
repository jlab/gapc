

#ifndef __NAMING_PATH_HH__
#define __NAMING_PATH_HH__


#include <string>


namespace Util {
	
	
	class NamingPath {
		
		private:
			
			// The character that is used as a separator when
			// the pretty print is created.
			static std::string separatorChar;
			
			// The prefix of this naming-path, or NULL if this
			// is the root of the path.
			NamingPath* prefix;
			// the name of this path element, or "" if this is
			// the root of the path.
			std::string* suffix;
			
			
		public:
			
			// Creates an empty naming path.
			NamingPath();
			// Creates a naming-path with the a first suffix
			// as given by the parameter 'name'.
			NamingPath (std::string* name);
			// Copy constructor, creates a deep copy of this instance.
			NamingPath (NamingPath& p);
			~NamingPath();
			
			// Returns a new naming path with this naming path
			// as prefix, and the 'newName' as suffix.
			NamingPath* createSubPath (std::string* newName);
			
			// Returns a string representation of this naming-path,
			// which is a forward-slash separated ('/') list of all its names.
			std::string toString();
			
			
	};
	
	
}


#endif	// ifndef __NAMING_PATH_HH__

