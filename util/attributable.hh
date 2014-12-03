

#ifndef __ATTRIBUTABLE_HH__
#define __ATTRIBUTABLE_HH__


#include <map>
#include <string>

#include "attribute.hh"


namespace Util {
	
	
	/*
	 * Stores and retrieves attributes for anything you like to put
	 * a lable on. Note that there is a one to one correspondence
	 * between an attribute name, and the stored attribute. That means
	 * there may not be more than one attribute stored for a given
	 * name. If there is already an attribute stored for a given
	 * name, while the user tries to set an other value, the old
	 * value is overwritten.
	 */
	class Attributable {
		
		private:
			
			std::map<std::string, Util::Attribute*> attributeMap;
			
			
		public:
			
			Attributable();
			Attributable (Attributable& a);
			~Attributable();
			
			
			// Stores the attribute under its attribute-ID.
			void setAttribute (Util::Attribute* attr);
			// Forces this attributable instance to store the attribute
			// under an other name than the attribute-ID.
			void setAttribute (std::string key, Util::Attribute* attr);
			Util::Attribute* getAttribute (std::string key);
			bool containsAttribute (std::string key);
			
			// Removes the attribute for the given key from this instance.
			// If this instance held no such attribute, the method returns
			// FALSE, otherwise TRUE is returned.
			bool removeAttribute (std::string key);
			// Removes all attributes from this instance.
			void clearAttributes();
			
			typedef std::map<std::string, Util::Attribute*>::iterator iterator;
			iterator begin();
			iterator end();
			
		
	};
	
	
}


#endif	// ifndef __ATTRIBUTABLE_HH__

