

#ifndef __CYCLE_PATH_INFO_ATTRIBUTE_HH__
#define __CYCLE_PATH_INFO_ATTRIBUTE_HH__


#include <list>
#include <set>

#include "../cfg/cfg.hh"
#include "../util/attribute.hh"


namespace Util {
	
	
	class CyclePathInfoAttribute : public Attribute {
		
		private:
			
			std::list< std::pair<std::string, CFG::Base*> > elements;
			
			
		public:
			
			CyclePathInfoAttribute();
			CyclePathInfoAttribute (CyclePathInfoAttribute& a);
			virtual ~CyclePathInfoAttribute();
			
			void addElement (std::string nonTerminalName, CFG::Base* fragment);
			void addElements (std::list< std::pair<std::string, CFG::Base*> >* elems, unsigned int startPos);
			
			typedef std::list< std::pair<std::string, CFG::Base*> >::iterator iterator;
			iterator begin();
			iterator end();
			
			virtual Attribute* clone();
			
	};
	
	
}


#endif	// ifndef __CYCLE_PATH_INFO_ATTRIBUTE_HH__

