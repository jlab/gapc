/*
 * grammar_transformation.cc
 *
 *  Created on: Mar 7, 2023
 *      Author: sjanssen
 */

#include "../grammar.hh"
#include "../ast.hh"

bool Grammar::check_outside_parse_empty_word() {
  if (this->ast.outside_generation()) {
    for (std::vector<Yield::Size>::const_iterator i = this->axiom->multi_ys().begin(); i != this->axiom->multi_ys().end(); ++i) {
      if ((*i).low() > 0) {
    	std::ostringstream msg;
        msg << "The minimal yield size of your grammar '" << *this->name << "' is ";
        (*i).low().put(msg);
        msg << ", i.e. it cannot parse the empty input string ''."
            <<  " For outside grammar generation, this means you are lacking a "
            << "recursion basis which will result in empty results for "
            << "ALL outside candidates! Try adding an alternative like nil(EMPTY) to your axiom.";
        Log::instance()->warning(this->location, msg.str());
        return false;
      }
    }
  }
  return true;
}
