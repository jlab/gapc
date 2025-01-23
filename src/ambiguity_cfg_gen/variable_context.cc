/* {{{

    This file is part of gapc (GAPC - Grammars, Algebras, Products - Compiler;
      a system to compile algebraic dynamic programming programs)

    Copyright (C) 2008-2011  Georg Sauthoff
         email: gsauthof@techfak.uni-bielefeld.de or gsauthof@sdf.lonestar.org

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

}}} */

#include "variable_context.hh"

#include <cassert>
#include <vector>
#include <string>

#include "../cfg/cfg.hh"
#include "../log.hh"


AmbiguityCFG::VariableContext::VariableContext()
  : /*parentContext (NULL),*/ closed(false) {
  this->declaredVariables = new Util::SymbolTable<std::string, VarInfoItem*>();
}


AmbiguityCFG::VariableContext::VariableContext(VariableContext* parentContext)
  : /*parentContext (NULL),*/ closed(false) {
  /*
  this->parentContext = parentContext;
  */
  this->declaredVariables = new Util::SymbolTable<std::string, VarInfoItem*> (
    parentContext->declaredVariables);
}


AmbiguityCFG::VariableContext::~VariableContext() {
  // TODO(who?): dealloc all table contents.
}


void AmbiguityCFG::VariableContext::clear() {
  this->declaredVariables->cleanDeep();
  copiedButNotStoredVariables.clear();
  /*
  if (this->parentContext != NULL) {
    this->parentContext->clear();
  }
  */
}


void AmbiguityCFG::VariableContext::removeLocalChanges() {
  this->declaredVariables->cleanLocally();
  this->localDefinitions.clear();
  this->copiedButNotStoredVariables.clear();
}


void AmbiguityCFG::VariableContext::removeLocallyAddedItems() {
  for (std::set<std::string>::iterator i = this->localDefinitions.begin();
       i != this->localDefinitions.end(); ++i) {
    this->declaredVariables->removeElementLocally(*i);
  }
  this->localDefinitions.clear();
  // If this is the root context, all elements must be
  // removed.
  /*
  if (this->parentContext == NULL) {
    this->removeLocalChanges();
  }
  // Otherwise we remove only those elements, that are
  else {
  }
  */
}


void AmbiguityCFG::VariableContext::defineLocalVariable(
  std::string* variableName) {
  this->localDefinitions.insert(*variableName);
}


void AmbiguityCFG::VariableContext::close() {
  this->closed = true;
}


bool AmbiguityCFG::VariableContext::isClosed() {
  return this->closed;
}


void AmbiguityCFG::VariableContext::setVarInfoItem(
  std::string* name, VarInfoItem* infoItem) {
  // If this variable has been copied from the parent context,
  // it will now be transferred into the local area of declared
  // variables.
  // First we remove the element from the list of copied items,
  // then we add the new item unconditionally to the list of
  // declared variables.
  if (this->copiedButNotStoredVariables.find(*name) !=
      this->copiedButNotStoredVariables.end()) {
    this->copiedButNotStoredVariables.erase(*name);
  }
  this->declaredVariables->setElement(*name, infoItem);
  /*
  // Just add the info item locally.
  this->declaredVariables[*name] = infoItem;
  */
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::VariableContext::getVarInfoItem(
  std::string* variableName) {
  return this->declaredVariables->getElement(*variableName);
  /*
  // First look up in the local list of declared variables...
  if (this->declaredVariables.find (*variableName) !=
  // this->declaredVariables.end()) {
    return this->declaredVariables[*variableName];
  }
  // ...second search the list of copied but not stored items...
  else if (this->copiedButNotStoredVariables.find (*variableName) !=
  this->copiedButNotStoredVariables.end()) {
    return this->copiedButNotStoredVariables[*variableName];
  }
  // ...if nothing was found so far, try the parent context:
  else {
    // the parent variable store must not be null of the variable
    // info item cannot be found in the local hashtable.
    assert (this->parentContext != NULL);

    // What follows is a kind of copy-on-write, except
    // that the copy is done on read, because after we return
    // a variable-info-item, we have no control over writing
    // changes into the item structure. Thus we create a local
    // copy when the item is read, and return the pointer to
    // that copy. Further access to the variable will be done
    // through the copied info item.
    // From the view point of performance is makes no difference
    // to copy-on-write since every retieval of an info item
    // leads in almost all cases to an update of its content.
    VarInfoItem* parentInfoItem = this->parentContext->getVarInfoItem (
    variableName);

    // There must be a result that we can clone, otherwise
    // this method has been called while the given variable name
    // has not been defined in this variable store.
    assert (parentInfoItem != NULL);

    if (parentInfoItem->is (VAR_DECL_INFO)) {
      // Just clone, store locally, and return the result.
      VarInfoItem* infoItemClone = parentInfoItem->clone();
      // The just created copy of the parent info item must be
      // of type VarDeclInfo! We cast the type in order to
      // access its production fragment and create a snapshot
      // from it before we store the clone into local context.
      VarDeclInfo* varDeclInfo = dynamic_cast<VarDeclInfo*> (infoItemClone);
      //varDeclInfo->productionFragment = new Snapshot (
      varDeclInfo->productionFragment);
      this->copiedButNotStoredVariables[*variableName] = varDeclInfo;
      //this->declaredVariables[*variableName] = varDeclInfo;
      // Return the snapshot:
      return varDeclInfo;
    }
    else {
      return parentInfoItem;
    }

  }
  */
}


bool AmbiguityCFG::VariableContext::containsVarInfoItem(
  std::string* variableName) {
  return this->declaredVariables->containsElement(*variableName);
  /*
  // The item name is either defined locally, or in the list of copied but
  // not stored items, or in the parent context. If no parent context is
  // given, and the item name is not defined in this context iteself, we
  // return 'false'.
  if (this->declaredVariables.find (*variableName) !=
  this->declaredVariables.end()) {
    return true;
  }
  else if (this->copiedButNotStoredVariables.find (*variableName) !=
  this->copiedButNotStoredVariables.end()) {
    return true;
  }
  else if (this->parentContext != NULL) {
    return this->parentContext->containsVarInfoItem (variableName);
  }
  else {
    return false;
  }
  */
}


bool AmbiguityCFG::VariableContext::containsVarInfoItemDirectly(
  std::string* variableName) {
  return this->declaredVariables->containsElementLocally(*variableName);
  /*
  // We only search for the variable name in the local
  // variable context.
  return this->declaredVariables.find (*variableName) !=
  this->declaredVariables.end();
  */
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::MultiVariableContext::MultiVariableContext() {
  this->contexts.push_back(new VariableContext());
}


AmbiguityCFG::MultiVariableContext::MultiVariableContext(
  AmbiguityCFG::VariableContext* parentContext) {
  this->contexts.push_back(parentContext);
}


AmbiguityCFG::MultiVariableContext::MultiVariableContext(
  AmbiguityCFG::MultiVariableContext* parentContext) {
  // For each sub-context of the parent context we create an
  // entry in the contexts-vector of this multi-context.
  for (std::vector<VariableContext*>::iterator i =
       parentContext->contexts.begin();
       i != parentContext->contexts.end(); ++i) {
    this->contexts.push_back(new VariableContext(*i));
  }
}


AmbiguityCFG::MultiVariableContext::MultiVariableContext(
  AmbiguityCFG::MultiVariableContext* fstContext,
  AmbiguityCFG::MultiVariableContext* sndContext) {
  // We simply add each sub-context of the first and second
  // context parameter to the list of sub-contexts of this
  // new instance. We start with the parameter fstContext, and
  // continue with the parameter sndContext.
  for (std::vector<VariableContext*>::iterator i = fstContext->contexts.begin();
       i != fstContext->contexts.end(); ++i) {
    this->contexts.push_back(new VariableContext(*i));
  }
  for (std::vector<VariableContext*>::iterator i = sndContext->contexts.begin();
       i != sndContext->contexts.end(); ++i) {
    this->contexts.push_back(new VariableContext(*i));
  }
}


AmbiguityCFG::MultiVariableContext::~MultiVariableContext() {
}


void AmbiguityCFG::MultiVariableContext::removeLocalChanges() {
  for (std::vector<VariableContext*>::iterator i = this->contexts.begin();
       i != this->contexts.end(); ++i) {
    (*i)->removeLocalChanges();
  }
}


void AmbiguityCFG::MultiVariableContext::removeLocallyAddedItems() {
  for (std::vector<VariableContext*>::iterator i = this->contexts.begin();
       i != this->contexts.end(); ++i) {
    (*i)->removeLocallyAddedItems();
  }
}


void AmbiguityCFG::MultiVariableContext::defineLocalVariable(
  std::string* variableName) {
  for (std::vector<VariableContext*>::iterator i = this->contexts.begin();
       i != this->contexts.end(); ++i) {
    (*i)->defineLocalVariable(variableName);
  }
}


void AmbiguityCFG::MultiVariableContext::setVarInfoItem(
  std::string* name, AmbiguityCFG::VarInfoItem* infoItem) {
  switch (infoItem->getType()) {
    case VAR_DECL_INFO:
    case RECORD_DECL_INFO: {
      // For input of simple data (i.g. data that represents
      // a single values at a time) we just set that value
      // for the given variable name in all sub-contexts.
      for (std::vector<VariableContext*>::iterator i = this->contexts.begin();
           i != this->contexts.end(); ++i) {
        (*i)->setVarInfoItem(name, infoItem->clone());
      }
      break;
    }
    case MULTI_VAR_DECL_INFO:
    case MULTI_RECORD_DECL_INFO: {
      // For multi-valued input data we first check if the
      // dimensions of the sub-context and the input are equal.
      // Second we copy all elements of the input into the
      // corresponding sub-context, i.g. the first element of
      // the input is copied into the first sub-context, and
      // so on.

      MultiVarInfoItem* multiInfoItem = dynamic_cast<MultiVarInfoItem*> (
        infoItem);
      std::vector<VariableContext*>::iterator i = this->contexts.begin();
      for (int pos = 0; pos < multiInfoItem->getDimension(); pos++) {
        if (i == this->contexts.end()) {
          throw LogError(
            "gap-00223: Dimension-mismatch of info item to be stored and "
            "sub-contexts");
        }
        (*i)->setVarInfoItem(name, multiInfoItem->getVarInfoItemAt(pos));
        ++i;
      }

      break;
    }
    default: {
      throw LogError(
        "gap-00210: can not set VarInfoImte, unhandled operand type.");
    }
  }
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::MultiVariableContext::getVarInfoItem(
  std::string* variableName) {
  // If this is the single sub-context case, we just return
  // a single item, not a multi one.
  if (this->contexts.size() == 1) {
    return this->contexts[0]->getVarInfoItem(variableName);
  }

  // Otherwise, we need to find out whether we need an info
  // item for records or for variables. The easiest way is
  // to create both kinds of container, fill them according
  // to the element type of each single variable info item,
  // and
  bool notFound = false;
  bool containedVarDecls = false;
  bool containedRecordDecls = false;
  MultiVarDeclInfo* multiVarDeclInfo = new MultiVarDeclInfo();
  MultiRecordDeclInfo* multiRecordDeclInfo = new MultiRecordDeclInfo();
  for (std::vector<VariableContext*>::iterator i = this->contexts.begin();
       i != this->contexts.end(); ++i) {
    bool result = (*i)->containsVarInfoItem(variableName);
    if (!result) {
      // if an item is not in the local variable contexts,
      // we just abort the loop and try to find it in a
      // parent context.
      notFound = true;
      break;
    }
    VarInfoItem* item = (*i)->getVarInfoItem(variableName);
    switch (item->getType()) {
      case VAR_DECL_INFO: {
        multiVarDeclInfo->addVarInfoItem(item);
        containedVarDecls = true;
        break;
      }
      case RECORD_DECL_INFO: {
        multiRecordDeclInfo->addVarInfoItem(item);
        containedRecordDecls = true;
        break;
      }
      case MULTI_VAR_DECL_INFO: {
        MultiVarInfoItem* multiVarInfo = dynamic_cast<MultiVarInfoItem*> (item);
        for (int i = 0; i < multiVarInfo->getDimension(); i++) {
          multiVarDeclInfo->addVarInfoItem(multiVarInfo->getVarInfoItemAt(i));
        }
        containedVarDecls = true;
        break;
      }
      case MULTI_RECORD_DECL_INFO: {
        MultiVarInfoItem* multiVarInfo = dynamic_cast<MultiVarInfoItem*> (item);
        for (int i = 0; i < multiVarInfo->getDimension(); i++) {
          multiRecordDeclInfo->addVarInfoItem(multiVarInfo->getVarInfoItemAt(
            i));
        }
        containedRecordDecls = true;
        break;
      }
      default: {
        throw LogError("gap-00220: unsupported simultaneous storage type.");
      }
    }
  }

  // This method must not be called when there is no
  // information item stored for the given variable name.
  // The compiler kept posting this warning that 'notFound'
  // has been set but never used. assert (!notFound) does
  // not seem to count for the compiler. This is a hack:
  if (!notFound) {
    assert(notFound == false);
  }

  // No mixed info items are allowed, that is all sub-contexts
  // must return items that are the same subclass of VarInfoItem,
  // either VarDeclInfo or RecordDeclInfo. If both kinds were found
  // we raise an exception
  if (containedVarDecls && containedRecordDecls) {
    throw LogError(
      "gap-00221: sub-contexts contained different types of variable info"
      " items.");
  }

  if (containedVarDecls) {
    return multiVarDeclInfo;
  } else if (containedRecordDecls) {
    return multiRecordDeclInfo;
  }

  // this line must not be reached!
  throw LogError(
    "gap-00222: no variable declaration nor any record declarations were "
    "found.");
}


bool AmbiguityCFG::MultiVariableContext::containsVarInfoItem(
  std::string* variableName) {
  ///////////////////////////////////////////////////////////////
  // NOTE: This algorithm is a copy of the algorithm in the method
  // AmbiguityCFG::MultiVariableContext::containsVarInfoItem (std::string*
  // variableName)
  // The only difference is the call to the method 'containsVarInfoItem'
  // instead of the method 'containsVarInfoItemDirectly'. If you find any
  // mistakes in any of these two methods, please be sure to fix
  // it in both of them!
  ///////////////////////////////////////////////////////////////

  // We probe each sub-context individually, one by one.
  // While we iterate through all items
  bool containedInAll = true;
  bool containedInNone = false;
  for (std::vector<VariableContext*>::iterator i = this->contexts.begin();
       i != this->contexts.end(); ++i) {
    bool result = (*i)->containsVarInfoItem(variableName);
    containedInAll &= result;
    containedInNone |= result;
  }
  // Invert the value of containedInNone, because
  // its name does not match the calculated value.
  containedInNone = !containedInNone;
  // Neither both of the two results may be true
  // or false at the same time. Both results must have
  // a different value, because the item we are searching
  // for must be either present in all subcontexts, or
  // or in none at all.
  assert(containedInAll ^ containedInNone);
  // We do not look any further into any parent context,
  // because each sub-context of its own has its own parent
  // context. Thus the flag containedInAll reflects our
  // desired information.
  return containedInAll;
}


bool AmbiguityCFG::MultiVariableContext::containsVarInfoItemDirectly(
  std::string* variableName) {
  ///////////////////////////////////////////////////////////////
  // NOTE: This algorithm is a copy of the algorithm in the method
  // AmbiguityCFG::MultiVariableContext::containsVarInfoItem (
  // std::string* variableName)
  // The only difference is the call to the method 'containsVarInfoItemDirectly'
  // instead of the method 'containsVarInfoItem'. If you find any
  // mistakes in any of these two methods, please be sure to fix
  // it in both of them!
  ///////////////////////////////////////////////////////////////

  // We probe each sub-context individually, one by one.
  // While we iterate through all items
  bool containedInAll = true;
  bool containedInNone = false;
  for (std::vector<VariableContext*>::iterator i = this->contexts.begin();
       i != this->contexts.end(); ++i) {
    bool result = (*i)->containsVarInfoItemDirectly(variableName);
    containedInAll &= result;
    containedInNone |= result;
  }
  // Invert the value of containedInNone, because
  // its name does not match the calculated value.
  containedInNone = !containedInNone;
  // Neither both of the two results may be true
  // or false at the same time. Both results must have
  // a different value, because the item we are searching
  // for must be either present in all subcontexts, or
  // or in none at all.
  assert(containedInAll ^ containedInNone);
  // We do not look any further into any parent context,
  // because each sub-context of its own has its own parent
  // context. Thus the flag containedInAll reflects our
  // desired information.
  return containedInAll;
}
