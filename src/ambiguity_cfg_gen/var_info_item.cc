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

#include "var_info_item.hh"

#include <iostream>
#include <cassert>
#include <vector>
#include <string>


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::VarInfoItem::VarInfoItem(InfoType type)
  : type(type), numberOfUses(0) {
}


AmbiguityCFG::VarInfoItem::~VarInfoItem() {
}


bool AmbiguityCFG::VarInfoItem::is(InfoType type) {
  return this->type == type;
}


AmbiguityCFG::InfoType AmbiguityCFG::VarInfoItem::getType() {
  return this->type;
}


void AmbiguityCFG::VarInfoItem::increaseNumerOfUses() {
  this->numberOfUses++;
}


int AmbiguityCFG::VarInfoItem::getNumberOfUses() {
  return this->numberOfUses;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::NamedAccess::~NamedAccess() {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::VarDeclInfo::VarDeclInfo()
  : VarInfoItem(VAR_DECL_INFO) {
  this->variableName = new std::string("");
  this->productionFragment = NULL;
  this->parameterPosition = -1;
}


AmbiguityCFG::VarDeclInfo::~VarDeclInfo() {
}


CFG::Base* AmbiguityCFG::VarDeclInfo::combine(CFG::Base* prod) {
  if (this->productionFragment == NULL) {
    // this operation makes only sense if the production given as
    // parameter is not NULL, because otherwise we would permit
    // null values being returned as valid results of the combination
    // of two variable declaration states.
    assert(prod != NULL);
    // If the production fragment is null, there is nothing
    // to combine, thus
    return prod;
  } else {
    return this->productionFragment->combine(prod);
  }
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::VarDeclInfo::clone() {
  VarDeclInfo* newInstance = new VarDeclInfo();
  newInstance->variableName = this->variableName;
  newInstance->productionFragment = this->productionFragment->clone();
  return newInstance;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::MultiVarInfoItem::MultiVarInfoItem(AmbiguityCFG::InfoType type)
  : VarInfoItem(type) {
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::MultiVarDeclInfo::MultiVarDeclInfo()
  : MultiVarInfoItem(MULTI_VAR_DECL_INFO) {
}


AmbiguityCFG::MultiVarDeclInfo::MultiVarDeclInfo(
  std::string* variableName, std::vector<CFG::Base*> fragments)
  : MultiVarInfoItem(MULTI_VAR_DECL_INFO) {
  for (unsigned int i = 0; i < fragments.size(); i++) {
    VarDeclInfo* infoItem = new VarDeclInfo();
    infoItem->variableName = variableName;
    infoItem->productionFragment = fragments[i];
    this->infoItems.push_back(infoItem);
  }
}


AmbiguityCFG::MultiVarDeclInfo::~MultiVarDeclInfo() {
}


int AmbiguityCFG::MultiVarDeclInfo::getDimension() {
  return this->infoItems.size();
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::MultiVarDeclInfo::getVarInfoItemAt(
  unsigned int pos) {
  assert(pos >=0 && pos < this->infoItems.size());
  return this->infoItems[pos];
}


void AmbiguityCFG::MultiVarDeclInfo::addVarInfoItem(VarInfoItem* infoItem) {
  this->infoItems.push_back(infoItem);
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::MultiVarDeclInfo::clone() {
  MultiVarDeclInfo* newInstance = new MultiVarDeclInfo();
  // TODO(who?): implement this method.
  return newInstance;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::RecordDeclInfo::RecordDeclInfo()
  : VarInfoItem(RECORD_DECL_INFO) {
}


AmbiguityCFG::RecordDeclInfo::~RecordDeclInfo() {
}


void AmbiguityCFG::RecordDeclInfo::setVarInfoItem(
  std::string* elementName, AmbiguityCFG::VarInfoItem* infoItem) {
  this->recordElements[*elementName] = infoItem;
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::RecordDeclInfo::getVarInfoItem(
  std::string* elementName) {
  // Is the element really stored in the list of elements for this record
  // info item?
  if (this->recordElements.find(*elementName) == this->recordElements.end()) {
    // throw LogError ("gap-00128: No record member with name '" + *
    // elementName + "' exists.");
  }
  // After the check we can safely access the element of
  // the hashtable:
  return this->recordElements[*elementName];
}


bool AmbiguityCFG::RecordDeclInfo::containsVarInfoItem(
  std::string* elementName) {
  return this->recordElements.find(*elementName) != this->recordElements.end();
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::RecordDeclInfo::clone() {
  RecordDeclInfo* newInstance = new RecordDeclInfo();
  // TODO(who?): iterate through all record elements and clone them.
  return newInstance;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////


AmbiguityCFG::MultiRecordDeclInfo::MultiRecordDeclInfo()
  : MultiVarInfoItem(MULTI_RECORD_DECL_INFO) {
}


AmbiguityCFG::MultiRecordDeclInfo::~MultiRecordDeclInfo() {
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::MultiRecordDeclInfo::clone() {
  MultiRecordDeclInfo* newInstance = new MultiRecordDeclInfo();
  for (std::vector<RecordDeclInfo*>::iterator i = this->recordInfos.begin();
       i != this->recordInfos.end(); ++i) {
    newInstance->addVarInfoItem(*i);
  }
  return newInstance;
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::MultiRecordDeclInfo::getVarInfoItem(
  std::string* elementName) {
  // We are expecting two possible types of result:
  // 1) a set of simultaneous active values, which
  //    will be stored in a MultiVarDeclInfoItem
  // 2) a set of simultaneous record pointers, which
  //    will be stored in a MultiRecordDeclInfo.
  MultiVarDeclInfo* resultMultiVar = new MultiVarDeclInfo();
  bool isMultiVar = false;
  MultiRecordDeclInfo* resultMultiRec = new MultiRecordDeclInfo();
  bool isMultiRec = false;
  for (std::vector<RecordDeclInfo*>::iterator i = this->recordInfos.begin();
       i != this->recordInfos.end(); ++i) {
    VarInfoItem* item = (*i)->getVarInfoItem(elementName);
    switch (item->getType()) {
      case VAR_DECL_INFO: {
        resultMultiVar->addVarInfoItem(item);
        isMultiVar = true;
        break;
      }
      case RECORD_DECL_INFO: {
        resultMultiRec->addVarInfoItem(item);
        isMultiRec = true;
        break;
      }
      default: {
        assert(!new std::string("unexpected variable info item"));
      }
    }
  }

  // We have an internal error if there were both kind of
  // info items. Actually this assertion is contained in
  // the one following below.
  assert(!(isMultiVar && isMultiRec));
  // Also there must be at least one result of one kind
  // that we can return to the caller of this function.
  assert(isMultiVar ^ isMultiRec);

  if (isMultiVar) {
    return resultMultiVar;
  } else if (isMultiRec) {
    return resultMultiRec;
  }

  return NULL;
}


void AmbiguityCFG::MultiRecordDeclInfo::setVarInfoItem(
  std::string* elementName, VarInfoItem* infoItem) {
  for (std::vector<RecordDeclInfo*>::iterator i = this->recordInfos.begin();
       i != this->recordInfos.end(); ++i) {
    (*i)->setVarInfoItem(elementName, infoItem);
  }
}


bool AmbiguityCFG::MultiRecordDeclInfo::containsVarInfoItem(
  std::string* elementName) {
  // Look in each variable info item for
  bool containedInAll = true;
  bool containedInNone = false;
  for (std::vector<RecordDeclInfo*>::iterator i = this->recordInfos.begin();
       i != this->recordInfos.end(); ++i) {
    bool result = (*i)->containsVarInfoItem(elementName);
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


int AmbiguityCFG::MultiRecordDeclInfo::getDimension() {
  return this->recordInfos.size();
}


AmbiguityCFG::VarInfoItem* AmbiguityCFG::MultiRecordDeclInfo::getVarInfoItemAt(
  unsigned int pos) {
  assert(pos >= 0 && pos < this->recordInfos.size());
  return this->recordInfos[pos];
}


void AmbiguityCFG::MultiRecordDeclInfo::addVarInfoItem(
  AmbiguityCFG::VarInfoItem* infoItem) {
  assert(!infoItem->is(RECORD_DECL_INFO));
  this->recordInfos.push_back(dynamic_cast<RecordDeclInfo*> (infoItem));
}
