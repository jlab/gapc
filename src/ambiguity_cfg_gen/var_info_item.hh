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

#ifndef SRC_AMBIGUITY_CFG_GEN_VAR_INFO_ITEM_HH_
#define SRC_AMBIGUITY_CFG_GEN_VAR_INFO_ITEM_HH_


#include <string>
#include <vector>
#include "../hashtable.hh"

#include "../cfg/cfg.hh"


namespace AmbiguityCFG {


// Enumeration of all subtypes of the base class VarInfoItem.
// The base class VarInfoItem needs no constant itself because
// no instance can be created directly from that class.
enum InfoType {VAR_DECL_INFO, MULTI_VAR_DECL_INFO, RECORD_DECL_INFO,
MULTI_RECORD_DECL_INFO};


// The base class of all variable information items. It is
// only used as a common pointer type and to distinguish
// easily the subclass an instance is of.
class VarInfoItem {
 protected:
    // stores the type of the subclass of this base class
    InfoType type;

    // Counter that counts the number of uses of this
    // variable information. An item is used when its
    // current state is appended to the current state
    // of an other variable.
    int numberOfUses;

    // A protected constructor and a pure virtual function
    // (clone) ensures that we can not create an instance
    // of this class.
    explicit VarInfoItem(InfoType type);
    virtual ~VarInfoItem();

 public:
    bool is(InfoType type);
    InfoType getType();
    virtual VarInfoItem* clone() = 0;

    void increaseNumerOfUses();
    int getNumberOfUses();
};


// This class provides an interface for accessing stored
// data elements in a kind of dictionary structure.
class NamedAccess {
 public:
    // A virtual destructor needed to make this interface
    // class work.
    virtual ~NamedAccess();

    // Gets the content element stored for the given name.
    virtual VarInfoItem* getVarInfoItem(std::string* elementName) = 0;
    // Sets the given content for the given name.
    virtual void setVarInfoItem(
      std::string* elementName, VarInfoItem* infoItem) = 0;
    // Determines whether the named-access instance contains
    // the variable info item
    virtual bool containsVarInfoItem(std::string* elementName) = 0;
};


// A variable declaration information item used to store some
// information about a variable.
class VarDeclInfo : public VarInfoItem {
 public:
    // The name of the variable.
    std::string *variableName;
    // The CFG fragment value of this variable
    CFG::Base* productionFragment;
    // The position of the variable in the argument
    // list of an algebra function, or -1 if this
    // variable is local inside of an algebra function.
    int parameterPosition;

    VarDeclInfo();
    ~VarDeclInfo();

    // TODO(who?): delete this two methods? They seem to be residuals
    // of the NamedAccess interface implementation.
    VarInfoItem* getInfoItem(std::string* elementName);
    void setInfoItem(std::string* elementName, VarInfoItem* infoItem);

    // combines the list of production fragments with all elements
    // of production fragments of the list given as vector.
    CFG::Base* combine(CFG::Base* prod);
    // Creates a copy of this info item.
    virtual VarInfoItem* clone();
};


// This is a marker Interface for all info items that
// carry multiple values.
class MultiVarInfoItem : public VarInfoItem {
 protected:
    // The protected constructor simply passes the type
    // information to the parent class VarInfoItem.
    explicit MultiVarInfoItem(InfoType type);

 public:
    // Returns the dimension of this info item, that is
    // the number of simultaneous held VarInfoItems.
    virtual int getDimension() = 0;
    // Returns the VarInfoItem at the given position. The
    // position parameter must be less than the dimension
    // of this instance, otherwise an error will be thrown.
    virtual VarInfoItem* getVarInfoItemAt(unsigned int pos) = 0;
    // Adds a VarInfoItem to this instance.
    virtual void addVarInfoItem(VarInfoItem* infoItem) = 0;
};


// A variable declaration item that holds multiple values
// at a time.
class MultiVarDeclInfo : public MultiVarInfoItem {
 private:
    // This vector holds all simultaneous values.
    std::vector<VarInfoItem*> infoItems;

 public:
    MultiVarDeclInfo();
    MultiVarDeclInfo(
      std::string* variableName, std::vector<CFG::Base*> fragments);
    ~MultiVarDeclInfo();

    // Returns the dimension of this info item, that is
    // the number of simultaneous held VarInfoItems.
    int getDimension();
    // Returns the VarInfoItem at the given position. The
    // position parameter must be less than the dimension
    // of this instance, otherwise an error will be thrown.
    VarInfoItem* getVarInfoItemAt(unsigned int pos);
    // Adds a VarInfoItem to this instance.
    void addVarInfoItem(VarInfoItem* infoItem);

    // Creates a copy of this info item.
    VarInfoItem* clone();
};


// A record declaration information item is used to
// define a record/struct of variable-information-items.
// It is a direct subclass of VarInfoItem because it
// is used to declare information about data. It is also
// a subclass of NamedAccess because it provides access
// to instances of VarInfoItems by using their name
// as a string.
class RecordDeclInfo : public VarInfoItem, public NamedAccess {
 private:
    hashtable<std::string, VarInfoItem*> recordElements;

 public:
    RecordDeclInfo();
    ~RecordDeclInfo();

    VarInfoItem* getVarInfoItem(std::string* elementName);
    void setVarInfoItem(std::string* elementName, VarInfoItem* infoItem);
    bool containsVarInfoItem(std::string* elementName);

    // Creates a copy of this info item.
    VarInfoItem* clone();
};


class MultiRecordDeclInfo : public MultiVarInfoItem, public NamedAccess {
 private:
    // Stores all parallel valid record info items.
    std::vector<RecordDeclInfo*> recordInfos;

 public:
    MultiRecordDeclInfo();
    ~MultiRecordDeclInfo();

    // Creates a copy of this info item.
    VarInfoItem* clone();

    // Gets the content element stored for the given name.
    VarInfoItem* getVarInfoItem(std::string* elementName);
    // Sets the given content for the given name.
    void setVarInfoItem(std::string* elementName, VarInfoItem* infoItem);
    // Determines whether the named-access instance contains
    // the variable info item
    bool containsVarInfoItem(std::string* elementName);

    // Returns the dimension of this info item, that is
    // the number of simultaneous held VarInfoItems.
    int getDimension();
    // Returns the VarInfoItem at the given position. The
    // position parameter must be less than the dimension
    // of this instance, otherwise an error will be thrown.
    VarInfoItem* getVarInfoItemAt(unsigned int pos);
    // Adds a VarInfoItem to this instance.
    void addVarInfoItem(VarInfoItem* infoItem);
};


}  // namespace AmbiguityCFG


#endif  // SRC_AMBIGUITY_CFG_GEN_VAR_INFO_ITEM_HH_
