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

#ifndef SRC_UTIL_SYMBOL_TABLE_HH_
#define SRC_UTIL_SYMBOL_TABLE_HH_


#include <cstddef>

#include "../hashtable.hh"


namespace Util {


// A SymbolTable is basically a hashtable, extended by
// a nesting mechanism, that is all access methods use
// first the local table as lookup and storage destination
// and only if the item is not defined locally, broadens
// the search into the parent table.
// NOTE that this is a template class, thus the whole implementation
// is made inline in this header file.
template<class TKey, class TValue> class SymbolTable {
 private:
    // The parent symbol table. This pointer is NULL
    // if this is the root table.
    SymbolTable<TKey, TValue>* parentTable;

    // The table that holds all local definitions.
    hashtable<TKey, TValue> table;

 public:
    SymbolTable()
      : parentTable(NULL) {
    }


    explicit SymbolTable(SymbolTable<TKey, TValue>* parent)
      : parentTable(parent) {
    }


    ~SymbolTable() {
      if (this->parentTable != NULL) {
        // this->parentTable->~SymbolTable();
      }
    }


    // Returns the value stored in this table for the
    // key. If key is not stored locally, the parent
    // table is used to look up the assiciated value.
    TValue getElement(TKey key) {
      if (this->table.find(key) != this->table.end()) {
        return this->table[key];
      } else if (this->parentTable != NULL) {
        return this->parentTable->getElement(key);
      } else {
        return NULL;
      }
    }


    // Sets the value for a given key locally. If the
    // key is already set, the old value will be overwritten
    // by the newly provided value.
    void setElement(TKey key, TValue value) {
      this->table[key] = value;
    }


    // Removes key and its associated value from this table.
    // If the element is stored in the parent table, it will
    // be removed from the parent table. This method preserves
    // any further definitions of the key in deeper nested
    // parent tables and stops removing elements after the
    // first element has been removed, of the bottom of the
    // hierarchy is hit.
    void removeElement(TKey key) {
      if (this->table.find(key) != this->table.end()) {
        this->table.erase(key);
      } else if (this->parentTable != NULL) {
        this->parentTable->removeElement(key);
      }
    }


    // Removes the key and its associated value from the local
    // table and from the parent table. This method removes
    // all occurences of key and its corresponding values. After
    // this method has been called, the key has been removed
    // from all tables including all nested parent tables
    // from top to bottom of the table hierarchy.
    void removeElementDeep(TKey key) {
      if (this->table.find(key) != this->table.end()) {
        this->table.erase(key);
      }
      if (this->parentTable != NULL) {
        this->parentTable->removeElementDeep(key);
      }
    }


    // Removes the key and its associated value from the local
    // table. The parent table is left unchanged.
    void removeElementLocally(TKey key) {
      if (this->table.find(key) != this->table.end()) {
        this->table.erase(key);
      }
    }


    // Checks if the key is stored in this symbol table.
    // The search for the element also includes the
    // parent table.
    bool containsElement(TKey key) {
      if (this->table.find(key) != this->table.end()) {
        return true;
      } else if (this->parentTable != NULL) {
        return this->parentTable->containsElement(key);
      } else {
        return false;
      }
    }


    // Checks if the key is stored locally in this
    // symbol table.
    bool containsElementLocally(TKey key) {
      if (this->table.find(key) != this->table.end()) {
        return true;
      } else {
        return false;
      }
    }


    // Cleans this symbol table and its parent table as well.
    void cleanDeep() {
      this->cleanLocally();
      if (this->parentTable != NULL) {
        this->parentTable->cleanDeep();
      }
    }

    // Cleans this table only locally leaving its parent
    // table unclanged.
    void cleanLocally() {
      this->table.clear();
    }
};


};  // namespace Util


#endif  // SRC_UTIL_SYMBOL_TABLE_HH_
