#include <string>
#include <iostream>
#include <map>
#include <cassert>

using namespace std;

class Foo {
  private:
  public:
    string n;
  Foo(const string &s) : n(s) {}
  ~Foo() { cerr << "Destroyed: " << n << endl; }
};

int main(int argc, char **argv)
{
  map<string, Foo*> *bar = new map<string, Foo*>();

  (*bar)[string("1")] = new Foo("1");
  (*bar)[string("2")] = new Foo("2");
  (*bar)[string("3")] = new Foo("3");

  map<string, Foo*>::iterator i = bar->find(string("4"));

  //cerr << i->first << endl;
  cerr << (i == bar->end()) << " foo 4 " << (i == bar->begin())  << " bar " << endl;

  i = bar->find(string("1"));
  cerr << (i == bar->end()) << " foo 1 " << (i == bar->begin())  << " bar " << endl;
  i = bar->find(string("2"));
  cerr << (i == bar->end()) << " foo 2 " << (i == bar->begin())  << " bar " << endl;
  i = bar->find(string("3"));
  cerr << (i == bar->end()) << " foo 3 " << (i == bar->begin())  << " bar " << endl;

  i->second = new Foo("4adas");
  cerr << "XXX " << (bar->find("4") == bar->end()) << std::endl;

  for (map<string, Foo*>::iterator i = bar->begin(); i != bar->end(); ++i)
    cerr << "  X " << i->first << " -> " << i->second->n << endl;

  delete (*bar)["1"];

  delete bar;

  return 0;
}
