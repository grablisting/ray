#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H
#include <string>
#include <map>
#include <cstdlib>

using namespace std;

class Sexpr;
class Environment
{
public:
	Environment(){};
	void print(ostream& out);
	void insert(Sexpr* key, Sexpr* value);
	void set(Sexpr* key, Sexpr* value);
	Sexpr* lookup(Sexpr* key);
private:
	map<string, Sexpr*> env;
};
#endif
