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
	Environment(Environment* lastenv);
	void print(ostream& out);
	void insert(Sexpr* key, Sexpr* value);
	void set(Sexpr* key, Sexpr* value);
	void extend(Sexpr* keys, Sexpr* vals);
	Sexpr* lookup(Sexpr* key);
private:
	map<string, Sexpr*> env;
	Environment* next;
};
#endif
