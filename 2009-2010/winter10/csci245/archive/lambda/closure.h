#ifndef CLOSURE_H
#define CLOSURE_H
#include <iostream>
#include "sexpr.h"
#include "environment.h"

using namespace std;

class Closure : public Sexpr
{
public:
	Closure(Sexpr* p, Sexpr* b, Environment* env);
	Sexpr* apply(Sexpr* actualparameters);
	void print(ostream& out);
private:
	Sexpr* formalparameters;
	Sexpr* body;
	Environment* storedenv;
};

#endif
