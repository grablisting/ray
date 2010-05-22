#ifndef SEXPR_H
#define SEXPR_H
#include <iostream>

using namespace std;

class Sexpr
{
	public:
	virtual void read(istream& in){};
	virtual void print(ostream& out){};
};

#endif