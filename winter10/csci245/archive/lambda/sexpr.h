#ifndef SEXPR_H
#define SEXPR_H
#include <iostream>
#include <string>
#include <cstdlib>
#include <typeinfo>
#include "environment.h"
#include "bignum.h"

using namespace std;

class Sexpr
{
public:
	virtual void read(istream& in){};
	virtual void print(ostream& out){};
	virtual Sexpr* eval(Environment* env);
	virtual Sexpr* eval_map(Environment* env);
	virtual Sexpr* eval_block(Environment* env);
	virtual Sexpr* add_list(Environment* env);
	virtual Sexpr* subtract_list(Environment* env);
	virtual Sexpr* multiply_list(Environment* env);
	virtual Sexpr* apply(Sexpr* actualparameters);
	virtual string get_string();
	virtual bignum get_number();
	virtual bool zero();
	virtual bool eq(Sexpr* other);
	virtual Sexpr* get_first();
	virtual Sexpr* get_rest();
	virtual Sexpr* add(Sexpr* other);
	virtual Sexpr* subtract(Sexpr* other);
	virtual Sexpr* multiply(Sexpr* other);
};

#endif
