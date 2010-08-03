#ifndef SEXPR_H
#define SEXPR_H
#include <iostream>
#include <string>
#include <cstdlib>
#include <typeinfo>
#include "bignum.h"

using namespace std;

class Managed;
class ManagedEnv;
class Sexpr
{
public:
	virtual void read(istream& in){};
	virtual void print(ostream& out){};
	virtual Managed eval(ManagedEnv env);
	virtual Managed eval_map(ManagedEnv env);
	virtual Managed eval_block(ManagedEnv env);
	virtual Managed add_list(ManagedEnv env);
	virtual Managed subtract_list(ManagedEnv env);
	virtual Managed multiply_list(ManagedEnv env);
	virtual Managed apply(Managed actualparameters);
	virtual string get_string();
	virtual bignum get_number();
	virtual bool zero();
	virtual bool eq(Managed other);
	virtual Managed get_first();
	virtual Managed get_rest();
	virtual Managed add(Managed other);
	virtual Managed subtract(Managed other);
	virtual Managed multiply(Managed other);
	virtual ~Sexpr() {};
	virtual void init();
	virtual void inc();
	virtual void dec();
private:
	int refcount;
};

#endif
