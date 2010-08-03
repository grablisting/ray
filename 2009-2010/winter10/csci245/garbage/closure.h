#ifndef CLOSURE_H
#define CLOSURE_H
#include <iostream>
#include "sexpr.h"
#include "managed.h"
#include "managedenv.h"

using namespace std;

class Closure : public Sexpr
{
public:
	/**
	 Closure constructor; sets refcount to zero;
	 sets formalparameters, body, and storedenv to NULL.
	 */
	Closure();
	
	/**
	 Closure constructor; sets refcount to zero.
	 @param p sets formalparameters.
	 @param b sets body.
	 @param env sets storedenv.
	 */
	Closure(Managed p, Managed b, ManagedEnv env);
	
	/**
	 Closure destructor; sets formalparameters, body, and storedenv to NULL.
	 */
	~Closure();
	
	/**
	 Evaluates body argument in a new environment.
	 @param actualparameters are set to formalparameters in a new environment.
	 @param return is the result of the evaluated body.
	 */
	Managed apply(Managed actualparameters);
	
	/**
	 Prints the formalparameters and body list.
	 @param out is the destination.
	 */
	void print(ostream& out);
private:
	Managed formalparameters;
	Managed body;
	ManagedEnv storedenv;
};

#endif
