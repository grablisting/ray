#ifndef SYMBOL_H
#define SYMBOL_H
#include "managed.h"
#include "environment.h"
#include <iostream>

class Symbol : public Sexpr
{
public:
	/**
	 Symbol constructor; initializes refcount to zero.
	 */
	Symbol();
	
	/**
	 Returns the symbol.
	 @param return is a string.
	 */
	string get_string();
	
	/**
	 Retrieves the value from an environment.
	 @param env is the environment to reference the symbol.
	 @param returns it's value from the environment's map.
	 */
	Managed eval(ManagedEnv env);
	
	/**
	 Compares strings between two symbols.
	 @param returns true if symbols are identical.
	 */
	bool eq(Managed other);
	
	/**
	 Reads Symbols.
	 @param in is the source.
	 */
	void read(istream& in);
	
	/**
	 Prints Symbols.
	 @param out is the destination.
	 */
	void print(ostream& out);
	
private:
	string name;
};

#endif
