#ifndef SYMBOL_H
#define SYMBOL_H
#include "sexpr.h"
#include <iostream>

class Symbol : public Sexpr
{
public:
	Symbol() {};
	
	/**
	 Reads Symbols.
	 @param in is the source.
	 */
	virtual void read(istream& in);
	
	/**
	 Prints Symbols.
	 @param out is the destination.
	 */
	virtual void print(ostream& out);
	
private:
	string name;
};

#endif