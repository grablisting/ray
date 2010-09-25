#ifndef SYMBOL_H
#define SYMBOL_H
#include "sexpr.h"
#include <iostream>

class Symbol : public Sexpr
{
public:
	Symbol(){};
	
	/**
	 Returns the symbol.
	 @param return is a string.
	 */
	string get_string();
	
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
