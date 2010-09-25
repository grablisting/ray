#ifndef CONS_H
#define CONS_H
#include "sexpr.h"
#include <iostream>

class Cons : public Sexpr
{
public:
	Cons();
	
	/**
	 Reads Constructors.
	 @param in is the source.
	 */
	virtual void read(istream& in);
	
	/**
	 Creates a new Constructor tier.
	 @param out is the destination.
	 */
	virtual void print(ostream& out)
	{
		print_cons(out, true);
	};
	
	/**
	 Prints the atoms of Constructor molecules.
	 @param out is the destination.
	 @param starting is Boolean sentinel for punctuation.
	 */
	void print_cons(ostream& out, bool starting);
	
private:
	Sexpr* first;
	Sexpr* rest;
};

#endif