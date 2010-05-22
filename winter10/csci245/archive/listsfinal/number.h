#ifndef NUMBER_H
#define NUMBER_H
#include "sexpr.h"
#include <iostream>

class Number : public Sexpr
{
public:
	Number() {};
	
	/**
	 Reads numbers.
	 @param in is the source.
	 */
	virtual void read(istream& in);
	
	/**
	 Prints numbers.
	 @param out is the destination.
	 */
	virtual void print(ostream& out);
	
private:
	string name;
};

#endif