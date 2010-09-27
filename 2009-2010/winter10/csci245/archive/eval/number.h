#ifndef NUMBER_H
#define NUMBER_H
#include "sexpr.h"
#include <iostream>
#include "bignum.h"

class Number : public Sexpr
{
public:
	Number(){};
	
	/**
	 Stores an integer as a new bignum.
	 @param b is the integer used to create the bignum.
	 */
	Number(int b);
	
	/**
	 Creates a new bignum.
	 @param b is the bignum used to create the bignum.
	 */
	Number(bignum b);
	
	/**
	 Reads numbers.
	 @param in is the source.
	 */
	void read(istream& in);
	
	/**
	 Prints numbers.
	 @param out is the destination.
	 */
	void print(ostream& out);
	
	/**
	 Calls bignum_add.
	 @param other is the number to add.
	 @param return is a pointer to the result.
	 */
	Sexpr* add(Sexpr* other);
	
	/**
	 Calls bignum_subtract.
	 @param other is the number to subtract.
	 @param return is a pointer to the difference.
	 */
	Sexpr* subtract(Sexpr* other);
	
	/**
	 Calls bignum_multiply.
	 @param other is the number to multiply.
	 @param return is a pointer to the product.
	 */
	Sexpr* multiply(Sexpr* other);
	
	/**
	 Returns the value of the Sexpr::Number.
	 @param return is the Sexpr::Number.
	 */
	bignum get_number();
	
	/**
	 Compares Val[0] to zero.
	 @param return is a boolean answer.
	 */
	bool zero();
	
private:
	bignum Val;
};

#endif
