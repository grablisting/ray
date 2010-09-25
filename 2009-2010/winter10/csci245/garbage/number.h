#ifndef NUMBER_H
#define NUMBER_H
#include "sexpr.h"
#include <iostream>
#include "bignum.h"
#include "managed.h"

class Number : public Sexpr
{
public:
	/**
	 Number constructor; initializes refcount to zero.
	 */
	Number();
	
	
	/**
	 Number constructor; initializes refcount to zero.
	 @param b sets an integer to bignum Val.
	 */
	Number(int b);
	
	
	/**
	 Number constructor; initializes refcount to zero.
	 @param b sets a bignum value to bignum Val.
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
	 Adds two numbers via bignum_add.
	 @param other is the number to add to Val.
	 @param return is a pointer to a new, resulting number.
	 */
	Managed add(Managed other);
	
	/**
	 Subtracts two numbers via bignum_subtract.
	 @param other is the number to subtract from Val.
	 @param return is a pointer to the difference.
	 */
	Managed subtract(Managed other);
	
	/**
	 Multiplies two numbers via bignum_multiply.
	 @param other is the number to multiply to Val.
	 @param return is a pointer to the product.
	 */
	Managed multiply(Managed other);
	
	/**
	 Get_Number provides access to Val.
	 @param return is bignum Val.
	 */
	bignum get_number();
	
	/**
	 Compares Val to zero.
	 @param return is a boolean answer.
	 */
	bool zero();
	
private:
	bignum Val;
};

#endif
