#ifndef BIGNUM_H
#define BIGNUM_H
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

const int BASE = 1000;
const int BASE_EXPONENT = int(log10((double)BASE));
typedef vector<int> bignum;

/** 
 Collects digits until it finds an operator or end of line marker.
 @param in is the source.
 @param return acquired bignum vector.
 */
bignum bignum_read(istream& in);

/** 
 Carries the 1 until each vector of a Bignum exists between 0 and BASE.
 @param c is the given Bignum.
 @param return is the resultant Bignum.
 */
bignum bignum_carry(bignum& c);

/**
 Multiplies two Bignums together.
 @param a is the first.
 @param b is the second.
 @param return is the resultant Bignum.
 */
bignum bignum_multiply(bignum& a, bignum& b);

/** 
 Adds two Bignums together.
 @param a is the first.
 @param b is the second.
 @param return is the resultant Bignum.
 */
bignum bignum_add(bignum& a, bignum& b);

/**
 Subtracts one Bignum from another.
 @param a is the first.
 @param b is the second.
 @param return is the resultant Bignum.
 */
bignum bignum_subtract(bignum& a, bignum& b);

/**
 Pops leading zeroes of a Bignum.
 @param c is the vector to be printed.
 @param return is the resulting vector.
 */
bignum bignum_popzeroes(bignum& c);

/**
 Prints Bignum Vectors.
 @param print is the destination.
 @param c is the Bignum to be printed.
 */
void bignum_print(ostream& print, bignum& c);

#endif
