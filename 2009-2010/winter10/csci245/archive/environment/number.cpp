#include <iostream>
#include "number.h"

using namespace std;

Number::Number(int b)
{
	Val.push_back(b);
}

Number::Number(bignum b)
{
	Val = b;
}

bool Number::zero()
{
	if (Val[0] == 0)
		return true;
	else
		return false;
}

void Number::read(istream& in)
{
	Val = bignum_read(in);
}

void Number::print(ostream& out)
{
	bignum_print(out, Val);
}

Sexpr* Number::add(Sexpr* other)
{
	bignum item = other->get_number();
	this->Val = bignum_add(Val, item);
	return this;
}

Sexpr* Number::subtract(Sexpr* other)
{
	bignum item = other->get_number();
	this->Val = bignum_subtract(Val, item);
	return this;
}

Sexpr* Number::multiply(Sexpr* other)
{
	bignum item = other->get_number();
	this->Val = bignum_multiply(Val, item);
	return this;
}

bignum Number::get_number()
{
	return Val;
}
