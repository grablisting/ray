#include <iostream>
#include "number.h"

using namespace std;

Number::Number()
{
	this->init();
}

Number::Number(int b)
{
	this->init();
	this->Val.push_back(b);
	Val = bignum_carry(Val);
}

Number::Number(bignum b)
{
	this->init();
	this->Val = b;
}

bool Number::zero()
{
	if (this->Val.size() == 1 && Val[0] == 0)
		return true;
	else
		return false;
}

void Number::read(istream& in)
{
	this->Val = bignum_read(in);
}

void Number::print(ostream& out)
{
	bignum_print(out, this->Val);
}

Managed Number::add(Managed other)
{
	bignum item = other->get_number();
	return new Number(bignum_add(this->Val, item));
}

Managed Number::subtract(Managed other)
{
	bignum item = other->get_number();
	return new Number(bignum_subtract(this->Val, item));
}

Managed Number::multiply(Managed other)
{
	bignum item = other->get_number();
	return new Number(bignum_multiply(this->Val, item));
}

bignum Number::get_number()
{
	return this->Val;
}
