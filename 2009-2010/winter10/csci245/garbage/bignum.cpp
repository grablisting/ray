/**
 CSCI245 Homework 1
 @file bignum.cpp
 @author Ray Peters W00843595
 */

#include "bignum.h"
#include <iostream>
#include <iomanip>
#include <cstdlib>
using namespace std;

bignum bignum_read(istream& in)
{
	bignum a;
	string num;
	char ch = in.peek();

	while (!isspace(ch) && ch != '(' && ch != ')')
	{
		in.get(ch);
		if (ch == '+' || ch == '-' || ch == '*')
		{
			in.putback(ch);
			break;
		}
		else if (isdigit(ch))
		{
			num += ch;
		}
		ch = in.peek();
	}
	
	for(unsigned int i = num.length(); i > num.length() % BASE_EXPONENT; i -= BASE_EXPONENT)
	{
		string subNum = num.substr(i - BASE_EXPONENT, BASE_EXPONENT);
		int subVector = atoi(subNum.c_str());
		a.push_back(subVector);
	}
	
	if (num.length() % BASE_EXPONENT != 0)
	{
		string subNum = num.substr(0, num.length() % BASE_EXPONENT);
		int subVector = atoi(subNum.c_str());
		a.push_back(subVector);
	}
	else if (num.length() == 0)
	{
		a.push_back(0);
	}
	
	a = bignum_popzeroes(a);
	return a;
}

bignum bignum_carry(bignum& c)
{
	for (unsigned int i = 0; i < c.size() - 1; i++)
	{
		if (c[i] >= BASE)
		{
			c[i + 1] += c[i] / BASE;
			c[i] = c[i] % BASE;
		}
	}
	return c;
}

bignum bignum_multiply(bignum& a, bignum& b)
{
	bignum c(a.size() + b.size() + 1);

	for (unsigned int i = 0; i < a.size(); i++)
	{
		for (unsigned int j = 0; j < b.size(); j++)
		{
			c[i + j] += a[i] * b[j];
		}
		c = bignum_carry(c);
	}
	c = bignum_popzeroes(c);
	return c;
}

bignum bignum_add(bignum& a, bignum& b)
{
	bignum c;
	int k;
	
	for (unsigned int i = 0; i < a.size() + b.size(); i++)
	{
		k = 0;
		
		if (i < a.size())
			k += a[i];
		
		if (i < b.size())
			k += b[i];
		
		c.push_back(k);
	}
	c = bignum_carry(c);
	c = bignum_popzeroes(c);
	return c;
}

bignum bignum_subtract(bignum& a, bignum& b)
{	
	bignum c;
	if (b.size() > a.size())
		c.push_back(0);
	else if (a.size() == b.size() && b.back() > a.back())
		c.push_back(0);
	else
	{
		int k;
		for (unsigned int i = 0; i < a.size() + b.size(); i++)
		{
			k = 0;
		
			if (i < a.size())
				k += a[i];
		
			if (i < b.size())
				k -= b[i];
	
			c.push_back(k);
		}
		
		for (unsigned int i = 0; i < c.size() - 1; i++)
		{
			while (c[i] < 0)
			{
				c[i + 1] -= 1;
				c[i] += BASE;
			}
		}
	}
	c = bignum_popzeroes(c);
	return c;
}

bignum bignum_popzeroes(bignum& c)
{
	if (c.size() > 1)
	{
		while (c.size() > 1 && c.back() == 0)
			c.pop_back();
	}
	return c;
}

void bignum_print(ostream& print, bignum& c)
{
	c = bignum_popzeroes(c);
	
	cout << c.back();
	if (c.size() - 2 >= 0)
	{
		for (unsigned int i = c.size()-2; i + 1 > 0; i--)
			cout << setfill('0') << setw(3) << c[i];
	}
}
