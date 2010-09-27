/**
 CSCI245 Homework 1
 @file bignum.cpp
 @author Ray Peters W00843595
 */

#include <iostream>
#include <vector>
#include <cmath>
#include <iomanip>
using namespace std;

const int BASE = 1000;
const double BASE_EXPONENT = int(log10((double)BASE));
typedef vector<int> bignum;

/** 
 Gathers digits to build a bignum vector until it reaches an operator or end_of_line marker.
 @param in is the source of the input.
 @param return acquired bignum vector.
 */
bignum bignum_read(istream& in)
{
	bignum b;
	char ch = '0';

	while (!(ch == '\n'))
	{
		in.get(ch);
		if ((ch == '+') || (ch == '-') || (ch == '*'))
		{
			in.putback(ch);
			break;
		}
		else if (isdigit(ch))
		{
			b.push_back(ch - '0');
		}
	}
	
	int j = 0;
	int k = 0;
	bignum a((b.size() / BASE_EXPONENT) + 1);
	int l = (a.size() - 1);
	
	for(unsigned int i = b.size(); i > 0; i--)
	{
		if (j == 0)
		{
			a[l] = 0;
		}
		
		k = b[i-1];
		
		if (j == 1)
			k *= 10;
		else if (j == 2)
			k *= 100;
		
		a[l] += k;
		j++;
		
		if (j == 3)
		{
			l--;
			j = 0;
		}
	}
	
	return a;
}

/**
 Shifts the vector leftward until nonzero digits exist in first vector position.
 @param c is the number to align.
 @param return is the modified number.
 */

bignum bignum_popzeros(bignum& c)
{
	while (c[0] == 0)
	{
		for (unsigned int j = 1; j < c.size(); j++)
		{
			c[j - 1] = c[j];
		}
		c.pop_back();
	}
	return c;
}

/** 
 Cycles through the vector to carry and borrow integers until they are 0 <= x < BASE.
 @param c is the input bignum vector.
 @param return is the new, simplified vector.
 */
bignum bignum_carry(bignum& c)
{
	bignum d(c.size());
	
	do
	{
		d = c;
		if (c[0] >= BASE)
		{
			c.push_back(c[d.size() - 1]);
			for (unsigned int j = 1; j < d.size(); j++)
			{
				c[d.size() - j] = c[d.size() - j - 1];
			}
			c[1] -= BASE;
			c[0] = 1;
		}
		
		for (unsigned int i = c.size() - 1; i > 0; i--)
		{
			while (c[i] >= BASE)
			{
				c[i - 1] += 1;
				c[i] -= BASE;
			}
		}
		
		for (unsigned int i = c.size() - 1; i > 0; i--)
		{
			while (c[i] < 0)
			{
				c[i - 1] -= 1;
				c[i] += BASE;
			}
		}
	} while (c != d);
	if (c[0] == 0) {
		c = bignum_popzeros(c);
	}
	return c;
}

/**
 Multiplies two Vectors together.
 @param a is the first bignum vector.
 @param b is the second bignum vector.
 @param return is the resultant vector.
 */
bignum bignum_multiply(bignum& a, bignum& b)
{
	bignum c(a.size() + b.size() - 1);

	for (unsigned int i = 0; i < a.size(); i++)
	{
		for (unsigned int j = 0; j < b.size(); j++)
		{
			c[i+j] += a[i] * b[j];
		}
		c = bignum_carry(c);
	}
	return c;
}

/** 
 Adds two Vectors together.
 @param a is the first bignum vector.
 @param b is the second bignum vector.
 @param return is the resultant vector.
 */
bignum bignum_add(bignum& a, bignum& b)
{
	int j;
	if (a.size() > b.size())
	{
		j = a.size();
	}
	else
	{
		j = b.size();
	}
	
	int k;
	bignum c(j);
	for (unsigned int i = 1; i < j + 1; i++)
	{
		k = 0;
		
		if ((a.size() - i) >= 0)
		{
			k += a[a.size() - i];
		}
		
		if ((b.size() - i) >= 0)
		{
			k += b[b.size() - i];
		}
		
		c[c.size() - i] = k;
	}
	c = bignum_carry(c);
	return c;	
}
/**
 Subtracts one Vector from another Vector.
 @param a is the first bignum vector.
 @param b is the second bignum vector.
 @param return is the resultant vector.
 */
bignum bignum_subtract(bignum& a, bignum& b)
{
	int j;
	if (a.size() > b.size())
	{
		j = a.size();
	}
	else
	{
		j = b.size();
	}
	
	int k;
	bignum c(j);
	for (unsigned int i = 1; i < (c.size() + 1); i++)
	{
		k = 0;
		
		if ((a.size() - i) >= 0)
		{
			k += a[a.size() - i];
		}
		
		if ((b.size() - i) >= 0)
		{
			k -= b[b.size() - i];
		}
		
		c[c.size() - i] = k;
	}
	c = bignum_carry(c);
	return c;	
}
/**
 Prints Vectors to an output stream.
 @param print is the location of the output.
 @param c is the vector to be printed.
 */
void bignum_print(ostream& print, bignum& c)
{
	cout << c[0];
	for (unsigned int i = 1; i < c.size(); i++)
	{	
		cout << ',';
		cout << setfill('-') << setw(3) << c[i];
	}
}


int main () 
{
	bignum a, b, c;
	char op;
	while(1)
	{	
		a = bignum_read(std::cin);
		std::cin >> op;
		b = bignum_read(std::cin);
		if (op == '+')		 c = bignum_add(a,b);
		else if (op == '-') c = bignum_subtract(a,b);
		else if (op == '*')  c = bignum_multiply(a,b);
		bignum_print(std::cout, c);
		std::cout << endl;
	}
    return 0;
}
