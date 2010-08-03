#include <iostream>
#include "number.h"

using namespace std;

void Number::read(istream& in)
{
	char ch = in.peek();
	while(!isspace(ch) && (ch != '(') && (ch != ')'))
	{
		in.get(ch);
		name.push_back(ch);
		ch = in.peek();
	}
}

void Number::print(ostream& out)
{
	for (unsigned int i = 0; i < name.length(); i++)
	{
		cout << name[i];
	}
}