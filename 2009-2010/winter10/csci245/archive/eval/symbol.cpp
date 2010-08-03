#include <iostream>
#include "symbol.h"

using namespace std;

string Symbol::get_string()
{
	return name;
}

void Symbol::read(istream& in)
{
	char ch = in.peek();
	while(!isspace(ch) && (ch != '(') && (ch != ')'))
	{
		in.get(ch);
		name.push_back(ch);
		ch = in.peek();
	}
}

void Symbol::print(ostream& out)
{
	for (unsigned int i = 0; i < name.length(); i++)
	{
		cout << name[i];
	}
}
