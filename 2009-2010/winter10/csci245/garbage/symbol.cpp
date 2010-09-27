#include <iostream>
#include "symbol.h"

using namespace std;

Symbol::Symbol()
{	
	this->init();
}

string Symbol::get_string()
{
	return this->name;
}

bool Symbol::eq(Managed other)
{
	if(this->name == other->get_string())
		return true;
	else
		return false;
}

Managed Symbol::eval(ManagedEnv env)
{
	return env->lookup(this);
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
	for (unsigned int i = 0; i < this->name.length(); i++)
		cout << this->name[i];
}
