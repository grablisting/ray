#include <iostream>
#include "cons.h"
#include "functions.h"

using namespace std;

Cons::Cons()
{
	first = NULL;
	rest = NULL;
}

void Cons::read(istream& in)
{
	first = read_sexpr(in);
	skip_whitespace(in);
	
	char ch;
	ch = in.peek();
	if (ch == '.')
	{
		in.get(ch);
		rest = read_sexpr(in);
		skip_whitespace(in);
		
		in.get(ch);
		if (ch != ')')
			throw 2;
	}
	else if (ch == ')')
	{
		in.get(ch);
		rest = NULL;
	}
	else
	{
		rest = new Cons();
		rest->read(in);
	}
}

void Cons::print_cons(ostream& out, bool starting)
{
	if (starting)
		out << '(';
	
	if (first != NULL)
		first->print(out);
		
	if (rest == NULL)
	{
		out << ')';
	}
	else
	{
		if (first != NULL)
			out << ' ';
		
		if (typeid(*rest) != typeid(Cons))
		{
			rest->print(out);
			out << ')';
		}
		else
		{
			static_cast<Cons*>(rest)->print_cons(out, false);
		}
	}
	delete first;
	delete rest;
}