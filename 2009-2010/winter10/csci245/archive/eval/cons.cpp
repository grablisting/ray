#include <iostream>
#include "cons.h"
#include "functions.h"

using namespace std;

Cons::Cons()
{
	first = NULL;
	rest = NULL;
}

Cons::Cons(Sexpr* newfirst, Sexpr* newrest)
{
	first = newfirst;
	rest = newrest;
}

Sexpr* Cons::get_first()
{
	return first;
}

Sexpr* Cons::get_rest()
{
	return rest;
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
		out << ')';
	else
	{
		if (typeid(*rest) != typeid(Cons))
		{
			out << " . ";
			rest->print(out);
			out << ')';
		}
		else
		{
			out << ' ';
			static_cast<Cons*>(rest)->print_cons(out, false);
		}
	}
}

Sexpr* Cons::add_list()
{
	Sexpr* next;
	while (rest != NULL && typeid(*first) == typeid(Number))
	{
		if (typeid(*rest) == typeid(Number))
		{
			first->add(rest);
			rest = NULL;
		}
		else
		{
			next = rest->get_first();
			first->add(next);
			rest = rest->get_rest();
		}
	}
	return first;
}

Sexpr* Cons::subtract_list()
{
	Sexpr* next = rest->get_first();
	first->subtract(next);
	return first;
}

Sexpr* Cons::multiply_list()
{
	Sexpr* next;
	while (rest != NULL && typeid(*first) == typeid(Number))
	{
		if (typeid(*rest) == typeid(Number))
		{
			first->multiply(rest);
			rest = NULL;
		}
		else
		{
			next = rest->get_first();
			first->multiply(next);
			rest = rest->get_rest();
		}
	}
	return first;
}

Sexpr* Cons::eval()
{
	if (rest == NULL)
	{
		if (first == NULL)
			return new Cons();
		else if (typeid(*first) != typeid(Cons))
			return first;
		else
			return first->eval();
	}
	else if (rest != NULL)
	{
		rest = rest->eval_map();
	}
	
	first = first->eval();
	string command = first->get_string();
	
	if (command == "+")
		return rest->add_list();
	else if (command == "-")
		return rest->subtract_list();
	else if (command == "*")
		return rest->multiply_list();
	else if (command == "list")
		return rest;
	else if (command == "cons")
	{
		Sexpr* car = rest->get_first();
		Sexpr* cdr = rest->get_rest()->get_first();
		return new Cons(car, cdr);
	}
	else if (command == "zero?")
	{
		if (rest->get_first()->zero())
			return new Number(1);
		else
			return new Cons();
	}
	else if (command == "null?")
	{
		Sexpr* sexpr = rest->eval();
		
		if (sexpr == NULL)
			return new Number(1);
		else if (typeid(*sexpr) == typeid(Cons))
		{
			if (sexpr->get_first() == NULL)
				return new Number(1);
		}
		else
			return new Cons();
	}
	else if (command == "car")
	{
		if (rest->get_first() == NULL)
			return NULL;
		else
			return rest->get_first()->get_first();
	}
	else if (command == "cdr")
	{
		return rest->get_first()->get_rest();
	}
	else
	{
		return NULL;
	}
	return NULL;
}

Sexpr* Cons::eval_map()
{
	Sexpr* newfirst = first->eval();
	Sexpr* newrest = NULL;
	
	if (rest != NULL)
	{
		if (typeid(*rest) == typeid(Number))
			newrest = rest->eval();
		else
			newrest = rest->eval_map();
	}
	
	Sexpr* newCons = new Cons(newfirst, newrest);
	return newCons;
}
