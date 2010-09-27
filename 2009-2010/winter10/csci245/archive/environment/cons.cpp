#include <iostream>
#include "cons.h"
#include "functions.h"
#include "environment.h"

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
	Sexpr* sum = new Number(this->get_first()->get_number());
	while (rest != NULL && typeid(*first) == typeid(Number))
	{
		if (typeid(*rest) == typeid(Number))
		{
			sum->add(rest);
			rest = NULL;
		}
		else
		{
			next = rest->get_first();
			sum->add(next);
			rest = rest->get_rest();
		}
	}
	return sum;
}

Sexpr* Cons::subtract_list()
{
	Sexpr* difference = new Number(this->get_first()->get_number());
	return difference->subtract(rest->get_first());
}

Sexpr* Cons::multiply_list()
{
	Sexpr* next;
	Sexpr* product = new Number(this->get_first()->get_number());
	while (rest != NULL && typeid(*first) == typeid(Number))
	{
		if (typeid(*rest) == typeid(Number))
		{
			product->multiply(rest);
			rest = NULL;
		}
		else
		{
			next = rest->get_first();
			product->multiply(next);
			rest = rest->get_rest();
		}
	}
	return product;
}

Sexpr* Cons::eval(Environment* env)
{
	if (first == NULL && rest == NULL)
		return NULL;
	
	string command = first->get_string();
	
	if (command == "define")
	{
		Sexpr* arg1 = rest->get_first();
		Sexpr* arg2 = rest->get_rest()->eval_map(env);
		if (arg2->get_first() != NULL)
			arg2 = arg2->get_first();
		env->insert(arg1, arg2);
		return arg1;
	}
	else if (command == "if")
	{
		Sexpr* arg1 = rest->get_first()->eval(env);
		if (arg1->eval(env) != NULL)
			return rest->get_rest()->get_first()->eval(env);
		else
			return rest->get_rest()->get_rest()->get_first()->eval(env);
	}
	else if (command == "quote")
	{
		return rest->get_first();
	}
	else if (command == "set!")
	{
		Sexpr* arg1 = rest->get_first();
		Sexpr* arg2 = rest->get_rest()->get_first()->eval(env);
		env->set(arg1, arg2);
		return arg1;
	}
	
	if (rest != NULL)
	{
		rest = rest->eval_map(env);

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
			if (rest->get_first() == NULL)
				return new Number(1);
			else
				return new Cons();
		}
		else if (command == "car")
		{
			if (rest->get_first() == NULL)
				return new Cons();
			else
				return rest->get_first()->get_first();
		}
		else if (command == "cdr")
		{
			return rest->get_first()->get_rest();
		}
		else if (command == "eq?")
		{
			Sexpr* arg1 = rest->get_first();
			Sexpr* arg2 = rest->get_rest()->get_first();
			if (arg1 == NULL || arg2 == NULL)
			{
				if (arg1 == arg2)
					return new Number(1);
				else
					return new Cons();
			}
			else if (arg1->eq(arg2))
				return new Number(1);
			else
				return new Cons();
		}
	}
	else if (rest == NULL)
	{
		if (command == "env")
		{
			env->print(std::cout);
			return NULL;
		}
		else if (command == "exit")
		{
			std::cout << "Farewell!" << endl;
			exit(0);
		}
	}
	return NULL;
}

Sexpr* Cons::eval_map(Environment* env)
{
	Sexpr* newfirst = NULL;
	
	if (first != NULL)
		newfirst = first->eval(env);
	
	Sexpr* newrest = NULL;
	
	if (newfirst != NULL)
	{
		if (typeid(*newfirst) == typeid(Cons))
		{
			if (newfirst->get_first() == NULL && newfirst->get_rest() == NULL)
				newfirst = NULL;
		}
	}
	
	if (rest != NULL)
		newrest = rest->eval_map(env);

	return new Cons(newfirst, newrest);
}
