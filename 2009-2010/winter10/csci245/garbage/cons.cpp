#include <iostream>
#include "cons.h"
#include "functions.h"
#include "closure.h"
#include "environment.h"
#include "managed.h"

using namespace std;

Cons::Cons()
{
	this->init();
	this->first = NULL;
	this->rest = NULL;
}

Cons::Cons(Managed newfirst, Managed newrest)
{
	this->init();
	this->first = newfirst;
	this->rest = newrest;
}

Cons::~Cons()
{
	this->first = NULL;
	this->rest = NULL;
}

Managed Cons::get_first()
{
	return this->first;
}

Managed Cons::get_rest()
{
	return this->rest;
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
	
	print_sexpr(out, this->first);
		
	if (this->rest == NULL)
		out << ')';
	else
	{
		if (typeid(*this->rest) != typeid(Cons))
		{
			out << " . ";
			this->rest->print(out);
			out << ')';
		}
		else
		{
			out << ' ';
			static_cast<Cons*>(this->rest.get_sp())->print_cons(out, false);
		}
	}
}

Managed Cons::add_list(ManagedEnv env)
{
	if (this->rest == NULL)
		return new Number(this->first->get_number());
	
	Managed totalsum = NULL;
	if (typeid(*this->first) == typeid(Cons))
	{
		Managed restsum = this->first->eval(env)->get_first();
		totalsum = restsum->add(this->rest->add_list(env));
	}
	else
	{
		totalsum = this->first->add(this->rest->add_list(env)); 
	}
	
	return totalsum;
}

Managed Cons::subtract_list(ManagedEnv env)
{
	Managed difference = new Number(this->first->get_number());
	return difference->subtract(this->rest->get_first());
}

Managed Cons::multiply_list(ManagedEnv env)
{
	if (this->rest == NULL)
		return new Number(this->first->get_number());
	
	Managed totalproduct = NULL;
	if (typeid(*this->first) == typeid(Cons))
	{
		Managed restproduct = this->first->eval(env)->get_first();
		totalproduct = restproduct->multiply(this->rest->multiply_list(env));
	}
	else
	{
		totalproduct = this->first->multiply(this->rest->multiply_list(env)); 
	}

	return totalproduct;
}

Managed Cons::eval(ManagedEnv env)
{
	string command = "";
	Managed newrest = NULL;
	
	if (typeid(*this->first) == typeid(Symbol))
		command = first->get_string();
	
	if (command == "define")
	{
		Managed arg1 = this->rest->get_first();
		Managed arg2 = eval_sexpr(this->rest->get_rest()->get_first(), env);
		
		if (env->lookup(arg1) != NULL)
			cout << "Error: symbol is already defined: ";
		else
			env->insert(arg1, arg2);
		
		return arg1;
	}
	else if (command == "if")
	{
		if (eval_sexpr(this->rest->get_first(), env) != NULL)
		{
			Managed truecase = eval_sexpr(this->rest->get_rest()->get_first(), env);
			return truecase;
		}
		else
		{
			Managed falsecase = eval_sexpr(this->rest->get_rest()->get_rest()->get_first(), env);
			return falsecase;
		}
	}
	else if (command == "quote")
	{
		return this->rest->get_first();
	}
	else if (command == "print")
	{
		if (this->rest != NULL)
		{
			newrest = rest->eval_map(env);
		}
		
		if (newrest != NULL)
		{
			newrest->get_first()->print(cout);
		}
		
		cout << endl;
		return new Sexpr;
	}
	else if (command == "set!")
	{
		Managed arg1 = this->rest->get_first();
		Managed arg2 = eval_sexpr(rest->get_rest()->get_first(), env);
		
		if (env->lookup(arg1) != NULL)
		{
			env->set(arg1, arg2);
			return arg1;
		}
			
		return NULL;
	}
	else if (command == "begin")
	{
		return this->rest->eval_block(env);
	}
	else if (command == "lambda")
	{
		Managed p = this->rest->get_first();
		Managed b = this->rest->get_rest();
		return new Closure(p, b, env);
	}
	
	if (this->rest == NULL)
	{
		if (command == "env")
		{
			cout << "Environment begin:" << endl;
			env->print(cout);
			cout << "Environment end.";
			return new Sexpr();
		}
		else if (command == "clearenv")
		{
			env->clear();
			return new Sexpr();
		}
		else if (command == "exit")
		{
			cout << "Farewell!" << endl;
			exit(0);
		}
	}
	
	if (this->rest != NULL)
	{
		newrest = this->rest->eval_map(env);	
	}
	
	if (command == "+")
		return newrest->add_list(env);
	else if (command == "-")
		return newrest->subtract_list(env);
	else if (command == "*")
		return newrest->multiply_list(env);
	else if (command == "list")
		return newrest;
	else if (command == "cons")
	{
		Managed car = newrest->get_first();
		Managed cdr = newrest->get_rest()->get_first();
		return new Cons(car, cdr);
	}
	else if (command == "zero?")
	{
		if (newrest->get_first()->zero())
			return new Number(1);
		else
			return NULL;
	}
	else if (command == "null?")
	{
		if (newrest->get_first() == NULL)
			return new Number(1);
		else
			return NULL;
	}
	else if (command == "car")
	{
		return newrest->get_first()->get_first();
	}
	else if (command == "cdr")
	{
		return newrest->get_first()->get_rest();
	}
	else if (command == "eq?")
	{
		Managed arg1 = newrest->get_first();
		Managed arg2 = newrest->get_rest()->get_first();
		
		if (arg1 == NULL || arg2 == NULL)
		{
			if (arg1 == arg2)
				return new Number(1);
			else
				return NULL;
		}
		else if (arg1->eq(arg2))
			return new Number(1);
		else
			return NULL;
	}
	
	Managed newfirst = eval_sexpr(this->first, env);
	
	if (newfirst != NULL)
	{
		if (typeid(*newfirst) == typeid(Closure))
			return newfirst->apply(newrest);
	}
	
	return newfirst;
}

Managed Cons::eval_map(ManagedEnv env)
{
	Managed newfirst = eval_sexpr(first, env);
	Managed newrest = NULL;
	
	if (this->rest != NULL)
		newrest = this->rest->eval_map(env);

	return new Cons(newfirst, newrest);
}

Managed Cons::eval_block(ManagedEnv env)
{
	Managed blockresult = eval_sexpr(this->first, env);
	
	if (this->rest != NULL)
		blockresult = this->rest->eval_block(env);
	
	return blockresult;
}
