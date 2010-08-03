#include <iostream>
#include "cons.h"
#include "functions.h"
#include "environment.h"
#include "closure.h"

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
	
	print_sexpr(out, first);
		
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

Sexpr* Cons::add_list(Environment* env)
{
	if (rest == NULL)
		return new Number(first->get_number());
	
	Sexpr* totalsum = NULL;
	if (typeid(*first) == typeid(Cons))
	{
		Sexpr* restsum = first->eval(env)->get_first();
		totalsum = restsum->add(rest->add_list(env));
	}
	else
	{
		totalsum = first->add(rest->add_list(env)); 
	}
	
	return totalsum;
}

Sexpr* Cons::subtract_list(Environment* env)
{
	Sexpr* difference = new Number(this->get_first()->get_number());
	return difference->subtract(rest->get_first());
}

Sexpr* Cons::multiply_list(Environment* env)
{
	if (rest == NULL)
		return new Number(first->get_number());
	
	Sexpr* totalproduct = NULL;
	if (typeid(*first) == typeid(Cons))
	{
		Sexpr* restproduct = first->eval(env)->get_first();
		totalproduct = restproduct->multiply(rest->multiply_list(env));
	}
	else
	{
		totalproduct = first->multiply(rest->multiply_list(env)); 
	}

	return totalproduct;
}

Sexpr* Cons::eval(Environment* env)
{
	string command = "";
	
	if (typeid(*first) == typeid(Symbol))
		command = first->get_string();
	
	if (command == "define")
	{
		Sexpr* arg1 = rest->get_first();
		Sexpr* arg2 = eval_sexpr(rest->get_rest()->get_first(), env);
		
		if (env->lookup(arg1) != NULL)
			cout << "Error: symbol is already defined: ";
		else
			env->insert(arg1, arg2);
		
		return arg1;
	}
	else if (command == "if")
	{
		if (eval_sexpr(rest->get_first(), env) != NULL)
		{
			Sexpr* truecase = eval_sexpr(rest->get_rest()->get_first(), env);
			return truecase;
		}
		else
		{
			Sexpr* falsecase = eval_sexpr(rest->get_rest()->get_rest()->get_first(), env);
			return falsecase;
		}
	}
	else if (command == "quote")
	{
		return rest->get_first();
	}
	else if (command == "set!")
	{
		Sexpr* arg1 = rest->get_first();
		Sexpr* arg2 = eval_sexpr(rest->get_rest()->get_first(), env);
		
		if (env->lookup(arg1) != NULL)
		{
			env->set(arg1, arg2);
			return arg1;
		}
			
		return NULL;
	}
	else if (command == "begin")
	{
		return rest->eval_block(env);
	}
	else if (command == "lambda")
	{
		Sexpr* p = rest->get_first();
		Sexpr* b = rest->get_rest();
		return new Closure(p, b, env);
	}
	
	if (rest == NULL)
	{
		if (command == "env")
		{
			env->print(std::cout);
			return new Sexpr();
		}
		else if (command == "exit")
		{
			std::cout << "Farewell!" << endl;
			exit(0);
		}
	}
	
	Sexpr* newrest = NULL;
	
	if (rest != NULL)
	{
		newrest = rest->eval_map(env);	
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
		Sexpr* car = newrest->get_first();
		Sexpr* cdr = newrest->get_rest()->get_first();
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
		Sexpr* arg1 = newrest->get_first();
		Sexpr* arg2 = newrest->get_rest()->get_first();
		
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
	
	Sexpr* newfirst = eval_sexpr(first, env);
	
	if (newfirst != NULL)
	{
		if (typeid(*newfirst) == typeid(Closure))
			return newfirst->apply(newrest);
	}
	
	return newfirst;
}

Sexpr* Cons::eval_map(Environment* env)
{
	Sexpr* newfirst = eval_sexpr(first, env);
	Sexpr* newrest = NULL;
	
	if (rest != NULL)
		newrest = rest->eval_map(env);

	return new Cons(newfirst, newrest);
}

Sexpr* Cons::eval_block(Environment* env)
{
	Sexpr* blockresult = eval_sexpr(this->first, env);
	
	if (rest != NULL)
		blockresult = rest->eval_block(env);
	
	return blockresult;
}
