#include "environment.h"
#include <iostream>
#include "sexpr.h"

using namespace std;

Environment::Environment(Environment* lastenv)
{
	next = lastenv;
}

void Environment::print(ostream& out)
{	
	if (!env.empty())
	{
		out << "Environment begin:" << endl;
		map<string, Sexpr*>::iterator p;
		for(p = env.begin(); p != env.end(); p++)
		{
			out << "   " << (*p).first << " : ";
			
			if ((*p).second == NULL)
				out << "()";
			else
				(*p).second->print(out);
			
			out << endl;
		}
		out << "Environment end.";
		if (next != NULL)
		{
			out << endl;
			next->print(out);
		}
	}
	else
	{
		out << "*environment is empty*";
	}
}

void Environment::insert(Sexpr* key, Sexpr* value)
{	
	env[key->get_string()] = value;
}

void Environment::extend(Sexpr* keys, Sexpr* vals)
{
	if (keys != NULL)
	{
		this->insert(keys->get_first(), vals->get_first());
	
		if (keys->get_rest() != NULL)
			this->extend(keys->get_rest(), vals->get_rest());
	}
}

void Environment::set(Sexpr* key, Sexpr* value)
{
	map<string, Sexpr*>::iterator p;
	p = env.find(key->get_string());
	
	if (p == env.end())
	{
		if (next != NULL)
		{
			next->set(key, value);
		}
	}
	env[key->get_string()] = value;
}

Sexpr* Environment::lookup(Sexpr* key)
{
	map<string, Sexpr*>::iterator p;
	p = env.find(key->get_string());
	
	if (p != env.end())
		return env.find(key->get_string())->second;
	else if (p == env.end() && next != NULL)
		return next->lookup(key);
	else
		return NULL;
}
