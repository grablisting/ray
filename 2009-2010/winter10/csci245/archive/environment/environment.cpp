#include "environment.h"
#include <iostream>
#include "sexpr.h"

using namespace std;

void Environment::print(ostream& out)
{
	if (!env.empty())
	{
	out << "Environment begin:" << endl;
	map<string, Sexpr*>::iterator p;
	p = env.begin();
	for(p = env.begin(); p != env.end(); p++)
	{
		out << "   " << (*p).first << " : ";
		(*p).second->print(out);
		out << endl;
	}
	out << "Environment end.";
	}
	else
		out << "*environment is empty*";
}

void Environment::insert(Sexpr* key, Sexpr* value)
{
	env[key->get_string()] = value;
}

void Environment::set(Sexpr* key, Sexpr* value)
{
	map<string, Sexpr*>::iterator p;
	p = env.find(key->get_string());
	
	if (p != env.end())
		this->insert(key, value);
	else
		std::cout << "Error: symbol has no value: ";
}

Sexpr* Environment::lookup(Sexpr* key)
{
	map<string, Sexpr*>::iterator p;
	p = env.find(key->get_string());
	
	if (p != env.end())
		return env.find(key->get_string())->second;
	else
	{
		std::cout << "Error: symbol has no value: ";
		return key;
	}
}
