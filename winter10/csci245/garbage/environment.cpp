#include "environment.h"
#include <iostream>
#include "sexpr.h"
#include "managed.h"

using namespace std;

Environment::Environment()
{
	this->refcount = 0;
	this->next = NULL;
}

Environment::Environment(ManagedEnv lastenv)
{
	this->refcount = 0;
	this->next = lastenv;
}

Environment::~Environment()
{	
	this->clear();
	this->next = NULL;
}

void Environment::clear()
{
	this->env.clear();
}

void Environment::print(ostream& out)
{	
	if (!this->env.empty())
	{
		map<string, Managed>::iterator p;
		for(p = this->env.begin(); p != env.end(); p++)
		{
			out << "   " << p->first << " : ";
			
			if (p->second == NULL)
				out << "()";
			else
				p->second->print(out);
			
			out << endl;
		}
		if (this->next != NULL)
		{
			if (!this->next->env.empty())
			{
				cout << "Next environment:" << endl;
				this->next->print(out);
			}
		}
	}
	else
	{
		out << "*environment is empty*" << endl;
	}
}

void Environment::insert(Managed key, Managed value)
{	
	this->env[key->get_string()] = value;
}

void Environment::extend(Managed keys, Managed vals)
{
	if (keys != NULL)
	{
		this->insert(keys->get_first(), vals->get_first());
	
		if (keys->get_rest() != NULL)
			this->extend(keys->get_rest(), vals->get_rest());
	}
}

void Environment::set(Managed key, Managed value)
{
	map<string, Managed>::iterator p;
	p = this->env.find(key->get_string());
	
	if (p == this->env.end())
	{
		if (this->next != NULL)
		{
			this->next->set(key, value);
		}
	}
	this->env[key->get_string()] = value;
}

Managed Environment::lookup(Managed key)
{
	map<string, Managed>::iterator p;
	p = this->env.find(key->get_string());
	
	if (p != this->env.end())
		return this->env.find(key->get_string())->second;
	else if (p == this->env.end() && this->next != NULL)
		return this->next->lookup(key);
	else
		return NULL;
}

void Environment::inc()
{
	this->refcount += 1;
}

void Environment::dec()
{
	if (this->refcount > 0)
	{
		this->refcount -= 1;

		if (this->refcount == 0)
			delete this;
	}
}
