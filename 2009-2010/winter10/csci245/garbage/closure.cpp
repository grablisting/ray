#include "closure.h"
#include "environment.h"

using namespace std;

Closure::Closure()
{	
	this->init();
	this->formalparameters = NULL;
	this->body = NULL;
	this->storedenv = NULL;
}

Closure::Closure(Managed p, Managed b, ManagedEnv env)
{
	this->init();
	this->formalparameters = p;
	this->body = b;
	this->storedenv = env;
}

Closure::~Closure()
{
	this->formalparameters = NULL;
	this->body = NULL;
	this->storedenv = NULL;
}	
	
Managed Closure::apply(Managed actualparameters)
{
	ManagedEnv instance = new Environment(this->storedenv);
	instance->extend(this->formalparameters, actualparameters);
	return this->body->eval_block(instance);
}

void Closure::print(ostream& out)
{
	cout << "closure : ";
	
	if (this->formalparameters == NULL)
		out << "()";
	else
		this->formalparameters->print(out);
	
	cout << " ";
	this->body->print(out);
}
