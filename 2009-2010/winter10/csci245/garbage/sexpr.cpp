#include "sexpr.h"
#include "managed.h"
#include "environment.h"

using namespace std;

Managed Sexpr::eval(ManagedEnv env)
{
	return this;
}
	
Managed Sexpr::eval_map(ManagedEnv env)
{
	cout << "eval_map failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

Managed Sexpr::eval_block(ManagedEnv env)
{
	cout << "eval_block failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

Managed Sexpr::add_list(ManagedEnv env)
{
	cout << "Add_list failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

Managed Sexpr::subtract_list(ManagedEnv env)
{
	cout << "Subtract_list failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

Managed Sexpr::multiply_list(ManagedEnv env)
{
	cout << "multiply_list failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

Managed Sexpr::apply(Managed actualparameters)
{
	cout << "apply failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

string Sexpr::get_string()
{
	cout << "Get_string failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

bignum Sexpr::get_number()
{
	cout << "get_number failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

bool Sexpr::zero()
{
	return false;
}

bool Sexpr::eq(Managed other)
{
	return false;
}

Managed Sexpr::get_first()
{
	cout << "Get_first failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

Managed Sexpr::get_rest()
{
	cout << "get_rest failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

Managed Sexpr::add(Managed other)
{
	cout << "add failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

Managed Sexpr::subtract(Managed other)
{
	cout << "subtract failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

Managed Sexpr::multiply(Managed other)
{
	cout << "multiply failed, received this-> ";
	this->print(cout);
	cout << endl;
	exit(2);
}

void Sexpr::init()
{
	this->refcount = 0;
}

void Sexpr::inc()
{
	this->refcount += 1;
}

void Sexpr::dec()
{
	if (this->refcount > 0)
	{
		this->refcount -= 1;
	
		if (this->refcount == 0)
			delete this;
	}
}

