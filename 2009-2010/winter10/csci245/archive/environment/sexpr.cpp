#include "sexpr.h"

Sexpr* Sexpr::eval(Environment* env)
{
	return this;
}
	
Sexpr* Sexpr::eval_map(Environment* env)
{
	std::cout << "eval_map failed, received this-> ";
	this->print(std::cout);
	std::cout << endl;
	exit(2);
}

Sexpr* Sexpr::add_list()
{
	std::cout << "Add_list failed, received this-> ";
	this->print(std::cout);
	std::cout << endl;
	exit(2);
}

Sexpr* Sexpr::subtract_list()
{
	std::cout << "Subtract_list failed, received this-> ";
	this->print(std::cout);
	std::cout << endl;
	exit(2);
}

Sexpr* Sexpr::multiply_list()
{
	std::cout << "multiply_list, received this-> ";
	this->print(std::cout);
	std::cout << endl;
	exit(2);
}

string Sexpr::get_string()
{
	std::cout << "Get_string failed, received this-> ";
	this->print(std::cout);
	std::cout << endl;
	exit(2);
}

bignum Sexpr::get_number()
{
	std::cout << "get_number failed, received this-> ";
	this->print(std::cout);
	std::cout << endl;
	exit(2);
}

bool Sexpr::zero()
{
	return false;
}

bool Sexpr::eq(Sexpr* other)
{
	return false;
}

Sexpr* Sexpr::get_first()
{
	std::cout << "Get_first failed, received this-> ";
	this->print(std::cout);
	std::cout << endl;
	exit(2);
}

Sexpr* Sexpr::get_rest()
{
	std::cout << "get_rest failed, received this-> ";
	this->print(std::cout);
	std::cout << endl;
	exit(2);
}

Sexpr* Sexpr::add(Sexpr* other)
{
	std::cout << "add failed, received this-> ";
	this->print(std::cout);
	std::cout << endl;
	exit(2);
}

Sexpr* Sexpr::subtract(Sexpr* other)
{
	std::cout << "subtract failed, received this-> ";
	this->print(std::cout);
	std::cout << endl;
	exit(2);
}

Sexpr* Sexpr::multiply(Sexpr* other)
{
	std::cout << "multiply failed, received this-> ";
	this->print(std::cout);
	std::cout << endl;
	exit(2);
}
