#include "closure.h"

Closure::Closure(Sexpr* p, Sexpr* b, Environment* env)
{
	formalparameters = p;
	body = b;
	storedenv = env;
}

Sexpr* Closure::apply(Sexpr* actualparameters)
{
	Environment* instance = new Environment(storedenv);
	instance->extend(formalparameters, actualparameters);
	return body->eval_block(instance);
}

void Closure::print(ostream& out)
{
	cout << "closure : ";
	if (formalparameters == NULL)
		out << "()";
	else
		formalparameters->print(out);
	cout << " ";
	body->print(out);
}
