#include "managed.h"
#include "sexpr.h"


Managed::Managed()
{
	this->sp = NULL;
}

Managed::Managed(Sexpr* newsp)
{
	this->sp = NULL;
	this->assign(newsp);
}

Managed::Managed(const Managed& M)
{
	this->sp = NULL;
	this->assign(M.sp);
}

Managed::~Managed()
{
	this->assign(NULL);
}

Managed& Managed::operator=(Sexpr* rhs)
{
	this->assign(rhs);
	return *this;
}

Managed& Managed::operator=(const Managed& rhs)
{
	this->assign(rhs.sp);
	return *this;
}

bool Managed::operator==(const Managed& rhs)
{
	if (this->sp == rhs.sp)
		return true;
	else
		return false;
}

bool Managed::operator!=(const Managed& rhs)
{
	if (this->sp != rhs.sp)
		return true;
	else
		return false;
}

Sexpr* Managed::operator->()
{
	return this->sp;
}

void Managed::assign(Sexpr* newsp)
{
	if (newsp != this->sp)
	{	
		if (this->sp != NULL)
			this->sp->dec();

		if (newsp != NULL)
			newsp->inc();

		this->sp = newsp;
	}
}
