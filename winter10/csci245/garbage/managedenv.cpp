#include "managedenv.h"
#include "environment.h"

using namespace std;

ManagedEnv::ManagedEnv()
{
	this->ep = NULL;
}

ManagedEnv::ManagedEnv(Environment* newep)
{
	this->ep = NULL;
	this->assign(newep);
}

ManagedEnv::ManagedEnv(const ManagedEnv& Ep)
{
	this->ep = NULL;
	this->assign(Ep.ep);
}

ManagedEnv::~ManagedEnv()
{
	this->assign(NULL);
}

ManagedEnv& ManagedEnv::operator=(const ManagedEnv& rhs)
{
	this->assign(rhs.ep);
	return *this;
}

bool ManagedEnv::operator==(const ManagedEnv& rhs)
{
	if (this->ep == rhs.ep)
		return true;
	else
		return false;
}

bool ManagedEnv::operator!=(const ManagedEnv& rhs)
{
	if (this->ep != rhs.ep)
		return true;
	else
		return false;
}

Environment* ManagedEnv::operator->()
{
	return this->ep;
}

void ManagedEnv::assign(Environment* newep)
{
	if (newep != this->ep)
	{
		if (this->ep != NULL)
			this->ep->dec();
	
		if (newep != NULL)
			newep->inc();
	
		this->ep = newep;
	}
}
