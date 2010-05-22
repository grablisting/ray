#ifndef MANAGEDENV_H
#define MANAGEDENV_H
#include <iostream>

using namespace std;

class Environment;
class ManagedEnv
{
public:
	ManagedEnv();
	ManagedEnv(Environment* newep);
	ManagedEnv(const ManagedEnv& Ep);
	~ManagedEnv();
	ManagedEnv& operator=(const ManagedEnv& rhs);
	bool operator==(const ManagedEnv& rhs);
	bool operator!=(const ManagedEnv& rhs);
	Environment* operator->();
	friend Environment& operator*(ManagedEnv& m) { return *m.ep; }
	void assign(Environment* newep);
	Environment* get_ep() const { return ep; }
private:
	Environment* ep;
};
#endif
