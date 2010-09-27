#ifndef MANAGED_H
#define MANAGED_H
#include <iostream>
#include "sexpr.h"

using namespace std;

class Managed
{
public:
	Managed();
	Managed(Sexpr* newsp);
	Managed(const Managed& M);
	~Managed();
	Managed& operator=(Sexpr* rhs);
	Managed& operator=(const Managed& rhs);
	bool operator==(const Managed& rhs);
	bool operator!=(const Managed& rhs);
	Sexpr* operator->();
	friend Sexpr& operator*(Managed& m) { return *m.sp; }
	void assign(Sexpr* newsp);
	Sexpr* get_sp() const { return sp; }
private:
	Sexpr* sp;
};
#endif
