/**
 CSCI245 Homework 6
 @file main.cpp
 @author Ray Peters W00843595
 */
#include <iostream>
#include "managed.h"
#include "functions.h"
#include "environment.h"

using namespace std;

int main () {
	ManagedEnv GlobalEnv = new Environment();
	Managed sexpr;
	
	while(1) {
		cout << "> ";
		sexpr = read_sexpr(cin);
		sexpr = eval_sexpr(sexpr, GlobalEnv);
		print_sexpr(cout, sexpr);
		cout << endl;
	}
	return 0;
}
