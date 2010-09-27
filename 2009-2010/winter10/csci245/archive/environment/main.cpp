/**
 CSCI245 Homework 4
 @file main.cpp
 @author Ray Peters W00843595
 */
#include <iostream>
#include "sexpr.h"
#include "functions.h"
#include "environment.h"

using namespace std;

int main () {
	Environment* GlobalEnv = new Environment();
	Sexpr *sexpr;
	while(1) {
		cout << "> ";
		sexpr = read_sexpr(std::cin);
		//cout << "In: ";
		//print_sexpr(std::cout, sexpr);
		//cout << endl;
		//cout << "Out: ";
		sexpr = eval_sexpr(sexpr, GlobalEnv);
		print_sexpr(std::cout, sexpr);
		cout << endl;
	}
	return 0;
}
