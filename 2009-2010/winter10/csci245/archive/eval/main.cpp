/**
 CSCI245 Homework 3
 @file main.cpp
 @author Ray Peters W00843595
 */
#include <iostream>
#include "sexpr.h"
#include "functions.h"
using namespace std;

int main () {
	Sexpr *sexpr;
	while(1) {
		cout << "> ";
		sexpr = read_sexpr(std::cin);
		cout << "In: ";
		print_sexpr(std::cout, sexpr);
		cout << endl;
		sexpr = eval_sexpr(sexpr);
		cout << "Out: ";
		print_sexpr(std::cout, sexpr);
		cout << endl;
	}
	return 0;
}
