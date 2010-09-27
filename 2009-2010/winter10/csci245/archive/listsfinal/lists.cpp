/**
 CSCI245 Homework 2
 @file lists.cpp
 @author Ray Peters W00843595
 */
#include <iostream>
#include "sexpr.h"
#include "functions.h"
using namespace std;

int main () {
	Sexpr *sexpr;
	while(1) {
		sexpr = read_sexpr(std::cin);
		print_sexpr(std::cout, sexpr);
		std::cout << endl;
		delete sexpr;
	}
	return 0;
}