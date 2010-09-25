#ifndef CONS_H
#define CONS_H
#include "sexpr.h"
#include "functions.h"
#include "number.h"
#include "symbol.h"
#include <iostream>

class Cons : public Sexpr
{
public:
	Cons();
	
	/**
	 Creates a new Cons molecule.
	 @param newfirst is the car pointer.
	 @param newrest is the cdr pointer.
	 */
	Cons(Sexpr* newfirst, Sexpr* newrest);
	
	/**
	 Evalutes a Cons molecule.
	 @param return is a pointer to the start of a linked list.
	 */
	Sexpr* eval();
	
	/**
	 Creates a new linked list by evaluating an old a linked list.
	 @param return points to the start of the new list.
	 */
	Sexpr* eval_map();
	
	/**
	 Adds all numbers in a list together.
	 @param return points to the sum of all the arguments.
	 */
	Sexpr* add_list();
	
	/**
	 Subtracts two numbers in a list. Ignores extra arguments.
	 @param return points to the difference of the first two numbers.
	 */
	Sexpr* subtract_list();
	
	/**
	 Multiplies all numbers in a list together.
	 @param return points to the product of all the arguments.
	 */
	Sexpr* multiply_list();
	
	/**
	 Provide the car.
	 @param return is the pointer.
	 */
	Sexpr* get_first();
	
	/**
	 Provide the cdr.
	 @param return is the pointer.
	 */
	Sexpr* get_rest();
	
	/**
	 Reads Cons.
	 @param in is the source.
	 */
	void read(istream& in);
	
	/**
	 Creates a new Cons tier.
	 @param out is the destination.
	 */
	void print(ostream& out)
	{
		print_cons(out, true);
	};
	
	/**
	 Prints the atoms of Cons molecules.
	 @param out is the destination.
	 @param starting is Boolean sentinel for punctuation.
	 */
	void print_cons(ostream& out, bool starting);
	
private:
	Sexpr* first;
	Sexpr* rest;
};

#endif
