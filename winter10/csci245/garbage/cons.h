#ifndef CONS_H
#define CONS_H
#include "sexpr.h"
#include "environment.h"
#include "functions.h"
#include "number.h"
#include "symbol.h"
#include "managed.h"
#include <iostream>

class Cons : public Sexpr
{
public:
	/**
	 Cons constructor; initializes refcount to zero; sets first and rest to NULL.
	 */
	Cons();
	
	/**
	 Cons constructor; initializes refcount to zero.
	 @param newfirst is set to first.
	 @param newrest is set to rest.
	 */
	Cons(Managed newfirst, Managed newrest);
	
	/**
	 Cons destructor; sets first and rest to NULL.
	 */
	~Cons();
	
	/**
	 Eval evaluates a Cons sexpr.
	 @param env is the environment to reference symbols to values.
	 @param return is a pointer to the result of the function.
	 */
	Managed eval(ManagedEnv env);
	
	/**
	 Eval_map creates a new list of evaluated Cons Sexprs.
	 @param env is the environment to reference symbols to values.
	 @param return points to the start of the new list.
	 */
	Managed eval_map(ManagedEnv env);
	
	/**
	 Eval_block creates a new list of evaluated Cons Sexprs.
	 @param env is the environment to reference symbols to values.
	 @param return points to the last thing of the evaluated list.
	 */
	Managed eval_block(ManagedEnv env);
	
	/**
	 Adds all numbers in a list together.
	 @param env is the environment to reference symbols to values.
	 @param return points to the sum of the list.
	 */
	Managed add_list(ManagedEnv env);
	
	/**
	 Subtracts two numbers in a list. Ignores extra arguments.
	 @param env is the environment to reference symbols to values.
	 @param return points to the difference of the first two numbers.
	 */
	Managed subtract_list(ManagedEnv env);
	
	/**
	 Multiplies all numbers in a list together.
	 @param env is the environment to reference symbols to values.
	 @param return points to the product of the list.
	 */
	Managed multiply_list(ManagedEnv env);
	
	/**
	 Provide the car.
	 @param return is the pointer.
	 */
	Managed get_first();
	
	/**
	 Provide the cdr.
	 @param return is the pointer.
	 */
	Managed get_rest();
	
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
	Managed first;
	Managed rest;
};

#endif
