#ifndef FUNCTIONS_H
#define FUNCTIONS_H
#include "sexpr.h"
#include "managed.h"
#include "managedenv.h"
#include <iostream>

using namespace std;

/**
 Reads Symbolic Expressions.
 @param in is the source.
 @param return is the pointer to the first Cons molecule.
 */
Managed read_sexpr(istream& in);

/**
 Evaluates a Sexpr List.
 @param sexpr points to the start of a linked list.
 @param env is the environment to reference symbols to values.
 @param return is a pointer to the new, evaluated Sexpr list.
 */
Managed eval_sexpr(Managed sexpr, ManagedEnv env);

/** 
 Prints Symbolic Expressions.
 @param out is the destination.
 @param sexpr calls the first Cons molecule.
 */
void print_sexpr(ostream& out, Managed sexpr);

/**
 Skips whitespace between Sexprs.
 @param in is the source.
 */
void skip_whitespace(istream& in);

#endif
