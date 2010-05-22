#ifndef FUNCTIONS_H
#define FUNCTIONS_H
#include "sexpr.h"
#include <iostream>

using namespace std;

/**
 Reads Symbolic Expressions.
 @param in is the source.
 @param return is the pointer to the first Cons molecule.
 */
Sexpr* read_sexpr(istream& in);

/**
 Evaluates a Sexpr List.
 @param sexpr points to the start of a linked list.
 @param return is a pointer to the new, evaluated Sexpr list.
 */
Sexpr* eval_sexpr(Sexpr* sexpr, Environment* env);

/** 
 Prints Symbolic Expressions.
 @param out is the destination.
 @param sexpr calls the first Cons molecule.
 */
void print_sexpr(ostream& out, Sexpr* sexpr);

/**
 Skips whitespace between Sexprs.
 @param in is the source.
 */
void skip_whitespace(istream& in);

#endif
