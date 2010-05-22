#include <iostream>
#include "functions.h"
#include "sexpr.h"
#include "symbol.h"
#include "number.h"
#include "cons.h"

using namespace std;

void skip_whitespace(istream& in)
{
	char ch = in.peek();
	while (isspace(ch))
	{
		in.get(ch);
		ch = in.peek();
	}
}

Sexpr* read_sexpr(istream& in)
{
	skip_whitespace(in);
	char ch;
	in.get(ch);
	
	if (ch == '(')
	{
		skip_whitespace(in);
		ch = in.peek();
		if (ch == ')')
		{
			in.get(ch);
			return NULL;
		}
		else
		{
			Sexpr* newcons = new Cons();
			newcons->read(in);
			return newcons;
		}
	}
	else if (isdigit(ch))
	{		
		in.unget();
		Sexpr* newdigit = new Number();
		newdigit->read(in);
		return newdigit;
	}
	else if (isgraph(ch))
	{	
		in.unget();
		Sexpr* newsymbol = new Symbol();
		newsymbol->read(in);
		return newsymbol;
	}
	else
	{
		throw 2;
	}
}

Sexpr* eval_sexpr(Sexpr* sexpr, Environment* env)
{
	if (sexpr != NULL)
		return sexpr->eval(env);
	else
		return NULL;
}

void print_sexpr(ostream& out, Sexpr* sexpr)
{
	if (sexpr != NULL)
		sexpr->print(out);
	else
		out << "()";
}
