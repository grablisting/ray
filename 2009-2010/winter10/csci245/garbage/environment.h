#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H
#include <string>
#include <map>
#include <cstdlib>
#include "managedenv.h"

using namespace std;

class Managed;
class Environment
{
public:
	/**
	 Constructs environment; sets refcount to zero; sets next environment to NULL.
	 */
	Environment();
	
	/**
	 Constructs environment; sets refcount to zero.
	 @param lastenv points to the old environment.
	 */
	Environment(ManagedEnv lastenv);
	
	/**
	 Destructs environment; clears map; sets next pointer to NULL.
	 */
	~Environment();
	
	/**
	 Prints all contents of the environment's map.
	 @param out is the destination.
	 */
	void print(ostream& out);
	
	/**
	 Clears the contents of the map
	 */
	void clear();
	
	/**
	 Inserts a symbol and value into a map.
	 @param key is the symbol.
	 @param value is the thing to be referenced.
	 */
	void insert(Managed key, Managed value);
	
	/**
	 Set changes an existing map entry to a different value.
	 @param key is the existing, mapped symbol.
	 @param value is the new thing to be referenced.
	 */
	void set(Managed key, Managed value);
	
	/**
	 Extend copies a list of keys and values into a map.
	 @param keys is the list of symbols.
	 @param vals is the list of values to be referenced.
	 */
	void extend(Managed keys, Managed vals);
	
	/**
	 Lookup retrieves a pointer to a key's value.
	 @param key is the entry to find.
	 @param return is the key's value; NULL if the key was not mapped.
	 */
	Managed lookup(Managed key);
	
	/**
	 Increments refcount by 1.
	 */
	void inc();
	
	/**
	 Decrements refcount by 1; delete's itself if refcount is zero.
	 */
	void dec();
private:
	map<string, Managed> env;
	ManagedEnv next;
	int refcount;
};
#endif
