/**
 CSCI245 Homework 5
 @file main.cpp
 @author Ray Peters W00843595
 */
#include <iostream>
#include "sexpr.h"
#include "functions.h"
#include "environment.h"

using namespace std;

#include <fstream>
void AssertTestCases(string inputFile, string outputFile, Environment* env);
bool IsSameSexprOutput(string actualSexprString, string correctSexprString);
void skip_space(istream& in);

int main () {
	Environment* GlobalEnv = new Environment(NULL);
	Sexpr *sexpr;
	
	//AssertTestCases("testcases.txt", "textcases_checker.txt", GlobalEnv);
	
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

void AssertTestCases(string inputFile, string outputFile, Environment* env)
{
	fstream testInputStream(inputFile.c_str());
	fstream::pos_type currentActualInputStreamPosition;
	
	// Clear the output file from the last run.
	fstream outputFileStream(outputFile.c_str(), ios::out|ios::trunc);
	outputFileStream.close();
	bool firstTime = true;
	bool lastTestCaseFailed = false;
	int testCaseNumber = 0;
	
	Sexpr* sexpr = NULL;
	
	while(!testInputStream.eof())
	{
		char inputChar = NULL;
		skip_space(testInputStream);
		inputChar = testInputStream.peek();
		
		// Begining of a test case correct output
		if(inputChar == '/')
		{
			// Get that input Char.
			testInputStream >> inputChar;
			
			char testChar = NULL;
			
			// Get correct Sexpr string from input file
			// Delimited by /
			string correctSexprString = "";
			while(testInputStream.peek() != '/')
			{
				testInputStream >> testChar;
				correctSexprString += testChar;
			}
			
			// Get the last from the test input stream/
			testInputStream.get(testChar);
			
			// Dont check out put if the last test case failed to print
			// Set failed flag to false.
			if(lastTestCaseFailed)
			{
				lastTestCaseFailed = false;
				cout << endl;
				cout << "--------------------------------------------------" << endl;				
				cout << "Actual: " << endl;
				cout << "Expected: " << correctSexprString << endl << endl;
				cout << "ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << endl << endl;
				cout << "--------------------------------------------------" << endl << endl;				
				continue;
			}
			
			// Reset test char
			testChar = NULL;
			
			// Get actual sexpr string
			// Delimited by /
			fstream actualInputStream(outputFile.c_str());
			actualInputStream.seekg(currentActualInputStreamPosition, ios::beg);
     		string actualSexprString = "";
			
			//skip_space(actualInputStream);
			while(actualInputStream.peek() != '/')
			{
				actualInputStream >> testChar;
				actualSexprString += testChar;
			}
			
			// End of a test case
			// Get the last /
			actualInputStream >> testChar;
			currentActualInputStreamPosition = actualInputStream.tellg();
			
			// Close output file so it can be writen to again.
			actualInputStream.close();
			
			if(!IsSameSexprOutput(actualSexprString, correctSexprString))
			{
				cout << "--------------------------------------------------" << endl;				
				cout << "Actual: " << actualSexprString << endl;
				cout << "Expected: " << correctSexprString << endl << endl;
				cout << "ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << endl << endl;
				cout << "--------------------------------------------------" << endl << endl;				
				
			}
			
			continue;
		}
		else if(inputChar == '(')
		{
			// Read, eval, and print to the output file then assert the output to the correct output
			fstream outputStream(outputFile.c_str(), ios::app);
			
			if(!firstTime)
			{
				outputStream << endl;
			}
			
			firstTime = false;
			
			sexpr = read_sexpr(testInputStream);
			print_sexpr(cout, sexpr);
			cout << endl;
			sexpr = eval_sexpr(sexpr, env);
			print_sexpr(outputStream, sexpr);
			
			testCaseNumber++;
			
			// Flag the last test case failing to print
			if(sexpr == NULL)
			{
				lastTestCaseFailed = true;
			}
			else
			{
				// Delimit output by /
				outputStream << '/';
			}
			
			// Close output so it can be read in later to check for correctness.
			outputStream.close();
		}
	}
	
	delete sexpr;
	
	testInputStream.close();
}

bool IsSameSexprOutput(string actualSexprString, string correctSexprString)
{
	if(actualSexprString.size() != correctSexprString.size())
	{
		return false;
	}
	
	for(int i = 0; i < actualSexprString.size(); i++)
	{
		if(actualSexprString.at(i) != correctSexprString.at(i))
		{
			return false;
		}
	}
	return true;
}

void skip_space(istream& in)
{
	char inputChar = NULL;
	
	while(isspace(in.peek()))
	{
		in.get(inputChar);
	}
}
