#include "Generator.h"
#include"parser.h" 
#include<iostream>
#include<vector>


using namespace std;


int main(void)
{
	setlocale(LC_ALL, "RUS");
	Init();
	Token token;
	
	try
	{
		Generator Gen("in.txt");
		Gen.Pars.parse();
		Gen.MakeFileAsm("msort.asm");
		system("PAUSE");
		return 0;
	}
	catch (LexErr err)
	{
		system("PAUSE");
		exit(1);
	}
	catch (ParseErr err)
	{
		system("PAUSE");
		exit(1);
	}
	catch (SemErr err)
	{
		system("PAUSE");
		exit(1);
	}
	system("PAUSE");
	return 0;
}