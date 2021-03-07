#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <time.h>

#include "parser.h"

class Generator
{
public:
	string _Sign = "COMMENT*\n\n\tTHIS ASM CODE WAS AUTOMATICALY BUILT \n\tBY FORTRAN->MASM32 KOMPILER, WHICH WAS MADE BY BGTU STUDENTS FROM GROUP I584:\n \tVITOV ARTEM, MOROZOV KIRILL, MORDOVSKIY ALEKSANDR\n\t";
	string _Head = ".386\n.model flat, stdcall\n.stack 4096h\ninclude \\masm32\\include\\masm32rt.inc\ninclude customMacros.inc\n\n";
	string _Data = ".data\n";
	string _Code = ".code\n";
	string _Init_Vars = "";
	string _name_of_func = "";
	int _M_count;
	int _Int_Real;
	parser Pars;
	Generator(const char* filename) : Pars(filename)
	{}
	void Int_Real_recursive(node* current_node, size_t level);
	void Generate_recursive(node* current_node, size_t level);
	int Else_If_Count_recursive(node* current_node, size_t level, int count, int flag);
	void MakeFileAsm(const char* filename);
};