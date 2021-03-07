#include "parser.h"
#include <cstring>


void parser::parse()
{
	// не забудем вызвать метод у лексера, чтобы разбить исходный код
	_lex->split();

	// вызываем первый нетерминал грамматики
	_ast->tree = full_program();

	_ast->print();

	//строим таблицу переменных для различных функций
	_ast->designate_program_recursive(_ast->tree);
	//выводим таблицу переменных
	_ast->_all_programs.print_program_table();

	//проверяем, все ли используемые массивы и переменные объявлены
	_ast->check_declarations(_ast->tree);

	cout << "FINISH ANALYSIS" << endl;
}

//Для семантического анализа
variable_type string_to_var_type(string st)
{
	if (st == "INTEGER")
		return variable_type::INTEGER;
	else if (st == "REAL")
		return variable_type::REAL;
	else if (st == "DOUBLEPRECISION")
		return variable_type::DOBLE_PRECISION;
	else if (st == "LOGICAL")
		return variable_type::LOGICAL;
	else if (st == "CHAR")
		return variable_type::CHAR;
	else
		return variable_type::UNDEFINED;
}

string var_to_string_type(variable_type type)
{
	if (type == variable_type::INTEGER)
		return "INTEGER";
	else if (type == variable_type::REAL)
		return "REAL";
	else if (type == variable_type::DOBLE_PRECISION)
		return "DOUBLE_PRECISION";
	else if (type == variable_type::LOGICAL)
		return "LOGICAL";
	else if (type == variable_type::CHAR)
		return "CHAR";
	else
		return "UNDEFINED";
}

program_type node_type_to_program_type(node_type type)
{
	if (type == node_type::PROGRAM_BODY)
		return program_type::PROGRAM;
	else if (type == node_type::SUBROUTINE_DECL || type == node_type::SUBROUTINE_REC)
		return program_type::SUBROUTINE;
}

string program_to_string_type(program_type type)
{
	if (type == program_type::PROGRAM)
		return "PROGRAM";
	else if (type == program_type::SUBROUTINE)
		return "SUBROUTINE";
	else
		return "UNDEFINED";
}

void ast::print_recursive(node* current_node, size_t level)
{
	// если узел не существует, то просто выходим из функции
	if (current_node == nullptr)
		return;

	for (int i = 0; i < level; ++i)
	{
		// выводим level раз отступ в два пробела
		cout << "  ";
	}

	// выводим обозначение узла
	cout << "+-";

	// в зависимости от типа узла
	// выводим нужное
	switch (current_node->type)
	{
	case node_type::ADD_EXP:
		cout << "ADD EXP ("<<current_node->value<<")";
		break;
	case node_type::ASSIGNMENT:
		cout << "ASSIGNMENT (=)";
		break;
	case node_type::ATTRIBUTE:
		cout << "ATTRIBUTE ("<<current_node->value<<")";
		break;
	case node_type::CONTINUE_NODE:
		cout << "CONTIINUE";
		break;
	case node_type::CYCLE_NODE:
		cout << "CYCLE";
		break;
	case node_type::DO_CYCLE:
		cout << "DO CYCLE";
		break;
	case node_type::DO_WHILE_CYCLE:
		cout << "DO WHILE CYCLE";
		break;
	case node_type::ELSEIF:
		cout << "ELSE IF";
		break;
	case node_type::EXIT_NODE:
		cout << "EXIT";
		break;
	case node_type::EXPRESSION:
		cout << "EXPRESSION";
		break;
	case node_type::FULL_PROGRAM:
		cout << "FULL PROGRAM";
		break;
	case node_type::FUNC_ARG:
		cout << "FUNC ARG ";
		break;
	case node_type::FUNC_BODY:
		cout << "FUNC BODY";
		break;
	case node_type::FUNC_CALL:
		cout << "FUNC CALL";
		break;
	case node_type::FUNC_DECL:
		cout << "FUNC DECL";
		break;
	case node_type::RETURN_NODE:
		cout << "RETURN";
		break;
	case node_type::SUBROUTINE_CALL:
		cout << "SUBROUTINE CALL";
		break;
	case node_type::SUBROUTINE_CALL_ARG:
		cout << "SUBROUTINE CALL ARG";
		break;
	case node_type::SUBROUTINE_BODY:
		cout << "SUBROUTINE BODY";
		break;
	case node_type::SUB_DECL_LIST:
		cout << "SUB DECL LIST";
		break;
	case node_type::SUB_VAR_DECL:
		cout << "SUB VAR DECL";
		break;
	case node_type::SUBOUTINE_ARG:
		cout << "SUBOUTINE ARG";
		break;
	case node_type::SUBROUTINE_DECL:
		cout << "SUBROUTINE DECL";
		break;
	case node_type::SUBROUTINE_DECL_LIST:
		cout << "SUBROUTINE DECL LIST";
		break;
	case node_type::SUBROUTINE_REC:
		cout << "SUBROUTINE REC";
		break;
	case node_type::SUB_MAS_DECL:
		cout << "SUB MAS DECL";
		break;
	case node_type::SUB_MAS_LIST:
		cout << "SUB MAS LIST";
		break;
	case node_type::FUNC_REC_DECL:
		cout << "FUNC RECURSIVE DECL";
		break;
	case node_type::FUNC_DECL_LIST:
		cout << "FUNC DECL LIST";
		break;
	case node_type::IF_CONDITION:
		cout << "IF";
		break;
	case node_type::INPUT_READ:
		cout << "READ";
		break;
	case node_type::INTEGER_LITERAL:
		cout << "INTEGER LITERAL (" << current_node->value << ")";
		break;
	case node_type::LOGICAL_LITERAL:
		cout << "LOGICAL LITERAL (" << current_node->value << ")";
		break;
	case node_type::LOG_AND_EXP:
		cout << "LOG AND EXP (" << current_node->value << ")";
		break;
	case node_type::LOG_EQV_EXP:
		cout << "LOG EQV EXP (" << current_node->value << ")";
		break;
	case node_type::LOG_NOT_EXP:
		cout << "LOG NOT EXP (" << current_node->value << ")";
		break;
	case node_type::LOG_OR_EXP:
		cout << "LOG OR EXP (" << current_node->value << ")";
		break;
	case node_type::MULT_EXP:
		cout << "MULT EXP (" << current_node->value << ")";
		break;
	case node_type::OUTPUT_LIST:
		cout << "OUTPUT LIST";
		break;
	case node_type::OUTPUT_PRINT:
		cout << "PRINT";
		break;
	case node_type::OUTPUT_WRITE:
		cout << "WRITE";
		break;
	case node_type::POWER_EXP:
		cout << "POWER EXP (" << current_node->value << ")";
		break;
	case node_type::SQRT_EXP:
		cout << "SQRT EXP";
		break;
	case node_type::PRIMARY_EXP:
		cout << "PRIMARY EXP";
		break;
	case node_type::PROGRAM_BODY:
		cout << "PROGRAM BODY";
		break;
	case node_type::REAL_LITERAL:
		cout << "REAL LITERAL (" << current_node->value << ")";
		break;
	case node_type::REL_EXP:
		cout << "REL EXP (" << current_node->value << ")";
		break;
	case node_type::STATEMENT:
		cout << "STATEMENT";
		break;
	case node_type::STATEMENTS:
		cout << "STATEMENTS";
		break;
	case node_type::STOP_NODE:
		cout << "STOP";
		break;
	case node_type::TYPE:
		cout << "TYPE ("<<current_node->value<<")";
		break;
	case node_type::UNAR_EXP:
		cout << "UNAR EXP (" << current_node->value << ")";
		break;
	case node_type::VAR:
		cout << "VAR";
		break;
	case node_type::VAR_LIST:
		cout << "VAR LIST";
		break;
	case node_type::VAR_DECL:
		cout << "VAR DECL";
		break;
	case node_type::MAS_SIZE:
		cout << "MAS SIZE ("<< current_node->value<<")";
		break;
	case node_type::MAS_START:
		cout << "MAS START (" << current_node->value << ")";
		break;
	case node_type::MAS_FINISH:
		cout << "MAS FINISH (" << current_node->value << ")";
		break;
	case node_type::MAS_DECL:
		cout << "MAS DECL";
		break;
	case node_type::MAS_LIST:
		cout << "MAS LIST";
		break;
	case node_type::MAS:
		cout << "MAS";
		break;
	case node_type::MAS_ELEM:
		cout << "MAS ELEM";
		break;
	case node_type::MAS_INIT:
		cout << "MAS INIT";
		break;
	case node_type::ELEM_NUMBER:
		cout << "ELEM NUMBER (" << current_node->value << ")";
		break;
	case node_type::DECL_LIST:
		cout << "DECL LIST";
		break;
	case node_type::INPUT_LIST:
		cout << "INPUT LIST";
		break;
	case node_type::CALL_NODE:
		cout << "CALL";
		break;
	case node_type::CONSTANT:
		cout << "CONST ";
		cout << "(" << current_node->value << ")";
		break;
	case node_type::ID:
		cout << "ID ";
		cout << "(" << current_node->value << ")";
		break;
	}
	cout << endl;

	// выводим первого потомка
	print_recursive(current_node->operand1, level + 1);
	// выводим второго потомка
	print_recursive(current_node->operand2, level + 1);
	// выводим тетьего потомка
	print_recursive(current_node->operand3, level + 1);
	// выводим четвертого потомка
	print_recursive(current_node->operand4, level + 1);
	// выводим пятого потомка
	print_recursive(current_node->operand5, level + 1);
	// выводим шестого потомка
	print_recursive(current_node->operand6, level + 1);
	// выводим седьмого потомка
	print_recursive(current_node->operand7, level + 1);
}	

//РАБОТАЕТ
node* parser::full_program() //[<func_decl_list>] <program_body>[<func_decl_list>]
{
	node* new_node = new node(node_type::FULL_PROGRAM);
	
	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	if (_lex->current_token_type() == TokenType::RECURSIVE ||  _lex->current_token_type() == TokenType::SUBROUTINE)
	{
		new_node->operand1 = subroutine_decl_list();
		//_lex->next_token();
	}
	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	new_node->operand2 = program_body();
	_lex->next_token();

	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	/*if (_lex->current_token_type() == TokenType::PROGRAM)
	{
		throw ParseErr("Another declaration of PROGRAM");
	}*/

	if (_lex->current_token_type() == TokenType::RECURSIVE || _lex->current_token_type() == TokenType::SUBROUTINE)
	{
		new_node->operand3 = subroutine_decl_list();
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::program_body() //( [“program” [<ID>]] | [”program”]); <var_decl_list> <statements> “end”[“program”[<ID>]]
{
	node* new_node = nullptr;
	string str_1, str_2;

	if (_lex->current_token_type() == TokenType::PROGRAM)
	{
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::IDENT)
		{
			string program_name_str = str_1 = _lex->current_token()._value;
			new_node = new node(node_type::PROGRAM_BODY, program_name_str);
			new_node->operand1 = identifier();
			_lex->next_token();
		}
		else
		{
			new_node = new node(node_type::PROGRAM_BODY);
		}
	}
	else
	{
		new_node = new node(node_type::PROGRAM_BODY);
	}

	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		throw ParseErr("Separator expected");
	}
	_lex->next_token();

	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	if (_lex->current_token_type() == TokenType::INTEGER ||
		_lex->current_token_type() == TokenType::REAL || _lex->current_token_type() == TokenType::CHARACTER ||
		_lex->current_token_type() == TokenType::DOUBLE || _lex->current_token_type() == TokenType::LOGICAL)
	{
		new_node->operand2 = decl_list();
	}

	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	new_node->operand3 = statements(node_type::PROGRAM_BODY);
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::END)
	{
		throw ParseErr("'END' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::PROGRAM)
	{
		_lex->next_token();
		
		if (_lex->current_token_type() == TokenType::IDENT)
		{
			str_2 = _lex->current_token()._value;
			if (_stricmp(str_1.c_str(), str_2.c_str()) != 0)
			{
				throw ParseErr("Incorrect name of program");
			}
		}
		else
		{
			_lex->prev_token();
		}
	}
	else
	{
		_lex->prev_token();
	}

	return new_node;
}

node* parser::sub_mas_decl()//<type> “, ”(“DIMENSION” “(” <integer_literal>[“:” <integer_literal>] “)” | “INTENT” “(”(“IN” | “OUT” | “IN” “OUT”) “)”)[“, ”(“DIMENSION” “(” <integer_literal>[“:” <integer_literal>] “)” | “INTENT” “(”(“IN” | “OUT” | “IN” “OUT”) “)”)] “:” ” : ”(<ID> | <mas_init>) { (<ID> | <mas_init>) }
{
	node* new_node = new node(node_type::SUB_MAS_DECL);
	string start_str;
	int status = 0;
	// 1 - сначала идёт DIMENSION
	// 0 - сначала идёт INTEND
	int init_status = 0;
	// 1 - без INTENT, можно инициализировать 
	// 0 - с INTENT, нельзя инициализировать 
	int mass_size = 0;
	new_node->operand1 = type();
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::COMMA)
	{
		throw ParseErr("',' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::INTENT)
	{
		_lex->next_token();

		if (_lex->current_token_type() != TokenType::LPAR)
		{
			throw ParseErr("'(' expected");
		}
		_lex->next_token();

		if (_lex->current_token_type() != TokenType::IN && _lex->current_token_type() != TokenType::OUT)
		{
			throw ParseErr("IN OUT expected");
		}

		if (_lex->current_token_type() == TokenType::OUT)
		{
			string attribute_str = _lex->current_token()._value;
			node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
			new_node->operand2 = att_node;
			_lex->next_token();
		}

		else if (_lex->current_token_type() == TokenType::IN)
		{
			string attribute_str = _lex->current_token()._value;
			_lex->next_token();
			if (_lex->current_token_type() == TokenType::OUT)
			{
				attribute_str = attribute_str + _lex->current_token()._value;
				node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
				new_node->operand2 = att_node;
				_lex->next_token();
			}
			else
			{
				_lex->prev_token();
				node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
				new_node->operand2 = att_node;
				_lex->next_token();
			}
		}

		if (_lex->current_token_type() != TokenType::RPAR)
		{
			throw ParseErr("')' expected");
		}
		_lex->next_token();
		status = 0;
	}

	else if (_lex->current_token_type() == TokenType::DIMENSION)
	{
		_lex->next_token();

		if (_lex->current_token_type() != TokenType::LPAR)
		{
			throw ParseErr("'(' expected");
		}
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::PLUS || _lex->current_token_type() == TokenType::MINUS)
		{
			start_str = _lex->current_token()._value;
			_lex->next_token();
		}

		if (_lex->current_token_type() != TokenType::INT)
		{
			throw ParseErr("INTEGER_LITERAL expected");
		}
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::DDOT)
		{
			int start = 0, finish = 0;

			_lex->prev_token();
			string mas_start_str = start_str + _lex->current_token()._value;
			node* mas_start_node = new node(node_type::MAS_START, mas_start_str);
			new_node->operand3 = mas_start_node;
			_lex->next_token();
			_lex->next_token();

			if (_lex->current_token_type() != TokenType::INT)
			{
				throw ParseErr("plurable INTEGER_LITERAL expected");
			}
			else
			{
				string mas_finish_str = _lex->current_token()._value;
				node* mas_finish_node = new node(node_type::MAS_FINISH, mas_finish_str);
				new_node->operand4 = mas_finish_node;

				start = stoi(new_node->operand3->value);
				finish = stoi(new_node->operand4->value);

				if ((finish - start + 1) <= 0)
				{
					mass_size = 0;
					string mas_size_str = to_string(0);
					node* mas_size_node = new node(node_type::MAS_SIZE, mas_size_str);
					new_node->operand5 = mas_size_node;
				}
				else
				{
					mass_size = finish - start + 1;
					string mas_size_str = to_string(mass_size);
					node* mas_size_node = new node(node_type::MAS_SIZE, mas_size_str);
					new_node->operand5 = mas_size_node;
				}
			}
			_lex->next_token();
		}
		else
		{
			_lex->prev_token();
			string check = start_str + _lex->current_token()._value;
			string mas_size_str;
			if (stoi(check) <= 0)
			{
				mas_size_str = to_string(0);
			}
			else
			{
				mas_size_str = _lex->current_token()._value;
			}
			node* mas_size_node = new node(node_type::MAS_SIZE, mas_size_str);
			new_node->operand5 = mas_size_node;
			mass_size = atoi(mas_size_str.c_str());
			string mas_start_str = to_string(1);
			node* mas_start_node = new node(node_type::MAS_START, mas_start_str);
			new_node->operand3 = mas_start_node;
			string mas_finish_str = to_string(mass_size);
			node* mas_finish_node = new node(node_type::MAS_FINISH, mas_finish_str);
			new_node->operand4 = mas_finish_node;
			_lex->next_token();
		}

		if (_lex->current_token_type() != TokenType::RPAR)
		{
			throw ParseErr("')' expected");
		}
		_lex->next_token();
		status = 1;
	}

	else
	{
	throw ParseErr("Attribute expected");
    }

	if (_lex->current_token_type() == TokenType::COMMA)
	{
		_lex->next_token();
		if (status)
		{
			if (_lex->current_token_type() == TokenType::DIMENSION)
			{
				throw ParseErr("Repeating DIMENSION");
			}
		}
		else
		{
			if (_lex->current_token_type() == TokenType::INTENT)
			{
				throw ParseErr("Repeating INTENT");
			}

			if (_lex->current_token_type() != DIMENSION)
			{
				throw ParseErr("DIMENTION expected");
			}
		}

		if (_lex->current_token_type() == TokenType::INTENT)
		{
			_lex->next_token();

			if (_lex->current_token_type() != TokenType::LPAR)
			{
				throw ParseErr("'(' expected");
			}
			_lex->next_token();

			if (_lex->current_token_type() != TokenType::IN && _lex->current_token_type() != TokenType::OUT)
			{
				throw ParseErr("IN OUT expected");
			}

			if (_lex->current_token_type() == TokenType::OUT)
			{
				string attribute_str = _lex->current_token()._value;
				node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
				new_node->operand2 = att_node;
				_lex->next_token();
			}

			else if (_lex->current_token_type() == TokenType::IN)
			{
				string attribute_str = _lex->current_token()._value;
				_lex->next_token();
				if (_lex->current_token_type() == TokenType::OUT)
				{
					attribute_str = attribute_str + _lex->current_token()._value;
					node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
					new_node->operand2 = att_node;
					_lex->next_token();
				}
				else
				{
					_lex->prev_token();
					node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
					new_node->operand2 = att_node;
					_lex->next_token();
				}
			}

			if (_lex->current_token_type() != TokenType::RPAR)
			{
				throw ParseErr("')' expected");
			}
			_lex->next_token();
		}

		else if (_lex->current_token_type() == TokenType::DIMENSION)
		{
			_lex->next_token();

			if (_lex->current_token_type() != TokenType::LPAR)
			{
				throw ParseErr("'(' expected");
			}
			_lex->next_token();

			if (_lex->current_token_type() == TokenType::PLUS || _lex->current_token_type() == TokenType::MINUS)
			{
				start_str = _lex->current_token()._value;
				_lex->next_token();
			}

			if (_lex->current_token_type() != TokenType::INT)
			{
				throw ParseErr("INTEGER_LITERAL expected");
			}
			_lex->next_token();

			if (_lex->current_token_type() == TokenType::DDOT)
			{
				int start = 0, finish = 0;

				_lex->prev_token();
				string mas_start_str = start_str + _lex->current_token()._value;
				node* mas_start_node = new node(node_type::MAS_START, mas_start_str);
				new_node->operand3 = mas_start_node;
				_lex->next_token();
				_lex->next_token();

				if (_lex->current_token_type() != TokenType::INT)
				{
					throw ParseErr("plurable INTEGER_LITERAL expected");
				}
				else
				{
					string mas_finish_str = _lex->current_token()._value;
					node* mas_finish_node = new node(node_type::MAS_FINISH, mas_finish_str);
					new_node->operand4 = mas_finish_node;

					start = stoi(new_node->operand3->value);
					finish = stoi(new_node->operand4->value);

					if ((finish - start + 1) <= 0)
					{
						mass_size = 0;
						string mas_size_str = to_string(0);
						node* mas_size_node = new node(node_type::MAS_SIZE, mas_size_str);
						new_node->operand5 = mas_size_node;
					}
					else
					{
						mass_size = finish - start + 1;
						string mas_size_str = to_string(mass_size);
						node* mas_size_node = new node(node_type::MAS_SIZE, mas_size_str);
						new_node->operand5 = mas_size_node;
					}
				}
				_lex->next_token();
			}
			else
			{
				_lex->prev_token();
				string check = start_str + _lex->current_token()._value;
				string mas_size_str;
				if (stoi(check) <= 0)
				{
					mas_size_str = to_string(0);
				}
				else
				{
					mas_size_str = _lex->current_token()._value;
				}
				node* mas_size_node = new node(node_type::MAS_SIZE, mas_size_str);
				new_node->operand5 = mas_size_node;
				mass_size = atoi(mas_size_str.c_str());
				string mas_start_str = to_string(1);
				node* mas_start_node = new node(node_type::MAS_START, mas_start_str);
				new_node->operand3 = mas_start_node;
				string mas_finish_str = to_string(mass_size);
				node* mas_finish_node = new node(node_type::MAS_FINISH, mas_finish_str);
				new_node->operand4 = mas_finish_node;
				_lex->next_token();
			}

			if (_lex->current_token_type() != TokenType::RPAR)
			{
				throw ParseErr("')' expected");
			}
			_lex->next_token();
		}
	}

	if (_lex->current_token_type() != TokenType::DDOT)
	{
		throw ParseErr("':' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::DDOT)
	{
		throw ParseErr("':' expected");
	}
	_lex->next_token();

	string in_str = "IN";
	string out_str = "OUT";
	string inout_str = "INOUT";
	
	if (new_node->operand2 != nullptr)
	{
		if (_stricmp(new_node->operand2->value.c_str(), in_str.c_str()) == 0 ||
			_stricmp(new_node->operand2->value.c_str(), out_str.c_str()) == 0 ||
			_stricmp(new_node->operand2->value.c_str(), inout_str.c_str()) == 0)
		{
			new_node->operand6 = sub_mas_list(mass_size, 0);
		}
		else
		{
			new_node->operand6 = sub_mas_list(mass_size, 1);
		}
	}
	else
	{
		new_node->operand6 = sub_mas_list(mass_size, 1);
	}
	return new_node;
}

node* parser::sub_mas_list(int mass_size, int init_work) //Для построения дерева, где в 1 ноде будет mas, а во второй будет mas_list
{
	node* new_node = new node(node_type::SUB_MAS_LIST);
	node* mas_node = new node(node_type::MAS);
	if (_lex->current_token_type() != TokenType::IDENT)
	{
		throw ParseErr("Identificatror expected");
	}
	else
	{
		string ident_str = _lex->current_token()._value;
		mas_node->operand1 = new node(node_type::ID, ident_str);
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::EQUAL)
		{
			if (init_work)
			{
				_lex->next_token();

				if (_lex->current_token_type() != TokenType::LPAR)
				{
					throw ParseErr("'(' expected");
				}
				_lex->next_token();

				if (_lex->current_token_type() != TokenType::DIVISION)
				{
					throw ParseErr("'/' expected");
				}
				_lex->next_token();

				mas_node->operand2 = mas_init(mass_size);
				_lex->next_token();

				if (_lex->current_token_type() != TokenType::DIVISION)
				{
					throw ParseErr("'/' expected");
				}
				_lex->next_token();

				if (_lex->current_token_type() != TokenType::RPAR)
				{
					throw ParseErr("')' expected");
				}
				_lex->next_token();
			}
			else
			{
				throw ParseErr("You cannot initialize a DUMMI array");
			}
		}
		else
		{
			mas_node->operand2 = nullptr;
		}

		if (_lex->current_token_type() == TokenType::COMMA)
		{
			_lex->next_token();
			new_node->operand2 = mas_list(mass_size);
		}
		else
		{
			_lex->prev_token();
		}
	}
	new_node->operand1 = mas_node;
	return new_node;
}


node* parser::sub_decl_list()//<sub_decl_list>:: = (<sub_var_decl> | <sub_mas_decl>) | (<sub_var_decl> | <sub_mas_decl>) < sub_decl_list >
{
	node* new_node = new node(node_type::SUB_DECL_LIST);

	if (_lex->current_token_type() == TokenType::INTEGER || _lex->current_token_type() == TokenType::DOUBLE || _lex->current_token_type() == TokenType::CHARACTER || _lex->current_token_type() == TokenType::LOGICAL || _lex->current_token_type() == TokenType::REAL)
	{
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::COMMA)
		{
			_lex->next_token();

			if (_lex->current_token_type() == TokenType::PARAMETER)
			{
				_lex->prev_token();
				_lex->prev_token();
				new_node->operand1 = sub_var_decl();
			}
			else if(_lex->current_token_type() == TokenType::DIMENSION)
			{
				_lex->prev_token();
				_lex->prev_token();
				new_node->operand1 = sub_mas_decl();
			}
			else if (_lex->current_token_type() == TokenType::INTENT)
			{
				int status = 0;
				_lex->next_token();

				if (_lex->current_token_type() != TokenType::LPAR)
				{
					throw ParseErr("'(' expected");
				}
				_lex->next_token();

				if (_lex->current_token_type() != TokenType::IN && _lex->current_token_type() != TokenType::OUT)
				{
					throw ParseErr("IN OUT expected");
				}

				if (_lex->current_token_type() == TokenType::OUT)
				{
					_lex->next_token();
				}

				else if (_lex->current_token_type() == TokenType::IN)
				{
					_lex->next_token();
					if (_lex->current_token_type() == TokenType::OUT)
					{
						_lex->next_token();
						status = 1;
					}
				}

				if (_lex->current_token_type() != TokenType::RPAR)
				{
					throw ParseErr("')' expected");
				}
				_lex->next_token();

				if (_lex->current_token_type() == TokenType::COMMA)
				{
					_lex->prev_token();
					_lex->prev_token();
					_lex->prev_token();
					_lex->prev_token();
					_lex->prev_token();
					_lex->prev_token();
					if (status)
					{
						_lex->prev_token();
					}
					new_node->operand1 = sub_mas_decl();
				}
				else
				{
					_lex->prev_token();
					_lex->prev_token();
					_lex->prev_token();
					_lex->prev_token();
					_lex->prev_token();
					_lex->prev_token();
					if (status)
					{
						_lex->prev_token();
					}
					new_node->operand1 = sub_var_decl();
				}
			}
			else
			{
				throw ParseErr("Attriute expected");
			}
		}
		else
		{
			_lex->prev_token();
			new_node->operand1 = sub_var_decl();
		}
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		throw ParseErr("Separator expected");
	}
	_lex->next_token();

	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	if (_lex->current_token_type() == TokenType::INTEGER || _lex->current_token_type() == TokenType::DOUBLE || _lex->current_token_type() == TokenType::CHARACTER || _lex->current_token_type() == TokenType::LOGICAL || _lex->current_token_type() == TokenType::REAL)
	{
		new_node->operand2 = sub_decl_list();
	}
	else
	{
		_lex->prev_token();
	}

	return new_node;
}

node* parser::sub_var_decl()  //<type>[“, ”(“PARAMETER” | “INTENT” “(”(“IN” | “OUT” | “IN” “OUT”) “)”)] ”:” ” : ”(<ID> | <assignment>) { (<ID> | <assignment >) } | <type> <ID_list>
{
	node* new_node = new node(node_type::SUB_VAR_DECL);
	new_node->operand1 = type();
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::COMMA || _lex->current_token_type() == TokenType::DDOT)
	{
		if (_lex->current_token_type() == TokenType::COMMA)
		{
			_lex->next_token();

			if (_lex->current_token_type() == TokenType::PARAMETER)
			{
				string attribute_str = _lex->current_token()._value;
				node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
				new_node->operand2 = att_node;
				_lex->next_token();
			}

			else if (_lex->current_token_type() == TokenType::INTENT)
			{
				_lex->next_token();

				if (_lex->current_token_type() != TokenType::LPAR)
				{
					throw ParseErr("'(' expected");
				}
				_lex->next_token();

				if (_lex->current_token_type() != TokenType::IN && _lex->current_token_type() != TokenType::OUT)
				{
					throw ParseErr("IN OUT expected");
				}

				if (_lex->current_token_type() == TokenType::OUT)
				{
					string attribute_str = _lex->current_token()._value;
					node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
					new_node->operand2 = att_node;
					_lex->next_token();
				}

				else if (_lex->current_token_type() == TokenType::IN)
				{
					string attribute_str = _lex->current_token()._value;
					_lex->next_token();
					if (_lex->current_token_type() == TokenType::OUT)
					{
						attribute_str = attribute_str + _lex->current_token()._value;
						node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
						new_node->operand2 = att_node;
						_lex->next_token();
					}
					else
					{
						_lex->prev_token();
						node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
						new_node->operand2 = att_node;
						_lex->next_token();
					}
				}

				if (_lex->current_token_type() != TokenType::RPAR)
				{
					throw ParseErr("')' expected");
				}
				_lex->next_token();

			}
		}

		if (_lex->current_token_type() != TokenType::DDOT)
		{
			throw ParseErr("':' expected");
		}
		_lex->next_token();

		if (_lex->current_token_type() != TokenType::DDOT)
		{
			throw ParseErr("':' expected");
		}
		_lex->next_token();

		string par_str = "PARAMETER";
		string in_str = "IN";
		string out_str = "OUT";
		string inout_str = "INOUT";
		if (new_node->operand2 != nullptr)
		{
			if (_stricmp(new_node->operand2->value.c_str(), par_str.c_str()) == 0)
			{
				new_node->operand3 = var_list(2);
			}
		}
		if(new_node->operand2 != nullptr)
		{
			if (_stricmp(new_node->operand2->value.c_str(), in_str.c_str()) == 0 ||
				_stricmp(new_node->operand2->value.c_str(), out_str.c_str()) == 0 ||
				_stricmp(new_node->operand2->value.c_str(), inout_str.c_str()) == 0)
			{
				new_node->operand3 = var_list(3);
			}
		}
		else
		{
			new_node->operand3 = var_list(1);
		}
	}

	else
	{
		new_node->operand2 = nullptr;
		new_node->operand3 = var_list(0);
	}

	return new_node;
}

node* parser::subroutine_call_arg()  //фактические аргументы вызова функции
{
	node* new_node = new node(node_type::SUBOUTINE_ARG);

	new_node->operand1 = expression();
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::COMMA)
	{
		_lex->next_token();
		new_node->operand2 = subroutine_call_arg();
	}
	else
	{
		_lex->prev_token();
	}

	return new_node;
}

node* parser::subroutine_call()//<subroutine_call> :: = “call” <ID> “(” <expression> { “, ” <expression> } “)”
{
	node* new_node = new node(node_type::CALL_NODE);

	if (_lex->current_token_type() != TokenType::CALL)
	{
		throw ParseErr("CALL expected");
	}
	_lex->next_token();

	new_node->operand1 = identifier();
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::LPAR)
	{
		throw ParseErr("'('expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::IDENT)
	{
		new_node->operand2 = subroutine_call_arg();
		_lex->next_token();
	}

	if (_lex->current_token_type() != TokenType::RPAR)
	{
		throw ParseErr("')'expected");
	}
	return new_node;
}

node* parser::subroutine_decl_list()  // <subroutine_decl> | <subroutine_decl> <subroutine_decl_list>
{
	node* new_node = new node(node_type::SUBROUTINE_DECL_LIST);
	new_node->operand1 = subroutine_decl();
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::SEPARATOR && _lex->current_token_type() != TokenType::NUL)
	{
		throw ParseErr("Separator expected");
	}


	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	if (_lex->current_token_type() == TokenType::RECURSIVE || _lex->current_token_type() == TokenType::SUBROUTINE)
	{
		new_node->operand2 = subroutine_decl_list();
	}

	//пропускаем лишние '\n' и ';'
	//while (_lex->current_token_type() == TokenType::SEPARATOR)
	//	_lex->next_token();

	return new_node;
}

node* parser::subroutine_decl()  //[“RECURSIVE”] “SUBROUTINE” <ID> “(” <subroutine_arg> “)” <subroutine_body> “END” “SUBROUTINE”[<ID>]
{
	node* new_node = new node(node_type::SUBROUTINE_DECL);

	if (_lex->current_token_type() == TokenType::RECURSIVE)
	{
		string attribute_str = _lex->current_token()._value;
		node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
		new_node->operand1 = att_node;
		new_node->type = node_type::SUBROUTINE_REC;
		_lex->next_token();
	}

	if (_lex->current_token_type() != TokenType::SUBROUTINE)
	{
		throw ParseErr("'SUBROUTINE' expected");
	}
	_lex->next_token();

	new_node->operand2 = identifier();
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::LPAR)
	{
		throw ParseErr("'(' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::IDENT)
	{
		new_node->operand3 = subroutine_arg();
		_lex->next_token();
	}

	if (_lex->current_token_type() != TokenType::RPAR)
	{
		throw ParseErr("')' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		throw ParseErr("Separator expected");
	}
	_lex->next_token();

	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	new_node->operand4 = subroutine_body();
	//_lex->next_token();

	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();
	if (_lex->current_token_type() != TokenType::END)
	{
		throw ParseErr("'end' expected");
	}
	_lex->next_token();
	if (_lex->current_token_type() != TokenType::SUBROUTINE)
	{
		throw ParseErr("'SUBROUTINE' expected");
	}
	_lex->next_token();
	if (_lex->current_token_type() == TokenType::IDENT)
	{
		new_node->operand5 = identifier();

		if (new_node->operand2 != nullptr && new_node->operand5 != nullptr)
		{
			if (new_node->operand2->value != new_node->operand5->value)
			{
				throw ParseErr("Diffrenet names of functionas");
			}
		}
	}
	return new_node;
}

node* parser::subroutine_body()// < sub_decl_list> <statements>
{
	node* new_node = new node(node_type::SUBROUTINE_BODY);

	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();


	if (_lex->current_token_type() == TokenType::INTEGER ||
		_lex->current_token_type() == TokenType::REAL || _lex->current_token_type() == TokenType::CHARACTER ||
		_lex->current_token_type() == TokenType::DOUBLE || _lex->current_token_type() == TokenType::LOGICAL)
	{
		new_node->operand1 = sub_decl_list();
	}

	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	new_node->operand2 = statements(node_type::SUBROUTINE_BODY);

	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	return new_node;
}


node* parser::subroutine_arg()  //<ID> | <ID> “, ” <subroutine_arg>
{
	node* new_node = identifier();
	return new_node;
}

//РАБОТАЕТ
node* parser::decl_list() //<var_decl> | <var_decl> <var_decl_list>
{
	node* new_node = new node(node_type::DECL_LIST);

	if (_lex->current_token_type() == TokenType::INTEGER || _lex->current_token_type() == TokenType::DOUBLE || _lex->current_token_type() == TokenType::CHARACTER || _lex->current_token_type() == TokenType::LOGICAL || _lex->current_token_type() == TokenType::REAL)
	{
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::COMMA)
		{
			_lex->next_token();

			if(_lex->current_token_type() == TokenType::PARAMETER)
			{
				_lex->prev_token();
				_lex->prev_token();
				new_node->operand1 = var_decl();
			}
			else
			{
				_lex->prev_token();
				_lex->prev_token();
				new_node->operand1 = mas_decl();
			}
		}
		else
		{
			_lex->prev_token();
			new_node->operand1 = var_decl();
		}
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		throw ParseErr("Separator expected");
	}
	_lex->next_token();
	
	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	if (_lex->current_token_type() == TokenType::INTEGER || _lex->current_token_type() == TokenType::DOUBLE || _lex->current_token_type() == TokenType::CHARACTER || _lex->current_token_type() == TokenType::LOGICAL || _lex->current_token_type() == TokenType::REAL)
	{
		new_node->operand2 = decl_list();
	}
	else
	{
		_lex->prev_token();
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::mas_decl()   //<type> “,” “DIMENSION” “(” <integer_literal> [ “:” <integer_literal> ] “)” “:” ”:” (<ID> | <mas_init>) {(<ID> | <mas_init>)}
{
	node* new_node = new node(node_type::MAS_DECL, "MASDECL");
	string start_str;
	int mass_size=0;
	new_node->operand1 = type();
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::COMMA)
	{
		throw ParseErr("',' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::DIMENSION)
	{
		throw ParseErr("DIMENSION expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::LPAR)
	{
		throw ParseErr("'(' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::PLUS || _lex->current_token_type() == TokenType::MINUS)
	{
		start_str = _lex->current_token()._value;
		_lex->next_token();
	}

	if (_lex->current_token_type() != TokenType::INT)
	{
		throw ParseErr("INTEGER_LITERAL expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::DDOT)
	{
		int start = 0, finish = 0;

		_lex->prev_token();
		string mas_start_str = start_str + _lex->current_token()._value;
		node* mas_start_node = new node(node_type::MAS_START, mas_start_str);
		new_node->operand2 = mas_start_node;
		_lex->next_token();
		_lex->next_token();

		if (_lex->current_token_type() != TokenType::INT)
		{
			throw ParseErr("plurable INTEGER_LITERAL expected");
		}
		else
		{
			string mas_finish_str = _lex->current_token()._value;
			node* mas_finish_node = new node(node_type::MAS_FINISH, mas_finish_str);
			new_node->operand3 = mas_finish_node;

			start = stoi(new_node->operand2->value);
			finish = stoi(new_node->operand3->value);

			if ((finish - start + 1) <= 0)
			{
				mass_size = 0;
				string mas_size_str = to_string(0);
				node* mas_size_node = new node(node_type::MAS_SIZE, mas_size_str);
				new_node->operand4 = mas_size_node;
			}
			else
			{
				mass_size = finish - start + 1;
				string mas_size_str = to_string(mass_size);
				node* mas_size_node = new node(node_type::MAS_SIZE, mas_size_str);
				new_node->operand4 = mas_size_node;
			}
		}
		_lex->next_token();
	}
	else
	{
		_lex->prev_token();
		string check = start_str + _lex->current_token()._value;
		string mas_size_str;
		if (stoi(check) <= 0)
		{
			mas_size_str = to_string(0);
		}
		else
		{
			mas_size_str = _lex->current_token()._value;
		}
		node* mas_size_node = new node(node_type::MAS_SIZE, mas_size_str);
		new_node->operand4 = mas_size_node;
		mass_size = atoi(mas_size_str.c_str());
		string mas_start_str = to_string(1);
		node* mas_start_node = new node(node_type::MAS_START, mas_start_str);
		new_node->operand2 = mas_start_node;
		string mas_finish_str = to_string(mass_size);
		node* mas_finish_node = new node(node_type::MAS_FINISH, mas_finish_str);
		new_node->operand3 = mas_finish_node;
		_lex->next_token();
	}

	if (_lex->current_token_type() != TokenType::RPAR)
	{
		throw ParseErr("')' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::DDOT)
	{
		throw ParseErr("':' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::DDOT)
	{
		throw ParseErr("':' expected");
	}
	_lex->next_token();

	new_node->operand5 = mas_list(mass_size);

	return new_node;
}

//РАБОТАЕТ
node* parser::mas_list(int mass_size) //Для построения дерева, где в 1 ноде будет mas, а во второй будет mas_list
{
	node* new_node = new node(node_type::MAS_LIST);
	node* mas_node = new node(node_type::MAS);
	if (_lex->current_token_type() != TokenType::IDENT)
	{
		throw ParseErr("Identificatror expected");
	}
	else
	{
		string ident_str = _lex->current_token()._value;
		mas_node->operand1 = new node(node_type::ID, ident_str);
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::EQUAL)
		{
			_lex->next_token();

			if (_lex->current_token_type() != TokenType::LPAR)
			{
				throw ParseErr("'(' expected");
			}
			_lex->next_token();

			if (_lex->current_token_type() != TokenType::DIVISION)
			{
				throw ParseErr("'/' expected");
			}
			_lex->next_token();

			mas_node->operand2 = mas_init(mass_size);
			_lex->next_token();

			if (_lex->current_token_type() != TokenType::DIVISION)
			{
				throw ParseErr("'/' expected");
			}
			_lex->next_token();

			if (_lex->current_token_type() != TokenType::RPAR)
			{
				throw ParseErr("')' expected");
			}
			_lex->next_token();
		}
		else
		{
			mas_node->operand2 = nullptr;
		}

		if (_lex->current_token_type() == TokenType::COMMA)
		{
			_lex->next_token();
			new_node->operand2 = mas_list(mass_size);
		}
		else
		{
			_lex->prev_token();
		}
	}
	new_node->operand1 = mas_node;
	return new_node;
}

//РАБОТАЕТ
node* parser::mas_init(int mass_size, int elem_counter) //Для потроения дерева инициализации массива, где в 1 ноде записано значение элемента массива по порядку, а во второй mas_init
{
	node* new_node = new node(node_type::MAS_INIT);

	new_node->operand1 = expression(); 
	elem_counter++;
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::COMMA)
	{
		_lex->next_token();

		new_node->operand2 = mas_init(mass_size, elem_counter);
	}
	else
	{
		_lex->prev_token();
		if (elem_counter != mass_size)
		{
			throw ParseErr("Different shape for array assignment on dimension");
		}
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::var_decl() //<type>[“,” <attribute>] ”:” ”:” (<ID> |	<assignment>) {(<ID> | <assignment>)} | <type> <ID_list>
{
	node* new_node = new node(node_type::VAR_DECL);
	new_node->operand1 = type();
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::COMMA || _lex->current_token_type() == TokenType::DDOT)
	{
		if (_lex->current_token_type() == TokenType::COMMA)
		{
			_lex->next_token();

			if (_lex->current_token_type() == TokenType::PARAMETER)
			{
				string attribute_str = _lex->current_token()._value;
				node* att_node = new node(node_type::ATTRIBUTE, attribute_str);
				new_node->operand2 = att_node;
				_lex->next_token();
			}
		}

		if (_lex->current_token_type() != TokenType::DDOT)
		{
			throw ParseErr("':' expected");
		}
		_lex->next_token();

		if (_lex->current_token_type() != TokenType::DDOT)
		{
			throw ParseErr("':' expected");
		}
		_lex->next_token();

		string par_str = "PARAMETER";
		string in_str = "IN";
		string out_str = "OUT";
		string inout_str = "INOUT";
		if (new_node->operand2 != nullptr)
		{
			if (_stricmp(new_node->operand2->value.c_str(), par_str.c_str()) == 0)
			{
				new_node->operand3 = var_list(2);
			}
		}
		else if (new_node->operand2 != nullptr)
		{
			if (_stricmp(new_node->operand2->value.c_str(), in_str.c_str()) == 0 ||
				_stricmp(new_node->operand2->value.c_str(), out_str.c_str()) == 0 ||
				_stricmp(new_node->operand2->value.c_str(), inout_str.c_str()) == 0)
			{
				new_node->operand3 = var_list(3);
			}
		}
		else
		{
			new_node->operand3 = var_list(1);
		}
	}

	else
	{
		new_node->operand2 = nullptr;
		new_node->operand3 = var_list(0);
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::var_list(int situation) //Для построения дерева, где в 1 ноде будет var, а во второй будет var_list
{
	node* new_node = new node(node_type::VAR_LIST);
	node* var_node = new node(node_type::VAR);
	if (_lex->current_token_type() != TokenType::IDENT)
	{
		throw ParseErr("IIdentificatror expected");
	}
	else
	{
		string ident_str = _lex->current_token()._value;
		var_node->operand1 = new node(node_type::ID, ident_str);
		_lex->next_token();

		if (situation == 1)  
		{
			if ( _lex->current_token_type() == TokenType::EQUAL)
			{
				_lex->prev_token();
				var_node->operand2 = assignment();
				_lex->next_token();
			}
			else
			{
				var_node->operand2 = nullptr;
			}
		}
		else if (situation == 2)
		{
			_lex->prev_token();
			var_node->operand2 = assignment();
			_lex->next_token();
		}
		else if (situation == 3)
		{
			if (_lex->current_token_type() == TokenType::EQUAL)
			{
				throw ParseErr("You cannot initialize dummi arguments");
			}
			else
			{
				var_node->operand2 = nullptr;
			}
		}
		else
		{
				var_node->operand2 = nullptr;
		}

		if (_lex->current_token_type() == TokenType::COMMA)
		{
			_lex->next_token();
			new_node->operand2 = var_list(situation);
		}
		else
		{
			_lex->prev_token();
		}
	}
	new_node->operand1 = var_node;
	return new_node;
}

//РАБОТАЕТ
node* parser::type() //“INTEGER" |	“REAL” | “DOBLE””PRECISION” | “LOGICAL” | “CHARACTER”
{
	node* new_node = nullptr;

	//INTEGER
	if (_lex->current_token_type() == TokenType::INTEGER)
	{
		string type_str = _lex->current_token()._value;
		new_node = new node(node_type::TYPE, type_str);
	}

	//REAL
	else if (_lex->current_token_type() == TokenType::REAL)
	{
		string type_str = _lex->current_token()._value;
		new_node = new node(node_type::TYPE, type_str);
	}

	//LOGICAL
	else if (_lex->current_token_type() == TokenType::LOGICAL)
	{
		string type_str = _lex->current_token()._value;
		new_node = new node(node_type::TYPE, type_str);
	}

	//CHARACTER
	else if (_lex->current_token_type() == TokenType::CHARACTER)
	{
		string type_str = _lex->current_token()._value;
		new_node = new node(node_type::TYPE, type_str);
	}

	//DOUBLE PRECISION
	else if (_lex->current_token_type() == TokenType::DOUBLE)
	{
		string type_str = _lex->current_token()._value;
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::PRECISION)
		{
			type_str = type_str + _lex->current_token()._value;
			new_node = new node(node_type::TYPE, type_str);
		}
		else
		{
			throw ParseErr("'DOUBLE PRECISION' expected");
		}
	}

	else
	{
		throw ParseErr("Variable type expected");
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::expression() //<log_eqv_exp>
{
	node* new_node = log_eqv_exp();
	return new_node;
}

//РАБОТАЕТ
node* parser::log_eqv_exp()  //<log_or_exp> | <log_or_exp> <log_eqv_op> <log_or_exp>
{
	node* operand1_node = log_or_exp();
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::EQV || _lex->current_token_type() == TokenType::NEQV || _lex->current_token_type() == TokenType::XOR)
	{
		string eqv_oper_str = _lex->current_token()._value;
		node* new_node = new node(node_type::LOG_EQV_EXP, eqv_oper_str);

		_lex->next_token();
		new_node->operand1 = operand1_node;
		new_node->operand2 = log_or_exp();

		return new_node;
	}
	else
	{
		_lex->prev_token();
		return operand1_node;
	}
}

//РАБОТАЕТ
node* parser::log_or_exp()  //<log_and_exp> | <log_and_exp> “.OR.” <log_or_exp>
{
	node* operand1_node = log_and_exp();
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::OR)
	{
		string or_oper_str = _lex->current_token()._value;
		node* new_node = new node(node_type::LOG_OR_EXP, or_oper_str);

		_lex->next_token();
		new_node->operand1 = operand1_node;
		new_node->operand2 = log_or_exp();

		return new_node;
	}
	else
	{
		_lex->prev_token();
		return operand1_node;
	}
}

//РАБОТАЕТ
node* parser::log_and_exp()  //<log_not_exp> | <log_not_exp> “.AND.” <log_and_exp>
{
	node* operand1_node = log_not_exp();
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::AND)
	{
		string and_oper_str = _lex->current_token()._value;
		node* new_node = new node(node_type::LOG_AND_EXP, and_oper_str);

		_lex->next_token();
		new_node->operand1 = operand1_node;
		new_node->operand2 = log_and_exp();

		return new_node;
	}
	else
	{
		_lex->prev_token();
		return operand1_node;
	}
}

//РАБОТАЕТ
node* parser::log_not_exp() //[“.NOT.”]<rel_exp>
{
	node* new_node = nullptr;

	if (_lex->current_token_type() == TokenType::NOT)
	{
		string not_oper_str = _lex->current_token()._value;
		new_node = new node(node_type::LOG_NOT_EXP, not_oper_str);
		_lex->next_token();

		new_node->operand2 = rel_exp();
	}
	else
	{
		new_node = rel_exp();
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::rel_exp() //<add_exp> | <add_exp> <rel_op> <add_exp>
{
	node* operand1_node = add_exp();
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::EQ || _lex->current_token_type() == TokenType::EQEQ ||
		_lex->current_token_type() == TokenType::NE || _lex->current_token_type() == TokenType::NOTEQ ||
		_lex->current_token_type() == TokenType::GT || _lex->current_token_type() == TokenType::MORE ||
		_lex->current_token_type() == TokenType::GE || _lex->current_token_type() == TokenType::MOREEQ ||
		_lex->current_token_type() == TokenType::LT || _lex->current_token_type() == TokenType::LESS ||
		_lex->current_token_type() == TokenType::LE || _lex->current_token_type() == TokenType::LESSEQ)
	{
		string rel_oper_str = _lex->current_token()._value;
		node* new_node = new node(node_type::REL_EXP, rel_oper_str);

		_lex->next_token();
		new_node->operand1 = operand1_node;
		new_node->operand2 = add_exp();

		return new_node;
	}
	else
	{
		_lex->prev_token();
		return operand1_node;
	}
}

//РАБОТАЕТ
node* parser::add_exp() //<mult_exp> | <mult_exp> <add_op> <add_exp>
{
	node* operand1_node = mult_exp();
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::PLUS || _lex->current_token_type() == TokenType::MINUS)
	{
		string add_oper_str = _lex->current_token()._value;
		node* new_node = new node(node_type::ADD_EXP, add_oper_str);

		_lex->next_token();
		new_node->operand1 = operand1_node;
		new_node->operand2 = add_exp();

		return new_node;
	}
	else
	{
		_lex->prev_token();
		return operand1_node;
	}
}

//РАБОТАЕТ
node* parser::mult_exp() //<power_exp> | <power_exp> <mult_op> <mult_exp>
{
	node* operand1_node = power_exp();
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::MULTIPLE || _lex->current_token_type() == TokenType::DIVISION)
	{
		//Это чтобы не ломалась функция инициализации массива
		if (_lex->current_token_type() == TokenType::DIVISION)
		{
			_lex->next_token();
			if (_lex->current_token_type() == TokenType::RPAR)
			{
				_lex->prev_token();
				_lex->prev_token();
				return operand1_node;
			}
			else
			{
				_lex->prev_token();
			}
		}

		string mult_oper_str = _lex->current_token()._value;
		node* new_node = new node(node_type::MULT_EXP, mult_oper_str);

		_lex->next_token();
		new_node->operand1 = operand1_node;
		new_node->operand2 = mult_exp();

		return new_node;
	}
	else
	{
		_lex->prev_token();
		return operand1_node;
	}
}

//РАБОТАЕТ
node* parser::power_exp() //<unar_exp> | <unar_exp> ”**” <power_exp>
{
	node* operand1_node = unar_exp();
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::POW)
	{
		string power_oper_str = _lex->current_token()._value;
		node* new_node = new node(node_type::POWER_EXP, power_oper_str);


		_lex->next_token();
		new_node->operand1 = operand1_node;
		new_node->operand2 = power_exp ();

		return new_node;
	}
	else 
	{
		_lex->prev_token();
		return operand1_node;
	}
}

//РАБОТАЕТ
node* parser::unar_exp() //<unar_op><primary_exp>
{
	node* new_node = nullptr;

	if (_lex->current_token_type() == TokenType::PLUS || _lex->current_token_type() == TokenType::MINUS)
	{
		string unar_oper_str = _lex->current_token()._value;
		new_node = new node(node_type::UNAR_EXP, unar_oper_str);
		_lex->next_token();

		new_node->operand2 = primary_exp();
	}
	else
	{
		new_node = primary_exp();
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::primary_exp() //”(”<expression>”)” | <literal> | <ID> | <func_call> | <ID> ”(”<integer_literal>“)”
{
	node* new_node=nullptr;

	if (_lex->current_token_type() == TokenType::LPAR)
	{
		_lex->next_token();

		new_node = expression();
		_lex->next_token();

		if (_lex->current_token_type() != TokenType::RPAR)
		{
			throw ParseErr("')' expected");
		}
	}

	else if (_lex->current_token_type() == TokenType::SQRT)
	{
		new_node = sqrt();
	}

	else if (_lex->current_token_type() == TokenType::FLOAT || _lex->current_token_type() == TokenType::INT ||
		    _lex->current_token_type() == TokenType::TRUE || _lex->current_token_type() == TokenType::FALSE)
	{
		new_node = literal();
	}

	else if (_lex->current_token_type() == TokenType::IDENT)
	{
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::LPAR)
		{
			_lex->prev_token();
			new_node = mas_elem();
		}
		else
		{
			_lex->prev_token();

			string ident_str = _lex->current_token()._value;
			node* ident_node = new node(node_type::ID, ident_str);
			new_node = ident_node;
		}
	}

	else
	{
		throw ParseErr("Incorrect expression");
	}
	
	return new_node;
}

node* parser::sqrt() //функция взятия корня
{
	node* new_node = new node(node_type::SQRT_EXP);

	if (_lex->current_token_type() != TokenType::SQRT)
	{
		throw ParseErr("SQRT function expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::LPAR)
	{
		throw ParseErr("'(' expected");
	}
	_lex->next_token();

	new_node->operand1 = expression();
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::RPAR)
	{
		throw ParseErr("')' expected");
	}

	return new_node;
}

node* parser::mas_elem() // <ID> "(" <integer_literal> ")" В выражениях будет создавать узел, где будет в первом ребёнке отображаться имя массива и во втором ребёнке номер ячейки массива
{
	node* new_node = new node(node_type::MAS_ELEM);
	string elem_number_str;

	if (_lex->current_token_type() != TokenType::IDENT)
	{
		throw ParseErr("Identifier expected");
	}
	new_node->operand1 = identifier();
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::LPAR)
	{
		throw ParseErr("'(' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::PLUS || _lex->current_token_type() == TokenType::MINUS)
	{
		elem_number_str = _lex->current_token()._value;
		_lex->next_token();
	}

	if (_lex->current_token_type() == TokenType::INT)
	{
		elem_number_str = elem_number_str + _lex->current_token()._value;
		node* elem_number_node = new node(node_type::ELEM_NUMBER, elem_number_str);
		new_node->operand2 = elem_number_node;
	}
	else if (_lex->current_token_type() == TokenType::IDENT)
	{
		elem_number_str = elem_number_str + _lex->current_token()._value;
		node* elem_number_node = new node(node_type::ELEM_NUMBER, elem_number_str);
		new_node->operand2 = elem_number_node;
	}
	else
	{
		throw ParseErr("Massive element number expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::RPAR)
	{
		throw ParseErr("')' expected");
	}

	return new_node;
}

//РАБОТАЕТ
//Добавил по всем трём литералам функции, будем под литералами записывать их тип, мб потом пригодится. Нет - вырежем
//UPD. Не будем записывать ноды типа LITERAL. Будем записывать REAL_LITERAL, INTEGER_LITERAL и LOGICAL_LITERAL
node* parser::literal() //<logical_literal> | <real_literal> | <integer_literal>
{
	node* new_node=nullptr;

	if (_lex->current_token_type() == TokenType::FLOAT)
	{
		new_node = real_literal();
	}
	else if (_lex->current_token_type() == TokenType::INT)
	{
		new_node = integer_literal();
	}
	else if (_lex->current_token_type() == TokenType::TRUE && _lex->current_token_type() == TokenType::FALSE)
	{
		new_node = logical_literal();
	}
	else
	{
		throw ParseErr("'literal' expected");
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::real_literal() // <digit>{<digit>} [ “.”<digit>{<digit>}]
{
	if (_lex->current_token_type() != TokenType::FLOAT)
	{
		throw ParseErr("'integer_literal' expected");
	}

	//создаём новый узел, где будет храниться логический литерал 
	string real_literal_str = _lex->current_token()._value;
	node* real_literal_node = new node(node_type::REAL_LITERAL, real_literal_str);

	return real_literal_node;
}

//РАБОТАЕТ
node* parser::integer_literal()  // <digit>{ <digit> }
{
	if (_lex->current_token_type() != TokenType::INT)
	{
		throw ParseErr("'integer_literal' expected");
	}

	//создаём новый узел, где будет храниться логический литерал 
	string integer_literal_str = _lex->current_token()._value;
	node* integer_literal_node = new node(node_type::INTEGER_LITERAL, integer_literal_str);

	return integer_literal_node;
}

//РАБОТАЕТ
node* parser::logical_literal() //”.TRUE.”|”.FALSE.”
{
	if (_lex->current_token_type() != TokenType::TRUE && _lex->current_token_type() != TokenType::FALSE)
	{
		throw ParseErr("'logical_literal' expected");
	}

	//создаём новый узел, где будет храниться логический литерал 
	string logical_literal_str = _lex->current_token()._value;
	node* logical_literal_node = new node(node_type::LOGICAL_LITERAL, logical_literal_str);

	return logical_literal_node;
}

//РАБОТАЕТ
node* parser::statements(node_type parent) //{<statement>(“\n”|[“;”])}
{
	node* new_node = new node(node_type::STATEMENTS);

	//пропускаем лишние '\n' и ';'
	while (_lex->current_token_type() == TokenType::SEPARATOR)
		_lex->next_token();

	//В зависимости от места, откуда мы вызвали эту функцию
	//У нас будут разные условия выхода из рекурсии 
	//Мы можем вызвать statemets из:
	// <do_while_cycle>, <do_cycle>, <if_condition>, <elseif>, <func_body>, <program_body>
	if (parent == node_type::DO_CYCLE && _lex->current_token_type() == END)
	{
		_lex->prev_token();
		//Так как высказываний больше нет, то просто возврящаем нулевой узел
		return nullptr;	
	}

	if (parent == node_type::SUBROUTINE_BODY && _lex->current_token_type() == END)
	{
		_lex->prev_token();
		//Так как высказываний больше нет, то просто возврящаем нулевой узел
		return nullptr;
	}

	if (parent == node_type::DO_WHILE_CYCLE && _lex->current_token_type() == END)
	{
		_lex->prev_token();
		//Так как высказываний больше нет, то просто возврящаем нулевой узел
		return nullptr;
	}

	if (parent == node_type::IF_CONDITION && (_lex->current_token_type() == END || _lex->current_token_type() == ELSE))
	{
		_lex->prev_token();
		//Так как высказываний больше нет, то просто возврящаем нулевой узел
		return nullptr;
	}

	if (parent == node_type::ELSEIF && (_lex->current_token_type() == END || _lex->current_token_type() == ELSE))
	{
		_lex->prev_token();
		//Так как высказываний больше нет, то просто возврящаем нулевой узел
		return nullptr;
	}

	if (parent == node_type::FUNC_BODY && _lex->current_token_type() == END)
	{
		_lex->prev_token();
		//Так как высказываний больше нет, то просто возврящаем нулевой узел
		return nullptr;
	}

	if (parent == node_type::FUNC_BODY && _lex->current_token_type() == END)
	{
		_lex->prev_token();
		//Так как высказываний больше нет, то просто возврящаем нулевой узел
		return nullptr;
	}

	if (parent == node_type::PROGRAM_BODY && _lex->current_token_type() == END)
	{
		_lex->prev_token();
		//Так как высказываний больше нет, то просто возврящаем нулевой узел
		return nullptr;
	}

	//записываем встреченное высказывание в 1 узел
	new_node->operand1 = statement(parent);
	_lex->next_token();

	//проверяем, есть ли разделитель 
	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		throw ParseErr("'separator' expected");
	}
	_lex->next_token();

	//последующие высказывания будут рекурсивно записываться во второй узел
	new_node->operand2 = statements(parent);

	//в конце возвращаем построенное дерево 
	return new_node;
}

//РАБОТАЕТ
node* parser::statement(node_type parent) //<assignment> | <if_condition> | “stop” | <input_read> | <output_write> | <output_print> | “cycle” | “continue” | (“exit”[<ID>])
{
	node* new_node = nullptr;
	//у циклов есть дополнительные стейтменты, как "cycle" и "exit"
	//поэтому через параметр функции передаём, откуда мы вызывали эту функцию: из цикла или же нет 
	//и для цикла устраиваем отдельный сценарий обработки 
	if (parent == node_type::DO_CYCLE || parent == node_type::DO_WHILE_CYCLE)
	{
		if (_lex->current_token_type() == TokenType::IDENT)
		{
			new_node= assignment();
		}

		else if (_lex->current_token_type() == TokenType::IF)
		{
			new_node= if_condition();
		}

		else if (_lex->current_token_type() == TokenType::CALL)
		{
			new_node = subroutine_call();
		}

		else if (_lex->current_token_type() == TokenType::STOP)
		{
			string stop_str = _lex->current_token()._value;
			node* stop_node = new node(node_type::STOP_NODE, stop_str);
			new_node= stop_node;
		}

		else if (_lex->current_token_type() == TokenType::READ)
		{
			new_node= input_read();
		}

		else if (_lex->current_token_type() == TokenType::WRITE)
		{
			new_node= output_write();
		}

		else if (_lex->current_token_type() == TokenType::PRINT)
		{
			new_node= output_print();
		}

		else if (_lex->current_token_type() == TokenType::DO)
		{
			_lex->next_token();

			if (_lex->current_token_type() == TokenType::WHILE)
			{
				_lex->prev_token();
				new_node = do_while_cycle();
			}
			else
			{
				_lex->prev_token();
				new_node = do_cycle();
			}
		}

		else if (_lex->current_token_type() == TokenType::CYCLE)
		{
			string cycle_str = _lex->current_token()._value;
			node* cycle_node = new node(node_type::CYCLE_NODE, cycle_str);
			new_node= cycle_node;
		}

		else if (_lex->current_token_type() == TokenType::CONTINUE)
		{
			string continue_str = _lex->current_token()._value;
			node* continue_node = new node(node_type::CONTINUE_NODE, continue_str);
			new_node = continue_node;
		}

		else if (_lex->current_token_type() == TokenType::RETURN)
		{
			throw ParseErr("RETURN statement must be used only in subroutines!");
		}

		else if (_lex->current_token_type() == TokenType::EXIT)
		{
			string exit_str = _lex->current_token()._value;
			node* exit_node = new node(node_type::EXIT_NODE, exit_str);
			new_node= exit_node;
			//Если будем использовать именя циклов, надо будет добавить обработку идентификатора после exit
		}

		else throw ParseErr("'statement' expected");
	}

	else if(parent == node_type::SUBROUTINE_BODY)
	{
		if (_lex->current_token_type() == TokenType::IDENT)
		{
			new_node= assignment();
		}

		else if (_lex->current_token_type() == TokenType::CALL)
		{
			new_node = subroutine_call();
		}

		else if (_lex->current_token_type() == TokenType::IF)
		{
			new_node = if_condition();
		}

		else if (_lex->current_token_type() == TokenType::STOP)
		{
			string stop_str = _lex->current_token()._value;
			node* stop_node = new node(node_type::STOP_NODE, stop_str);
			new_node = stop_node;
		}

		else if (_lex->current_token_type() == TokenType::READ)
		{
			new_node = input_read();
		}

		else if (_lex->current_token_type() == TokenType::WRITE)
		{
			new_node = output_write();
		}

		else if (_lex->current_token_type() == TokenType::PRINT)
		{
			new_node = output_print();
		}

		else if (_lex->current_token_type() == TokenType::DO)
		{
			_lex->next_token();

			if (_lex->current_token_type() == TokenType::WHILE)
			{
				_lex->prev_token();
				new_node = do_while_cycle();
			}
			else
			{
				_lex->prev_token();
				new_node = do_cycle();
			}
		}

		else if (_lex->current_token_type() == TokenType::CYCLE)
		{
			throw ParseErr("'cycle' statement must be used only in cycles!");
		}

		else if (_lex->current_token_type() == TokenType::RETURN)
		{
			string return_str = _lex->current_token()._value;
			node* return_node = new node(node_type::RETURN_NODE, return_str);
			new_node = return_node;
		}

		else if (_lex->current_token_type() == TokenType::CONTINUE)
		{
			string continue_str = _lex->current_token()._value;
			node* continue_node = new node(node_type::CONTINUE_NODE, continue_str);
			new_node = continue_node;
		}

		else if (_lex->current_token_type() == TokenType::EXIT)
		{
			throw ParseErr("'exit' statement must be used only in cycles!");
		}

		else throw ParseErr("'statement' expected");
	}

	else
	{
	if (_lex->current_token_type() == TokenType::IDENT)
	{
		new_node = assignment();
	}

	else if (_lex->current_token_type() == TokenType::IF)
	{
		new_node = if_condition();
	}

	else if (_lex->current_token_type() == TokenType::CALL)
	{
		new_node = subroutine_call();
	}

	else if (_lex->current_token_type() == TokenType::STOP)
	{
		string stop_str = _lex->current_token()._value;
		node* stop_node = new node(node_type::STOP_NODE, stop_str);
		new_node = stop_node;
	}

	else if (_lex->current_token_type() == TokenType::READ)
	{
		new_node = input_read();
	}

	else if (_lex->current_token_type() == TokenType::WRITE)
	{
		new_node = output_write();
	}

	else if (_lex->current_token_type() == TokenType::PRINT)
	{
		new_node = output_print();
	}

	else if (_lex->current_token_type() == TokenType::DO)
	{
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::WHILE)
		{
			_lex->prev_token();
			new_node = do_while_cycle();
		}
		else
		{
			_lex->prev_token();
			new_node = do_cycle();
		}
	}

	else if (_lex->current_token_type() == TokenType::CYCLE)
	{
		throw ParseErr("'cycle' statement must be used only in cycles!");
	}

	else if (_lex->current_token_type() == TokenType::CONTINUE)
	{
		string continue_str = _lex->current_token()._value;
		node* continue_node = new node(node_type::CONTINUE_NODE, continue_str);
		new_node = continue_node;
	}

	else if (_lex->current_token_type() == TokenType::RETURN)
	{
		throw ParseErr("RETURN statement must be used only in subroutines!");
	}

	else if (_lex->current_token_type() == TokenType::EXIT)
	{
		throw ParseErr("'exit' statement must be used only in cycles!");
	}

	else throw ParseErr("'statement' expected");
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::assignment() //( <ID> | <ID> ”(” <integer_literal> “)” ) ” = ” <expression>
{
	node* new_node = new node(node_type::ASSIGNMENT);

	if (_lex->current_token_type() == TokenType::IDENT)
	{
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::LPAR)
		{
			_lex->prev_token();
			new_node->operand1= mas_elem();
		}
		else
		{
			_lex->prev_token();
			new_node->operand1 = identifier();
		}
	}
	else
	{
		throw ParseErr("Identifier expected");
	}
	_lex->next_token();
	
	if (_lex->current_token_type() != TokenType::EQUAL)
	{
		throw ParseErr("'=' expected");
	}
	_lex->next_token();

	new_node->operand2 = expression();

	return new_node;
}

//РАБОТАЕТ
node* parser::do_cycle() //[<ID> ”:”] “do”[<assignment> ”,” <expression>[“, ” <expression>]] <statements> “end” ”do”[<ID>]
{
	node* new_node = new node(node_type::DO_CYCLE);

	if (_lex->current_token_type() != TokenType::DO)
	{
		throw ParseErr("'do' expected");
	}
	_lex->next_token();

	//если после do стоит присваивание
	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		//записываем его в первый узел 
		new_node->operand1 = assignment();
		_lex->next_token();

		if (_lex->current_token_type() != TokenType::COMMA)
		{
			throw ParseErr("',' expected");
		}
		_lex->next_token();

		//если есть первое выражение, то обязательно(!) должно быть второе, записываем его во второй узел 
		new_node->operand2 = expression();
		_lex->next_token();

		//Если есть 3 выражение, то 
		if (_lex->current_token_type() != TokenType::SEPARATOR)
		{
			if (_lex->current_token_type() != TokenType::COMMA)
			{
				throw ParseErr("',' expected");
			}
			_lex->next_token();

			//записываем его 
			new_node->operand3 = expression();
			_lex->next_token();
		}
		else
			//Иначе, если третьего выражения нет, записываем nullptr (однако надо считать, что шаг цикла при этом дфелотный и равен ЕДИНИЦЕ)
		{
			new_node->operand3 = nullptr;
		}
	}
	//если не встречаем выражний, то записываем в узлы nullptr
	else
	{
		new_node->operand1 = nullptr;
		new_node->operand2 = nullptr;
		new_node->operand3 = nullptr;
	}
	
	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		throw ParseErr("Separator expected");
	}
	_lex->next_token();

	// в 4 узел дерева записывем различные действия 
	new_node->operand4 = statements(node_type::DO_CYCLE);
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::END)
	{
		throw ParseErr("'end' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::DO)
	{
		throw ParseErr("'do' expected");
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::do_while_cycle() //[<ID> ”:”] “do” “while” “(”<expression>”)” <statements> “end” ”do”[<ID>]
{
	node* new_node = new node(node_type::DO_WHILE_CYCLE);
	if (_lex->current_token_type() != TokenType::DO)
	{
		throw ParseErr("'do' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::WHILE)
	{
		throw ParseErr("'while' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::LPAR)
	{
		throw ParseErr("'(' expected");
	}
	_lex->next_token();

	new_node->operand1 = expression();
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::RPAR)
	{
		throw ParseErr("')' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		throw ParseErr("Separator expected");
	}
	_lex->next_token();

	new_node->operand2 = statements(node_type::DO_WHILE_CYCLE);
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::END)
	{
		throw ParseErr("'end' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::DO)
	{
		throw ParseErr("'do' expected");
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::output_print() //”print”“*”{“,”<expression>}
{
	node* new_node = new node(node_type::OUTPUT_PRINT);
	if (_lex->current_token_type() != TokenType::PRINT)
	{
		throw ParseErr("'print' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::MULTIPLE)
	{
		throw ParseErr("'*' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::COMMA)
	{
		_lex->next_token();
		if (_lex->current_token_type() != TokenType::SEPARATOR)
		{
			new_node->operand1 = output_list();
		}
		else
		{
			_lex->prev_token();
		}
	}
	else
	{
		_lex->prev_token();
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::output_write() //”write” ”(” “*” “,” “*” ”)” [<expression>]{ “,”<expression> }
{
	node* new_node = new node(node_type::OUTPUT_WRITE);
	if (_lex->current_token_type() != TokenType::WRITE)
	{
		throw ParseErr("'write' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::LPAR)
	{
		throw ParseErr("'(' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::MULTIPLE)
	{
		throw ParseErr("'*' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::COMMA)
	{
		throw ParseErr("',' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::MULTIPLE)
	{
		throw ParseErr("'*' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::RPAR)
	{
		throw ParseErr("')' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		new_node->operand1 = output_list();
	}
	else
	{
		_lex->prev_token();
	}

	return new_node;
}

node* parser::input_list() //Список переменных, констант и элементов массива, которые нужно ввести
{

	node* new_node = new node(node_type::INPUT_LIST);

	if (_lex->current_token_type() == TokenType::IDENT)
	{
		_lex->next_token();

		if (_lex->current_token_type() == TokenType::LPAR)
		{
			_lex->prev_token();
			new_node->operand1 = mas_elem();
		}
		else
		{
			_lex->prev_token();
			string ident_str = _lex->current_token()._value;
			node* ident_node = new node(node_type::ID, ident_str);
			new_node->operand1 = ident_node;
		}
	}
	_lex->next_token();
	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		if (_lex->current_token_type() != TokenType::COMMA)
		{
			throw ParseErr("',' expected");
		}
		_lex->next_token();

		new_node->operand2 = input_list();
	}
	else
	{
		_lex->prev_token();
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::output_list() //Список выражений, констант и т.п. что нужно вывести
{
	node* new_node = new node(node_type::OUTPUT_LIST);

	new_node->operand1 = expression();
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		if (_lex->current_token_type() != TokenType::COMMA)
		{
			throw ParseErr("',' expected");
		}
		_lex->next_token();

		new_node->operand2 = output_list();
	}
	else
	{
		_lex->prev_token();
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::input_read() //”read” ”(” “*” “,” “*” ”)” ID{“,”<ID>}
{
	node* new_node = new node(node_type::INPUT_READ);
	if (_lex->current_token_type() != TokenType::READ)
	{
		throw ParseErr("'read' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::LPAR)
	{
		throw ParseErr("'(' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::MULTIPLE)
	{
		throw ParseErr("'*' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::COMMA)
	{
		throw ParseErr("',' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::MULTIPLE)
	{
		throw ParseErr("'*' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::RPAR)
	{
		throw ParseErr("')' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		new_node->operand1 = input_list();
	}
	else
	{
		_lex->prev_token();
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::if_condition() //”if” ”(”<expression>”)” “then” <statements>[<elseif>][“else”<statements>]
{
	node* new_node = new node(node_type::IF_CONDITION);

	if (_lex->current_token_type() != TokenType::IF)
	{
		throw ParseErr("'if' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::LPAR)
	{
		throw ParseErr("'(' expected");
	}
	_lex->next_token();

	new_node->operand1 = expression();
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::RPAR)
	{
		throw ParseErr("')' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::THEN)
	{
		throw ParseErr("'than' expected");
	}
	_lex->next_token();

	//Добавил проверку на сепаратор
	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		throw ParseErr("'separator' expected");
	}
	_lex->next_token();

	new_node->operand2 = statements(node_type::IF_CONDITION);
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::ELSE)
	{
		_lex->next_token();
		if (_lex->current_token_type() == TokenType::IF)
		{
			_lex->prev_token();
			new_node->operand3 = elseif();
		}
		else
		{
			if (_lex->current_token_type() != TokenType::SEPARATOR)
			{
				throw ParseErr("Separator expected");
			}
			_lex->next_token();
			new_node->operand3 = statements(IF_CONDITION);
			_lex->next_token();
		}
	}

	if (_lex->current_token_type() != TokenType::END)
	{
		throw ParseErr("'end' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::IF)
	{
		throw ParseErr("'if' expected");
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::elseif() //”else” ”if” ”(”<expression>”)” “then” <statements> | ”else” ”if” ”(”<expression>”)” “then” <statements> <elseif>
{
	node* new_node = new node(node_type::ELSEIF);

	if (_lex->current_token_type() != TokenType::ELSE)
	{
		throw ParseErr("'else' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::IF)
	{
		throw ParseErr("'if' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::LPAR)
	{
		throw ParseErr("'(' expected");
	}
	_lex->next_token();

	new_node->operand1 = expression();
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::RPAR)
	{
		throw ParseErr("')' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::THEN)
	{
		throw ParseErr("'than' expected");
	}
	_lex->next_token();

	if (_lex->current_token_type() != TokenType::SEPARATOR)
	{
		throw ParseErr("'separator' expected");
	}
	_lex->next_token();

	new_node->operand2 = statements(node_type::ELSEIF);
	_lex->next_token();

	if (_lex->current_token_type() == TokenType::ELSE)
	{
		_lex->next_token();
		if (_lex->current_token_type() == TokenType::IF)
		{
			_lex->prev_token();
			new_node->operand3 = elseif();
		}
		else
		{
			if (_lex->current_token_type() != TokenType::SEPARATOR)
			{
				throw ParseErr("Separator expected");
			}
			_lex->next_token();
			new_node->operand3 = statements(node_type::ELSEIF);
			_lex->next_token();
		}
	}

	return new_node;
}

//РАБОТАЕТ
node* parser::identifier()
{
	// если тип текущего токена не идентефикатор
	if (_lex->current_token_type() != TokenType::IDENT)
	{
		throw ParseErr("Ident expected!");
	}
	// если же это идентефикатор то
	// получаем значение текущего токена, которое равно идентефикатору
	string ident_str = _lex->current_token()._value;

	// сдвигаем текущий токен
	_lex->next_token();

	// создаем новый узел с типом ID и значением равным значению
	// из лексемы текущего токена
	node* ident_node = new node(node_type::ID, ident_str);

	if (_lex->current_token_type() == TokenType::COMMA)
	{
		_lex->next_token();
		ident_node->operand1 = identifier();
	}
	else
	{
		_lex->prev_token();
	}
	// возвращаем полученный узел
	return ident_node;
}