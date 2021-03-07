#pragma once
#include "Lex.h"

enum class variable_type
{
	// если тип не установлен
	UNDEFINED = 0xffffff,

	INTEGER = 0x001000,
	REAL = 0x002000,
	DOBLE_PRECISION = 0x003000,
	LOGICAL = 0x004000,
	CHAR = 0x005000,
};

enum class program_type
{
	// если тип не установлен
	PROGRAM = 0x001000,
	SUBROUTINE = 0x002000,
};

typedef enum node_type {

	FULL_PROGRAM,
	PROGRAM_BODY,
	FUNC_CALL,
	FUNC_DECL_LIST,
	FUNC_DECL,
	FUNC_REC_DECL,
	FUNC_ARG,
	FUNC_BODY,

	SUBROUTINE_CALL,
	SUBROUTINE_CALL_ARG,
	SUBROUTINE_BODY,
	SUBOUTINE_ARG,
	SUBROUTINE_DECL,
	SUBROUTINE_DECL_LIST,
	SUBROUTINE_REC,

	SUB_VAR_DECL,
	SUB_MAS_DECL,
	SUB_MAS_LIST,
	SUB_DECL_LIST,

	DECL_LIST,
	VAR_DECL,
	VAR_LIST,
	VAR,
	MAS_SIZE,
	MAS_START,
	MAS_FINISH,
	MAS_DECL,
	MAS_LIST,
	MAS_ELEM,
	ELEM_NUMBER,
	MAS,
	MAS_INIT,
	ATTRIBUTE,
	ID_LIST,
	TYPE,
	EXPRESSION,
	LOG_EQV_EXP,
	LOG_OR_EXP,
	LOG_AND_EXP,
	LOG_NOT_EXP,
	REL_EXP,
	ADD_EXP,
	MULT_EXP,
	POWER_EXP,
	UNAR_EXP,
	SQRT_EXP,
	PRIMARY_EXP,
	LOG_EQV_OP,
	REL_OP,
	MULT_OP,
	ADD_OP,
	UNAR_OP,
	LITERAL,
	LOGICAL_LITERAL,
	REAL_LITERAL,
	INTEGER_LITERAL,
	CONSTANT,
	STATEMENTS,
	STATEMENT,			//Инструкция
	ASSIGNMENT,			//Определение переменной
	DO_CYCLE,			//циклы
	DO_WHILE_CYCLE,
	OUTPUT_PRINT,		//вывод
	OUTPUT_WRITE,
	OUTPUT_LIST,
	INPUT_LIST,
	INPUT_READ,			//ввод
	IF_CONDITION,		//условный оператор
	ELSEIF,
	ID,					//идентефикатор

	STOP_NODE,               //остановка программы
	CYCLE_NODE,              //преждевременное завершение итерации цикла
	CONTINUE_NODE,           //используют непонятно зачем, по сути ничего не делает 
	EXIT_NODE,                //используется для преждевременного выхода из цикла
	RETURN_NODE,              //выход из подпрограммы
	CALL_NODE,
};

class ParseErr {
public:
	inline ParseErr(string str)
	{
		cout << "Parse error!  " << str << endl;
	}
};

class SemErr {
public:
	inline SemErr(string str)
	{
		cout << "Semantic error!  " << str << endl;
	}
};

void semantic_error(const string& message);
variable_type string_to_var_type(string st);
string  var_to_string_type(variable_type type);
program_type node_type_to_program_type(node_type type);
string  program_to_string_type(program_type type);

class node
{
public:
	string value;
	node_type type;

	node* operand1;
	node* operand2;
	node* operand3;
	node* operand4;
	node* operand5;
	node* operand6;
	node* operand7;

	node(node_type type, const string& value = "", node* operand1 = nullptr,
		node* operand2 = nullptr, node* operand3 = nullptr, node* operand4 = nullptr, node* operand5 = nullptr, node* operand6 = nullptr, node* operand7 = nullptr)
	{
		this->type = type;
		this->value = value;
		this->operand1 = operand1;
		this->operand2 = operand2;
		this->operand3 = operand3;
		this->operand4 = operand4;
		this->operand5 = operand5;
		this->operand6 = operand6;
		this->operand7 = operand7;
	}
};

class variable
{
	// имя переменной
	string _variable_name;
	// и ее тип
	variable_type _variable_type;
	//атрибут переменной
	string _variable_attribute;
	//ссылка на поддерево с объявлением 
	node* _variable_assignment;
public:
	variable(const string& _variable_name, variable_type _variable_type,
		     const string& _variable_attribute = "", node* _variable_assignment = nullptr)
	{
		this->_variable_name = _variable_name;
		this->_variable_type = _variable_type;
		this->_variable_attribute = _variable_attribute;
		this->_variable_assignment = _variable_assignment;
	}
	string name()
	{
		return _variable_name;
	}

	variable_type type()
	{
		return _variable_type;
	}

	node* assignment()
	{
		return _variable_assignment;
	}

	string attribute()
	{
		return _variable_attribute;
	}
};


class variable_table
{
private:
	// массив указателей на переменную
	vector<variable*> _vars;

public:
	// конструктор по-умолчанию
	variable_table() = default;
	// деструктор
	~variable_table()
	{
		// очищаем массив, тем самым освобождая память под указатели под переменные
		_vars.clear();
	}

	//очистка массива
	void clear()
	{
		_vars.clear();
	}

	bool is_clear()
	{
		if (_vars.size() == 0)
		{
			return true;
		}
		else return false;
	}

	// основной метод для добавления переменных в таблицу
	void add_variable(variable* var)
	{
		_vars.push_back(var);
	}

	variable* get_variable_by_name(const string& name_)
	{
		// проходим по массиву переменных
		for (auto& variable : _vars)
		{
			// если имя переменной совпадает с необходимым именем
			if (_stricmp(variable->name().c_str(), name_.c_str()) == 0)
			{
				// то возращаем эту переменную
				return variable;
			}
		}

		// в случае, если переменной нет, возбуждаем исключение
		return nullptr;
	}

	bool has_variable(const string& name_)
	{
		// проходим по массиву переменных
		for (auto& variable : _vars)
		{
			// если имя переменной совпадает с необходимым именем
			if (_stricmp(variable->name().c_str(), name_.c_str()) == 0)
			{
				// то возращаем истину
				return true;
			}
		}

		// в случае, если переменной нет возращаем false
		return false;
	}

	void print_variable_table()
	{
		// проходим по массиву переменных
		for (auto& variable : _vars)
		{
			cout << "    VAR: " << variable->name() << endl;
			cout << "      TYPE: " << var_to_string_type(variable->type()) << endl;
			cout << "      ATTRIBUTE: " << variable->attribute() << endl;
			if (variable->assignment() != NULL)
				cout << "      INITIALIZATION: YES" << endl;
			else
				cout << "      INITIALIZATION: NO" << endl;
		}
	}
};

class massive
{
	// имя массива
	string _massive_name;
	// и его тип
	variable_type _massive_type;
	//атрибут массива INTENT
	string _massive_attribute_intent;
	//начальный индекс массива
	int _start_index;
	//конечный индекс массива
	int _stop_index;
	//размер
	int _size;
	//ссылка на поддерево с объявлением 
	node* _massive_assignment;
public:
	massive(const string& _massive_name, variable_type _massive_type,
		int _start_index, int _stop_index, int _size, node* _massive_assignment = nullptr, const string& _massive_attribute_intent = "")
	
	{
		this->_massive_name = _massive_name;
		this->_massive_type = _massive_type;
		this->_massive_attribute_intent = _massive_attribute_intent;
		this->_massive_assignment = _massive_assignment;
		this->_start_index = _start_index;
		this->_stop_index = _stop_index;
		this->_size = _size;
	}
	string name()
	{
		return _massive_name;
	}

	variable_type type()
	{
		return _massive_type;
	}

	node* assignment()
	{
		return _massive_assignment;
	}

	string attribute_intent()
	{
		return _massive_attribute_intent;
	}
	int start_index()
	{
		return _start_index;
	}
	int stop_index()
	{
		return _stop_index;
	}
	int size()
	{
		return _size;
	}
};

class massive_table
{
private:
	// массив указателей на переменную
	vector<massive*> _mass;

public:
	// конструктор по-умолчанию
	massive_table() = default;
	// деструктор
	~massive_table()
	{
		// очищаем массив, тем самым освобождая память под указатели под переменные
		_mass.clear();
	}

	//очистка массива
	void clear()
	{
		_mass.clear();
	}

	bool is_clear()
	{
		if (_mass.size() == 0)
		{
			return true;
		}
		else return false;
	}

	// основной метод для добавления переменных в таблицу
	void add_massive(massive* mas)
	{
		_mass.push_back(mas);
	}

	massive* get_massive_by_name(const string& name_)
	{
		// проходим по массиву переменных
		for (auto& massive : _mass)
		{
			// если имя переменной совпадает с необходимым именем
			if (_stricmp(massive->name().c_str(), name_.c_str()) == 0)
			{
				// то возращаем эту переменную
				return massive;
			}
		}

		return nullptr;
	}

	bool has_massive(const string& name_)
	{
		// проходим по массиву переменных
		for (auto& massive : _mass)
		{
			// если имя переменной совпадает с необходимым именем
			if (_stricmp(massive->name().c_str(), name_.c_str()) == 0)
			{
				// то возращаем истину
				return true;
			}
		}

		// в случае, если переменной нет возращаем false
		return false;
	}

	void print_massive_table()
	{
		// проходим по массиву переменных
		for (auto& massive : _mass)
		{
			cout << "    MASSIVE: " << massive->name() << endl;
			cout << "      TYPE: " << var_to_string_type(massive->type()) << endl;
			cout << "      ATTRIBUTE: " << massive->attribute_intent() << endl;
			cout << "      START: " << massive->start_index() << endl;
			cout << "      STOP: " << massive->stop_index() << endl;
			cout << "      SIZE: " << massive->size() << endl;
			if (massive->assignment() != NULL)
				cout << "      INITIALIZATION: YES" << endl;
			else
				cout << "      INITIALIZATION: NO" << endl;
		}
	}
};

class program
{
	// имя программы (если есть) /убрутины
	string _program_name;
	// тип: программа или субрутина
	program_type _program_type;
	//атрибут подпрограммы
	string _program_attribute;
	//таблица переменных 
	variable_table _all_variables;
	//таблица массивов
    massive_table _all_massives;

public:
	program(const string& _program_name, program_type _program_type,
		variable_table _all_variables, massive_table _all_massives, const string& _program_attribute = "")
	{
		this->_program_name = _program_name;
		this->_program_type = _program_type;
		this->_program_attribute = _program_attribute;
		this->_all_variables = _all_variables;
		this->_all_massives = _all_massives;
	}
	string name()
	{
		return _program_name;
	}

	program_type type()
	{
		return _program_type;
	}

	string attribute()
	{
		return _program_attribute;
	}

	variable_table all_variables()
	{
		return _all_variables;
	}

	massive_table all_massives()
	{
		return _all_massives;
	}
};

class program_table
{
private:
	// массив указателей на переменную
	vector<program*> _progs;

public:
	// конструктор по-умолчанию
	program_table() = default;
	// деструктор
	~program_table()
	{
		// очищаем массив, тем самым освобождая память под указатели под переменные
		_progs.clear();
	}

	//очистка массива
	void clear()
	{
		_progs.clear();
	}

	bool is_clear()
	{
		if (_progs.size() == 0)
		{
			return true;
		}
		else return false;
	}

	// основной метод для добавления переменных в таблицу
	void add_program(program* prog)
	{
		_progs.push_back(prog);
	}

	program* get_program_by_name(const string& name_)
	{
		// проходим по массиву программ
		for (auto& program : _progs)
		{
			// если имя переменной совпадает с необходимым именем
			if (_stricmp(program->name().c_str(), name_.c_str()) == 0)
			{
				// то возращаем эту переменную
				return program;
			}
		}

		// в случае, если переменной нет, возбуждаем исключение
		throw SemErr("Program not found!");
	}

	bool has_program(const string& name_)
	{
		// проходим по массиву переменных
		for (auto& program : _progs)
		{
			// если имя переменной совпадает с необходимым именем
			if (_stricmp(program->name().c_str(), name_.c_str()) == 0)
			{
				// то возращаем истину
				return true;
			}
		}

		// в случае, если переменной нет возращаем false
		return false;
	}


	//В первом параметре нужно задавать имя функции или же имя программы
	//Если имени программы нет - пишем просто PROGRAM
	//Во втором параметре задаём имя переменной
	variable* get_var_from_table(const string& name_func, const string& name_var)
	{
		variable* var = nullptr;
		// проходим по массиву программ
		for (auto& program : _progs)
		{
			// если имя переменной совпадает с необходимым именем
			if (_stricmp(program->name().c_str(), name_func.c_str()) == 0)
			{
				var = program->all_variables().get_variable_by_name(name_var);
				// то возращаем эту переменную
				return var;
			}
		}
	}

	massive* get_massive_from_table(const string& name_func, const string& name_mas)
	{
		massive* mas = nullptr;
		// проходим по массиву программ
		for (auto& program : _progs)
		{
			// если имя переменной совпадает с необходимым именем
			if (_stricmp(program->name().c_str(), name_func.c_str()) == 0)
			{
				mas = program->all_massives().get_massive_by_name(name_mas);
				// то возращаем эту переменную
				return mas;
			}
		}
	}

	void print_program_table()
	{
		cout << endl;
		cout << "SEMANTIC TABLE" << endl;
		cout << endl;
		// проходим по массиву переменных
		for (auto& program : _progs)
		{
			if (program->type() == program_type::PROGRAM)
			{
				cout << "PROGRAM: " << program->name() << endl;
			}
			else
			{
				cout << "SUBROUTINE: " << program->name() << endl;
			}
			cout << "  TYPE: " << program_to_string_type(program->type()) << endl;
			cout << "  ATTRIBUTE: " << program->attribute() << endl;
			cout << "  VAR LIST: " << endl;
			program->all_variables().print_variable_table();
			cout << "  MASSIVE LIST: " << endl;
			program->all_massives().print_massive_table();
		}
		cout << endl;
	}
};

class ast
{
public:
	node* tree;
	variable_table _all_variables;
	massive_table _all_massives;
	program_table _all_programs;

public:
	ast()
	{
		tree = nullptr;
	}
	~ast()
	{
		// после окончания глав про синтаксический анализ, вернитесь сюда
		// и подумайте, как реализовать очистку такого дерева, это не так сложно :)
	}
	void print_recursive(node* current_node, size_t level);
	void print()
	{
		// первым параметром мы передаем вершину дерева, а вторым нулевой уровень
		// с которого мы начинаем
		print_recursive(tree, 0);
	}

	void designate_variables_recursive(node* current_node)
	{
		// если текущая нода ноль, то делать с ней ничего нельзя
		// так что выходим из функции
		if (current_node == nullptr)
			return;

		// главная проверка
		if (current_node->type == node_type::VAR_DECL || current_node->type == node_type::SUB_VAR_DECL)
		{
			// значит текущий узел описывает объявление переменной
			node* tmp = current_node->operand3;

			// получаем тип из первого потомка, в котором мы храним тип переменной
			variable_type type = string_to_var_type(current_node->operand1->value);

			//записваем атрибут переменной, если он есть 
			string attribute = "";
			if (current_node->operand2 != nullptr)
			{
				attribute = current_node->operand2->value;
			}

			while (tmp != nullptr)
			{
				// получаем имя переменной из значения узла
				string name = tmp->operand1->operand1->value;
				// создаем новую переменную
				variable* var = new variable(name, type, attribute, tmp->operand1->operand2);
				// добавляем в таблицу
				if (!_all_variables.is_clear())
				{
					if (_all_variables.has_variable(var->name()))
					{
						throw SemErr("Repeating declaration of variable or massive");
					}
				}
				if (!_all_massives.is_clear())
				{
					if (_all_massives.has_massive(var->name()))
					{
						throw SemErr("Repeating declaration of variable or massive");
					}
				}

				_all_variables.add_variable(var);

				tmp = tmp->operand2;
			}
		}

		// идем в первого потомка
		designate_variables_recursive(current_node->operand1);
		// во второго
		designate_variables_recursive(current_node->operand2);
		// в третьего
		designate_variables_recursive(current_node->operand3);
		// в чертвертого
		designate_variables_recursive(current_node->operand4);
		//в пятого 
		designate_variables_recursive(current_node->operand5);
		//в шестого 
		designate_variables_recursive(current_node->operand6);
		//в седьмого
		designate_variables_recursive(current_node->operand7);
	}

	//ФУНКИЦИЯ ДЛЯ ПРОВЕРКИ ВЫХОДНЫХ АРГУМЕНТОВ
	//ФУНКИЦИЯ ДЛЯ ПРОВЕРКИ ВЫХОДНЫХ АРГУМЕНТОВ
	//ФУНКИЦИЯ ДЛЯ ПРОВЕРКИ ВЫХОДНЫХ АРГУМЕНТОВ
	//ФУНКИЦИЯ ДЛЯ ПРОВЕРКИ ВЫХОДНЫХ АРГУМЕНТОВ
	//ФУНКИЦИЯ ДЛЯ ПРОВЕРКИ ВЫХОДНЫХ АРГУМЕНТОВ
	//ФУНКИЦИЯ ДЛЯ ПРОВЕРКИ ВЫХОДНЫХ АРГУМЕНТОВ
	void what_you_get(node* current_node, string name_, vector<int>* count)
	{
		// если текущая нода ноль, то делать с ней ничего нельзя
		// так что выходим из функции
		if (current_node == nullptr)
			return;

		if (current_node->type == node_type::SUBROUTINE_DECL || current_node->type == node_type::SUBROUTINE_REC)
		{
			string call_name = current_node->operand2->value;
			if (_stricmp(call_name.c_str(), name_.c_str()) == 0)
			{
				int iterator = 1;

				node* tmp = current_node->operand3;

				while (tmp != nullptr)
				{
					if (_all_programs.get_program_by_name(name_)->all_massives().has_massive(tmp->value))
					{
						if (_stricmp(_all_programs.get_massive_from_table(name_, tmp->value)->attribute_intent().c_str(), "OUT") == 0 ||
							_stricmp(_all_programs.get_massive_from_table(name_, tmp->value)->attribute_intent().c_str(), "INOUT") == 0)
						{
							count->push_back(iterator);
						}
					}
					else if (_all_programs.get_program_by_name(name_)->all_variables().has_variable(tmp->value))
					{
						if (_stricmp(_all_programs.get_var_from_table(name_, tmp->value)->attribute().c_str(), "OUT") == 0 ||
							_stricmp(_all_programs.get_var_from_table(name_, tmp->value)->attribute().c_str(), "INOUT") == 0)
						{
							count->push_back(iterator);
						}
					}
					iterator++;
					tmp = tmp->operand1;

				}
				return;
			}
			else return;
		}

		// идем в первого потомка
		what_you_get(current_node->operand1, name_, count);
		// во второго
		what_you_get(current_node->operand2, name_, count);
		// в третьего
		what_you_get(current_node->operand3, name_, count);
		// в чертвертого
		what_you_get(current_node->operand4, name_, count);
		//в пятого 
		what_you_get(current_node->operand5, name_, count);
		//в шестого 
		what_you_get(current_node->operand6, name_, count);
		//в седьмого
		what_you_get(current_node->operand7, name_, count);
	}



	void designate_massive_recursive(node* current_node)
	{
		// если текущая нода ноль, то делать с ней ничего нельзя
		// так что выходим из функции
		if (current_node == nullptr)
			return;

		// главная проверка
		if (current_node->type == node_type::MAS_DECL)
		{
			// значит текущий узел описывает объявление массива
			node* tmp = current_node->operand5;

			// получаем тип из первого потомка, в котором мы храним тип переменной
			variable_type type = string_to_var_type(current_node->operand1->value);

			int start = stoi(current_node->operand2->value);
			int stop = stoi(current_node->operand3->value);
			int size = stoi(current_node->operand4->value);

			while (tmp != nullptr)
			{
				// получаем имя переменной из значения узла
				string name = tmp->operand1->operand1->value;
				// создаем новую переменную
				massive* mas = new massive(name, type, start, stop, size, tmp->operand1->operand2);

				if (!_all_variables.is_clear())
				{
					if (_all_variables.has_variable(mas->name()))
					{
						throw SemErr("Repeating declaration of variable or massive");
					}
				}
				if (!_all_massives.is_clear())
				{
					if (_all_massives.has_massive(mas->name()))
					{
						throw SemErr("Repeating declaration of variable or massive");
					}
				}

				// добавляем в таблицу
				_all_massives.add_massive(mas);

				tmp = tmp->operand2;
			}
		}

		else if (current_node->type == node_type::SUB_MAS_DECL)
		{
			// значит текущий узел описывает объявление массива
			node* tmp = current_node->operand6;

			// получаем тип из первого потомка, в котором мы храним тип переменной
			variable_type type = string_to_var_type(current_node->operand1->value);

			//записваем атрибут массива, если он есть 
			string attribute = "";
			if (current_node->operand2 != nullptr)
			{
				attribute = current_node->operand2->value;
			}

			int start = stoi(current_node->operand3->value);
			int stop = stoi(current_node->operand4->value);
			int size = stoi(current_node->operand5->value);

			while (tmp != nullptr)
			{
				// получаем имя переменной из значения узла
				string name = tmp->operand1->operand1->value;
				// создаем новую переменную
				massive* mas = new massive(name, type, start, stop, size, tmp->operand1->operand2, attribute);
				// добавляем в таблицу
				_all_massives.add_massive(mas);

				tmp = tmp->operand2;
			}
		}

		// идем в первого потомка
		designate_massive_recursive(current_node->operand1);
		// во второго
		designate_massive_recursive(current_node->operand2);
		// в третьего
		designate_massive_recursive(current_node->operand3);
		// в чертвертого
		designate_massive_recursive(current_node->operand4);
		//в пятого 
		designate_massive_recursive(current_node->operand5);
		//в шестого 
		designate_massive_recursive(current_node->operand6);
		//в седьмого
		designate_massive_recursive(current_node->operand7);
	}

	void designate_program_recursive(node* current_node)
	{
		// если текущая нода ноль, то делать с ней ничего нельзя
		// так что выходим из функции
		if (current_node == nullptr)
			return;

		// главная проверка
		if (current_node->type == node_type::PROGRAM_BODY)
		{
			program_type type = node_type_to_program_type(current_node->type);
			string name = current_node->value;

			if (_stricmp(name.c_str(), "") == 0)
			{
				name = "PROGRAM";
			}

			_all_variables.clear();
			_all_massives.clear();

			designate_variables_recursive(current_node);
			designate_massive_recursive(current_node);

			program* prog = new program(name, type, _all_variables, _all_massives);

			_all_programs.add_program(prog);
		}

		else if (current_node->type == node_type::SUBROUTINE_DECL || current_node->type == node_type::SUBROUTINE_REC)
		{
			program_type type = node_type_to_program_type(current_node->type);

			string name = current_node->operand2->value;

			string attribute = "";
			if (current_node->operand1 != nullptr)
			{
				attribute = current_node->operand1->value;
			}

			_all_variables.clear();
			_all_massives.clear();

			designate_variables_recursive(current_node);
			designate_massive_recursive(current_node);

			program* prog = new program(name, type, _all_variables, _all_massives, attribute);

			if (!_all_programs.is_clear())
			{
				if (_all_programs.has_program(prog->name()))
				{
					throw SemErr("Repeating declaration of SUBROUTINE");
				}
			}

			_all_programs.add_program(prog);
		}

		// идем в первого потомка
		designate_program_recursive(current_node->operand1);
		// во второго
		designate_program_recursive(current_node->operand2);
		// в третьего
		designate_program_recursive(current_node->operand3);
		// в чертвертого
		designate_program_recursive(current_node->operand4);
		//в пятого 
		designate_program_recursive(current_node->operand5);
		//в шестого 
		designate_program_recursive(current_node->operand6);
		//в седьмого
		designate_program_recursive(current_node->operand7);
	}

	void check_declarations(node* current_node, node_type status=node_type::FULL_PROGRAM, string sub_name="", node* prev_node = nullptr)
	{
		// если текущая нода ноль, то делать с ней ничего нельзя
		// так что выходим из функции
		if (current_node == nullptr)
			return;

		if (current_node->type == node_type::PROGRAM_BODY)
		{
			status = node_type::PROGRAM_BODY;
			if (current_node->value == "")
			{
				sub_name = "PROGRAM";
			}
			else
			{
				sub_name = current_node->value;
			}
		}

		if (current_node->type == node_type::SUBROUTINE_DECL || current_node->type == node_type::SUBROUTINE_REC)
		{
			status = node_type::SUBROUTINE_DECL;
			sub_name = current_node->operand2->value;
		}

		if (current_node->type == node_type::SUBROUTINE_REC)
		{
			status = node_type::SUBROUTINE_REC;
			sub_name = current_node->operand2->value;
		}

		// главная проверка
		if (current_node->type == node_type::STATEMENTS)
		{
			check_declarations_help(current_node->operand1, status, sub_name, current_node);
		}

		// идем в первого потомка
		check_declarations(current_node->operand1, status, sub_name, current_node);
		// во второго
		check_declarations(current_node->operand2, status, sub_name, current_node);
		// в третьего
		check_declarations(current_node->operand3, status, sub_name, current_node);
		// в чертвертого
		check_declarations(current_node->operand4, status, sub_name, current_node);
		//в пятого 
		check_declarations(current_node->operand5, status, sub_name, current_node);
		//в шестого 
		check_declarations(current_node->operand6, status, sub_name, current_node);
		//в седьмого
		check_declarations(current_node->operand7, status, sub_name, current_node);
	}

	void check_declarations_help(node* current_node, node_type status, string sub_name, node* prev_node)
	{
		// если текущая нода ноль, то делать с ней ничего нельзя
		// так что выходим из функции
		if (current_node == nullptr)
			return;

		// главная проверка
		if (current_node->type == node_type::MAS_ELEM)
		{
			string name = current_node->operand1->value;
			//cout << "MAS IDNDEX" << current_node->operand2->value<<endl;
			if (!_all_programs.get_program_by_name(sub_name)->all_massives().is_clear())
			{
				//cout << current_node->value << endl;
				if (!_all_programs.get_program_by_name(sub_name)->all_massives().has_massive(name))
				{
					throw SemErr("You cannot work with undeclarated MASSIVE");
				}
				else if (_stricmp(_all_programs.get_program_by_name(sub_name)->all_massives().get_massive_by_name(name)->attribute_intent().c_str(), "IN") == 0 &&
					     prev_node->type == node_type::ASSIGNMENT)
				{
					if (_stricmp(prev_node->operand1->value.c_str(), name.c_str()) == 0)
					{
						throw SemErr("You cannot change the value of dummy 'IN' massive");
					}
				}
				/*else if(_all_programs.get_program_by_name(sub_name)->all_massives().get_massive_by_name(name)->start_index() > stoi(current_node->operand2->value) ||
					    _all_programs.get_program_by_name(sub_name)->all_massives().get_massive_by_name(name)->stop_index() < stoi(current_node->operand2->value))
				{
					throw SemErr("You RUNAWAY beyond the array");
				}*/
			}
			else
			{
				throw SemErr("You cannot work with undeclarated MASSIVE");
			}
			return;
		}

		else if (current_node->type == node_type::CALL_NODE)
		{
			string name = current_node->operand1->value;
			if (!_all_programs.is_clear())
			{
				if (!_all_programs.has_program(name))
				{

					throw SemErr("You cannot CALL undeclarated SUBROUTINE");
				}
				else if (_all_programs.get_program_by_name(name)->type() == program_type::PROGRAM)
				{
					throw SemErr("You cannot CALL PROGRAM");
				}
				else if (status == node_type::SUBROUTINE_DECL && _stricmp(name.c_str(), sub_name.c_str()) == 0)
				{
					throw SemErr("You cannot CALL NON RECURSIVE SUBROUTINE inside the same SUBROUTINE");
				}
			}
			else
			{
				throw SemErr("You cannot CALL undeclarated SUBROUTINE");
			}
			return;
		}

		else if (current_node->type == node_type::ID)
		{
			string name = current_node->value;
			if (!_all_programs.get_program_by_name(sub_name)->all_variables().is_clear())
			{
				if (!_all_programs.get_program_by_name(sub_name)->all_variables().has_variable(name) && prev_node->type != node_type::MAS_ELEM)
				{
					throw SemErr("You cannot work with undeclarated VARIABLE");
				}
				else if((_stricmp(_all_programs.get_program_by_name(sub_name)->all_variables().get_variable_by_name(name)->attribute().c_str(), "PARAMETER") == 0 ||


					_stricmp(_all_programs.get_program_by_name(sub_name)->all_variables().get_variable_by_name(name)->attribute().c_str(), "IN") == 0) &&
					     prev_node->type == node_type::ASSIGNMENT)
				{
					if (_stricmp(prev_node->operand1->value.c_str(), name.c_str()) == 0)
					{
						throw SemErr("You cannot change the value of dummy 'IN' or parameter variable");
					}
				}
			}
			else
			{
				throw SemErr("You cannot work with undeclarated VARIABLE");
			}
			return;
		}

		// идем в первого потомка
		check_declarations_help(current_node->operand1, status, sub_name, current_node);
		// во второго
		check_declarations_help(current_node->operand2, status, sub_name, current_node);
		// в третьего
		check_declarations_help(current_node->operand3, status, sub_name, current_node);
		// в чертвертого
		check_declarations_help(current_node->operand4, status, sub_name, current_node);
		//в пятого 
		check_declarations_help(current_node->operand5, status, sub_name, current_node);
		//в шестого 
		check_declarations_help(current_node->operand6, status, sub_name, current_node);
		//в седьмого
		check_declarations_help(current_node->operand7, status, sub_name, current_node);
	}
};




class parser
{
private:
	// лексер
	Lex* _lex;


public:
	// AST
	ast* _ast;

	// конструктор
	parser(const char* filename)
	{
		_lex = new Lex(filename);
		// инициализируем AST
		_ast = new ast();
	}
	//~parser()
	//{
	//	// удаляем
	//	delete _lex;
	//	delete _ast;
	//}

	node* full_program(); //[<func_decl_list>] <program_body>[<func_decl_list>]
	node* program_body(); //( [“program” [<ID>]] | [”program”]); <var_decl_list> <statements> “end”[“program”[<ID>]]
	node* func_call(); //<ID> “(” <expression> “)”
	node* func_decl_list(); //<func_decl> | <func_delc>	<func_decl_list>
	node* func_decl();  //[<type>] [<attribute>] ”function” <ID> “(”{ <func_arg> } “)”[“result” ”(” <ID> “)”] <func_body> “end” “function”[<ID>]
	node* func_arg(); //<ID> | <ID> “,” <func_arg>
	node* func_body(); //<var_decl_list><statements>

	node* sub_var_list(int situation);//<sub_var_decl>:: = <type>[“, ”(“PARAMETER” | “INTENT” “(”(“IN” | “OUT” | “IN” “OUT”) “)”)] ”:” ” : ”(<ID> | <assignment>) { (<ID> | <assignment >) } | <type> <ID_list>
	node* sub_mas_decl();//<sub_mas_decl>:: = <type> “, ”(“DIMENSION” “(” <integer_literal>[“:” <integer_literal>] “)” | “INTENT” “(”(“IN” | “OUT” | “IN” “OUT”) “)”)[“, ”(“DIMENSION” “(” <integer_literal>[“:” <integer_literal>] “)” | “INTENT” “(”(“IN” | “OUT” | “IN” “OUT”) “)”)] “:” ” : ”(<ID> | <mas_init>) { (<ID> | <mas_init>) }
	node* sub_mas_list(int mass_size, int init_work);  //Для построения дерева, где в 1 ноде будет mas, а во второй будет mas_list
	node* sub_decl_list();//<sub_decl_list>:: = (<sub_var_decl> | <sub_mas_decl>) | (<sub_var_decl> | <sub_mas_decl>) < sub_decl_list >
	node* sub_var_decl();//Для построения дерева, где в 1 ноде будет mas, а во второй будет mas_list
	node* subroutine_decl_list();  // <subroutine_decl> | <subroutine_decl> <subroutine_decl_list>
	node* subroutine_call();//<subroutine_call> :: = “call” <ID> “(” <expression> { “, ” <expression> } “)”
	node* subroutine_call_arg();  //фактические аргументы вызова функции
	node* subroutine_decl();//<subroutine_decl> :: = [“RECURSIVE”] “SUBROUTINE” <ID> “(” <subroutine_arg> “)” <subroutine_body> “END” “SUBROUTINE”[<ID>]
	node* subroutine_body();//<subroutine_body> :: = < sub_decl_list><statements>
	node* subroutine_arg();//<ID> | <ID> “, ” <subroutine_arg>


	node* decl_list(); //<var_decl> | <var_decl> <var_decl_list>
	node* mas_decl();  //<type> “,” “DIMENSION” “(” <integer_literal> [ “:” <integer_literal> ] “)” “:” ”:” (<ID> | <mas_init>) {(<ID> | <mas_init>)}
	node* mas_list(int mass_size);//Для построения дерева, где в 1 ноде будет mas, а во второй будет mas_list
	node* mas_init(int mass_size, int elem_counter=0); //Для потроения дерева инициализации массива, где в 1 ноде записано значение элемента массива по порядку, а во второй mas_init
	node* var_decl(); //<type>[“,” "PARAMETER"] ”:” ”:” (<ID> |	<attribute>) {(<ID> | <attribute>)} | <type> <ID_list>
	//Ситуации: 
	// 0 - объявление без атрибутов и без инициализации и без двоеточий
	// 1 - объявление без атрибутов и с инициализацией
	// 2 - объявление с атрибутом PARAMETER и обязательной инициализацией 
	// 3 - объявление с атрибутом INTEND без инициализации
	node* var_list(int situation); //Для построения дерева, где в 1 ноде будет var, а во второй будет var_list
	node* type(); //“INTEGER” [( “(” ”2” ”)” | “(” ”4” “)” )] |	“REAL”[(“(” ”4” ”)” | “(” ”8” “)”)] | “DOBLE””PRECISION” | “LOGICAL”[(“(” ”2” ”)” |	“(” ”4” “)”)] | “CHARACTER”[“(” “<integer_literal>”“)”]
	node* expression(); //<log_exp>
	node* log_eqv_exp();  //<log_or_exp> | <log_or_exp> <log_eqv_op> <log_eqv_exp>
	node* log_or_exp();  //<log_and_exp> | <log_and_exp> “.OR.” <log_or_exp>
	node* log_and_exp();  //<log_not_exp> | <log_not_exp> “.AND.” <log_and_exp>
	node* log_not_exp(); //[“.NOT.”]<rel_exp>
	node* rel_exp(); //<add_exp> | <add_exp> <rel_op> <add_exp>
	node* add_exp(); //<mult_exp> | <mult_exp> <add_op> <add_exp>
	node* mult_exp(); //<power_exp> | <power_exp> <mult_op> <mult_exp>
	node* power_exp(); //<unar_exp> | <unar_exp> ”**” <primary_exp>
	node* unar_exp(); //<unar_op><primary_exp>
	node* primary_exp(); //”(”<expression>”)” | <literal> | <ID> |	<func_call> | <ID> ”(”<integer_literal>“)”
	node* sqrt(); //функция взятия корня
	node* mas_elem(); //В выражениях будет создавать узел, где будет в первом ребёнке отображаться имя массива и во втором ребёнке номер ячейки массива
	node* literal(); //<logical_literal> | <real_literal> | <integer_literal>
	node* logical_literal(); // <digit>{<digit>} [ “.”<digit>{<digit>}]
	node* real_literal(); // <digit>{<digit>} [ “.”<digit>{<digit>}]
	node* integer_literal();  // <digit>{ <digit> }
	node* statements(node_type parent = PROGRAM_BODY); //{<statement>(“\n”|[“;”])}
	node* statement(node_type parent = PROGRAM_BODY); //<assignment> | <if_condition> | “stop” | <input_read> | <output_write> | <output_print> | “cycle” | “continue” | (“exit”[<ID>])
	node* assignment(); //( <ID> | <ID> ”(” <integer_literal> “)” ) ” = ” <expression>
	node* do_cycle(); //[<ID> ”:”] “do”[<expression> ”,” <expression>[“, ” <expression>]] <statements> “end” ”do”[<ID>]
	node* do_while_cycle(); //[<ID> ”:”] “do” “while” “(”<expression>”)” <statements> “end” ”do”[<ID>]
	node* output_print(); //”print”“*”{“,”<expression>}
	node* output_write(); //”write” ”(” “*” “,” “*” ”)” [<expression>]{ “,”<expression> }
	node* input_read(); //”read” ”(” “*” “,” “*” ”)” ID{“,”<ID>}
	node* input_list(); //Список переменных, констант и элементов массива, которые нужно ввести
	node* output_list(); //Список выражений, констант и т.п. что нужно вывести
	node* if_condition(); //”if” ”(”<expression>”)” “then” <statements>[<elseif>][“else”<statements>]
	node* elseif(); //”else” ”if” ”(”<expression>”)” “then” <statements> | ”else” ”if” ”(”<expression>”)” “then” <statements> <elseif>
	node* number(); //int or double
	node* identifier_list(); //id{"," id}
	node* identifier(); //id
	void parse();
	
};