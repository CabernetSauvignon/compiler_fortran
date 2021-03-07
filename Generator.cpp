#include "Generator.h"


void Generator::Int_Real_recursive(node* current_node, size_t level)
{
	if (current_node == nullptr || current_node->type == node_type::DO_WHILE_CYCLE || current_node->type == node_type::IF_CONDITION || current_node->type == node_type::ELSEIF || current_node->type == node_type::CALL_NODE)
		return;
	if (current_node->type == node_type::INTEGER_LITERAL)
	{
		_Int_Real = 1;
		return;
	}
	else if (current_node->type == node_type::REAL_LITERAL)
	{
		_Int_Real = 0;
		return;
	}
	if (current_node->type == node_type::MAS_ELEM)
	{
		if (this->Pars._ast->_all_programs.get_massive_from_table(_name_of_func, current_node->operand1->value)->type() == variable_type::INTEGER)
			_Int_Real = 1;
		else 
			_Int_Real = 0;
		return;
	}
	if (current_node->type == node_type::ID)
	{
		if (this->Pars._ast->_all_programs.get_var_from_table(_name_of_func, current_node->value)->type() == variable_type::INTEGER)
			_Int_Real = 1;
		else
			_Int_Real = 0;
		return;
	}
	
	Int_Real_recursive(current_node->operand1, level + 1);
	Int_Real_recursive(current_node->operand2, level + 1);
	Int_Real_recursive(current_node->operand3, level + 1);
	Int_Real_recursive(current_node->operand4, level + 1);
	Int_Real_recursive(current_node->operand5, level + 1);
	Int_Real_recursive(current_node->operand6, level + 1);
	Int_Real_recursive(current_node->operand7, level + 1);

	return;
}

int Generator::Else_If_Count_recursive(node* current_node, size_t level, int count, int flag)
{
	if (current_node == nullptr)
		return count;
	int tmp = count;
	if (current_node->type == node_type::ELSEIF && flag != 0)
		count++;
	if (flag == 0)
		flag = 1;

	count += Else_If_Count_recursive(current_node->operand1, level + 1, tmp, flag);
	count += Else_If_Count_recursive(current_node->operand2, level + 1, tmp, flag);
	count += Else_If_Count_recursive(current_node->operand3, level + 1, tmp, flag);
	count += Else_If_Count_recursive(current_node->operand4, level + 1, tmp, flag);
	count += Else_If_Count_recursive(current_node->operand5, level + 1, tmp, flag);
	count += Else_If_Count_recursive(current_node->operand6, level + 1, tmp, flag);
	count += Else_If_Count_recursive(current_node->operand7, level + 1, tmp, flag);

	return count;
}

void Generator::Generate_recursive(node* current_node, size_t level)
{
	int tmp_counter;
	if (current_node == nullptr)
		return;
	if (current_node->type == node_type::PROGRAM_BODY)
	{
		_Init_Vars = "";
		_Code += "main PROC\n";
		_name_of_func = "main";
	}
	else if (current_node->type == node_type::SUBROUTINE_DECL || current_node->type == node_type::SUBROUTINE_REC)
	{

		_name_of_func = current_node->operand2->value;
		_Code += current_node->operand2->value + " proc ";
		if (current_node->operand3 != nullptr)
		{
			node* tmp = current_node->operand3;
			while (tmp != nullptr)
			{
				_Code += tmp->value + ":";
				if (Pars._ast->_all_programs.get_var_from_table(_name_of_func, tmp->value)->type() == variable_type::INTEGER)
					_Code += "dword";
				else 
					_Code += "real8";
				if (tmp->operand1 != nullptr)
					_Code += ", ";
				tmp = tmp->operand1;
			}
			
		}
		_Code += "\n";
	}
	else if (current_node->type == node_type::MAS_DECL)
	{
		string _pop = "";
		string _push = "";
		if (_Code.find(_Init_Vars))
		{
			_Code = _Code.substr(0, _Code.length() - _Init_Vars.length());
		}
		string step;
		_Code += "\tLOCAL " + current_node->operand5->operand1->operand1->value + "[" + current_node->operand4->value + "]: ";
		if (current_node->operand1->value == "INTEGER")
		{
			_pop = "\tpop ";
			_push = "\tpush ";
			_Code += "dword\n";
		}
		else if (current_node->operand1->value == "REAL")
		{
			_pop = "\tfstp ";
			_push = "\tfld FP8(";
			_Code += "real8\n";
		}
		if (current_node->operand5->operand1->operand2 != nullptr)
		{
			node* tmp = current_node->operand5->operand1->operand2;
			while (tmp != nullptr)
			{
				_Init_Vars += _push + tmp->operand1->value;
				if(current_node->operand1->value == "REAL")
					_Init_Vars += ")";
				_Init_Vars += "\n";
				tmp = tmp->operand2;
			}
			
			for (int i = atoi(current_node->operand4->value.c_str()) - 1; i >= 0; i--)
			{

				_Init_Vars += _pop + current_node->operand5->operand1->operand1->value + "[" + to_string(i);
				if (current_node->operand1->value == "INTEGER")
					_Init_Vars += "*4]\n";
				else if (current_node->operand1->value == "REAL")
					_Init_Vars += "*8]\n";
			}
		}
		_Code += _Init_Vars;

	}
	else if (current_node->type == node_type::CALL_NODE)
	{
		string str1 = "";
		string str2 = "";
		string strtmp = "";
		node* tmp = current_node->operand2;
		while (tmp != nullptr)
		{
			
			if (tmp->operand1 != nullptr)
			{
				if (tmp->operand1->type == node_type::MAS_ELEM)
				{
					Int_Real_recursive(tmp->operand1, level);
					if(_Int_Real)
						str2 += "\tmov eax, " + current_node->operand1->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
					else
						str2 += "\tmov eax, " + current_node->operand1->operand2->value + "\n\tmov ebx, 8\n\tmul ebx\n\tpush eax\n\tpop esi\n";
					str2 = "\tpush " + tmp->operand1->operand1->value + "[esi]";
				}
				else 
					str2 = "\tpush " + tmp->operand1->value + "\n";
				strtmp = str1;
				str1 = str2;	
				str1 += strtmp;
			}
			tmp = tmp->operand2;
		}
		_Code += str1;
		_Code += "\tcall " + current_node->operand1->value + "\n";
	}
	else if (current_node->type == node_type::STATEMENTS)
	{
		Int_Real_recursive(current_node, level);
	}
	else if (current_node->type == node_type::VAR_DECL)
	{
		if (_Code.find(_Init_Vars))
		{
			_Code = _Code.substr(0, _Code.length() - _Init_Vars.length());
		}
		string _type = current_node->operand1->value;
		node* temp = current_node->operand3;
		while (temp != nullptr)
		{
			if (temp->operand1->operand1->type == node_type::ID)
			{
				_Code += "\tLOCAL " + temp->operand1->operand1->value + ": ";
				if (_type == "REAL")
				{
					_Code += "real8\n";
				}
				else if (_type == "INTEGER")
				{
					_Code += "dword\n";
				}
				if (temp->operand1->operand2 != nullptr)
				{
					if (_type == "REAL")
					{
						if (temp->operand1->operand2->operand2->type == node_type::REAL_LITERAL)
							_Init_Vars += "\tfld FP8(" + temp->operand1->operand2->operand2->value + ")\n\tfstp " + temp->operand1->operand1->value + "\n";
						else if(temp->operand1->operand2->operand2->type == node_type::ID)
							_Init_Vars += "\tfld " + temp->operand1->operand2->operand2->value + "\n\tfstp " + temp->operand1->operand1->value + "\n";
					}
					else if (_type == "INTEGER")
					{
						if (temp->operand1->operand1->type == node_type::INTEGER_LITERAL)
							_Init_Vars += "\tpush " + temp->operand1->operand2->operand2->value + "\n\tpop " + temp->operand1->operand1->value + "\n";
						else if (temp->operand1->operand1->type == node_type::ID)
							_Init_Vars += "\tpush " + temp->operand1->operand2->operand2->value + "\n\tpop " + temp->operand1->operand1->value + "\n";
					}
				}
				
			}
			temp = temp->operand2;
		}
		_Code += _Init_Vars;
		return;
	}
	else if (current_node->type == node_type::MULT_EXP || current_node->type == node_type::ADD_EXP)
	{
		Int_Real_recursive(current_node, level);
		if (!_Int_Real)
		{
			if (current_node->operand1->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand1->operand2->value + "\n\tmov ebx, 8\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tfld " + current_node->operand1->operand1->value + "[esi]\n";
			}
			else if (current_node->operand1->type == node_type::ID)
				_Code += "\tfld " + current_node->operand1->value + "\n";
			else if (current_node->operand1->type == node_type::REAL_LITERAL)
				_Code += "\tfld FP8(" + current_node->operand1->value + ")\n";

			/*if (current_node->operand2->type == node_type::MAS_ELEM)
				_Code += "\tfld " + current_node->operand2->operand1->value + "[" + current_node->operand2->operand2->value + "*8]\n";
			else if (current_node->operand2->type == node_type::ID)
				_Code += "\tfld " + current_node->operand2->value + "\n";
			else if (current_node->operand2->type == node_type::REAL_LITERAL)
				_Code += "\tfld FP8(" + current_node->operand2->value + ")\n";*/
		}
		else
		{
			;//same
		}
	}
	else if (current_node->type == node_type::UNAR_EXP)
	{
		if (!_Int_Real)
		{
			if (current_node->operand2->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand2->operand2->value + "\n\tmov ebx, 8\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tfld " + current_node->operand2->operand1->value + "[esi]\n";
			}
			if (current_node->operand2->type == node_type::ID)
				_Code += "\tfld " + current_node->operand2->value + "\n";
		}
		else
		{
			;//вроде только в конце что то нужно делать
		}
	}
	else if (current_node->type == node_type::SQRT_EXP)
	{
		if (!_Int_Real)
		{
			if (current_node->operand1->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand1->operand2->value + "\n\tmov ebx, 8\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tfld " + current_node->operand1->operand1->value + "[esi]\n";
			}
			else if (current_node->operand1->type == node_type::ID)
				_Code += "\tfld " + current_node->operand1->value + "\n";
		}
		else throw ParseErr("lulz");
	}
	else if (current_node->type == node_type::OUTPUT_WRITE)
	{
		_Code += "\tprintf(\"";
		node* tmp = current_node->operand1;
		string tmpstr;
		while (tmp != nullptr)
		{
			if (tmp->operand1->type == node_type::MAS_ELEM)
			{
				Int_Real_recursive(tmp->operand1, level);
				if (tmp->operand2 != nullptr)
				{
					if (!_Int_Real)
						_Code += "%g ";
					else
						_Code += "%d ";
					tmpstr += tmp->operand1->operand1->value + "[" + tmp->operand1->operand2->value;
					if (_Int_Real)
						tmpstr += "*4]";		
					else
						tmpstr += "*8]";
					tmpstr += ", ";
				}
				else
				{
					if (!_Int_Real)
						_Code += "%g";
					else
						_Code += "%d";
					tmpstr += tmp->operand1->operand1->value + "[" + tmp->operand1->operand2->value;
					if (_Int_Real)
						tmpstr += "*4]";
					else
						tmpstr += "*8]";
				}
			}
			else if (tmp->operand1->type == node_type::ID || tmp->operand1->type == node_type::INTEGER_LITERAL || tmp->operand1->type == node_type::REAL_LITERAL)
			{
				Int_Real_recursive(tmp->operand1, level);
				if (tmp->operand2 == nullptr)
				{
					if (!_Int_Real)
						_Code += "%g";
					else
						_Code += "%d";
					tmpstr += tmp->operand1->value;
				}
				else
				{
					if (!_Int_Real)
						_Code += "%g ";
					else
						_Code += "%d ";
					tmpstr += tmp->operand1->value + ", ";
				}
			}
			tmp = tmp->operand2;
		}
		_Code += "\\n\", " + tmpstr + ")\n";
	}
	else if (current_node->type == node_type::DO_WHILE_CYCLE)
	{  //здесь добавить если элемент массива выводить его как эл-т массива
		tmp_counter = _M_count;
		_Code += "M" + std::to_string(tmp_counter) +"@:\n";

		_Code += "\tpush ";
		if (current_node->operand1->operand1->type == node_type::MAS_ELEM)
		{
			_Code += "\tmov eax, " + current_node->operand1->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
			_Code += current_node->operand1->operand1->operand1->value + "[esi]\n";
		}
		else
			_Code += current_node->operand1->operand1->value + "\n";

		_Code += "\tpush ";
		if (current_node->operand1->operand2->type == node_type::MAS_ELEM)
		{
			_Code += "\tmov eax, " + current_node->operand2->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
			_Code += current_node->operand1->operand2->operand1->value + "[esi]\n";
		}
		else
			_Code += current_node->operand1->operand2->value + "\n";

		_Code += "\tpop ebx\n\tpop eax\n\tcmp eax, ebx\n\t";
		if (current_node->operand1->value == "<")
		{
			_Code += "jge ";
		}
		else if (current_node->operand1->value == "<=")
		{
			_Code += "jg ";
		}
		else if (current_node->operand1->value == ">")
		{
			_Code += "jle ";
		}
		else if (current_node->operand1->value == ">=")
		{
			_Code += "jl ";
		}
		else
			throw ParseErr("Do while error\n");
		_Code += "M" + std::to_string(tmp_counter + 1) + "@\n\n";
		_M_count += 2;
	}
	else if (current_node->type == node_type::IF_CONDITION || current_node->type == node_type::ELSEIF)
	{//здесь добавить если элемент массива выводить его как эл-т массива
		tmp_counter = _M_count;
		
		if (current_node->operand1->operand1->type == node_type::MAS_ELEM)
		{
			_Code += "\tmov eax, " + current_node->operand1->operand1->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
			_Code += "\tpush ";
			_Code += current_node->operand1->operand1->operand1->value + "[esi]\n";
		}
		else
		{
			_Code += "\tpush ";
			_Code += current_node->operand1->operand1->value + "\n";
		}
		
		
		if (current_node->operand1->operand2->type == node_type::MAS_ELEM)
		{
			_Code += "\tmov eax, " + current_node->operand1->operand2->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
			_Code += "\tpush ";
			_Code += current_node->operand1->operand2->operand1->value + "[esi]\n";
		}
		else
		{
			_Code += "\tpush ";
			_Code += current_node->operand1->operand2->value + "\n";
		}
		
		_Code += "\tpop ebx\n\tpop eax\n\tcmp eax, ebx\n\t";
		if (current_node->operand1->value == "==")
		{
			_Code += "je ";
		}
		else if (current_node->operand1->value == "/=")
		{
			_Code += "jne ";
		}
		else if (current_node->operand1->value == "<")
		{
			_Code += "jl ";
		}
		else if (current_node->operand1->value == "<=")
		{
			_Code += "jle ";
		}
		else if (current_node->operand1->value == ">")
		{
			_Code += "jg ";
		}
		else if (current_node->operand1->value == ">=")
		{
			_Code += "jge ";
		}
		_Code += "M" + std::to_string(tmp_counter) + "@\n";
		_Code += "\tjmp M" + std::to_string(tmp_counter + 1) + "@\n\n";
		_M_count += 2;
	}
	


	// выводим первого потомка
	Generate_recursive(current_node->operand1, level + 1);
	
	if (current_node->type == node_type::IF_CONDITION || current_node->type == node_type::ELSEIF)
	{
		_Code += "M" + std::to_string(tmp_counter) + "@:\n";
	}
	// выводим второго потомка
	Generate_recursive(current_node->operand2, level + 1);
	if (current_node->type == node_type::IF_CONDITION || current_node->type == node_type::ELSEIF)
	{
		_Code += "\tjmp ";
		_Code += "M" + std::to_string(2 * (Else_If_Count_recursive(current_node, level, 0, 0) + 1) + tmp_counter) + "@\n\n";
		_Code += "M" + std::to_string(tmp_counter + 1) + "@:\n";
	}
	else if (current_node->type == node_type::MULT_EXP || current_node->type == node_type::ADD_EXP)
	{
		Int_Real_recursive(current_node, level);
		if (!_Int_Real)
		{
			if (current_node->operand2->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand2->operand2->value + "\n\tmov ebx, 8\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tfld " + current_node->operand2->operand1->value + "[esi]\n";
			}
			else if (current_node->operand2->type == node_type::ID)
				_Code += "\tfld " + current_node->operand2->value + "\n";
			else if (current_node->operand2->type == node_type::REAL_LITERAL)
				_Code += "\tfld FP8(" + current_node->operand2->value + ")\n";
		}
	}
	// выводим тетьего потомка
	Generate_recursive(current_node->operand3, level + 1);
	// выводим четвертого потомка
	Generate_recursive(current_node->operand4, level + 1);
	// выводим пятого потомка
	Generate_recursive(current_node->operand5, level + 1);
	// выводим шестого потомка
	Generate_recursive(current_node->operand6, level + 1);
	// выводим седьмого потомка
	Generate_recursive(current_node->operand7, level + 1);
	if (current_node->type == node_type::PROGRAM_BODY)
	{
		_Code += "\tinkey\n\tcall ExitProcess\nmain ENDP\nEND main\n\n";
	}
	else if (current_node->type == node_type::SUBROUTINE_DECL || current_node->type == node_type::SUBROUTINE_REC)
	{
		node* tmp = current_node->operand4->operand1;
		while (tmp != nullptr)
		{
			if (tmp->operand1 != nullptr)
			{
				if (tmp->operand1->operand2->value == "OUT")
					_Code += "\tmov eax, " + tmp->operand1->operand3->operand1->operand1->value + "\n";
			}
			tmp = tmp->operand2;
		}
		_Code += "\tret\n" + current_node->operand2->value + " endp\n\n";
	}
	else if (current_node->type == node_type::CALL_NODE)
	{
		
	}
	else if (current_node->type == node_type::MULT_EXP)
	{
		if (_Int_Real)
		{
			if (current_node->operand1->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand1->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tmov eax, " + current_node->operand1->operand1->value + "[esi]\n";
			}
			else if (current_node->operand1->type == node_type::ID || current_node->operand1->type == node_type::INTEGER_LITERAL)
				_Code += "\tmov eax, " + current_node->operand1->value + "\n";
			else
				_Code += "\tpop eax\n";
			
			if (current_node->operand2->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand2->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tmov ebx, " + current_node->operand2->operand1->value + "[esi]\n";
			}
			else if (current_node->operand2->type == node_type::ID || current_node->operand2->type == node_type::INTEGER_LITERAL)
				_Code += "\tmov ebx, " + current_node->operand2->value + "\n";
			else
				_Code += "\tpop ebx\n";

			if (current_node->value == "*")
				_Code += "\tmul ebx\n\tpush eax\n";
			else 
				_Code += "\tdiv ebx\n\tpush eax\n";
		}
		else
		{
			if (current_node->value == "*")
				_Code += "\tfmul\n";
			else
				_Code += "\tfdiv\n";
		}
	}
	else if (current_node->type == node_type::ADD_EXP)
	{
		if (_Int_Real)
		{
			if (current_node->operand1->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand1->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tmov eax, " + current_node->operand1->operand1->value + "[esi]\n";
			}
			else if (current_node->operand1->type == node_type::ID || current_node->operand1->type == node_type::INTEGER_LITERAL)
				_Code += "\tmov eax, " + current_node->operand1->value + "\n";
			else
				_Code += "\tpop eax\n";
			
			if (current_node->operand2->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand2->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tmov ebx, " + current_node->operand2->operand1->value + "[esi]\n";
			}
			else if (current_node->operand2->type == node_type::ID || current_node->operand2->type == node_type::INTEGER_LITERAL)
				_Code += "\tmov ebx, " + current_node->operand2->value + "\n";
			else
				_Code += "\tpop ebx\n";

			if (current_node->value == "+")
				_Code += "\tadd eax, ebx\n\tpush eax\n";
			else
				_Code += "\tsub eax, ebx\n\tpush eax\n";
		}
		else
		{
			if (current_node->value == "-")
				_Code += "\tfsub\n";
			else
				_Code += "\tfadd\n";
		}
	}
	else if (current_node->type == node_type::ASSIGNMENT)
	{
		if (_Int_Real)
		{
			if (current_node->operand2->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand2->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tpush " + current_node->operand2->operand1->value + "[esi]\n";
				
			}
			else if (current_node->operand2->type == node_type::ID || current_node->operand2->type == node_type::INTEGER_LITERAL)
				_Code += "\tpush " + current_node->operand2->value + "\n";

			if (current_node->operand1->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand1->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tpop " + current_node->operand1->operand1->value + "[esi]\n";
			}
			else 
				_Code += "\tpop " + current_node->operand1->value + "\n";
		}
		else
		{
			if (current_node->operand2->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand2->operand2->value + "\n\tmov ebx, 8\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tfld " + current_node->operand1->operand1->value + "[esi]\n";
			}
			else if (current_node->operand2->type == node_type::ID || current_node->operand2->type == node_type::REAL_LITERAL)
				_Code += "\tfld  " + current_node->operand1->value + "\n";

			if (current_node->operand1->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand1->operand2->value + "\n\tmov ebx, 8\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tfstp " + current_node->operand1->operand1->value + "[esi]\n";
			}
			else
				_Code += "\tfstp " + current_node->operand1->value + "\n";
		}
	}
	else if (current_node->type == node_type::UNAR_EXP)
	{
		if (!_Int_Real)
			_Code += "\tfld FP8(-1.0)\n\tfmul\n";
		else
		{
			_Code += "\tmov eax, -1";
			if (current_node->operand1->type == node_type::MAS_ELEM)
			{
				_Code += "\tmov eax, " + current_node->operand1->operand2->value + "\n\tmov ebx, 4\n\tmul ebx\n\tpush eax\n\tpop esi\n";
				_Code += "\tmov ebx, " + current_node->operand1->operand1->value + "[esi]\n";
			}
			if (current_node->operand1->type == node_type::ID || current_node->operand1->type == node_type::INTEGER_LITERAL)
				_Code += "\tmov ebx, " + current_node->operand1->value + "\n";
			else
				_Code += "\tpop ebx\n";
			_Code += "\tmul ebx\n\tpush eax\n";
		}
	}
	else if (current_node->type == node_type::SQRT_EXP)
	{
		_Code += "\tfsqrt\n";
	}
	else if (current_node->type == node_type::DO_WHILE_CYCLE)
	{
		_Code += "\tjmp M" + std::to_string(tmp_counter) + "@\nM" + std::to_string(tmp_counter + 1) + "@:\n";
	}
	else if (current_node->type == node_type::IF_CONDITION)
	{
		_Code += "M" + std::to_string(_M_count) + "@:\n";
	}



}

void Generator::MakeFileAsm(const char* filename)
{
	_M_count = 1;
	Generate_recursive(Pars._ast->tree, 0);
	time_t rawtime;
	struct tm * timeinfo;
	time(&rawtime);
	timeinfo = localtime(&rawtime);
	_Sign += asctime(timeinfo);
	_Sign += "\n*\n\n";
	ofstream F;
	F.open(filename);
	F << _Sign << _Head << _Data << _Code;
}