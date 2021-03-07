#include<map>
#include<iostream>
#include<string> 
#include<fstream>
#include<vector>

using namespace std;
//Класс лексического анализатора
typedef enum Statement {
	S0, S1, S2, S3, S4, S5,
	S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19, S20, S21, S22, SE
}; // Набор состояний

typedef enum Output {
	O0, O1, O2, O3, O4, O5, O6,
	O7, O8, O9, O10, O11, O12, O13, O14, O15, O16, O17, O18, O19, O20, O21, O22, OE
}; // Набор выходных сигналов

typedef enum TokenType {
	SPACE, IDENT, INT, FLOAT, PLUS, MINUS, MULTIPLE, POW, DIVISION, NOTEQ, EQUAL, EQEQ, LESS, LESSEQ, MORE, MOREEQ, COMMA, SEPARATOR, DDOT, LPAR, RPAR, NOT, AND, OR, XOR, EQV, NEQV, 
	TRUE, FALSE, EQ, NE, GT, GE, LT, LE, CYCLE, DIMENSION, PROGRAM, RECURSIVE, STOP, THEN, WHILE, DO, FUNCTION, PRINT, READ, WRITE, CHARACTER, DOUBLE, 
	EXIT, PARAMETER, PRECISION, REAL, CONTINUE, ELSE, END, IF, INTEGER, LOGICAL, RESULT, NUL, SUBROUTINE, IN, OUT, CALL, RETURN, SQRT, INTENT
}; //Типы лексем

typedef pair<Statement, Output> Result; // Пара(Новое Состояние& Выходной сигнал)
typedef pair<Statement, char> Input;  // Составной ключ(Текущее Состояние& Входной символ) 
typedef pair<Input, Result> Item; //Элемент таб-лицы переходов
extern map<Input, Result> M; // Таблица переходов КА

//Класс-исключение ошибки лексического анализатора
class LexErr {
public:
	inline LexErr(string str)
	{
		cout << "Ошибка лексического анализатора: " << str << endl;
	}
};

//Функция инициализации должна заполнить таблицу переходов конечного автомата
void Init(void);

//Класс"Лексема" 
class Token {
public:
	string _value; // Строковое значение
	TokenType _type; //Тип лексемы
	Token()
	{
		this->_value = "";
	}
	Token(const string& lexeme)
	{
		this->_value = lexeme;
	}
};

//Класс лексического анализатора
class Lex {
public:
	vector <Token> _tokens;
	int _current_token_index;

	void next_token()
	{
		// проверяем, чтобы не выйти за пределы массива токенов
		if (_current_token_index < _tokens.size() - 1)
		{
			++_current_token_index;
		}
	}

	void prev_token()
	{
		// проверяем, чтобы не выйти за пределы массива токенов
		if (_current_token_index > 0)
		{
			--_current_token_index;
		}
	}

	TokenType current_token_type()
	{
		return _tokens[_current_token_index]._type;
	}

	Token current_token()
	{
		return _tokens[_current_token_index];
	}

	Lex(const char*fname); //Конструктор
	~Lex();  //Деструктор
	bool GetToken(Token &token); //Функция получе-ния очередной лексемы
	void split();
	void print();
private:
	ifstream  _file; //Файловый поток для чтения символов входной цепочки
};