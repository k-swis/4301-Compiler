// David Roberts & Brett Hedden
// CS 4301
// Stage 0

#include <stage0.h>
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <ctime>
#include <cctype>
#include <stack>
#include <iomanip>

using namespace std;



// struct tableEntry {
	
   // string		setInternalName;
   // string		externalName;
   // storeTypes 	setDataType;
   // modes 		setMode;
   // string 		setValue;
   // allocation 	setAlloc;
   // int			setUnits;
// };
// vector<tableEntry> symbolTable;
// ifstream 	sourceFile;
// ofstream 	listingFile;
// ofstream 	objectFile;


// // Prototypes
// bool		isKeyword(string s); // determines if s is a keyword
// bool		isSpecialSymbol(char c); // determines if c is a special symbol
// bool		isNonKeyId(string s); // determines if s is a non_key_id
// bool		isInteger(string s) ; // determines if s is an integer
// bool		isBoolean(string s) ; // determines if s is a boolean
// bool		isLiteral(string s) ; // determines if s is a literal
// bool     isInSymbolTable(string);
// VERY  IMPIRTANT 


// void		createListingHeader();
// void		parser();
// void		createListingTrailer();
// void     LineNumber();            //Prints line numbers
// void     PrintSymbolTable();      //Prints Symbol Table to obj

// void 		prog();           	// stage 0, production 1
// void 		progStmt();       	// stage 0, production 2
// void 		consts();         	// stage 0, production 3
// void 		vars();           	// stage 0, production 4
// void 		beginEndStmt();   	// stage 0, production 5
// void 		constStmts();     	// stage 0, production 6
// void 		varStmts();       	// stage 0, production 7
// string   ids();          	   // stage 0, production 8

// // Action routines
 // void insert(string externalName, storeTypes inType, modes inMode,
              // string inValue, allocation inAlloc, int inUnits);
 // storeTypes whichType(string name); // tells which data type a name has
 // string whichValue(string name); // tells which value a name has
 // void code(string op, string operand1 = "", string operand2 = "");

 // // Emit Functions
 // void emit(string label = "", string instruction = "", string operands = "", string comment = "");
 // void emitPrologue(string progName, string = "");
 // void emitEpilogue(string = "", string = "");
 // void emitStorage();

 // Lexical routines

//this one Extremely

 // Other routines


 map<string, SymbolTableEntry>::iterator i;

  
 
  

// Functions
Compiler::Compiler(char **argv)
{
   //Open the input and output streams
   sourceFile.open(argv[1]);
   listingFile.open(argv[2]);
   objectFile.open(argv[3]);
}


Compiler::~Compiler(){   
 
 // PrintSymbolTable();
  sourceFile.close();
  listingFile.close();
  objectFile.close();
}

void Compiler::createListingHeader() {
	time_t now = time (NULL); // This returns the current calendar time of the system in number of seconds elapsed since January 1, 1970.
	listingFile << "STAGE0:" << "Brett Hedden & David Roberts " << ctime(&now) << endl;
	listingFile << "LINE NO." << setw(30) <<  "SOURCE STATEMENT" << endl << endl;
	
	//line numbers and source statements should be aligned under the headings
}
void Compiler::parser(){
	nextChar();
	//ch must be initialized to the first character of the source file
   
   
     //set charac to the first char in the file
   if(nextToken() != "program") 
   {
       //
      processError("keyword \"program\" expected");
   }
   //a call to nextToken() has two effec	// (1) the variable, token, is assigned the value of the next token
	// (2) the next token is read from the source file in order to make
	// the assignment. The value returned by nextToken() is also
	// the next token.ts

   prog();
   //parser implements the grammar rules, calling first rule
 
}
void Compiler::createListingTrailer()
{
	//print "COMPILATION TERMINATED", "# ERRORS ENCOUNTERED"
   if (errorCount == 1) {
      listingFile << "\nCOMPILATION TERMINATED      " << errorCount << " ERROR ENCOUNTERED" << endl;
   }
   else
      listingFile << "\nCOMPILATION TERMINATED      " << errorCount << " ERRORS ENCOUNTERED" << endl;
}
void Compiler::processError(string err)
{
	++errorCount;
	listingFile << endl << "Error: Line " << lineNo << ": " << err << endl;
	createListingTrailer();

	exit(1);	// exit with an error flag ON
}

///////////////////////////////////////////////////////////////////////////////
void Compiler::prog() //token should be "program"
{
	if (token != "program")
	{
		processError("keyword \"program\" expected");
	}
	progStmt();
	if (token == "const")
	{
		consts();
	}
	if (token == "var")
	{
		vars();
	}
	if (token != "begin")
	{
		processError("keyword \"begin\" expected");
	}
	beginEndStmt();
	if (token[0] != END_OF_FILE)
	{
		processError("no text may follow \"end\"");
	}
}
void Compiler::progStmt() //token should be "program"
{
	string x;
	if (token != "program")
	{
		processError("keyword \"program\" expected");
	}
	x = nextToken();
	if (!isNonKeyId(token))
	{
		processError("program name expected");
	}
	if (nextToken() != ";")
	{
		processError("semicolon expected");
	}
	nextToken();
	code("program", x);
	insert(x,PROG_NAME,CONSTANT,x,NO,0);
}
void Compiler::consts() //token should be "const"
{
	if (token != "const")
	{
		processError("keyword \"const\" expected");
	}
	if (!isNonKeyId(nextToken()))
	{
		processError("non-keyword identifier must follow \"const\"");
	}
	constStmts();
}
void Compiler::vars() //token should be "var"
{
	if (token != "var")
	{
		processError("keyword \"var\" expected");
	}
	if (!isNonKeyId(nextToken()))
	{
		processError("non-keyword identifier must follow \"var\"");
	}
	varStmts();
}
void Compiler::beginEndStmt() //token should be "begin"
{
	if (token != "begin")
	{
		processError("keyword \"begin\" expected");
	}
	if (nextToken() != "end")
	{	
      processError("keyword \"end\" expected");
	}
	if (nextToken() != ".")
	{
      processError("period expected after keyword \"end\"");
	}
	nextToken();
	code("end", ".");
}

void Compiler::constStmts()     // stage 0, production 6
{
	string x, y;

	if (!isNonKeyId(token))
		processError("non-keyword identifier expected");

	x = token;	// x has the constant's name

	// check for format x = y;
	if (nextToken() != "=")
		processError("\"=\" expected");

	y = nextToken();	// y has the value of that constant

	// if y is not one of "+","-","not",NON_KEY_ID,"true","false",INTEGER
	if (!isNonKeyId(y) && !isLiteral(y))	// y is not a number, true-false or a non-key ID
		processError("token to right of \"=\" illegal");

	if (y == "+" || y == "-")
	{
		if (!isInteger(nextToken()))
			processError("integer expected after sign");

		y += token;
	}

	else if (y == "not")
	{
		if (!isBoolean(nextToken()))	// if after not isn't "true" or "false"
			processError("boolean expected after \"not\"");

		if (token == "true")
			y = "false";
		else
			y = "true";
	}

	else if (isNonKeyId(y))	// if constant = another_constant -> search for it
	{
		// .find() will go through each key in the symbolTable map
		// if it reaches .end() -> did not find the key
		if (symbolTable.find(y) == symbolTable.end())
			processError("reference to undefined constant");
		else
		{
			if (whichType(y) == INTEGER)
				y = symbolTable.find(y)->second.getValue();
			else	// type of constant y is BOOLEAN
			{
				if (symbolTable.find(y)->second.getValue() == "0")
					y = "false";
				else
					y = "true";
			}
		}
	}

	// check for format: x = y;
	if (nextToken() != ";")
		processError("semicolon expected");

	if (whichType(y) != INTEGER && whichType(y) != BOOLEAN)
		processError("data type of token on the right-hand side must be INTEGER or BOOLEAN");

	insert(x, whichType(y), CONSTANT, whichValue(y), YES, 1);

	x = nextToken();

	// if after a constant declaration is another non_key_id or "var" or "begin"
	if (x != "begin" && x != "var" && !isNonKeyId(x))
		processError("non-keyword identifier, \"begin\", or \"var\" expected");

	if (isNonKeyId(x))
		constStmts();	// call it again to insert another constant

 
}

void Compiler::varStmts() //token should be NON_KEY_ID
{
  
	string x,y;
	if (!isNonKeyId(token))
	{
		processError("non-keyword identifier expected");
	}
	x = ids();
   
	if (token != ":")
	{
		processError("\":\" expected");
	}
	if (nextToken() != "integer" && token != "boolean")
	{
		processError("illegal type follows \":\"");
	}
	y = token;
   
	if (nextToken() != ";")
	{
		processError("semicolon expected");
	}
	insert(x,y == "integer"? INTEGER:BOOLEAN ,VARIABLE,"",YES,1);
   
	if (nextToken() != "begin"&& !isNonKeyId(token))
	{
		processError("non-keyword identifier or \"begin\" expected");
	}
	if (isNonKeyId(token))
	{
		varStmts();
	}
   
}	
string Compiler::ids() //token should be NON_KEY_ID
{
	string temp,tempString;
	if (!isNonKeyId(token))
	{
		processError("non-keyword identifier expected");
	}
	tempString = token;
	temp = token;
	if (nextToken() == ",")
	{
		if (!isNonKeyId(nextToken()))
		{
			processError("non-keyword identifier expected");
		}
		tempString = temp + "," + ids();
	}
	return tempString;
}
///////////////////////////////////////////////////////////////////////////////


string Compiler::genInternalName(storeTypes stype) const{
	static int boolctr = 0;
	static int intctr = 0;
	
	string internName;
	
	switch(stype) {
		case PROG_NAME: 
			internName = "P0";
			break;
		case INTEGER:
			internName =  "I" + to_string(intctr);
			intctr++;
			break;
		case BOOLEAN:
			internName = "B" + to_string(boolctr);
			boolctr++;
			break;
	}
	
	return internName;
}

void Compiler::insert(string externalName, storeTypes inType, modes inMode, string inValue, allocation inAlloc, int inUnits)
{
	uint i = 0;
	while (i < externalName.length())
	{
		string name = "";

		while (i < externalName.length() && externalName[i] != ',')
		{
			name += externalName[i];
			i++;
		}
		i++;	// go to the next character after ','

		if (name != "")
		{
			// we can only have max of 15 chars in table
			name = name.substr(0, 15);

			//symbolTable[name] is defined
			if (symbolTable.find(name) != symbolTable.end())
				processError("symbol " + name + " is multiply defined");
			else if (isKeyword(name))
				processError("illegal use of keyword");
			//create table entry
			else
			{
				if (isupper(name[0]))
					symbolTable.insert({ name, SymbolTableEntry(name, inType, inMode, inValue, inAlloc, inUnits) });
				else
					symbolTable.insert({ name, SymbolTableEntry(genInternalName(inType), inType, inMode, inValue, inAlloc, inUnits) });
			}
		}
	}

	if (symbolTable.size() > 256)
		processError("symbol table overflow -- max 256 entries");
}


storeTypes Compiler::whichType(string name) {
   //tells which data type a name has
   storeTypes type;
   if (isLiteral(name))
   {
      if (isBoolean(name))
      {
         type = BOOLEAN;
      }
      else if (isInteger(name))
      {
         type = INTEGER;
      }
   }
   else //name is an identifier and hopefully a constant
   {
      if (symbolTable.find(name) != symbolTable.end()) 
      {
         type = symbolTable.find(name)->second.getDataType();
      }
       else
      {
         
         processError("reference to undefined variable ");
      }

   }
   return type;
}
string Compiler::whichValue(string name){ //tells which value a name has

string value;
 if (isLiteral(name))
 {
   if ( name == "false")
      value = "0";
   else if ( name == "true") 
      value = "-1";
     else value = name;
 }
 else //name is an identifier and hopefully a constant
 {
   if (symbolTable.find(name) != symbolTable.end()) //here
   {
     value = symbolTable.at(name).getValue();
   }
   else
   {
      
      processError("reference to undefined constant");
   }
 }
return name;
}
///////////////////////////////////////////////////////////////////////////////
void Compiler::code(string op, string operand1, string operand2){
   if (op == "program")
   {
      emitPrologue(operand1);
   }
   else if (op == "end")
   {
      emitEpilogue();
   }



   else
   {
      processError("compiler error since function code should not be called with illegal arguments");
   }
}
///////////////////////////////////////////////////////////////////////////////
void Compiler::emit(string label, string instruction, string operands, string comment){
 // Turn on left justification in objectFile
 // Output label in a field of width 8
 // Output instruction in a field of width 8
 // Output the operands in a field of width 24
 // Output the comment
 objectFile << left << setw(8) << label;
 objectFile << setw(8) << instruction ;
 objectFile << setw(24) << operands;
 objectFile << setw(8)  << comment << endl;
}
void Compiler::emitPrologue(string progName, string operand2){
time_t now = time (NULL); 
 // Output identifying comments at beginning of objectFile
 objectFile << "; Brett Hedden & David Roberts      " << ctime(&now) << endl;
 objectFile << "%INCLUDE \"Along32.inc\" " << endl << "%INCLUDE \"Macros_Along.inc\"" << endl;
 // Output the %INCLUDE directives
 emit("\nSECTION", ".text");
 emit("global", "_start", "", "; program " + progName);
 emit("\n_start:");
 
}
void Compiler::emitEpilogue(string operand1, string operand2){
 emit("","Exit", "{0}");
 emitStorage();
}
void Compiler::emitStorage(){
   emit("SECTION", ".data");
   // for those entries in the symbolTable that have
   // an allocation of YES and a storage mode of CONSTANT
   // { call emit to output a line to objectFile }
   for ( i = symbolTable.begin(); i != symbolTable.end(); ++i){//iterators for maps 
      if(i->second.getAlloc() == YES && i->second.getMode() == CONSTANT){
         emit(i->second.getInternalName(), "dd",i->second.getValue(), "; " + i->first);
         
      }
   }
 
   emit("SECTION", ".bss");
   // for those entries in the symbolTable that have
   // an allocation of YES and a storage mode of VARIABLE
   // { call emit to output a line to objectFile }
   // if (isInSymbolTable == true && storeTypes(VARIABLE)) {
	// }
   for ( i = symbolTable.begin(); i != symbolTable.end(); ++i){
         if(i->second.getAlloc() == YES && i->second.getMode() == VARIABLE){
            emit(i->second.getInternalName(), "resd",i->second.getValue(), "; " + i->first);
         }
   }
}
///////////////////////////////////////////////////////////////////////////////
char Compiler::nextChar() //returns the next character or end of file marker
{
	sourceFile.get(ch);

	static char prevChar = '\n';

	if (sourceFile.eof())
	{
		ch = END_OF_FILE;
		return ch;
	}
	else
	{
		if (prevChar == '\n')
			listingFile << setw(5) << ++lineNo << '|';

		listingFile << ch;
	}

	prevChar = ch;
	return ch;
}

string Compiler::nextToken()	// returns the next token or END_OF_FILE marker
{
	token = "";
	
	while (token == "")
	{
		if (ch == '{')
		{
			while (nextChar() && ch != END_OF_FILE && ch != '}')
			{}	// do nothing, just skip the words
		
			if (ch == END_OF_FILE)
				processError("unexpected end of file");
			else
				nextChar();
		}
		
		else if (ch == '}')
			processError("'}' cannot begin token");
		
		else if (isspace(ch))
			nextChar();
		
		else if (isSpecialSymbol(ch))
		{
			token = ch;
			nextChar();
			
			// token now represent the first char in operators
			// ch represent the second one
			if (token == ":" && ch == '=')
			{
				token += ch;
				nextChar();
			}
			else if (token == "<" && (ch == '>' || ch == '='))
			{
				token += ch;
				nextChar();
			}
			else if (token == ">" && ch == '=')
			{
				token += ch;
				nextChar();
			}
		}
		
		else if (islower(ch))
		{
			token += ch;
			
			while (nextChar() && ((islower(ch) || isdigit(ch) || ch == '_')
				&& ch != END_OF_FILE))
					token += ch;
			
			if (ch == END_OF_FILE)
				processError("unexpected end of file");
		}
		
		else if (isdigit(ch))
		{
			token = ch;
			
			// build up the number or characters
			while (isdigit(nextChar()) && ch != END_OF_FILE
				&& !isSpecialSymbol(ch))
					token += ch;
			
			if (ch == END_OF_FILE)
				processError("unexpected end of file");
		}
		
		else if (ch == END_OF_FILE)
			token = ch;
		
		else
			processError("illegal symbol");
	}
	
	return token;
}


bool Compiler::isKeyword(string s) const{
  	if   (s == "program"
		|| s == "begin"
		|| s == "end"
		|| s == "var"
		|| s == "const"
		|| s == "integer"
		|| s == "boolean"
		|| s == "true"
		|| s == "false"
		|| s == "not"
		|| s == "mod"
		|| s == "div"
		|| s == "and"
		|| s == "or"
		|| s == "read"
		|| s == "write"
		|| s == "if"
		|| s == "then"
		|| s == "else"
		|| s == "repeat"
		|| s == "while"
		|| s == "do"
		|| s == "until") {
			return true;
	}
	return false;
}


bool Compiler::isSpecialSymbol(char s) const{
	if (  s == '='
		|| s == ':'
		|| s == ','
		|| s == ';'
		|| s == '.'
		|| s == '+'
		|| s == '-'
		|| s == '*'
		|| s == '<'
		|| s == '>'
		|| s == '('
		|| s == ')') {
		return true;
	}
	return false;
}

bool Compiler::isInteger(string s) const{  // allow for a + or - followed by one or more digits
// if s is a constant or variable found inside the table
	if (symbolTable.find(s) != symbolTable.end())
	{
		if (symbolTable.find(s)->second.getDataType() == INTEGER)
			return true;
		else
			return false;
	}

	// if s is not a constant or vriable found in the table

	// if s is only '+' or '-' reutrn false;
	if (s.length() == 1 && (s == "+" || s == "-"))
		return false;

	// s can be: 123 || +123 || -123
	for (uint i = 0; i < s.length(); ++i)
		if (!(isdigit(s[i]) || s[0] == '+' || s[0] == '-'))
			return false;

	return true;
}

bool Compiler::isBoolean(string s) const{
	if (s == "true" || s == "false" || s == "boolean") {
		return true;
	}
	return false;
}

bool Compiler::isLiteral(string s) const{
  return isInteger(s) || isBoolean(s) ||
		s == "+" || s == "-" || s == "not";
}


bool Compiler::isNonKeyId(string s) const{
   
   if (isKeyword(s))
		return false;

	// '_' cannot be at the beginning or the end
	if (s[s.length() - 1] == '_')
		return false;

	for (uint i = 0; i < s.length(); ++i)
		// it must satisfy: start with a lowercase character
		// and all the remaining characters must be lowercase or a digit
		if (!(islower(s[0]) && (isdigit(s[i]) || islower(s[i]) || !(s[i] == '_' && s[i + 1] == '_'))))
			return false;
   //T1 ,T0
	return true;
}


