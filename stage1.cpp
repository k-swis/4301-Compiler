// David Roberts & Brett Hedden
// CS 4301
// Stage 1

#include <stage1.h>
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



// void 		beginEndStmt();   	// stage 0, production 5



map<string, SymbolTableEntry>::iterator i;
bool RelOp(string);      
bool AddLevelOp(string);  
bool MultLevelOp(string);
  

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
	listingFile << "STAGE1:" << "Brett Hedden & David Roberts " << ctime(&now) << endl;
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
		processError("keyword \"begin\" expected");

	// change as of stage 1
	nextToken();
	if (isNonKeyId(token) || token == "read" || token == "write" || token == ";" || token == "begin") {
		execStmts();
	}
	
	if (token != "end")
		processError("keyword \"end\" expected");
	if (nextToken() != ".")
		processError("period expected");
	nextToken();
	code("end", ".");
}

void Compiler::constStmts()     // stage 0, production 6
{
	string x, y;

	if (!isNonKeyId(token)){
		processError("non-keyword identifier expected");
   }
	x = token;	// x has the constant's name

	// check for format x = y;
	if (nextToken() != "="){
		processError("\"=\" expected");
   }
	y = nextToken();	// y has the value of that constant

	// if y is not one of "+","-","not",NON_KEY_ID,"true","false",INTEGER
	if (!isNonKeyId(y) && !isLiteral(y)){	// y is not a number, true-false or a non-key ID
		processError("token to right of \"=\" illegal");
   }
	if (y == "+" || y == "-"){
		if (!isInteger(nextToken()))
			processError("integer expected after sign");

		y += token;
	}

	else if (y == "not"){
		if (!isBoolean(nextToken())){	// if after not isn't "true" or "false"
			processError("boolean expected after \"not\"");
      }
		if (token == "true"){
			y = "false";
      }
		else{
			y = "true";
      }
	}

	else if (isNonKeyId(y))	// if constant = another_constant -> search for it
	{
		// .find() will go through each key in the symbolTable map
		// if it reaches .end() -> did not find the key
		if (symbolTable.find(y) == symbolTable.end()){
			processError("reference to undefined constant");
      }
		else{
			if (whichType(y) == INTEGER){
				y = symbolTable.find(y)->second.getValue();
         }
			else{ // type of constant y is BOOLEAN
				if (symbolTable.find(y)->second.getValue() == "0"){
					y = "false";
            }
				else{
					y = "true";
            }
			}
		}
	}

	// check for format: x = y;
	if (nextToken() != ";"){
		processError("semicolon expected");
   }
	if (whichType(y) != INTEGER && whichType(y) != BOOLEAN){
		processError("data type of token on the right-hand side must be INTEGER or BOOLEAN");
   }
	insert(x, whichType(y), CONSTANT, whichValue(y), YES, 1);
	x = nextToken();

	// if after a constant declaration is another non_key_id or "var" or "begin"
	if (x != "begin" && x != "var" && !isNonKeyId(x)){
		processError("non-keyword identifier, \"begin\", or \"var\" expected");
   }
	if (isNonKeyId(x)){
		constStmts();	// call it again to insert another constant
   }
 
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
	insert(x,y == "integer"? INTEGER:BOOLEAN ,VARIABLE,"1",YES,1);
   
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
      case UNKNOWN:
		break;   
	}
	
	return internName;
}



void Compiler::insert(string externalName, storeTypes inType, modes inMode, string inValue, allocation inAlloc, int inUnits)
{
   //create symbol table entry for each identifier in list of external names
   //Multiply inserted names are illegal
   //symbolTable.find()   returns iterator != symboltable.end()
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
				processError("multiple name definition" + name);
			else if (isKeyword(name) && name != "true" && name != "false")
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

	if (symbolTable.size() > 256){
		processError("symbol table overflow -- max 256 entries");
   }
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
         type = symbolTable.at(name).getDataType();
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
   if ( name == "false"){
      value = "0";
   }else if ( name == "true") {
      value = "-1";
   }else value = name;
   
  
 }
 else //name is an identifier and hopefully a constant
 {
   if (symbolTable.find(name) != symbolTable.end()) 
   {
     value = symbolTable.at(name).getValue();
   }
   else
   {
      
      processError("reference to undefined constant");
   }
 }
return value;
}
///////////////////////////////////////////////////////////////////////////////
   void Compiler::code(string op, string operand1, string operand2)
{
	if (op == "program"){
		emitPrologue(operand1);
   }
	else if (op == "end"){
		emitEpilogue();
   }
	else if (op == "read"){
		emitReadCode(operand1);
   }
	else if (op == "write"){
		emitWriteCode(operand1);
   }
	else if (op == "+"){ // this must be binary '+'
		emitAdditionCode(operand1, operand2);
   }
	else if (op == "-"){ // this must be binary '-'
		emitSubtractionCode(operand1, operand2);
   }
	else if (op == "neg"){ // this must be unary '-'
		emitNegationCode(operand1);
   }
	else if (op == "not"){
		emitNotCode(operand1);
   }
	else if (op == "*"){
		emitMultiplicationCode(operand1, operand2);
   }
	else if (op == "div"){
		emitDivisionCode(operand1, operand2);
   }
	else if (op == "mod"){
		emitModuloCode(operand1, operand2);
   }
	else if (op == "and"){
		emitAndCode(operand1, operand2);
   }
	else if (op == "=") {
		emitEqualityCode(operand1, operand2);
	}
	else if (op == "<>") {
		emitInequalityCode(operand1, operand2);
	}
	else if (op == "or") {
		emitOrCode(operand1, operand2);
	}
	else if (op == "<") {
		emitLessThanCode(operand1, operand2);
	}
	else if (op == ">") {
		emitGreaterThanCode(operand1, operand2);
	}
	else if (op == "<=") {
		emitLessThanOrEqualToCode(operand1, operand2);
	}
	else if (op == ">=") {
		emitGreaterThanOrEqualToCode(operand1, operand2);
	}
	else if (op == ":=") {
		emitAssignCode(operand1, operand2);
	}
	else{
		processError("compiler error since function code should not be called with illegal arguments ");
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
 objectFile << "; Brett Hedden & David Roberts      " << ctime(&now);
 objectFile << "%INCLUDE \"Along32.inc\" " << endl << "%INCLUDE \"Macros_Along.inc\""  << endl;
 // Output the %INCLUDE directives
 emit("\nSECTION"," .text");
 emit("global", "_start", "", "; program " + progName);
 emit("\n_start:");
 
}
void Compiler::emitEpilogue(string operand1, string operand2){
 emit("","Exit", "{0}");
 objectFile << endl;
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
   objectFile << endl;
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

bool RelOp(string name)
{
	if (name == "=" || name == "<>" ||
		name == "<=" || name == ">=" ||
		name == "<" || name == ">")
	{
		return true;
	}
	return false;
}
bool AddLevelOp(string name) {
	if (name == "+" || name == "-" || name == "or") {
		return true;
	}
	return false;
}

bool MultLevelOp(string token) {
	if (token == "*" || token == "div" || token == "mod" || token == "and") {
		return true;
	}
	return false;
}

void Compiler::freeTemp() {
	currentTempNo--;
	if (currentTempNo < -1) {
		processError("compiler error, currentTempNo should be ≥ –1");
	}
}
string Compiler::getTemp() {//similar to getLabel
	string temp;
	currentTempNo++;
	temp = "T" + currentTempNo;
	if (currentTempNo > maxTempNo){
		insert(temp, UNKNOWN, VARIABLE, "1", NO, 1);
      symbolTable.at(temp).setInternalName(temp);
      maxTempNo++;
   }
	return temp;
}
string Compiler::getLabel() {
	//static local variabes started at -1 then increment by 1 then for temp be equal to .L
	static int labelNo = -1;
	string temp;
	labelNo++;
	temp = "L" + to_string(labelNo);
	return temp;
}

bool Compiler::isTemporary(string s) const { // determines if s represents a temporary
	if (s[0] == 'T') return true;
	else return false;
}

//stage0 work 100% - 
//0-------------------------------------------------------------------------0



void Compiler::emitAdditionCode(string operand1, string operand2) { //add operand1 to operand2

	
	if (symbolTable.count(operand1) == 0)
		processError("reference to undefined symbol " + operand1);
	else if (symbolTable.count(operand2) == 0)
		processError("reference to undefined symbol " + operand2);

	if (symbolTable.at(operand1).getDataType() != storeTypes::INTEGER 
		|| symbolTable.at(operand2).getDataType() != storeTypes::INTEGER)
		processError("binary '+' requires integer operands");

	if (contentsOfAReg[0] == 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName() 
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName())
	{
		
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(allocation::YES);
		contentsOfAReg = "";
	}

	if (!contentsOfAReg.empty() && contentsOfAReg[0] != 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName() 
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName())
	{
		contentsOfAReg = "";
	}

	if (contentsOfAReg != symbolTable.at(operand1).getInternalName() 
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = symbolTable.at(operand2).getInternalName();
	}

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "add", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " + " + operand1);
	else
		emit("", "add", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " + " + operand2);

	if (operand1[0] == 'T')
		freeTemp();
	if (operand2[0] == 'T')
		freeTemp();

	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(storeTypes::INTEGER);
	pushOperand(contentsOfAReg);
}

void Compiler::emitSubtractionCode(string operand1, string operand2) {    // op2 -  op1
if (symbolTable.find(operand1) == symbolTable.end())
		processError("reference to undefined symbol ");

	if (symbolTable.find(operand2) == symbolTable.end())
		processError("reference to undefined symbol ");

	if (!isInteger(operand1) || !isInteger(operand2)) {
		processError("binary '-' requires integer operands");
	}
	if (isTemporary(contentsOfAReg) && contentsOfAReg != operand2) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.find(contentsOfAReg)->second.setAlloc(allocation::YES);
		//deassign it
		contentsOfAReg = "";
	}
	if (contentsOfAReg != "" &&  contentsOfAReg[0] != 'T' && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		// deassign it
		contentsOfAReg = "";
	}
	if (contentsOfAReg != operand2) {
		//emit code to load operand2 into A register
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;

	}
	//emit code to perform register-memory subtraction
	if (contentsOfAReg == operand2)
		emit("", "sub", "eax,[" + symbolTable.at(operand1).getInternalName() + "]",
			"; AReg = " + operand2 + " - " + operand1);
	else {
		emit("", "sub", "eax,[" + symbolTable.at(operand2).getInternalName() + "]",
			"; AReg = " + operand1 + " - " + operand2);
	}


	//deassign all temporaries involved in the addition and free those names for reuse
	if (isTemporary(operand1)) {
		freeTemp();
	}
	if (isTemporary(operand2)) {
		freeTemp();
	}
	//A Register = next available temporary name and change type of its symbol table entry to integer
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	//push the name of the result onto operandStk
	operandStk.push(contentsOfAReg);
}

void Compiler::emitDivisionCode(string operand1, string operand2) { //divide operand2 by operand1
	if (symbolTable.find(operand1) == symbolTable.end())
		processError("reference to undefined symbol " + operand1);

	if (symbolTable.find(operand2) == symbolTable.end())
		processError("reference to undefined symbol " + operand2);

	if (!isInteger(operand1) || !isInteger(operand2)) {
		processError("binary 'div' requires integer operands");
	}
	if (contentsOfAReg != "" && contentsOfAReg[0] == 'T'   && contentsOfAReg != operand2) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		//deassign it
		contentsOfAReg = "";
	}
	//PSU: if A register holds a non-temp not operand2 then deassign it
	if (contentsOfAReg != "" &&  contentsOfAReg[0] != 'T' && contentsOfAReg != operand2) {
		// deassign it
		contentsOfAReg = "";
	}
	if (contentsOfAReg != operand2) { //op2 / op1 -> gan op2-> eax 
		//emit code to load operand2 into A register
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
		//"eax" = op2
	}
	emit("", "cdq", "", "; sign extend dividend from eax to edx:eax");
	emit("", "idiv", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " div " + operand1);
	//deassign all temporaries involved and free those names for reuse T2 / T1 eax, free T0,-> getTemp(T0) ->
	if (isTemporary(operand1)) {
		freeTemp();
	}
	if (isTemporary(operand2)) {
		freeTemp();
	}
	//A Register = next available temporary name and change type of its symbol table entry to integer
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	//push name of result onto operandStk;
	operandStk.push(contentsOfAReg);
}

void Compiler::emitMultiplicationCode(string operand1, string operand2) { // op2 *  op1
   if (symbolTable.find(operand1) == symbolTable.end())
		processError("reference to undefined symbol");

	if (symbolTable.find(operand2) == symbolTable.end())
		processError("reference to undefined symbol");

	if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
		processError("binary '*' requires integer operands");

//------
	if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2)
	{
		// emit code to store that temp into memory
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.find(contentsOfAReg)->second.setAlloc(YES);
		contentsOfAReg = "";
	}

	if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
		contentsOfAReg = "";
   }
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2)
	{	// emit code to load operand2 into A register
		emit("", "mov", "eax,[" + symbolTable.find(operand2)->second.getInternalName() + "]", "; AReg = " + whichValue(operand2));
		contentsOfAReg = operand2;
	}

	if (contentsOfAReg == operand2)
		emit("", "imul", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " * " + operand1);
	else
		emit("", "imul", "dword [" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " * " + operand2);

	// free the temporary being used
	if (isTemporary(operand1))
		freeTemp();

	if (isTemporary(operand2))
		freeTemp();

	contentsOfAReg = getTemp();
	symbolTable.find(contentsOfAReg)->second.setDataType(INTEGER);
	pushOperand(contentsOfAReg);
}

void Compiler::emitModuloCode(string operand1, string operand2) {         // op2 %  op1
   if (symbolTable.find(operand1) == symbolTable.end())
		processError("reference to undefined symbol " + operand1);

	if (symbolTable.find(operand2) == symbolTable.end())
		processError("reference to undefined symbol " + operand2);

	if (!isInteger(operand1) || !isInteger(operand2)) {
		processError("binary 'mod' requires integer operands");
	}
	if (contentsOfAReg != "" && contentsOfAReg[0] == 'T'   && contentsOfAReg != operand2) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		//deassign it
		contentsOfAReg = "";
	}
	//PSU: if A register holds a non-temp not operand2 then deassign it
	if (contentsOfAReg != "" &&  contentsOfAReg[0] != 'T' && contentsOfAReg != operand2) {
		// deassign it
		contentsOfAReg = "";
	}
	if (contentsOfAReg != operand2) { //op2 / op1 -> gan op2-> eax 
		//emit code to load operand2 into A register
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
		//"eax" = op2
	}
	emit("", "cdq", "", "; sign extend dividend from eax to edx:eax");
	emit("", "idiv", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " div " + operand1);
	emit("", "xchg", "eax,edx", "; exchange quotient and remainder");
   //deassign all temporaries involved and free those names for reuse T2 / T1 eax, free T0,-> getTemp(T0) ->
	if (isTemporary(operand1)) {
		freeTemp();
	}
	if (isTemporary(operand2)) {
		freeTemp();
	}
	//A Register = next available temporary name and change type of its symbol table entry to integer
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	//push name of result onto operandStk;
	pushOperand(contentsOfAReg);
}

void Compiler::emitNegationCode(string operand1, string operand2) {           // -op1
   if (symbolTable.find(operand1) == symbolTable.end())
		processError(" reference to undefined variable" + operand1);

	if (whichType(operand1) != INTEGER)
		processError(" unary '-' requires an integer operand: -" + operand1);

	if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1)
	{
		// emit code to store that temp into memory
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");

		symbolTable.find(contentsOfAReg)->second.setAlloc(YES);
		contentsOfAReg = "";
	}

	if (!isTemporary(contentsOfAReg) && contentsOfAReg != operand1)
		contentsOfAReg = "";

	if (contentsOfAReg != operand1)
	{	// emit instruction to do a register-memory load of operand1 into the A registe
		emit("", "mov", "eax,[" + symbolTable.find(operand1)->second.getInternalName() + ']', "; AReg = " + operand1);
		contentsOfAReg = operand1;
	}

	emit("", "neg", "eax", "; AReg = -AReg");

	if (isTemporary(operand1))
		freeTemp();

	contentsOfAReg = getTemp();
	symbolTable.find(contentsOfAReg)->second.setDataType(INTEGER);

	pushOperand(contentsOfAReg);
}

void Compiler::emitAndCode(string operand1, string operand2) { //and operand1 to operand2

	if (symbolTable.find(operand1) == symbolTable.end())
		processError(" reference to undefined variable: "+ operand1);

	if (symbolTable.find(operand2) == symbolTable.end())
		processError(" reference to undefined variable: " + operand2);

	if (whichType(operand1) != BOOLEAN && whichType(operand2) != BOOLEAN) {
		processError(" binary 'and' requires operands of the same type: " + operand2 + "and" + operand1);
	}
	if (contentsOfAReg != "" && contentsOfAReg.at(0) == 'T' && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		//deassign it
		contentsOfAReg = "";
	}
	if (contentsOfAReg != "" && contentsOfAReg.at(0) != 'T' && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		contentsOfAReg = "";
	}
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}

	if (contentsOfAReg == operand2)
		if (operand1 == "true") {
			emit("", "and", "eax,[TRUE]", "; AReg = " + operand2 + " and " + operand1);
		}
		else if (operand1 == "false")
			emit("", "and", "eax,[FALSE]", "; AReg = " + operand2 + " and " + operand1);
		else
			emit("", "and", "eax,[" + symbolTable.find(operand1)->second.getInternalName() + ']', "; AReg = " + operand2 + " and " + operand1);

	else
		if (operand2 == "true") {
			emit("", "and", "eax,[TRUE]", "; AReg = " + operand1 + " and " + operand2);
		}
		else if (operand2 == "false")
			emit("", "and", "eax,[FALSE]", "; AReg = " + operand1 + " and " + operand2);
		else
			emit("", "and", "eax,[" + symbolTable.find(operand2)->second.getInternalName() + ']', "; AReg = " + operand1 + " and " + operand2);

	if (isTemporary(operand1)) {
		freeTemp();
	}
	if (isTemporary(operand2)) {
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	//push name of result onto operandStk;
	operandStk.push(contentsOfAReg);
}

void Compiler::emitNotCode(string operand1, string operand2) {                // !op1
   if (symbolTable.find(operand1) == symbolTable.end())
		processError("reference to undefined variable");

	if (whichType(operand1) != BOOLEAN)
		processError("binary 'not' requires boolean operands");

	if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1)
	{
		// emit code to store that temp into memory
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");

		symbolTable.find(contentsOfAReg)->second.setAlloc(YES);
		contentsOfAReg = "";
	}

	if (!contentsOfAReg.empty() && !isTemporary(contentsOfAReg) && contentsOfAReg != operand1)
		contentsOfAReg = "";

	if (contentsOfAReg != operand1)
	{	// emit instruction to do a register-memory load of operand1 into the A registe
		emit("", "mov", "eax,[" + symbolTable.find(operand1)->second.getInternalName() + ']', "; AReg = " + operand1);
		contentsOfAReg = operand1;
	}

	emit("", "not", "eax", "; AReg = -AReg");

	if (isTemporary(operand1))
		freeTemp();

	contentsOfAReg = getTemp();
	symbolTable.find(contentsOfAReg)->second.setDataType(INTEGER);

	pushOperand(contentsOfAReg);
}

void Compiler::emitOrCode(string operand1, string operand2) {             // op2 || op1
   if (symbolTable.count(operand1) == 0)
		processError("reference to undefined symbol " + operand1);
	else if (symbolTable.count(operand2) == 0)
		processError("reference to undefined symbol " + operand2);
	
	if (symbolTable.at(operand1).getDataType() != storeTypes::BOOLEAN ||
		symbolTable.at(operand2).getDataType() != storeTypes::BOOLEAN)
		processError("binary 'or' requires boolean operands");

	if (contentsOfAReg[0] == 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName() 
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(allocation::YES);
		contentsOfAReg = "";
	}

	if (!contentsOfAReg.empty() && contentsOfAReg[0] != 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName())
		contentsOfAReg = "";

	if (contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = symbolTable.at(operand2).getInternalName();
	}

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "or", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " or " + operand1);
	else
		emit("", "or", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " or " + operand2);


	if (operand1[0] == 'T')
		freeTemp();
	if (operand2[0] == 'T')
		freeTemp();

	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(storeTypes::BOOLEAN);
	pushOperand(contentsOfAReg);
}

void Compiler::emitEqualityCode(string operand1, string operand2) { //test whether operand2 equals operand1

	/*
	if types of operands are not the same
		processError(incompatible types)
	if A Register holds a temp not operand1 nor operand2 then
		emit code to store that temp into memory
		change the allocate entry for it in the symbol table to yes
		deassign it
	if A register holds a non-temp not operand2 nor operand1 then deassign it
		if neither operand is in A register then
			emit code to load operand2 into the A register
			emit code to perform a register-memory compare
			emit code to jump if equal to the next available Ln (call getLabel)
			emit code to load FALSE into A register
	insert FALSE in symbol table with value 0 and external name false
	emit code to perform an unconditional jump to the next label (call getLabel should be L(n+1))
	emit code to label the next instruction with the first acquired label Ln
	emit code to load TRUE into A register
	insert TRUE in symbol table with value -1 and external name true
	emit code to label the next instruction with the second acquired label L(n+1)
	deassign all temporaries involved and free those names for reuse
	A Register = next available temporary name and change type of its symbol table entry to boolean
	push name of result onto operandStk
	*/
   if (symbolTable.count(operand1) == 0)
		processError("reference to undefined symbol " + operand1);
	else if (symbolTable.count(operand2) == 0)
		processError("reference to undefined symbol " + operand2);

	if (contentsOfAReg[0] == 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName() 
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(allocation::YES);
		contentsOfAReg = "";
	}
	
	if (!contentsOfAReg.empty() && contentsOfAReg[0] != 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName())
		contentsOfAReg = "";

	if (contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = symbolTable.at(operand2).getInternalName();
	}

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
	else
		emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);

	string firstLabel = getLabel(), secondLabel = getLabel();


	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "je", "." + firstLabel, "; if " + operand2 + " = " + operand1 + " then jump to set eax to TRUE");
	else 
		emit("", "je", "." + firstLabel, "; if " + operand1 + " = " + operand2 + " then jump to set eax to TRUE");

	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");

	if (symbolTable.count("false") == 0) {
		insert("false", storeTypes::BOOLEAN, modes::CONSTANT, "0", allocation::YES, 1);
		symbolTable.at("false").setInternalName("FALSE");
	}
	emit("", "jmp", "." + secondLabel, "; unconditionally jump");
	emit("." + firstLabel + ":");
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");

	if (symbolTable.count("true") == 0) {
		insert("true", storeTypes::BOOLEAN, modes::CONSTANT, "-1", allocation::YES, 1);
		symbolTable.at("true").setInternalName("TRUE");
	}
	emit("." + secondLabel + ":");

	if (operand1[0] == 'T')
		freeTemp();
	if (operand2[0] == 'T')
		freeTemp();

	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(storeTypes::BOOLEAN);
	pushOperand(contentsOfAReg);
   
   
   
}

void Compiler::emitInequalityCode(string operand1, string operand2) {     // op2 != op1
   if (symbolTable.count(operand1) == 0)
		processError("reference to undefined symbol " + operand1);
	else if (symbolTable.count(operand2) == 0)
		processError("reference to undefined symbol " + operand2);
	
	if (symbolTable.at(operand1).getDataType() != symbolTable.at(operand2).getDataType())
		processError("incompatible types for operator '<>'");

	if (contentsOfAReg[0] == 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName() 
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(allocation::YES);
		contentsOfAReg = "";
	}

	if (!contentsOfAReg.empty() && contentsOfAReg[0] != 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName())
		contentsOfAReg = "";

	if (contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = symbolTable.at(operand2).getInternalName();
	}

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
	else
		emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);

	string firstLabel = getLabel(), secondLabel = getLabel();

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "jne", "." + firstLabel, "; if " + operand2 + " <> " + operand1 + " then jump to set eax to TRUE");
	else
		emit("", "jne", "." + firstLabel, "; if " + operand1 + " <> " + operand2 + " then jump to set eax to TRUE");

	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");

	if (symbolTable.count("false") == 0) {
		insert("false", storeTypes::BOOLEAN, modes::CONSTANT, "0", allocation::YES, 1);
		symbolTable.at("false").setInternalName("FALSE");
	}
	emit("", "jmp", "." + secondLabel, "; unconditionally jump");
	emit("." + firstLabel + ":");
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");

	if (symbolTable.count("true") == 0) {
		insert("true", storeTypes::BOOLEAN, modes::CONSTANT, "-1", allocation::YES, 1);
		symbolTable.at("true").setInternalName("TRUE");
	}
	emit("." + secondLabel + ":");

	if (operand1[0] == 'T')
		freeTemp();
	if (operand2[0] == 'T')
		freeTemp();

	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(storeTypes::BOOLEAN);
	pushOperand(contentsOfAReg);
}

void Compiler::emitAssignCode(string operand1, string operand2) { //assign the value of operand1 to operand2

	//emit("operand1 = " + operand1, " operand2 = " + operand2, " AReg = " + contentsOfAReg);

	if (symbolTable.find(operand1) == symbolTable.end())
		processError(" reference to undefined variable" + operand1);

	if (symbolTable.find(operand2) == symbolTable.end())
		processError(" reference to undefined variable" + operand2);

	// the that non_key_id was not initialized before
	if (symbolTable.find(operand2)->second.getMode() == CONSTANT)
		processError(" symbol on left-hand side of assignment must have a storage mode of VARIABLE: "+ operand2+ " := " + operand1);

	if (whichType(operand1) != whichType(operand2))
		processError(" incompatible types for operator ':=': " + operand2 + ":=" + operand1);

	if (symbolTable.find(operand1) == symbolTable.end())
		processError(" reference to undefined variable '" + operand1 + "'");

	if (symbolTable.find(operand2) == symbolTable.end())
		processError(" reference to undefined variable '" + operand2 + "'");

	if (symbolTable.find(operand2)->second.getMode() != VARIABLE)
		processError(" symbol on left-hand side of assignment must have a storage mode of VARIABLE: "+ operand2 + " := " + operand1);

	if (operand1 == operand2)
		return;

	if (contentsOfAReg != operand1)
	{
		// emit code to load operand1 into the A register

		if (operand1 == "true") {
			emit("", "mov", "eax,[TRUE]", "; AReg = " + operand1);
			contentsOfAReg = "TRUE";
		}
		else if (operand1 == "false") {
			emit("", "mov", "eax,[FALSE]", "; AReg = " + operand1);
			contentsOfAReg = "FALSE";


		}
		else {
			emit("", "mov", "eax,[" + symbolTable.find(operand1)->second.getInternalName() + ']', "; AReg = " + operand1);
			contentsOfAReg = operand1;
		}

	}

	emit("", "mov", '[' + symbolTable.find(operand2)->second.getInternalName() + "],eax", "; " + operand2 + " = AReg");
	contentsOfAReg = operand2;

	// free any temp being used
	if (isTemporary(operand1))
		freeTemp();
}

void Compiler::emitReadCode(string operand, string operand2) {
	string name;

	for (uint i = 0; i < operand.size(); ++i) {
				
		if (operand[i] != ',' && i < operand.size()) {
			name += operand[i];
			continue;
		}

		if (name != "") {

			if (symbolTable.count(name) == 0)
				processError("reference to undefined symbol " + name);
			if (symbolTable.at(name).getDataType() != storeTypes::INTEGER)
				processError("can't read variables of this type");
			if (symbolTable.at(name).getMode() != modes::VARIABLE)
				processError("attempting to read to a read-only location");
			emit("", "call", "ReadInt", "; read int; value placed in eax");
			emit("", "mov", "[" + symbolTable.at(name).getInternalName() + "],eax", "; store eax at " + name);
			contentsOfAReg = symbolTable.at(name).getInternalName();
		}

		name = "";
	}

	if (name != "") {

		if (symbolTable.count(name) == 0)
			processError("reference to undefined symbol " + name);
		if (symbolTable.at(name).getDataType() != storeTypes::INTEGER)
			processError("can't read variables of this type");
		if (symbolTable.at(name).getMode() != modes::VARIABLE)
			processError("attempting to read to a read-only location");
		emit("", "call", "ReadInt", "; read int; value placed in eax");
		emit("", "mov", "[" + symbolTable.at(name).getInternalName() + "],eax", "; store eax at " + name);
		contentsOfAReg = symbolTable.at(name).getInternalName();
	}
}

void Compiler::emitWriteCode(string operand, string operand2) {
	/*
	string name
	static bool definedStorage = false
	while (name is broken from list (operand) and put in name != "")
	{
		if name is not in symbol table
			processError("reference to undefined symbol");
		if name is not in A register
			emit the code to load name in A register
			set the contentsOfAReg = name
		if data type of name is INTEGER
			emit code to call the Irvine WriteInt function
		else // data type is BOOLEAN
		{
			emit code to compare A register to 0
			acquire a new label Ln
			emit code to jump if equal to the acquired label Ln
			emit code to load address of TRUE literal in D register
			acquire a second label L(n + 1)
			emit code to unconditionally jump to label L(n + 1)
			emit code to label the next line with the first acquired label Ln
			emit code to load address of FALSE literal in D register
			emit code to label the next line with the second acquired label L(n + 1)
			emit code to call the Irvine WriteString function
			if static variable definedStorage is false
			{
				set definedStorage to true
				output an endl to objectFile
				emit code to begin a .data SECTION
				emit code to create label TRUELIT, instruction db, operands 'TRUE',0
				emit code to create label FALSELIT, instruction db, operands 'FALSE',0
				output an endl to objectFile
				emit code to resume .text SECTION
			} // end if
		} // end else
		emit code to call the Irvine Crlf function
	} // end while
	*/
   
   
   string name;
	static bool definedStorage = false;

	for (uint i = 0; i < operand.size(); ++i) {

		if (operand[i] != ',' && i < operand.size()) {
			name += operand[i];
			continue;
		}

		if (name != "") {
			if (symbolTable.count(name) == 0)
				processError("reference to undefined symbol " + name);
			if (symbolTable.at(name).getInternalName() != contentsOfAReg) {
				emit("", "mov", "eax,[" + symbolTable.at(name).getInternalName() + "]", "; load " + name + " in eax");
				contentsOfAReg = symbolTable.at(name).getInternalName();
			}
			if (symbolTable.at(name).getDataType() == storeTypes::INTEGER)
				emit("", "call", "WriteInt", "; write int in eax to standard out");
			else { //data type is BOOLEAN
				emit("", "cmp", "eax,0", "; compare to 0");
				string firstLabel = getLabel(), secondLabel = getLabel();
				emit("", "je", "." + firstLabel, "; jump if equal to print FALSE");
				emit("", "mov", "edx,TRUELIT", "; load address of TRUE literal in edx");
				emit("", "jmp", "." + secondLabel, "; unconditionally jump to ." + secondLabel);
				emit("." + firstLabel + ":");
				emit("", "mov", "edx,FALSLIT", "; load address of FALSE literal in edx");
				emit("." + secondLabel + ":");
				emit("", "call", "WriteString", "; write string to standard out");

				if (definedStorage == false) {
					definedStorage = true;
					objectFile << endl;
					emit("SECTION", ".data");
					emit("TRUELIT", "db", "'TRUE',0", "; literal string TRUE");
					emit("FALSLIT", "db", "'FALSE',0", "; literal string FALSE");
					objectFile << endl;
					emit("SECTION", ".text");
				} // end if
			} // end else

			emit("", "call", "Crlf", "; write \\r\\n to standard out");
		}
		name = "";
	} // end for loop

	if (symbolTable.count(name) == 0)
		processError("reference to undefined symbol " + name);
	if (symbolTable.at(name).getInternalName() != contentsOfAReg) {
		emit("", "mov", "eax,[" + symbolTable.at(name).getInternalName() + "]", "; load " + name + " in eax");
		contentsOfAReg = symbolTable.at(name).getInternalName();
	}
	if (symbolTable.at(name).getDataType() == storeTypes::INTEGER)
		emit("", "call", "WriteInt", "; write int in eax to standard out");
	else { //data type is BOOLEAN
		emit("", "cmp", "eax,0", "; compare to 0");
		string firstLabel = getLabel(), secondLabel = getLabel();
		emit("", "je", "." + firstLabel, "; jump if equal to print FALSE");
		emit("", "mov", "edx,TRUELIT", "; load address of TRUE literal in edx");
		emit("", "jmp", "." + secondLabel, "; unconditionally jump to ." + secondLabel);
		emit("." + firstLabel + ":");
		emit("", "mov", "edx,FALSLIT", "; load address of FALSE literal in edx");
		emit("." + secondLabel + ":");
		emit("", "call", "WriteString", "; write string to standard out");

		if (definedStorage == false) {
			definedStorage = true;
			objectFile << endl;
			emit("SECTION", ".data");
			emit("TRUELIT", "db", "'TRUE',0", "; literal string TRUE");
			emit("FALSLIT", "db", "'FALSE',0", "; literal string FALSE");
			objectFile << endl;
			emit("SECTION", ".text");
		} // end if
	} // end else

	emit("", "call", "Crlf", "; write \\r\\n to standard out");
   
   
   
}

void Compiler::emitLessThanCode(string operand1, string operand2) {       // op2 <  op1
   if (symbolTable.count(operand1) == 0)
		processError("reference to undefined symbol " + operand1);
	else if (symbolTable.count(operand2) == 0)
		processError("reference to undefined symbol " + operand2);
	
	if (symbolTable.at(operand1).getDataType() != symbolTable.at(operand2).getDataType())
		processError("incompatible types for operator '<'");

	if (contentsOfAReg[0] == 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(allocation::YES);
		contentsOfAReg = "";
	}

	if (!contentsOfAReg.empty() && contentsOfAReg[0] != 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName())
		contentsOfAReg = "";

	if (contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = symbolTable.at(operand2).getInternalName();
	}

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
	else
		emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);

	string firstLabel = getLabel(), secondLabel = getLabel();

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "jl", "." + firstLabel, "; if " + operand2 + " < " + operand1 + " then jump to set eax to TRUE");
	else 
		emit("", "jl", "." + firstLabel, "; if " + operand1 + " < " + operand2 + " then jump to set eax to TRUE");

	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");

	if (symbolTable.count("false") == 0) {
		insert("false", storeTypes::BOOLEAN, modes::CONSTANT, "0", allocation::YES, 1);
		symbolTable.at("false").setInternalName("FALSE");
	}
	emit("", "jmp", "." + secondLabel, "; unconditionally jump");
	emit("." + firstLabel + ":");
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");

	if (symbolTable.count("true") == 0) {
		insert("true", storeTypes::BOOLEAN, modes::CONSTANT, "-1", allocation::YES, 1);
		symbolTable.at("true").setInternalName("TRUE");
	}
	emit("." + secondLabel + ":");

	if (operand1[0] == 'T')
		freeTemp();
	if (operand2[0] == 'T')
		freeTemp();

	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(storeTypes::BOOLEAN);
	pushOperand(contentsOfAReg);
}

void Compiler::emitLessThanOrEqualToCode(string operand1, string operand2) { // op2 <= op1
if (symbolTable.count(operand1) == 0)
		processError("reference to undefined symbol " + operand1);
	else if (symbolTable.count(operand2) == 0)
		processError("reference to undefined symbol " + operand2);
	
	if (symbolTable.at(operand1).getDataType() != symbolTable.at(operand2).getDataType())
		processError("incompatible types for operator '<='");

	if (contentsOfAReg[0] == 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName() 
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(allocation::YES);
		contentsOfAReg = "";
	}

	if (!contentsOfAReg.empty() && contentsOfAReg[0] != 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName())
		contentsOfAReg = "";

	if (contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = symbolTable.at(operand2).getInternalName();
	}

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
	else
		emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);

	string firstLabel = getLabel(), secondLabel = getLabel();

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "jle", "." + firstLabel, "; if " + operand2 + " <= " + operand1 + " then jump to set eax to TRUE");
	else
		emit("", "jle", "." + firstLabel, "; if " + operand1 + " <= " + operand2 + " then jump to set eax to TRUE");

	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");

	if (symbolTable.count("false") == 0) {
		insert("false", storeTypes::BOOLEAN, modes::CONSTANT, "0", allocation::YES, 1);
		symbolTable.at("false").setInternalName("FALSE");
	}
	emit("", "jmp", "." + secondLabel, "; unconditionally jump");
	emit("." + firstLabel + ":");
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");

	if (symbolTable.count("true") == 0) {
		insert("true", storeTypes::BOOLEAN, modes::CONSTANT, "-1", allocation::YES, 1);
		symbolTable.at("true").setInternalName("TRUE");
	}
	emit("." + secondLabel + ":");

	if (operand1[0] == 'T')
		freeTemp();
	if (operand2[0] == 'T')
		freeTemp();

	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(storeTypes::BOOLEAN);
	pushOperand(contentsOfAReg);
}

void Compiler::emitGreaterThanCode(string operand1, string operand2) {    // op2 >  op1
if (symbolTable.count(operand1) == 0)
		processError("reference to undefined symbol " + operand1);
	else if (symbolTable.count(operand2) == 0)
		processError("reference to undefined symbol " + operand2);
	
	if (symbolTable.at(operand1).getDataType() != symbolTable.at(operand2).getDataType())
		processError("incompatible types for operator '>'");

	if (contentsOfAReg[0] == 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(allocation::YES);
		contentsOfAReg = "";
	}

	if (!contentsOfAReg.empty() && contentsOfAReg[0] != 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName())
		contentsOfAReg = "";

	if (contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = symbolTable.at(operand2).getInternalName();
	}

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
	else
		emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);

	string firstLabel = getLabel(), secondLabel = getLabel();

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "jg", "." + firstLabel, "; if " + operand2 + " > " + operand1 + " then jump to set eax to TRUE");
	else
		emit("", "jg", "." + firstLabel, "; if " + operand1 + " > " + operand2 + " then jump to set eax to TRUE");

	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");

	if (symbolTable.count("false") == 0) {
		insert("false", storeTypes::BOOLEAN, modes::CONSTANT, "0", allocation::YES, 1);
		symbolTable.at("false").setInternalName("FALSE");
	}
	emit("", "jmp", "." + secondLabel, "; unconditionally jump");
	emit("." + firstLabel + ":");
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");

	if (symbolTable.count("true") == 0) {
		insert("true", storeTypes::BOOLEAN, modes::CONSTANT, "-1", allocation::YES, 1);
		symbolTable.at("true").setInternalName("TRUE");
	}
	emit("." + secondLabel + ":");

	if (operand1[0] == 'T')
		freeTemp();
	if (operand2[0] == 'T')
		freeTemp();

	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(storeTypes::BOOLEAN);
	pushOperand(contentsOfAReg);
}

void Compiler::emitGreaterThanOrEqualToCode(string operand1, string operand2) { // op2 >= op1
if (symbolTable.count(operand1) == 0)
		processError("reference to undefined symbol " + operand1);
	else if (symbolTable.count(operand2) == 0)
		processError("reference to undefined symbol " + operand2);
	
	if (symbolTable.at(operand1).getDataType() != symbolTable.at(operand2).getDataType())
		processError("incompatible types for operator '>='");

	if (contentsOfAReg[0] == 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName() 
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(allocation::YES);
		contentsOfAReg = "";
	}

	if (!contentsOfAReg.empty() && contentsOfAReg[0] != 'T' && contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName())
		contentsOfAReg = "";

	if (contentsOfAReg != symbolTable.at(operand1).getInternalName()
		&& contentsOfAReg != symbolTable.at(operand2).getInternalName()) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = symbolTable.at(operand2).getInternalName();
	}

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
	else
		emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);

	string firstLabel = getLabel(), secondLabel = getLabel();

	if (contentsOfAReg == symbolTable.at(operand2).getInternalName())
		emit("", "jge", "." + firstLabel, "; if " + operand2 + " >= " + operand1 + " then jump to set eax to TRUE");
	else
		emit("", "jge", "." + firstLabel, "; if " + operand1 + " >= " + operand2 + " then jump to set eax to TRUE");

	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");

	if (symbolTable.count("false") == 0) {
		insert("false", storeTypes::BOOLEAN, modes::CONSTANT, "0", allocation::YES, 1);
		symbolTable.at("false").setInternalName("FALSE");
	}
	emit("", "jmp", "." + secondLabel, "; unconditionally jump");
	emit("." + firstLabel + ":");
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");

	if (symbolTable.count("true") == 0) {
		insert("true", storeTypes::BOOLEAN, modes::CONSTANT, "-1", allocation::YES, 1);
		symbolTable.at("true").setInternalName("TRUE");
	}
	emit("." + secondLabel + ":");

	if (operand1[0] == 'T')
		freeTemp();
	if (operand2[0] == 'T')
		freeTemp();

	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(storeTypes::BOOLEAN);
	pushOperand(contentsOfAReg);
}






///////////////////////////////////////////////////////////////////////////




void Compiler::execStmts()// stage 1, production 2 
{ 
	if (isNonKeyId(token) || token == "read" || token == "write")
	{
		execStmt();
		nextToken();
		execStmts(); 
	} 
}


void Compiler::execStmt() {// stage 1,production 3
	if (isNonKeyId(token)) {
		assignStmt();
	}
	else if (token == "read")
	{
		readStmt();
	}
	else if (token == "write") {
		writeStmt();
	}

}

void Compiler::assignStmt() {     // stage 1, production 4

string secondOperand, firstOperand;
	if (!isNonKeyId(token))
		processError("non - keyword identifier expected");

	//Token must be already defined
	if (symbolTable.count(token) == 0) processError("reference to undefined variable");

	pushOperand(token);
	nextToken();

	if (token != ":=") processError("':=' expected; found " + token);
	else pushOperator(":=");

	nextToken();

	if (token != "not" && token != "true" && token != "false" && token != "(" && token != "+"
		&& token != "-" && !isInteger(token) && !isNonKeyId(token) && token != ";")
		processError("one of \"*\", \"and\", \"div\", \"mod\", \")\", \"+\", \"-\", \";\", \"<\", \"<=\", \"<>\", \"=\", \">\", \">=\", or \"or\" expected");
	else express();

	secondOperand = popOperand();
	firstOperand = popOperand();
	code(popOperator(), secondOperand, firstOperand);
}

void Compiler::readStmt() {       // stage 1, production 5
	string x = "";
	if (nextToken() != "(") {
		processError("'(' expected");
	}

	nextToken();
	string readList = ids();

	for (unsigned int i = 0; i < readList.length(); i++) {
		if (readList[i] == ',') {
			code("read", x);
			x = "";
		}
		else {
			x += readList[i];
		}
	}
	code("read", x);

	if (token != ")") {
		processError("',' or ')' expected after non-keyword identifier");
	}
	if (nextToken() != ";") {
		processError("';' expected");
	}
}

void Compiler::writeStmt() {      // stage 1, production 7
	string x = "";
	if (nextToken() != "(") {
		processError("'(' expected");
	}

	nextToken();
	string writeList = ids();

	for (unsigned int i = 0; i < writeList.length(); i++) {
		if (writeList[i] == ',') {
			code("write", x);
			x = "";
		}
		else {
			x += writeList[i];
		}
	}
	code("write", x);

	if (token != ")") {
		processError("',' or ')' expected after non-keyword identifier");
	}
	if (nextToken() != ";") {
		processError("';' expected");
	}
}

void Compiler::express() {        // stage 1, production 9

	if (token != "not" && token != "true" && token != "false" && token != "(" && token != "+" && token != "-" && !isInteger(token) && !isNonKeyId(token)){
		processError("\"not\", \"true\", \"false\", \"(\", \"+\", \"-\", non - keyword identifier or integer expected");
   }
	term();

	if (token == "<>" || token == "=" || token == "<=" || token == ">=" || token == "<" || token == ">"){
		expresses();
   }  
}

void Compiler::expresses() {     // stage 1, production 10
	string x = "";
	string operand1, operand2;
	if (token != "=" && token != "<>" && token != "<=" && token != ">=" && token != "<" && token != ">")
		processError("\"=\", \"<>\", \"<=\", \">=\", \"<\", or \">\" expected");

	pushOperator(token);
	nextToken();

	if (token != "not" && token != "true" && token != "false" && token != "(" && token != "+"
		&& token != "-" && !isInteger(token) && !isNonKeyId(token))
		processError("\"not\", \"true\", \"false\", \"(\", \"+\", \"-\", integer, or non - keyword identifier expected");
	else term();

	operand1 = popOperand();
	operand2 = popOperand();

	code(popOperator(), operand1, operand2);

	if (token == "<>" || token == "=" || token == "<=" || token == ">=" || token == "<" || token == ">")
		expresses();
}

void Compiler::term() { //double check this function
	if (token == "not" ||isBoolean(token) || token == "(" 
		|| token == "+" || token == "-" || isInteger(token) || isNonKeyId(token)){
		factor();
		terms();
	}
	else {
		processError("'not', '(', '+', '-', boolean literal, integer literal, or non-keyword identifier expected");
	}
}

void Compiler::terms() {
	if (AddLevelOp(token)) {
		pushOperator(token);
		nextToken();
		factor();
		string lhs = popOperand().substr(0, 15);
		string rhs = popOperand().substr(0, 15);
		code(popOperator(), lhs, rhs);
		terms();
	}
	else if (RelOp(token) || token == ";" || token == ")")
	{
		//do nothing
	}
	else { 
   processError(" \"or\", \"+\", \"-\" expected"); 
   }
}

void Compiler::factor() {         // stage 1, production 13


	if (!(token == "not" || isBoolean(token) || token == "(" || token == "+" || token == "-" || isInteger(token) || isNonKeyId(token))) {
		processError("'not', '(', '+', '-', boolean literal, integer literal, or non-keyword identifier expected");
	}
	part();

	factors();

}
void Compiler::factors() {        // stage 1, production 14
	if (MultLevelOp(token)) {
		pushOperator(token);
		nextToken();
		part();
		string rhs = popOperand();
		string lhs = popOperand();
		code(popOperator(), rhs, lhs);
		factors();
	}

}

void Compiler::part()			// stage 1, production 15
{
	if (token == "not")
	{
		nextToken();

		if (token == "(")
		{
			nextToken();
			express();

			if (token != ")"){
				processError(" \")\" expected ");
         }
			code("not", popOperand().substr(0, 15));
		}

		else if (isNonKeyId(token)){
			code("not", token);
      }
		else if (isBoolean(token)){
		
			if (token == "true"){	// not true = false
				pushOperand("false");
         }
			else{					// not false = true
				pushOperand("true");
         }
		}

		else{
			processError("'(', boolean literal, or non-keyword identifier expected");
      }
	}

	else if (token == "+")
	{
		nextToken();

		if (token == "(")
		{
			nextToken();
			express();

			if (token != ")"){
				processError("')' expected: ");
         }
		}

		else if (isNonKeyId(token) || isInteger(token)){
			pushOperand(token);
      }
		else{
			processError("'(', integer literal, or non-keyword identifier expected");
      }
	}

	else if (token == "-")
	{
		nextToken();

		if (token == "(")
		{
			nextToken();
			express();

			if (token != ")"){
				processError("')' expected ");
         }
			code("neg", popOperand().substr(0, 15));
		}

		else if (isNonKeyId(token)){
			code("neg", token);
      }
		else if (isInteger(token)){
			pushOperand('-' + token);
      }
		else{
			processError("'(', integer literal, or non-keyword identifier expected");
      }
	}

	else if (isNonKeyId(token) || isInteger(token) || isBoolean(token)){
		pushOperand(token);
   }
	else if (token == "(")
	{
		nextToken();
		express();

		if (token != ")"){
			processError("')' expected");
      }
	}

	else{
		processError("'not', '(', '+', '-', boolean literal, integer literal, or non-keyword identifier expected");
   }
	nextToken();
}




//////////////////////////////
void Compiler::pushOperator(string op) //push name onto operatorStk
{
	// push name onto stack;
	operatorStk.push(op);
}

void Compiler::pushOperand(string operand) { //push name onto operandStk
 //if name is a literal, also create a symbol table entry for it

 // if name is a literal and has no symbol table entry
 // insert symbol table entry, call whichType to determine the data type of the literal
 // push name onto stack;
	if (isLiteral(operand) && symbolTable.find(operand) == symbolTable.end()) {
		insert(operand, whichType(operand), CONSTANT, whichValue(operand), YES, 1);
	}
	operandStk.push(operand);


}

string Compiler::popOperator() //pop name from operatorStk
{
   string temp;
	// if operatorStk is not empty
	// return top element removed from stack;
	// else
	// processError(compiler error; operator stack underflow)
	if (!operatorStk.empty()) {
		temp = operatorStk.top();
		operatorStk.pop();
		
	}else{
	processError("operator stack underflow");
   }
   
   return temp;
}

string Compiler::popOperand() //pop name from operandStk
{
   string temp;
	// if operandStk is not empty
	// return top element removed from stack;
	// else
	// processError(compiler error; operand stack underflow)
	if (!operandStk.empty()) {
		temp = operandStk.top();
		operandStk.pop();
		
	}else {
	processError("operand stack underflow");
   }
   return temp;
}







