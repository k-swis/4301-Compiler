// David Roberts & Brett Hedden
// CS 4301
// Stage 0

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

string		token; // the next token
char		ch; // the next character of the source file

uint		errorCount = 0; // total number of errors encountered
uint		lineNo = 0; // line numbers for the listing
int currentTempNo = -1; // current temp number
int maxTempNo = -1; // max temp number
string contentsOfAReg; // symbolic contents of A register


 // Lexical routines
 char nextChar(); // returns the next character or END_OF_FILE marker
 string nextToken(); // returns the next token or END_OF_FILE marker

 // Other routines
 string genInternalName(storeTypes stype);
 void processError(string err);
 map<string, SymbolTableEntry>::iterator i;
 void freeTemp();
 string getTemp();
 string getLabel();
 bool isTemporary(string s); // determines if s represents a temporary
 
  

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
	listingFile << "LINE NO." << setw(30) <<  "SOURCE STATEMENT" << endl;
	
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
   //a call to nextToken() has two effects
	// (1) the variable, token, is assigned the value of the next token
	// (2) the next token is read from the source file in order to make
	// the assignment. The value returned by nextToken() is also
	// the next token.
   prog();
   //parser implements the grammar rules, calling first rule
 
}
void Compiler::createListingTrailer()
{
	//print "COMPILATION TERMINATED", "# ERRORS ENCOUNTERED"
	listingFile << "COMPILATION TERMINATED        " << errorCount << " ERRORS ENCOUNTERED" << endl;
}
void Compiler::processError(string err)
{
	//Output err to listingFile
   listingFile << endl << err << endl;
   listingFile << endl << "Error: exiting program" << endl;
   exit(0);
	//Call exit() to terminate program
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
void Compiler::constStmts() //token should be NON_KEY_ID
{
   
	string x,y;
	if (!isNonKeyId(token))
	{
      
		processError("non-keyword identifier expected");
	}
	x = token;
	if (nextToken() != "=")
	{
		processError("\"=\" expected");
	}
	y = nextToken();
	if (y!= "+" && y!= "-" && y!= "not" && !isNonKeyId(y)&& y!="true" && y!="false" && !isInteger(y))
	{
		processError("token to right of \"=\" illegal");
	}
	if (y== "+" || y=="-")
	{
		if (!isInteger(nextToken()))
		{
			processError("integer expected after sign");
		}
		y = y + token;
	}
	if (y == "not")
	{
		if (nextToken() != "true" && token != "false")
		{
			processError("boolean expected after not");
		}
		if (token == "true")
		{
			y = "false";
		}
		else
		{
			y = "true";
		}
	} 
	if (nextToken() != ";")
	{
		processError("semicolon expected");
	}
   
	if (symbolTable.find(y)->second.getDataType() != INTEGER && symbolTable.find(y)->second.getDataType() != BOOLEAN)
		processError("data type of token on the right-hand side must be INTEGER or BOOLEAN");
	insert(x,whichType(y),CONSTANT,whichValue(y),YES,1);
	x = nextToken();
	if (x != "begin" && token != "var" && !isNonKeyId(x))
	{
		processError("non-keyword identifier, \"begin\", or \"var\" expected");
	}
	if (isNonKeyId(token))
	{
		constStmts();
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
		processError("':' expected");
	}
	if (nextToken() != "integer" && token != "boolean")
	{
		processError("illegal type follows ':'");
	}
	y = token;
   
	if (nextToken() != ";")
	{
		processError("semicolon expected");
	}
	insert(x,whichType(y),VARIABLE,"",YES,1);
   
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
void Compiler::insert(string externalName,storeTypes inType, modes inMode, string inValue,allocation inAlloc, int inUnits){ //!
 //create symbol table entry for each identifier in list of external names
 //Multiply inserted names are illegal
//symbolTable.find()   returns iterator != symboltable.end()
string name = externalName;

   if (symbolTable.count(name) >= 1){ //map functions
      processError("multiple name definition");
   }
   else if (isKeyword(name)){
      processError("illegal use of keyword");
   }
   else //create table entry
   {
   
      if(isupper(name.at(0))){
   
         symbolTable.emplace(name, SymbolTableEntry(name, inType,inMode,inValue,inAlloc,inUnits)); 
         
      }
      else{

         symbolTable.emplace(name, SymbolTableEntry(genInternalName(inType), inType,inMode,inValue,inAlloc,inUnits));
      }
   }
  
}


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



storeTypes Compiler::whichType(string name) {
   //tells which data type a name has
storeTypes type;
 if (isLiteral(name))
 {
   if (isBoolean(name))
   {
      type = BOOLEAN;
   }
   else
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
      
      processError("reference to undefined variable");
   }
   
 }
 return type;
}
string Compiler::whichValue(string name){ //tells which value a name has

string value;
 if (isLiteral(name))
 {
   value = name;
 }
 else //name is an identifier and hopefully a constant
 {
   if (symbolTable.find(name) != symbolTable.end()) //here
   {
     value = symbolTable.find(name)->second.getValue();
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
		emitEpilogue(operand2);
	}
	else if (op == "read")
	{
		emitReadCode();
	}
	else if (op == "write")
	{
		emitWriteCode();
	}
	else if (op == "+") // this must be binary '+'
	{
		emitAdditionCode(operand1, operand2);
	}
	else if (op == "-") // this must be binary '-'
	{
		emitSubtractionCode(operand1, operand2);
	}
	else if (op == "neg") // this must be unary '-'
	{
		emitNegationCode(operand1);
	}
	else if (op == "not")
	{
		emitNotCode(operand1);
	}
	else if (op == "*")
	{
		emitMultiplicationCode(operand1, operand2);
	}
	else if (op == "div")
	{
		emitDivisionCode(operand1, operand2);
	}
	else if (op == "mod")
	{
		emitModuloCode(operand1, operand2);
	}
	else if (op == "and")
	{
		emitAndCode(operand1, operand2);
	}
	//…
	else if (op == "=")
	{
		emitEqualityCode(operand1, operand2);
	}
	else if (op == ":=")
		
	{
		emitAssignCode(operand1, operand2);
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
 objectFile << ";" << comment << endl;
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
         emit(i->second.getInternalName(), "dd",i->second.getValue(), i->first);
         
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

void Compiler::emitAdditionCode(string operand1,string operand2) //add operand1 to operand2
{
	/*
	if type of either operand is not integer
		processError(illegal type)
	if A Register holds a temp not operand1 nor operand2 then
		emit code to store that temp into memory
		change the allocate entry for the temp in the symbol table to yes
		deassign it
	if A register holds a non-temp not operand1 nor operand2 then deassign it
		if neither operand is in A register then
			emit code to load operand2 into A register
			emit code to perform register-memory addition
			deassign all temporaries involved in the addition and free those names for reuse
	A Register = next available temporary name and change type of its symbol table entry to integer
	push the name of the result onto operandStk
	*/
}
void Compiler::emitDivisionCode(string operand1,string operand2) //divide operand2 by operand1
{
	/*
	if type of either operand is not integer
		processError("illegal type");
	if A Register holds a temp not operand2 then
		emit code to store that temp into memory
		change the allocate entry for it in the symbol table to yes
		deassign it
	if A register holds a non-temp not operand2 then deassign it
		if operand2 is not in A register
			emit instruction to do a register-memory load of operand2 into the A register
			emit code to extend sign of dividend from A register to edx:eax
			emit code to perform a register-memory division
			deassign all temporaries involved and free those names for reuse
	A Register = next available temporary name and change type of its symbol table entry to integer
	push name of result onto operandStk;
	*/
}
void Compiler::emitAndCode(string operand1,string operand2) //and operand1 to operand2
{
	/*
	if type of either operand is not boolean
		processError("illegal type");
	if A Register holds a temp not operand1 nor operand2 then
		emit code to store that temp into memory
		change the allocate entry for the temp in the symbol table to yes
		deassign it
	if A register holds a non-temp not operand1 nor operand2 then deassign it
		if neither operand is in A register then
			emit code to load operand2 into A register
			emit code to perform register-memory and
			deassign all temporaries involved in the addition and free those names for reuse
	A Register = next available temporary name and change type of its symbol table entry to boolean
	push the name of the result onto operandStk
	*/
}
void Compiler::emitEqualityCode(string operand1,string operand2) //test whether operand2 equals operand1
{
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
}
void Compiler::emitAssignCode(string operand1,string operand2) //assign the value of operand1 to operand2
{
	/*
	if types of operands are not the same
		processError(incompatible types)
	if storage mode of operand2 is not VARIABLE
		processError(symbol on left-hand side of assignment must have a storage mode of VARIABLE)
	if operand1 = operand2 return
		if operand1 is not in A register then
			emit code to load operand1 into the A register
			emit code to store the contents of that register into the memory location pointed to by
			operand2
			designate that contents of A register now contains operand2
	if operand1 is a temp then free its name for reuse
	//operand2 can never be a temporary since it is to the left of ':='
	*/
}
void Compiler::emitReadCode(string operand, string operand2)
{
	/*
	string name
	while (name is broken from list (operand) and put in name != "")
	{
		if name is not in symbol table
			processError("reference to undefined symbol");
		if data type of name is not INTEGER
			processError("can't read variables of this type");
		if storage mode of name is not VARIABLE
			processError("attempting to read to a read-only location");
		emit code to call the Irvine ReadInt function
		emit code to store the A register at name
		set the contentsOfAReg = name
	}
	*/
}
void Compiler::emitWriteCode(string operand, string operand2)
{
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
}

///////////////////////////////////////////////////////////////////////////////
string Compiler::nextToken() {

 token = "";
 while (token == "")
 {
   if(ch == '{') //process comment
	{
      nextChar();
		while (ch != END_OF_FILE && ch != '}')
      {   
         nextChar();
      }
      if (ch == END_OF_FILE)
      {
         processError("unexpected end of file");
      }else{
      nextChar();
      }
      
   }
   else if(ch == '}')
   { 
		processError("'}' cannot begin token");
   }
   else if(isspace(ch))
   {
		nextChar();
   }
   else if(isSpecialSymbol(ch)) 
   {
		token = ch;
      nextChar();
   }
   else if(islower(ch))
   {
		token = ch;
      nextChar();
      while (islower(ch) || isdigit(ch) || ch == '_')
      {
         token+=ch;
         nextChar();
      }
      if (ch == END_OF_FILE)
      {
         processError("keyword \"program\" expected");
      }
   }
   else if(isdigit(ch))
   {
		token = ch;
      while (isdigit(nextChar()) && nextChar() != END_OF_FILE) //changed this
      {
        token+=ch;
      }
      if (ch == END_OF_FILE)
		{
         processError("keyword \"program\" expected");
		}
   }
   else if(ch == END_OF_FILE)
   {
		token = ch;
	}	
   else
   {
		processError("illegal symbol");
   }
   
   }  
   
   return token;
 }




char Compiler::nextChar() { //returns the next character or end of file marker
	sourceFile.get(ch);
	
	static char prevCh = '\n';
	
	if (sourceFile.eof()){
		ch = END_OF_FILE;
		return ch;
	} else {
		if (prevCh == '\n') {
			listingFile << setw(5) << ++lineNo << '|';
		}
		listingFile << ch;
	}
	
	prevCh = ch;
	return ch;
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
	
   // for(uint k = 0; k <= s.size(); k++){
      // if(isdigit(s(k))){
         // return true;
      // }
      // else if(s(0) = "+" || s(0) = "-" && isdigit(s(k))){
         // return true;
      // }
      // else{
         // return false;
      // }
      
      
   // }
  return isdigit(s[0])||(s[0]=='-'&&isdigit(s[1]))||(s[0]=='+'&&isdigit(s[1]));
  
  
  
   // try {
		// stoi(s);
	// } catch (const invalid_argument& ia) {
		// return false;
	// }
	// return true;
   
}

bool Compiler::isBoolean(string s) const{
	if (s == "true" || s == "false") {
		return true;
	}
	return false;
}

bool Compiler::isLiteral(string s) const{
   return (s == "true" || s == "false" || isdigit(s[0]) || (s[0]=='-'&&isdigit(s[1])) 
            || (s[0]=='+'&&isdigit(s[1])) || s == "integer" || s == "boolean" );
}


bool Compiler::isNonKeyId(string name) const{
   
   return !(isKeyword(name) || isSpecialSymbol(name[0]) || isInteger(name));
}



void Compiler::freeTemp(){
 currentTempNo--;
 if (currentTempNo < -1){
 processError("compiler error, currentTempNo should be ≥ –1");
 }
}
 string Compiler::getTemp(){//similar to getLabel
 string temp;
 currentTempNo++;
 temp = "T" + currentTempNo;
 if (currentTempNo > maxTempNo)
 insert(temp, UNKNOWN, VARIABLE, "", NO, 1);
 maxTempNo++;
 return temp;
 }
 string Compiler::getLabel(){
 //static local variabes started at -1 then increment by 1 then for temp be equal to .L
 static int labelNo = -1;
 string temp;
 labelNo++;
 temp = ".L" + to_string(labelNo);
 return temp;
}
bool Compiler::isTemporary(string s) const{
 // no pseudo
}
