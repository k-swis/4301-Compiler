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

string		token; // the next token
char		ch; // the next character of the source file

uint		errorCount = 0; // total number of errors encountered
uint		lineNo = 0; // line numbers for the listing


// // Prototypes
// bool		isKeyword(string s); // determines if s is a keyword
// bool		isSpecialSymbol(char c); // determines if c is a special symbol
// bool		isNonKeyId(string s); // determines if s is a non_key_id
// bool		isInteger(string s) ; // determines if s is an integer
// bool		isBoolean(string s) ; // determines if s is a boolean
// bool		isLiteral(string s) ; // determines if s is a literal
// bool     isInSymbolTable(string);

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
 char nextChar(); // returns the next character or END_OF_FILE marker
 string nextToken(); // returns the next token or END_OF_FILE marker

 // Other routines
 string genInternalName(storeTypes stype);
 void processError(string err);
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
	listingFile << "COMPILATION TERMINATED" << errorCount << "ERRORS ENCOUNTERED" << endl;
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
	if (y!= "+" && y!="-" && y!="not" && !isNonKeyId(y)&& y!="true" && y!="false" && !isInteger(y))
	{
		processError("token to right of \"=\" illegal");
	}
	if (y== "+" || y=="-")
	{
		if (isInteger(nextToken()))
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
	if (isInteger(y) != true && isBoolean(y) != true)
		processError("data type of token on the right-hand side must be INTEGER or BOOLEAN");
	insert(x,whichType(y),CONSTANT,whichValue(y),YES,1);
	x = nextToken();
	if (nextToken() != "begin" && token != "var" && !isNonKeyId(token))
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
	if (isNonKeyId(token))
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
	insert(x,whichType(y),CONSTANT,whichValue(y),YES,1);
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

 	// for(uint i = 0, word = 0; i < externalName.length(); i++) {
		// if(externalName[i] == ',') {
			// word++;
			// continue;
		// }
		// name.at(word) += externalName[i];
	// }
 
   
      if (symbolTable.count(name) >= 1){ //map functions
         processError("multiple name definition");
      }
      else if (isKeyword(name)){
         processError("illegal use of keyword");
      }
      else //create table entry
      {
        
         
         
         if(isupper(name.at(0))){
            
            // newentry.setInternalName = (name[i]);
            
            // newentry.externalName = name[i].substr(0,15);
            // newentry.setDataType = inType;
            // newentry.setMode = inMode;
            // newentry.setValue = inValue;
            // newentry.setAlloc = inAlloc;
            // newentry.setUnits = inUnits;
            
            symbolTable.emplace(name, SymbolTableEntry(name, inType,inMode,inValue,inAlloc,inUnits)); 
            
         }
         else{
            
            // newentry.setInternalName(genInternalName(inTypes));
            // SymbolTableEntry newentry;
            // newentry.externalName = name[i].substr(0,15);
            // newentry.setDataType = inType;
            // newentry.setMode = inMode;
            // newentry.setValue = inValue;
            // newentry.setAlloc = inAlloc;
            // newentry.setUnits = inUnits;
            
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
 objectFile << setw(8) << instruction;
 objectFile << setw(24) << operands;
 objectFile << setw(8) << comment;
}
void Compiler::emitPrologue(string progName, string operand2){
 // Output identifying comments at beginning of objectFile
 objectFile << progName;
 // Output the %INCLUDE directives
 emit("SECTION", ".text");
 emit("global", "_start", "", "; program" + progName);
 emit("_start:");
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
            emit(i->second.getInternalName(), "resd",i->second.getValue(), i->first);
         }
   }
}
///////////////////////////////////////////////////////////////////////////////
string Compiler::nextToken() {

 token = "";
 while (token == "")
 {
   if(ch == '{') //process comment
	{
		while (nextChar() != END_OF_FILE && nextChar() != '}')
      {   
         nextChar();
      }
      if (ch == END_OF_FILE)
      {
         processError("unexpected end of file");
      }
      nextChar();
      
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
      while (islower(nextChar()) || isdigit(ch) || ch == '_')
      {
         token+=ch;
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

bool Compiler::isInteger(string s) const{  
	return isdigit(s[0]);
}

bool Compiler::isBoolean(string s) const{
	if (s == "true" || s == "false") {
		return true;
	}
	return false;
}

bool Compiler::isLiteral(string s) const{
   return (s == "true" || s == "false" || isdigit(s[0]) || (s[0]=='-'&&isdigit(s[1])) || (s[0]=='+'&&isdigit(s[1])));
}


bool Compiler::isNonKeyId(string name) const{
   return !(isKeyword(name) || isSpecialSymbol(name[0]));
}
