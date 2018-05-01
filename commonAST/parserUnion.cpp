#include "assert.h"
#include "parserUnion.h"

using namespace std;

//prototypes
bool printDebug = false;
vector<string> noLevelBreak;

class Parser{
	public:
	
		Parser(const string& filename) : file(filename.c_str()) {};

		Parser(const string& filename, map<string, vector<string > > nodesToCount): file(filename.c_str()) {
			visitor = CounterVisitor(nodesToCount);
		}

		ASTNode* parseNode(int parLevel=-1){
			Token* t = getToken();
			if(t->value == "END"){
				return NULL;	
			}else if(t->value == "functionDef"){
				return parseFunction(t);
			}else if(t->value == "variableDecl"){
				return parseVariableDecl(t->level);
			}else if(t->value == "unaryOp"){
				return parseUnOp(t->level);
			}else if(t->value == "ifBlock"){
				return parseIfBlock(t->level);
			}else if(t->value == "ifStatement"){
				return parseIf(t->level);
			}else if(t->value == "assignment" || t->value == "augAssign"){
				return parseAssignment(t);
			}else if(t->value == "forLoop"){
				return parseFor(t->level);
			}else if(t->value == "switch"){
				return parseSwitch(t->level);
			}else if(t->value == "module"){
				return parseModule();
			}else if(find(noLevelBreak.begin(), noLevelBreak.end(), t->value) != noLevelBreak.end()){
				return parseNoChildren(t, parLevel);
			}else if(t->value == "binaryOp" || t->value == "Call" || t->value == "CXXDeleteExpr"){
				return parseExprChildren(t);
			}else{
				return parseASTNode(t);	
			}
		}

		UnaryOp* parseUnOp(int level){
			UnaryOp* u = new UnaryOp();
			u->type = "unaryOp";	
			if(!isStmt(getLookaheadToken()->value) && getLookaheadToken()->level > level && getLookaheadToken()->value !="END"){
				u->children.push_back(parseNode(level));	
			}

			return u;
		}

		ASTNode* parseSwitch(int level){
			ASTNode* node = new ASTNode();
			node->type = "Switch";
			while(getLookaheadToken()->value != "compoundStmt"){
				node->children.push_back(parseNode(level));
			}
			
			node->children.push_back(parseNode(level));
			return node;
		}


		For* parseFor(int level){
			For* f = new For();
			f->type = "For";
			while(getLookaheadToken()->value != "compoundStmt" && getLookaheadToken()->level > level && getLookaheadToken()->value != "END"){
				f->children.push_back(parseNode(level));	
			}

			if(getLookaheadToken()->value == "compoundStmt"){
				f->children.push_back(parseNode(level));
			}

			return f;
		}

		Module* parseModule(){
			Module* module = new Module();
			module->type = "Module";
			while(getLookaheadToken()->value != "END"){
					module->children.push_back(parseNode());
			}
			return module;
		}


		VariableDecl* parseVariableDecl(int level){
			VariableDecl* vd = new VariableDecl();
			vd->type = "VariableDecl";

			while(getLookaheadToken()->value != "/variableDecl" && getLookaheadToken()->value != "END"){
				vd->children.push_back(parseNode(level));
			}

			//throwaway </variableDecl> token
			getToken();


			//for array declarations
			while(getLookaheadToken()->value == "IntegerLiteral"){
				getToken();
				ASTNode* il = new ASTNode();
				il->type = "IntegerLiteral";	
				vd->children.push_back(il);
			}

			
			return vd;
		}

		
		Assignment* parseAssignment(Token* t){
			Assignment* a = new Assignment();
			a->type = t->value;

			while(!isStmt(getLookaheadToken()->value) && getLookaheadToken()->level > t->level){
				a->children.push_back(parseNode(t->level));
			}

			return a;
		}


		IfBlock* parseIfBlock(int level){
			IfBlock* ib = new IfBlock();
			ib->type = "IfBlock";
			while(getLookaheadToken()->level > level && getLookaheadToken()->value != "END" && 
					(getLookaheadToken()->value == "ifStatement" || getLookaheadToken()->value == "elseStatement")){
				ib->children.push_back(parseNode(level));
			}

			return ib;
		}

		If* parseIf(int level){
			If* i = new If();
			i->type = "If";
			while(getLookaheadToken()->level >= level && getLookaheadToken()->value != "END" 
				&& getLookaheadToken()->value != "ifStatement" && getLookaheadToken()->value != "elseStatement"){
				i->children.push_back(parseNode(level));
			}

			return i;
		}


		Function* parseFunction(Token* t){
			Function* func = new Function();	
			func->type = "Function";
			getToken(); //name of the function 

			while((getLookaheadToken()->level > t->level || (find(noLevelBreak.begin(), noLevelBreak.end(), getLookaheadToken()->value)) != noLevelBreak.end()) 
					&& getLookaheadToken()->value != "END"){
				func->children.push_back(parseNode(t->level));
			}

			return func;
		}

		ASTNode* parseExprChildren(Token* t){
			ASTNode* node = new ASTNode();	
			node->type = t->value;
			
			while(!isStmt(getLookaheadToken()->value) && (getLookaheadToken()->level > t->level || 
				find(noLevelBreak.begin(), noLevelBreak.end(), getLookaheadToken()->value) != noLevelBreak.end())
				&& getLookaheadToken()->value != "END"){
				node->children.push_back(parseNode(t->level));
			}

			return node;

		}

		ASTNode* parseNoChildren(Token* t, int parLevel){
			ASTNode* p = new ASTNode();
			p->type = t->value;

			if(t->level > parLevel && parLevel != -1 && getLookaheadToken()->value != "END"){
				//p->children.push_back(parseNode(t->level));
			}

			return p;
		}

	
		ASTNode* parseASTNode(Token* t){
			ASTNode* node = new ASTNode();
			if(t->value.find("name") != string::npos || t->value.find("base:") != string::npos){ 
				node->type = "Identifier";	
			}else{
				node->type = t->value;
			}

			while(getLookaheadToken()->level > t->level || find(noLevelBreak.begin(), noLevelBreak.end(), getLookaheadToken()->value) != noLevelBreak.end()){
				node->children.push_back(parseNode(t->level));
			}

			return node;
		}


		bool isStmt(string val){
			return (val == "CompoundStmt" || val == "ifBlock" || val == "Function" || val == "If" || val == "For" || val == "do" || val == "While");
		}
		

		Token* getToken(){
			Token* token = new Token();
			file >> ws;
			string word;
			getline(file,word);
			if(word.size() == 0){
				token->value = "END";
				token->level = -1;
				return token;
			}

			token->value = getWord(word);
			token->level = getLevel(word);
			return token;
		}

		Token* getLookaheadToken(){
			Token* token = new Token();
			string word;
			int len = file.tellg();
			file >> ws;
			getline(file,word);
			if(word.size() == 0){
				token->value = "END";
				token->level = -1;
				return token;
			}

			token->value = getWord(word);

			token->level = getLevel(word);
			file.seekg(len ,std::ios_base::beg);	
			return token;

		}

		int getASTNodes() const{
			return visitor.getASTNodes();
		}

		void traverse(ASTNode* node){
			node->accept(visitor);
		}


	private:
		ifstream file;
		CounterVisitor visitor;

		string getWord(string word){
			size_t pos = word.find(",");	
			if(pos == string::npos){
				cout << "ERROR. Word: " << word << " not in format \",level\". Exiting" << endl;
				exit(1);
			}

			return word.substr(1,pos-1);
		}

		int getLevel(string word){

			size_t pos = word.find(",");
			if(pos ==  string::npos){
				cout << "ERROR. Word: " << word << " not in format \",level\". Exiting" << endl;

				exit(1);
			}

			string level = word.substr(pos+1, word.size()-1);
			return atoi(level.c_str());
		}
};


void printASTasJSON(ASTNode* node, int level=0, bool addComma=false){

	list<ASTNode*> children = node->getChildren();

	cout << getIndentation(level);
	cout << "{" << endl;
	cout << getIndentation(level+1);
	cout << "\"children\": ["; 

	list<ASTNode*>::iterator itr;
	list<ASTNode*>::iterator temp;
	for(itr=children.begin(), temp=itr; itr!= children.end(); itr++){
		cout << endl;
		bool comma = (++temp) != children.end();
		printASTasJSON(*itr, level+2, comma);
	}

	if(children.size() > 0){
		cout << getIndentation(level+1);
	}
	cout << "]," << endl;

	node->printNodeAsJSON(level+1);
	cout << getIndentation(level);

	cout << "}";
	if(addComma){
		cout << ",";
	}else{
		cout << endl;
	}
}

void printAST(ASTNode* node, int level=0){

	list<ASTNode*> children = node->getChildren();
	
	node->printNode(level);

	list<ASTNode*>::iterator itr;
	for(itr=children.begin(); itr!= children.end(); itr++){
		printAST(*itr, level+1);

	}

}

int main(int argc, char** argv){

	if(argc < 2){
		cerr << "ERROR: no input file specified" << endl;
		exit(1);
	}

	bool jsonOutput = false;
	
	string inputFile = argv[1];

	map<string, vector<string> > nodesToCount;
	noLevelBreak.push_back("ParmVar");
	noLevelBreak.push_back("FloatingLiteral");
	noLevelBreak.push_back("IntegerLiteral");
	noLevelBreak.push_back("StringLiteral");
	noLevelBreak.push_back("CXXBoolLiteralExpr");
	noLevelBreak.push_back("ImplicitCastExpr");
	noLevelBreak.push_back("DeclRefExpr");
	noLevelBreak.push_back("CXXUnresolvedConstructExpr");



	//skips the name of the excecutable and skips the filename
	argv += 2;

	if(*argv == "json"){
		//hack to skip the for loop
		argv += argc;
	}

	for(int i=2; i<argc; i+=2){

		string itemToCount = *argv;
		argv++;
		if(!(*argv)){
			jsonOutput = (itemToCount == "json");
			break;
		}
		string argsString = *argv;

		vector<string> args;	
		//ADD SOME ERROR HANDLING. MAKE SURE THE USER HAS SOME ARGUMENTS SUCH AS VOID

		if(argsString.find(",") == string::npos){
			args.push_back(argsString);
		}


		while(argsString.find(",") != string::npos){
			int index = argsString.find(",");
			args.push_back(argsString.substr(0,index));
			argsString = argsString.substr(index+1);
		}

		if(nodesToCount.find(itemToCount) == nodesToCount.end()){
			nodesToCount[itemToCount] = args;
		}else{
			//do this later
		}

		argv++;
	}


	if(jsonOutput){
		Parser parser(inputFile);
		ASTNode* root = parser.parseNode();
		printASTasJSON(root);
		exit(0);
	}

	Parser parser(inputFile, nodesToCount);
	ASTNode* root = parser.parseNode();

	parser.traverse(root);


	map<string, vector<string> >::iterator itr;
	for(itr= nodesToCount.begin(); itr != nodesToCount.end(); itr++){
		cout << parser.getASTNodes() << endl;
	}
}

