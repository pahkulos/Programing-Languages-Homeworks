%{
	int isOP_OC=0;
%}
%%
[aA][nN][dD]         		{ printf("KW_AND\n"); }
[oO][rR]          			{ printf("KW_OR\n"); }
[nN][oO][tT]         		{ printf("KW_NOT\n"); }
[eE][qQ][uU][aA][lL]        { printf("KW_EQUAL\n"); }
[lL][eE][sS][sS]        	{ printf("KW_LESS\n"); }
[nN][iI][lL]       	 		{ printf("KW_NIL\n"); }
[lL][iI][sS][tT]        	{ printf("KW_LIST\n"); }
[aA][pP][pP][eE][nN][dD]    { printf("KW_APPEND\n"); }
[cC][oO][nN][cC][aA][tT]    { printf("KW_CONCAT\n"); }
[sS][eE][tT]        		{ printf("KW_SET\n"); }
[dD][eE][fF][fF][uU][nN]    { printf("KW_DEFFUN\n"); }
[fF][oO][rR]        		{ printf("KW_FOR\n"); }
[iI][fF]          			{ printf("KW_IF\n"); }
[eE][xX][iI][tT]       	 	{ printf("KW_EXIT\n"); }
[lL][oO][aA][dD]       		{ printf("KW_LOAD\n"); }
[dD][iI][sS][pP]        	{ printf("KW_DISP\n"); }
[tT][rR][uU][eE]        	{ printf("KW_TRUE\n"); }
[fF][aA][lL][sS][eE]       	{ printf("KW_FALSE\n"); }

\+          { printf("OP_PLUS\n"); }
\-          { printf("OP_MINUS\n"); }
\/          { printf("OP_DIV\n"); }
\*          { printf("OP_MULT\n"); }
\(          { printf("OP_OP\n"); }
\)          { printf("OP_CP\n"); }
\*\*        { printf("OP_DBMULT\n"); }
\"			{ isOP_OC == 0 ? printf("OP_OC\n") : printf("OP_CC\n"); isOP_OC = !isOP_OC; }
,           { printf("OP_COMMA\n"); }
;;(.*)                  { printf("COMMENT\n"); }
(0)|([1-9][0-9]*)       { printf("VALUE\n"); }
[a-zA-Z][0-9a-zA-Z]*    { printf("IDENTIFIER\n"); }

[0-9]+[a-zA-Z]+			{ printf("SYNTAX_ERROR %s cannot be tokenized!\n",yytext);}

[ \t\r]	{} /*space is ignored*/
%%
int yywrap(){}
FILE* openFileToWriteInput();
FILE* openFileToRead();

int main(int argc, char **argv)
{
	FILE* out = openFileToWriteInput();
	char line[50];
	while(1){
   		fgets(line, 50, stdin);
		if(strcmp(line,"(exit)\n")==0)
			break;
		fprintf(out,"%s",line);
	}
	printf("\n");
	fclose(out);

	yyin=openFileToRead();
	yylex();
	remove("file.txt");
}

FILE* openFileToWriteInput()
{
	char *fileName="file.txt";
	FILE* out = NULL;
	if(!(out = fopen(fileName,"w")))
		fprintf(stderr,"ERROR: No such file: %s",fileName);

	return out;
}
FILE* openFileToRead()
{
	char *fileName="file.txt";
	FILE* in=NULL;
	if(!(in = fopen(fileName,"r")))
		fprintf(stderr,"ERROR: File couldn't be opened: %s\n",fileName);	

	return in;
}