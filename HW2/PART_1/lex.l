%{
	int isOPenC=0;
	#include "parser.tab.h"
%}
%%

[aA][nN][dD]					return KW_AND;
[oO][rR]						return KW_OR;
[nN][oO][tT]					return KW_NOT;
[eE][qQ][uU][aA][lL] 			return KW_EQUAL;
[nN][iI][lL]					return KW_NIL;
[aA][pP][pP][eE][nN][dD]		return KW_APPEND;
[cC][oO][nN][cC][aA][tT]		return KW_CONCAT;
[sS][eE][tT]					return KW_SET;
[iI][fF]						return KW_IF;
[tT][rR][uU][eE]				return KW_TRUE;
[fF][aA][lL][sS][eE]			return KW_FALSE;
[eE][xX][iI][tT]				return KW_EXIT;
[lL][iI][sS][tT]				return KW_LIST;
[+] 							return OP_PLUS;
[-] 							return OP_MINUS;
[/] 							return OP_DIV;
[*] 							return OP_MULT;
[*][*] 							return OP_DBLMULT;
[(] 							return OP_OP;
[)] 							return OP_CP;

								

[a-zA-Z_][a-zA-Z0-9_]*			{strcpy(yylval.str, yytext); return IDENTIFIER;}

[0]|[1-9][0-9]*					{yylval.val=atoi(yytext); return VALUE;}



[\n]							
[ \t]+							
.								return 0;

%% 
int yywrap(){}
