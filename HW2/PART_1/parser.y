%{
    #include "gpp_implementations.h"
    #include <stdio.h>
	int dblmult(int base, int exponent);
%}
%union { char str[16]; int val; void *arr; }
%start INPUT
%token KW_AND
%token KW_OR
%token KW_NOT
%token KW_EQUAL
%token KW_NIL
%token KW_APPEND
%token KW_CONCAT
%token KW_SET
%token KW_IF
%token KW_TRUE
%token KW_FALSE
%token KW_LIST
%token KW_EXIT
%token OP_PLUS
%token OP_MINUS
%token OP_DIV
%token OP_MULT
%token OP_DBLMULT
%token OP_OP
%token OP_CP
%token <val> VALUE
%token <str> IDENTIFIER

%type <val> INPUT
%type <val> EXIT
%type <val> EXPI
%type <val> EXPB
%type <arr> EXPLISTI
%type <arr> LISTVALUE
%type <arr> VALUES
%%

INPUT:
    EXPI    { printf("\nResult: %d\n", $1); }
    |
	EXPB	{ if($1==1) {printf("\nTRUE\n");} else{ printf("\nFALSE\n");} }
	|
    EXPLISTI { printf("\nResult: "); print($1); };
	|
	INPUT EXPI { printf("\nResult: %d\n", $2); }
	|
	INPUT EXPB { if($2==1) {printf("\nTRUE\n");} else{ printf("\nFALSE\n");} }
	|
	INPUT EXPLISTI { printf("\nResult: "); print($2); }
	|
	INPUT EXIT
    ;

EXPI:
	OP_OP OP_PLUS EXPI EXPI OP_CP {$$ = $3 + $4;}	/* EXPI -> (+ EXPI EXPI)*/
	|
	OP_OP OP_MINUS EXPI EXPI OP_CP {$$ = $3 - $4;}	/* EXPI -> (- EXPI EXPI)*/
	|
	OP_OP OP_MULT EXPI EXPI OP_CP {$$ = $3 * $4;}	/* EXPI -> (* EXPI EXPI)*/
	|
	OP_OP OP_DIV EXPI EXPI OP_CP {$$ = $3 / $4;}	/* EXPI -> (/ EXPI EXPI)*/
	|
	OP_OP OP_DBLMULT EXPI EXPI OP_CP {$$ = dblmult($3,$4);}	/* EXPI -> (** EXPI EXPI)*/
	|
	IDENTIFIER { int res = setGet($1); if(res == -1) $$ = 1; else $$ = res; }/* IDENTIFIER*/
	|
	VALUE {$$ = $1;}	/*VALUE*/
	|
	OP_OP KW_SET IDENTIFIER EXPI OP_CP { $$ = $4; setAdd($3, $4); } /*/* EXPI -> (set IDENTIFIER EXPI)*/
	|
	OP_OP KW_IF EXPB EXPI OP_CP { $$ = $3 == 1 ? $4 : 0; } /* EXPI -> (if (and (equal 1 1) (equal 2 2)) EXPI)*/
	|
	OP_OP KW_IF EXPB EXPI EXPI OP_CP { $$ = $3 == 1 ? $4 : $5; } /* EXPI -> (if (and (equal 1 1) (equal 2 2)) EXPI EXPI)*/
	;

EXPB:
	OP_OP KW_AND EXPB EXPB OP_CP { $$ = $3 && $4; } /* EXPI -> (and (equal 1 1) (equal 2 2))*/
	|	
	OP_OP KW_OR EXPB EXPB OP_CP { $$ = $3 || $4; }	/* EXPI -> (or (equal 1 1) (equal 2 2))*/
	|
	OP_OP KW_NOT EXPB OP_CP { $$ = !$3;}	/* EXPI -> (not (equal 1 1))*/
	|
	OP_OP KW_EQUAL EXPB EXPB OP_CP { $$ = $3 == $4 ? 1 : 0; }	/* EXPI -> (equal (equal 1 1) (equal 2 2))*/
	|
	OP_OP KW_EQUAL EXPI EXPI OP_CP { $$ = $3 == $4 ? 1 : 0; }	/* EXPI -> (equal 1 1)*/
	|
	KW_TRUE { $$ = 1; }
	|
	KW_FALSE { $$ = 0; }
	;

EXPLISTI:
	OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP { $$ = concatList($3, $4); }/*(concat (list 1 2) (list 3 4))*/
	|
	OP_OP KW_APPEND EXPI EXPLISTI OP_CP { $$ = appendList($4, $3); }/*(append 3 (list 3 4))*/
	|
	LISTVALUE { $$ = $1; }	
	;

LISTVALUE:
	OP_OP KW_LIST VALUES OP_CP { $$ = $3; }	/*(list 3 4)*/
	|
	OP_OP KW_LIST OP_CP { $$ = createList(); }/*(list)*/
	|
	KW_NIL { $$ = NULL; }	/*NULL*/
	;

VALUES:
	VALUES VALUE { $$ = addList($1, $2); }
	|
	VALUE { $$ = addList(NULL, $1); }
	;

EXIT:
	OP_OP KW_EXIT OP_CP {exit(0);}/*(exit)*/
	;

%%
int dblmult(int base, int exponent)	/*DBLMULT*/
{
	int result=1;
	for (exponent; exponent>0; exponent--)
	{
		result = result * base;
	}
	return result;
}
int yyerror(char* str) 
{ 
	printf("SYNTAX_ERROR Expression not recognized\n");
	return 0; 
}

int main(){
	createSet();	/*HASHSET*/
    yyparse();
}