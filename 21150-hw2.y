%{

#include <stdio.h>

void yyerror(const char * msg)
{
printf("%s\n",msg);
}

%}

%token EQCHECK
%token SEMICOLON
%token IDENT
%token EQ
%token LPAR
%token VAR
%token ELSE
%token IN
%token LBRACE
%token RBRACE
%token PLUS
%token NOT
%token STRING
%token INT
%token FOR
%token LBRKT
%token RBRKT
%token REAL
%token COLON
%token COMMA
%token WHILE
%token RPAR
%token MINUS
%token PERCENT
%token LT
%token STAR
%token GT
%token IF
%token FUNCTION

%left EQCHECK
%left LT GT
%left EQ
%left PLUS MINUS
%left STAR
%left NOT

//%nonassoc THEN
//%nonassoc ELSE

%%

prog: 			 statementList
			 ;
statementList:   	 statementList statement 
	        	 | statementList SEMICOLON statement
			 | statement
			 ;
statement: 		 assign 
                         | if 
                         | expr 
                         | statementBlock 
			 | while 
                         | for 
                         | functionCall 
                         | functionDeclaration 
			 ;
assign:	         	 IDENT EQ expr 
			 | VAR IDENT EQ expr
			 ; 
if:			 ifPart elsePart
			 ; 
ifPart: 		 IF LPAR expr RPAR statementBlock
			 ; 
elsePart: 		 ELSE statementBlock
                         |
			 ;
/*if:			 IF LPAR expr RPAR THEN statementBlock
			 | IF LPAR expr RPAR statementBlock ELSE statementBlock
			 ;*/
while: 		 	 WHILE LPAR expr RPAR statementBlock
			 ; 
for: 			 FOR LPAR IDENT IN expr RPAR statementBlock 
			 | FOR LPAR VAR IDENT IN expr RPAR statementBlock 
			 ;
functionDeclaration: 	 FUNCTION IDENT LPAR exprList RPAR statementBlock 
			 | FUNCTION IDENT LPAR RPAR statementBlock
			 ;
statementBlock:		 LBRACE statementList RBRACE
			 ; 
functionCall: 		 IDENT LPAR exprList RPAR 
			 | IDENT LPAR RPAR
			 ; 
expr: 			 IDENT 
                         | REAL 
                         | INT 
                         | STRING 
			 | LBRKT RBRKT 
                         | LBRKT exprList RBRKT 
			 | LBRACE RBRACE 
                         | LBRACE propertyList RBRACE 
			 | NOT expr 
                         | expr PLUS expr 
                         | expr MINUS expr 
                         | expr STAR expr 
			 | expr EQCHECK expr
			 | expr LT expr 
                         | expr GT expr
			 ; 
exprList: 		 expr 
                         | exprList COMMA expr
			 ; 
propertyList: 		 IDENT COLON expr 
                         | propertyList COMMA IDENT COLON expr
			 ;

%%

int main (){

if(yyparse()){
printf("There is an error in the input\n");
}

else {
printf("The input is grammatically correct\n");
}

}
