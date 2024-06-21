/*****************************************************************************/
/** Ejemplo de FLEX: S E M - 2           2023-2024 <bvalrod@etsinf.upv.es>**/

/**  LA FUNCION DE RETORNAR ES LA QUE HACE EL ECHO Y REPITE LO QUE LEE SI   **/
/**  ES UN SIMBOLO ACCEPTADO POR EL ANALIZADOR                              **/
/**  (SI -V ESTA ESCRITO EN EL PROMPT)                                      **/
/*****************************************************************************/
%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "asin.h"

#define  retornar(x) {if (verbosidad) ECHO; return (x); }
/*el return devuelve el codigo que le hemos definido en el bison a cada token */
%}
/*-----------------------------------------------------------------------------
  Estas opciones evitan "warnings" innecesarios y permiten obtener el
  numero de linea actual en la variable global "yylineno".
-----------------------------------------------------------------------------*/
%option noinput       
%option nounput       
%option yylineno  

/*-----------------------------------------------------------------------------
  DEFINICIONES/DECLARACIONES DE LAS EXPRESIONES REGULARES
-----------------------------------------------------------------------------*/

delimitador    [ \t\v]+
digito         [0-9]
letra          [A-Za-z]
variable       {letra}({letra}|{digito}|"_")*
constante      {digito}*"."{digito}+|{digito}+"."{digito}*|{digito}+
comentario     \/\/.*\n 

%%
{delimitador}  { if (verbosidad) ECHO; }
"\n"           { if (verbosidad) {ECHO; fprintf(stdout, "%3d.- ", yylineno); } } 
{comentario}   { if (verbosidad) {ECHO; fprintf(stdout, "%3d.- ", yylineno); } }  

struct         { retornar (STRUCT_);   }
int            { retornar (INT_);   }
bool           { retornar (BOOL_);   }
return         { retornar (RETURN_);   }
read           { retornar (READ_);   }
print          { retornar (PRINT_);   }
if             { retornar (IF_);   }
else           { retornar (ELSE_);   }
while          { retornar (WHILE_);   }
true           { retornar (TRUE_);   }
false          { retornar (FALSE_);   }

"+"             { retornar (MAS_); }
"="             { retornar (IGUAL_); }
"+="            { retornar (MASIGUAL_); }
"-="            { retornar (MENOSIGUAL_); }
"*="            { retornar (PORIGUAL_); }
"/="            { retornar (DIVIGUAL_); }
"&&"            { retornar (AND_); }
"||"            { retornar (OR_); }
"=="            { retornar (DOBLEIGUAL_); }
"!="            { retornar (DISTINTO_); }
"-"             { retornar (MENOS_); }
"*"             { retornar (POR_); }
"%"             { retornar (MODULO_); }
"/"             { retornar (DIV_); }
"("             { retornar (ABREPAR_); }
")"             { retornar (CIERRAPAR_); }
">"             { retornar (MAYOR_); }
">="            { retornar (MAYORIGUAL_); }
"<"             { retornar (MENOR_); }
"<="            { retornar (MENORIGUAL_); }
"!"             { retornar (EXCLAMA_); }
"++"            { retornar (DOBLEMAS_); }
"--"            { retornar (DOBLEMENOS_); }
";"             { retornar (PUNTOCOMA_); }
"{"             { retornar (ABRELLAVE_); }
"}"             { retornar (CIERRALLAVE_); }
"]"             { retornar (CIERRALISTA_); }
"["             { retornar (ABRELISTA_); }
","             { retornar (COMA_); }
"."             { retornar (PUNTO_); }

{constante}    { yylval.cent = atoi(yytext); retornar (CTE_); }
{variable}     { yylval.ident = strdup(yytext); retornar (ID_);   }

.	       { yyerror("Caracter desconocido"); }

%%
/*****************************************************************************/
