/*****************************************************************************/
/**   Ejemplo de un posible fichero de cabeceras donde situar las           **/
/** definiciones de constantes, variables y estructuras para MenosC. Los    **/
/** alumnos deberan adaptarlo al desarrollo de su propio compilador.    **/
/*****************************************************************************/
#ifndef _HEADER_H
#define _HEADER_H

/****************************************************** Constantes generales */
#define TRUE  1
#define FALSE 0

#define TALLA_TIPO_SIMPLE 1 /* Talla asociada a los tipos simples */
#define TALLA_SEGENLACES 2 /* Talla del segmento de Enlaces de Control */

typedef struct lc /******************************** Estructura para la lista de campos*/
{
  int   ref;             /* Campo de referencia de usos multiples            */
  int   talla;             /* Campo de referencia de usos multiples            */
} LC;

typedef struct lpf /******************************** Estructura para la lista de campos*/
{
  int   ref;             /* Campo de referencia de usos multiples            */
  int   talla;             /* Campo de referencia de usos multiples            */
} LPF;


typedef struct exp{
   int t;                /*  Tipo de la expresion */
   int d;                /*  Desplazamiento de la expresion */
} EXPRE;

typedef struct inst{
   int ini;               /*Inicio*/
   int lf;                /*Lista falso */
   int lv;                /*Lista verdad*/
   int fin;               /*Fin*/
} INST;



/************************ Variables externas definidas en Programa Principal */
extern int verTdS;             /* Flag para saber si mostrar la TdS */

/************************************* Variables externas definidas en el AL */
extern int yylex();
extern int yyparse();

extern FILE *yyin;                           /* Fichero de entrada           */
extern int   yylineno;                       /* Contador del numero de linea */
extern char *yytext;                         /* Patron detectado             */
/********* Funciones y variables externas definidas en el Programa Principal */
extern void yyerror(const char * msg) ;   /* Tratamiento de errores          */

extern int verbosidad;                   /* Flag si se desea una traza       */
extern int numErrores;              /* Contador del numero de errores        */

#endif  /* _HEADER_H */
/*****************************************************************************/
