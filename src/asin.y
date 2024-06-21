/*****************************************************************************/
/** Ejemplo  S E M - 2                   2023-2024 <bvalrod@etsinf.upv.es>**/
/*****************************************************************************/
%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "libtds.h"
#include "libgci.h"
%}
/********************************************************************/
 
%union {
  char *ident; /* Nombre del identificador */
  int cent;    /* Valor de la cte numerica entera */
  LC lc;       /* Estructura para la lista de campos */
  LPF lpf;     /* Estructura para l lista de parametros formales */
  EXPRE exp;   /* Para las expre */
  INST inst;
}

/** Palabras reservadas */
%token       STRUCT_ INT_ BOOL_ RETURN_ READ_ PRINT_ IF_ ELSE_ 
%token       WHILE_ TRUE_ FALSE_

/** Operadores y separadores*/
%token       MAS_ IGUAL_ MASIGUAL_ MENOSIGUAL_ PORIGUAL_ DIVIGUAL_ AND_
%token       OR_ DOBLEIGUAL_ DISTINTO_ MENOS_ POR_ MODULO_ DIV_ ABREPAR_
%token       CIERRAPAR_ MAYOR_ MAYORIGUAL_ MENOR_ MENORIGUAL_ EXCLAMA_
%token       DOBLEMAS_ DOBLEMENOS_ PUNTOCOMA_ ABRELLAVE_ CIERRALLAVE_
%token       ABRELISTA_ CIERRALISTA_ COMA_ PUNTO_

/** Tokens con atributos */
%token<ident> ID_
%token<cent> CTE_

/** Estructuras creadas por nosotros **/
%type<lc> listCamp
%type<lpf> listParamForm paramForm
%type<exp> expre expreLogic expreIgual expreRel expreAd expreMul expreUna expreSufi const

/** El resto **/
%type<cent> tipoSimp listDecla decla declaVar declaFunc 
%type<cent> paramAct listParamAct 
%type<cent> opLogic opIgual opRel opAd opMul opUna opIncre

%%

programa      : {
                  /************* Inicializar las variables globales del compilador */
                  niv = 0;  
                  dvar = 0;
                  si = 0;
                  cargaContexto(niv); 
                  
                  /************* Reservar espacio variables globales del programa */
                  $<inst>$.ini = creaLans(si);
                  emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(-1));

                  /************* Emitir el salto al comienzo de la funci´on ‘‘main’’ */
                  $<inst>$.fin = creaLans(si);
                  emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1));

                  //DUDA
                  //$<inst>$.lf = NULO;
                  //$<inst>$.lv = NULO;

                }
                listDecla
                {
                  /******* Comprobar si el programa tiene ``main'' */
                  if ($2 == 0) {
                    yyerror("No hay declaración de ninguna función main");
                  }
                  
                  /******* Completar reserva espacio para variables globales programa */
                  completaLans($<inst>1.ini, crArgEnt(dvar));
                  
                  /******* Completar salto al comienzo de la funcion ``main'' */ 
                  SIMB simb = obtTdS("main");
                  //$<inst>$.ref3 = simb.d;
                  completaLans($<inst>1.fin, crArgEtq(simb.d/*$<aux>$.ref3*/));

                  //DUDA
                  //$<inst>$.lf = NULO;
                  //$<inst>$.lv = NULO;
                }
              ;

listDecla     : decla { $$ = $1; }
              | listDecla decla { $$ = $1 + $2; }
              ;


decla         : declaVar { $$ = 0; } /*para iniciarla*/
              | declaFunc { $$ = $1;} /*para darle el valor de la funcion*/     
              ;

declaVar      : tipoSimp  ID_  PUNTOCOMA_
                {
                  if(! insTdS($2, VARIABLE, $1, niv, dvar, -1))
                    yyerror("Identificador repetido");
                  //Gestión de memoria T5
                  else 
                    dvar += TALLA_TIPO_SIMPLE;
                }
              | tipoSimp ID_ ABRELISTA_ CTE_ CIERRALISTA_ PUNTOCOMA_ 
                {
                  int nelem = $4;/*numero de elemento*/
                  if(! (nelem > 0)){ // si nelem es <=0
                    yyerror("Talla inapropiada del array");
                    nelem = 0;
                  }
                                    
                  int ref = insTdA($1,nelem); /*inserta en la tabla de array*/
                  if(! insTdS($2, VARIABLE, T_ARRAY, niv, dvar, ref)){
                    yyerror("Identificador repetido");
                  }
                  //Gestión de memoria T5
                  else {
                    dvar += nelem * TALLA_TIPO_SIMPLE; 
                  }
                }    
              | STRUCT_ ABRELLAVE_ listCamp CIERRALLAVE_ ID_ PUNTOCOMA_ 
                {
                  if(! insTdS($5, VARIABLE, T_RECORD, niv, dvar, $3.ref)){
                    yyerror("Identificador repetido");
                  }
                  //Gestión de memoria T5
                  else {
                    dvar += $3.talla /*talla lista camp*/ * TALLA_TIPO_SIMPLE; 
                  }
                }
              ;

tipoSimp      : INT_ { $$ = T_ENTERO; }
              | BOOL_ { $$ = T_LOGICO; }
              ;

listCamp      : tipoSimp ID_ PUNTOCOMA_ 
                {
                  int ref = insTdR(-1, $2, $1, 0);
                  if(ref == -1) 
                    yyerror("Identificador repetido");
                  //Gestión de memoria T5
                  else {
                    $$.ref = ref;
                    $$.talla = TALLA_TIPO_SIMPLE;
                  }
                }
              | listCamp tipoSimp ID_ PUNTOCOMA_
                {
                  int ref = insTdR($1.ref, $3, $2, $1.talla);
                  if(ref == -1) 
                    yyerror("Identificador repetido");
                  //Gestión de memoria T5
                  else {
                    $$.ref = ref;
                    $$.talla += TALLA_TIPO_SIMPLE;
                  }

                }
              ;

declaFunc     : tipoSimp ID_  //BLOQUE LLAMADO
                {/* Gestion del contexto y guardar ``dvar'' */
                  niv++; cargaContexto(niv); $<cent>$=dvar; dvar=0;
                }                
                ABREPAR_ paramForm CIERRAPAR_ 
                {/* Insertar informacion de la funcion en la TdS */
                  if (!insTdS($2, FUNCION, $1, niv-1, si, $5.ref)) {
                    //omega (si) generación de código (ahora de momento ponemos -1)
                    yyerror("Identificador repetido");
                  }
                  /******* Cargar los enlaces de control */   
                  //Emite(push(fp)) fp = sp
                  emite(PUSHFP,crArgNul(),crArgNul(),crArgNul()); //Emite(push(fp))
                  emite(FPTOP,crArgNul(),crArgNul(),crArgNul()); //fp = sp
                  
                  /******* Reserva de espacio para variables locales y temporales */
                  //D.d = CreaLans(omega); Emite(sp = sp+(x));
                  $<cent>$ = creaLans(si);
                  emite(INCTOP,crArgNul(),crArgNul(),crArgEnt(-1)); // INCTOP aumenta la cima de la stack (pila) en x posiciones, como no sabemos la cantidad de posiciones se pone -1
                  // cuando se haga el completa Lans, entonces cambiara el -1 a las posiciones que toca
                }
                ABRELLAVE_ declaVarLocal listInst RETURN_ expre PUNTOCOMA_ CIERRALLAVE_
                {/* Comprobacion de tipos del RETURN */
                if(strcmp($2, "main\0")==0) {
                    $$ = -1;
                  } 
                else {
                    $$ = 0;
                  }
                if ($12.t != T_ERROR){ //Para que solo se compruebe un error
                  if( $1 != $12.t) {
                    yyerror("Comprobación de tipos del return errónea");
                  } 
                }

                
                /******* C´alculo del desplazamiento del valor retorno en el RA */
                //dvr = TSEC + PF.talla+talla(E.t)
                //dvr -> direccion valor retorno
                int dvr = TALLA_SEGENLACES + TALLA_TIPO_SIMPLE + $5.talla;
               
                /******* Completa reserva espacio para variables locales y temporales */
                // emite(dvr = E.d),
                emite(EASIG, crArgPos(niv, $12.d), crArgNul(), crArgPos(niv, -dvr));

                /******* Guardar valor de retorno */
                // completaLAns(D.d, dvar);
                completaLans($<cent>7, crArgEnt(dvar));
                
                /******* Libera el segmento de variables locales y temporales */
                // emite(sp = fp); //TOPFP = La cima de la pila apunta a la misma posici´on que el fp
                emite(TOPFP, crArgNul(), crArgNul(), crArgNul() );
                
                /******* Descarga de los enlaces de control */
                // Emite(fp=pop) // FPPOP = El fp apunta a la misma posición que la cima de la pila
                emite(FPPOP, crArgNul(), crArgNul(), crArgNul() );

                /******* Emite FIN si es ‘‘main’’ y RETURN si no lo es */
                // if (main) Emite(FIN)
                if (strcmp($2, "main\0")==0) {emite(FIN,crArgNul(),crArgNul(),crArgNul());}
                // if (!main) Emite(return(pop))
                else {emite(RET,crArgNul(),crArgNul(),crArgNul());}

                /* Mostrar la informacion de la funcion en la TdS */
                if (verTdS)  mostrarTdS();
                /* Gestion del contexto y recuperar ``dvar'' */
                descargaContexto(niv); niv--; dvar=$<cent>3; /*$cent3 es porque nos  guadabamos $cent$ con el dvar y si contamos es el tercer $*/
                 
                }
              ;

paramForm     : /*cadena vacia*/
                {
                  $$.ref = insTdD(-1, T_VACIO); //esta función calcula automaticamente la talla del segmento de parámetros
                  $$.talla = 0;
                }
              | listParamForm 
                { 
                  $$.ref = $1.ref;
                  $$.talla = $1.talla - TALLA_SEGENLACES; //obras nuestro solo talla

                  
                }
              ;

listParamForm : tipoSimp ID_ 
                {
                  $$.ref = insTdD(-1, $1); //como es el primer parametro es -1
                  int talla = TALLA_TIPO_SIMPLE + TALLA_SEGENLACES;
                  $$.talla = talla;
                  if(!insTdS($2, PARAMETRO, $1, niv, -talla, -1))
                      yyerror("Parámetro ya declarado");
                }
              | tipoSimp ID_ COMA_ listParamForm
                {
                  int talla = $4.talla + TALLA_TIPO_SIMPLE; //lo actualizamos con la talla de lista de parametros que ya habia
                  $$.talla = talla;
                  if(!insTdS($2, PARAMETRO, $1, niv, -talla, -1)) {
                      yyerror("Parámetro ya declarado");
                      $$.ref = $4.ref; $$.talla = $4.talla;
                  }
                  else { $$.ref = insTdD($4.ref, $1); } //insertar en tabla de dominio el tipo de $1
                }
              ;

declaVarLocal : /*cadena vacia*/
              | declaVarLocal declaVar
              ;

listInst      : /*cadena vacia*/
              | listInst inst
              ;

inst          : ABRELLAVE_ listInst CIERRALLAVE_
              | instExpre
              | instEntSal
              | instSelec
              | instIter
              ;

instExpre     : expre PUNTOCOMA_  // expre; 
              | PUNTOCOMA_
              ;

instEntSal    : READ_ ABREPAR_ ID_ CIERRAPAR_ PUNTOCOMA_
                {
                  SIMB simb = obtTdS($3);
                  if (simb.t == T_ERROR) {yyerror("Objeto no está declarado");}
                  else if (!(simb.t == T_ENTERO)) {yyerror("El argumento del \"read\" debe ser \"entero\"");}

                  emite(EREAD, crArgNul(), crArgNul(),crArgPos(simb.n , simb.d));  
                }
              | PRINT_ ABREPAR_ expre CIERRAPAR_ PUNTOCOMA_ 
                { 
                  if ($3.t != T_ERROR)
                    if (!($3.t == T_ENTERO)) {yyerror("El argumento del \"print\" debe ser \"entero\"");}
                
                  emite(EWRITE, crArgNul(), crArgNul(), crArgPos(niv, $3.d)); 
                }

              ;

instSelec     : IF_ ABREPAR_ expre CIERRAPAR_
               {
                  if ($3.t != T_ERROR) {
                    if ($3.t != T_LOGICO){
                      yyerror("La expresión de evaluación de un \"if\" ha de ser de tipo lógico");       
                    }
                    
                  }
                  $<inst>$.lf = creaLans(si);
                  emite(EIGUAL, crArgPos(niv, $3.d), crArgEnt(0), crArgEtq(-1));  //inicializamos omega a -1 y despues en el completaLans se pone el valor correcto
                  
                  /*$<inst>$.lv = NULO;
                  $<inst>$.ini = NULO;
                  $<inst>$.fin = NULO;*/
                 
               }
               inst  {
                  //S.fin=CreaLans(omega) Emite(goto omega) CompletaLans(S.lf, omega)
                  $<inst>$.fin = creaLans(si);
                  emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1));
                  completaLans($<inst>5.lf, crArgEtq(si));
                  
               } ELSE_ inst {
                  //completaLans(S.fin, omega)
                  completaLans($<inst>7.fin, crArgEtq(si));
               }
              ;

instIter      : WHILE_ 
                {  // S.ini   = omega
                 $<inst>$.ini = si;
                  
                } ABREPAR_ expre CIERRAPAR_ 
                { 
                  if ($4.t != T_ERROR){
                    if($4.t != T_LOGICO) {
                      yyerror("La expresión de evaluación de un bucle \"while\" ha de ser de tipo lógico");
                    }
                  }
                  //S.lf=CreaLans(omega) Emite(if E.d = '0' goto omega)
                  $<inst>$.lf = creaLans(si);
                  //    op ==      si $4           ==     0           
                  emite(EIGUAL, crArgPos(niv, $4.d), crArgEnt(0), crArgEtq(-1));  //inicializamos omega a -1 y despues en el completaLans se pone el valor correcto
                }
                inst{
                  //Emite(goto S.ini) CompletaLans(S.lf, omega)
                  emite (GOTOS, crArgNul(), crArgNul(), crArgEtq($<inst>2.ini));
                  completaLans($<inst>6.lf, crArgEtq(si));
                }

                
              ;

expre         : expreLogic 
                {
                  $$.t = $1.t;
                  $$.d=$1.d;
                }                         
              | ID_ IGUAL_ expre 
                {
                  $$.t = T_ERROR; //inicializamos $$ a error
                  
                  if (! ($3.t == T_ERROR)){
                    SIMB simb = obtTdS($1);
                    if (simb.t == T_ERROR)  
                    {
                      yyerror("El objeto no está declarado");
                    }
                    else if (simb.t != $3.t) //1 = 1; true = false; reg1 = reg2
                    {
                      yyerror("Los tipos de la asignación no son compatibles");
                    }
                    else 
                    {
                      $$.t = simb.t;
                      $$.d = $3.d;
                    }

                    emite(EASIG,crArgPos(niv,$3.d),crArgNul(),crArgPos(simb.n,simb.d));

                    //EN OBRAS
                    //$$.d = creaVarTemp();
                    //emite(EASIG, crArgPos(niv, $3.d), crArgNul(), crArgPos(niv, $$.d));
                  
                  
                  }
                  
                  /*
                    RECUERDA: ¡¡¡Notas del profe en la pizarra!!!
                      expre | ID_ ASIG_ expre
                      SIMB simb = obtTds(-)
                      emite(EASIG,CrArgPos(niv,$3.d),CrArgNul(),CrArgPos(niv,sim.d)) //<- está mal! No sabemos el nivel de la variable ID_ (bien cuando es local, mal cuando es global)
                      emite(EASIG,CrArgPos(niv,$3.d),CrArgNul(),CrArgPos(sim.n,sim.d)) //<- así sí nivel del sim
                  */
                }                                                         
              | ID_ ABRELISTA_ expre CIERRALISTA_ IGUAL_ expre //a[expre] = expre //Completar          
                { 
                  $$.t = T_ERROR; //inicializamos $$ a error
                  if ($3.t != T_ENTERO){
                    yyerror("El indice del \"array\" debe ser entero");
                  }
                  else {
                    if (! ($3.t == T_ERROR) && ! ($6.t == T_ERROR)){
                      SIMB simb = obtTdS($1);
                      if (simb.t == T_ERROR)  
                      {
                        yyerror("El objeto no está declarado");
                      }
                      else if (simb.t != T_ARRAY) //1 = 1; true = false; reg1 = reg2
                      {
                        //no es un array
                        yyerror("La variable debe ser de tipo \"array\" "); 
                      }
                      else {//sí es un array T_RECORD, accedo a lo de dentro
                        DIM dim = obtTdA(simb.ref);
                        if (dim.telem != T_ERROR){
                          if (dim.telem != $6.t){
                            yyerror("Error de tipos en la asignacion a un \"array\"");
                          } else {
                            $$.t = $6.t;
                            $$.d = $6.d;
                          }
                        }                      
                      }
                      //a[expre] = expre //Completar          

                      //Emite(E1.d = E1.d * talla(E.t));
                      //      $3.d = $3.d * talla($$.talla)
                      //emite(EMULT, crArgPos(niv, $3.d), crArgEnt(TALLA_TIPO_SIMPLE), crArgPos(niv, $3.d)); 
                      //Emite(E.d = did [ E1.d ] );
                      //          array                    [  posicion  ]     =     res
                      emite(EVA, crArgPos(simb.n, simb.d), crArgPos(niv, $3.d), crArgPos(niv, $6.d)); //Asigna un elemento de un vector a una variable: res = arg1[arg2]                                               
                    }
                  }
                }
              | ID_ PUNTO_ ID_ IGUAL_ expre                    // persona.posicion = 3 //completar
              {
                  $$.t = T_ERROR; //inicializamos $$ a error
                  if (! ($5.t == T_ERROR)){
                    SIMB simb = obtTdS($1);
                    if (simb.t == T_ERROR)  
                    {
                      yyerror("El objeto no está declarado");
                    }
                    else if (simb.t != T_RECORD) 
                    {
                      //no es un registro
                      yyerror("La variable debe ser de tipo \"registro\" ");
                    }
                    else {//sí es un registro, accedo a lo de dentro
                      CAMP camp = obtTdR(simb.ref, $3);
                      if (! (camp.t == T_ERROR)){
                        if (camp.t == $5.t) {
                          $$.t = $5.t;
                          int desplazamiento = simb.d + camp.d;
                          //Emite(E.d = desp)
                          emite(EASIG, crArgPos(niv, $5.d), crArgNul(), crArgPos(simb.n, desplazamiento));
                        
                        }else {
                          yyerror("Error en la \"asignacion\" a una \"struct\"");
                        }
                      } 
                    }
                  }
                }
              ;

expreLogic    : expreIgual {$$.t = $1.t;$$.d=$1.d;}
              | expreLogic opLogic expreIgual 
              {
                $$.t = T_ERROR;
                if ($1.t != T_ERROR && $3.t != T_ERROR)
                {
                  if ($1.t != $3.t) yyerror("Los tipos no coinciden en operación lógica"); 
                  else if ($1.t != T_LOGICO) yyerror("El operador logico no acepta tipos enteros"); //no puedes: 1 && 2
                  else $$.t = T_LOGICO;
                }

                $$.d = creaVarTemp();
                /***************** Expresión a partir de un operador aritmetico */
                if ($2 == EMULT) { //AND
                    emite(EMULT, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
                }else { //OR
                    emite(ESUM, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
                    emite(EMENEQ, crArgPos(niv, $$.d), crArgEnt(1), crArgEtq(si+2)); // (si + 2) es cuando el or da 1 o 0 y te saltas la instruccion de abajo  
                    emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.d)); // esto es para cuando el or 1+1 da 2 y lo ponemos a 1 
                }
              }
              ;

expreIgual    : expreRel {$$.t = $1.t;$$.d=$1.d;}
              | expreIgual opIgual expreRel 
                {
                  $$.t = T_ERROR;
                  if ($1.t != T_ERROR && $3.t != T_ERROR) 
                  {
                    if ($1.t != $3.t) yyerror("Los tipos no coinciden en la operación de igualdad");
                    else $$.t = T_LOGICO; //se puede las dos: true == true o 1 == 2 o 1 != 0
                  }

                  /***************** Expresi´on a partir de un operador aritm´etico */
                  $$.d = creaVarTemp();
                  emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.d));
                  emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgEtq(si + 2));
                  emite(EASIG, crArgEnt(0), crArgNul(), crArgPos(niv, $$.d));
                }
              ;

expreRel      : expreAd {$$.t = $1.t;$$.d=$1.d;}
              | expreRel opRel expreAd
                {
                  $$.t = T_ERROR;
                  if ($1.t != T_ERROR && $3.t != T_ERROR)
                  {
                    if ($1.t != $3.t) yyerror("Los tipos no coinciden en la operación relacional");
                    else if ($1.t == T_LOGICO) yyerror("Operador relacional no acepta tipos lógicos");//solo acepta numero, no puede: true > false 
                    else $$.t = T_LOGICO; // 1 > 2 = false
                  }
                  $$.d = creaVarTemp();
                  emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.d)); //asignas true (por defecto)
                  emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgEtq(si + 2)); //si se cumple la opRel ($2: <,>,<=,>=), como ya tenías asignado 1 saltas a (si+2) para evitar asignar el 0
                  emite(EASIG, crArgEnt(0), crArgNul(), crArgPos(niv, $$.d)); //asigna false                
                }
              ; 

expreAd       : expreMul {$$.t = $1.t;$$.d=$1.d;}
              | expreAd opAd expreMul
                {
                  $$.t = T_ERROR;
                  if ($1.t != T_ERROR && $3.t != T_ERROR) 
                  {
                    if ($1.t != $3.t) yyerror("Error de tipos en \"expresion aditiva\"");
                    else if ($1.t == T_LOGICO) yyerror("El operador aditivo no acepta tipos lógicos");  // true + false no se puede
                    else $$.t = T_ENTERO;
                  }

                  $$.d = creaVarTemp();
                  /***************** Expresi´on a partir de un operador aritm´etico */
                  emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
                }
              ;

expreMul      : expreUna {$$.t = $1.t; $$.d=$1.d;}
              | expreMul opMul expreUna   
                {
                  $$.t = T_ERROR;
                  if ($1.t != T_ERROR && $3.t != T_ERROR)
                  {
                    if ($1.t != $3.t) yyerror("Error de tipos en \"expresion multiplicativa\"");
                    else if ($1.t == T_LOGICO) yyerror("El operador multiplicativo no acepta tipos lógicos");//solo acepta numero, no puede: true * false 
                    else $$.t = T_ENTERO;
                  }
                  
                  $$.d = creaVarTemp();
                  /***************** Expresi´on a partir de un operador aritm´etico */
                  emite($2, crArgPos(niv, $1.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d));
                  
                }             
              ;

expreUna      : expreSufi {$$.t = $1.t;   $$.d = $1.d;}
              | opUna expreUna  // +expre o -expre o !expre      expre puede ser por ejemplo +1, -1, !true         int a  = 4 ;     print(-a);  a =- 1
                {
                  $$.t = T_ERROR;
                  if ($2.t != T_ERROR)
                  {
                      if ($2.t == T_ENTERO)
                      {
                          if ($1 == ESIG) 
                          {
                              yyerror("Operador not (!) no es válido con tipos enteros"); //no puede: !2
                          } 
                          else 
                          {
                              $$.t = T_ENTERO;
                              $$.d = creaVarTemp();
                              emite($1, crArgEnt(0), crArgPos(niv, $2.d), crArgPos(niv, $$.d)); // 0 - (3) = -3  / 0 - (-3) = 3 / 0 + 3 = 3       :D
                          }
                      }  
                      else if ($2.t == T_LOGICO) {
                            if ($1 == ESIG) {
                              $$.t = T_LOGICO; // !true       
                              $$.d = creaVarTemp();                                                 
                              emite(EDIF, crArgEnt(1), crArgPos(niv, $2.d), crArgPos(niv, $$.d));   // 1 -    (0)     = 1    ;   1 - (1) = 0      ;        RECUERDA:  TRUE = 1  FALSE = 0 
                                                                                                    // 1 -    false   = true ;   1 - true = false ;
                            } else { //no se puede: +true o -false
                                yyerror("Operador (+/-) no es válido con tipo lógico");
                            }
                    }
                  } 
                }
              | opIncre ID_     // ++e  --e
                {
                  $$.t = T_ERROR;
                  SIMB simb = obtTdS($2);
                  if (! (simb.t == T_ERROR)){
                    if(simb.t == T_ENTERO) { $$.t = T_ENTERO;}   //  ++e
                    else {yyerror("Operador prefijo requiere tipos enteros");} // NO PUEDE ++true , ++(struct)...    SOLO ES PARA ENTERO
                  }
                  else {yyerror("El objeto no está declarado");} // esto es cuando la variable no ha sido declarada, ejemplo usamos variable++ pero variable no la hemos declarado
                
                  $$.d = creaVarTemp();
                  emite($1, crArgPos(simb.n, simb.d), crArgEnt(1), crArgPos(simb.n, simb.d)); // incrementas la variable
                  emite(EASIG, crArgPos(simb.n, simb.d), crArgNul(), crArgPos(niv, $$.d));              
                
                }
              ;

expreSufi     : const
                {
                  $$.t = $1.t;
                  $$.d = creaVarTemp();
                  emite(EASIG, crArgEnt($1.d), crArgNul(), crArgPos(niv, $$.d));
                
                } // true false cte
              | ABREPAR_ expre CIERRAPAR_ 
                { 
                  $$.t = $2.t; $$.d = $2.d; 
                 
                } // ( expre )
              | ID_
               {
                  $$.t = T_ERROR;
                  SIMB simb = obtTdS($1); /*con esto obtenemos todos sus datos*/
                  if (! (simb.t == T_ERROR)) {
                    if( simb.t == T_LOGICO || simb.t == T_ENTERO){ 
                      $$.t = simb.t;
                    }                  
                    //Tenemos que comprobar el resto de símbolos?
                    else {yyerror("Operador sufijo requiere tipos enteros o lógicos");}   
                    
                  }
                  else {yyerror("El objeto no está declarado");}
                  
                  $$.d = creaVarTemp();
                  emite(EASIG, crArgPos(simb.n, simb.d), crArgNul(), crArgPos(niv, $$.d));
               }
              | ID_ opIncre  
               {
                  $$.t = T_ERROR;
                  SIMB simb = obtTdS($1);
                  if (! (simb.t == T_ERROR)){
                    if(simb.t == T_ENTERO){
                      $$.t = T_ENTERO;
                    }
                    else {yyerror("Operador sufijo requiere tipos enteros");}                  
                  }
                  else {yyerror("El objeto no está declarado");} // esto es cuando la variable no ha sido declarada, ejemplo usamos variable++ pero variable no la hemos declarado
                  
                  $$.d = creaVarTemp();
                  emite(EASIG, crArgPos(simb.n, simb.d), crArgNul(), crArgPos(niv, $$.d)); // aqui a++  por eso usas primero y lo pasas a $$ para que vaya para arriba
                  emite($2, crArgPos(simb.n, simb.d), crArgEnt(1), crArgPos(simb.n, simb.d)); // lo pone en la tabla de simbolos,   aqui ya le incrementas la variable
                  //emite(EASIG, crArgPos(simb.n, simb.d), crArgNul(), crArgPos(niv, $$.d)); 
               }                               // e++   e-- 
              | ID_ PUNTO_ ID_                           
               {
                  $$.t = T_ERROR;
                  SIMB simb = obtTdS($1);
                  if (simb.t != T_ERROR){ //si no es de tipo error y es registro
                    if ( simb.t == T_RECORD) {
                      CAMP camp = obtTdR(simb.ref, $3);
                      if (! (camp.t == T_ERROR)) { 
                        $$.t = camp.t; 
                        //      desp       =  did   +  dcmp
                        int desplazamiento = simb.d + camp.d;
                        //E.d = CreaVarTemp(E.t);
                        $$.d = creaVarTemp();
                        //Emite(E.d = desp)
                        emite(EASIG, crArgPos(simb.n, desplazamiento), crArgNul(), crArgPos(niv, $$.d));
                        
                        }

                        
                      else {yyerror("Campo no declarado en la estructura");}
                    } else {
                      yyerror("El identificador debe ser tipo estructura");
                    }
                  }
                  else {yyerror("El objeto no está declarado");} // esto es cuando la variable no ha sido declarada, ejemplo usamos variable++ pero variable no la hemos declarado
               }    
              | ID_ ABRELISTA_ expre CIERRALISTA_         // a  [ expre ]
               {
                  $$.t = T_ERROR;
                  SIMB simb = obtTdS($1);
                  if ($3.t != T_ENTERO){
                    yyerror("El índice del \"array\" debe ser entero");
                  }
                  else 
                  { 
                    if (simb.t != T_ERROR){ //si no es de tipo error y es array
                      if (simb.t == T_ARRAY) {
                        DIM dim = obtTdA(simb.ref);
                        if (! (dim.telem == T_ERROR)) { 
                          $$.t = dim.telem;
                        }   
                        else {yyerror("Error en la obtención de la array");}
                      } else {
                        yyerror("El identificador debe ser tipo array");
                      }
                    }
                    else {yyerror("El objeto no está declarado");} // esto es cuando la variable no ha sido declarada, ejemplo usamos variable++ pero variable no la hemos declarado
                  }

                  //Emite(E1.d = E1.d * talla(E.t));
                  //      $3.d = $3.d * talla($$.talla)
                  //emite(EMULT, crArgPos(niv, $3.d), crArgEnt(TALLA_TIPO_SIMPLE), crArgPos(niv, $3.d)); 
                  //E.d = CreaVarTemp(E.t);
                  $$.d = creaVarTemp();
                  //Emite(E.d = did [ E1.d ] );
                  emite(EAV, crArgPos(simb.n, simb.d), crArgPos(niv, $3.d), crArgPos(niv, $$.d)); //Asigna un elemento de un vector a una variable: res = arg1[arg2]                          
                  
               }
              | ID_  //BLOQUE LLAMADOR

                /********************* Reservar espacio para el valor de retorno */
                {   
                  //sp = sp + VR.talla   INCTOP=Incrementa la cima de la pila en res posicione
                  emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(TALLA_TIPO_SIMPLE)); 
                }

                ABREPAR_ paramAct CIERRAPAR_          // a (cad vacia o parametros, ...)
               {
                  $$.t = T_ERROR;
                  SIMB simb = obtTdS($1);
                  if (! (simb.t == T_ERROR)){
                    if (simb.c == FUNCION) {
                      INF inf = obtTdD(simb.ref);
                      if (! (inf.tipo == T_ERROR)){  // el rango de funcion (tipo que devuelve la funcion)
                        if (cmpDom(simb.ref, $4)) {  //los parametros actuales tienen el mismo tipo que los formales   
                          $$.t = inf.tipo;
                          
                          //Emite(push(omega+2));
                          //emite(EPUSH, crArgNul(), crArgNul(), crArgEnt(si+2));//duda se haría?
                          /************************** Llamada a la funcion */
                          emite(CALL, crArgNul(), crArgNul(), crArgEtq(simb.d) );
                          /************************** Desapilar el segmento de parametros */
                          emite(DECTOP, crArgNul(), crArgNul(), crArgEnt(inf.tsp));                         
                          /************************** Desapilar y asignar el valor de retorno */
                          $$.d = creaVarTemp();
                          emite(EPOP, crArgNul(), crArgNul(), crArgPos(niv, $$.d));

                        } else{
                            yyerror("Los tipos de los parametros actuales son diferentes de los formales"); //no son del mismo tipo los parametros formales y los actuales
                        } 
                      } else { yyerror("La función no está compilada");} // funcion no compilada ver header
                    } else {
                      yyerror("La variable debe ser una función");
                    }
                  }
                  else {yyerror("El objeto no está declarado");}
               }
              ;

const         : CTE_   
               {
                $$.t = T_ENTERO;
                $$.d = $1; // devuelve el contenido de ese entero
               }
              | TRUE_  
              {
                $$.t = T_LOGICO;
                $$.d = TRUE; //Contantes del header TRUE = 1
              }
              | FALSE_ 
              {
                $$.t = T_LOGICO;
                $$.d = FALSE; //Contantes del header FALSE = 0
              }
              ;

paramAct      : /*cadena vacia*/  
               {
                  $$ = insTdD(-1, T_VACIO);
                  /*Si la funcion no tiene parametros, debe crearse un dominio
                  vacio con: "refe = -1" y "tipo = T_VACIO".*/
              }
              | listParamAct{ $$ = $1; }
              ;

listParamAct  : expre  
               {
                  $$ = insTdD(-1, $1.t);
                  emite(EPUSH, crArgNul(), crArgNul(), crArgPos(niv, $1.d));
               }
              | expre COMA_ 
               {
                emite(EPUSH, crArgNul(), crArgNul(), crArgPos(niv, $1.d));
               } 
              listParamAct
               {
                  $$ = insTdD($4, $1.t); /*$3 es la lista de parametros que vas a 
                  insertar con el tipo de expre $1*/
               }
              ;

opLogic       : AND_  {$$ = EMULT;}
              | OR_   {$$ = ESUM;}
              ;

opIgual       : DOBLEIGUAL_  {$$ = EIGUAL;}
              | DISTINTO_    {$$ = EDIST;}
              ;

opRel         : MAYOR_       {$$ = EMAY;}
              | MENOR_       {$$ = EMEN;}
              | MAYORIGUAL_  {$$ = EMAYEQ;}
              | MENORIGUAL_  {$$ = EMENEQ;}
              ;

opAd          : MAS_   {$$ = ESUM;}
              | MENOS_ {$$ = EDIF;}
              ;

opMul         : POR_ {$$ = EMULT;}
              | DIV_ {$$ = EDIVI;}
              ;

opUna         : MAS_      {$$ = ESUM;}
              | MENOS_    {$$ = EDIF;}
              | EXCLAMA_  {$$ = ESIG;}
              ;

opIncre       : DOBLEMAS_   {$$ = ESUM;}
              | DOBLEMENOS_ {$$ = EDIF;}
              ;

%%
