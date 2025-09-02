*indices:
sets
    i grupo térmico /GAL,ARAG,CAT,EXT_AND,MUR,VAL,CAST,PV_NAV/
    j grupo hidráulico /Tajo, Duero,Sil/
    k bloque /B1,B2,B3,B4/
    s escenario /S1,S2,S3,S4/
;
alias (k,ii);
alias (s,sp);

*parametros:
parameters
    Pmax(i) potencia maxima de cada subsistema termico i /GAL 3000, ARAG 2600, CAT 5200, EXT_AND 7000, MUR 3500, VAL 4000, CAST 2000, PV_NAV 4000/
    Pmin(i) potencia minima de cada subsistema termico i /GAL 300, ARAG 300, CAT 400, EXT_AND 500, MUR 300, VAL 400, CAST 250, PV_NAV 200/
    RS(i) rampa de subida de cada subsistema termico i /GAL 1500, ARAG 1500, CAT 1700, EXT_AND 2500, MUR 1100, VAL 1700, CAST 1000, PV_NAV 1700/
    RB(i) rampa de bajada de cada subsistema termico i /GAL 1500, ARAG 1500, CAT 1700, EXT_AND 2500, MUR 1100, VAL 1700, CAST 1000, PV_NAV 1700/
    CA(i) coste de arranque de cada susbsistema termico i /GAL 2000, ARAG 1800, CAT 3600, EXT_AND 4000, MUR 1600, VAL 1800, CAST 3000, PV_NAV 1200/
    CP(i) coste de parada de cada susbsistema termico i /GAL 400, ARAG 360, CAT 720, EXT_AND 800, MUR 320, VAL 500, CAST 600, PV_NAV 440/
    TI(i) termino independiente de cada susbsistema termico i /GAL 50, ARAG 30, CAT 40, EXT_AND 80, MUR 60, VAL 70, CAST 90, PV_NAV 55/
    TL(i) termino lineal unitario de cada susbsistema termico i /GAL 6, ARAG 5.1, CAT 4, EXT_AND 3.5, MUR 5.5, VAL 4.5, CAST 3, PV_NAV 5/
    D(k) demanda(inelastica) bloque k /B1 23000, B2 30000, B3 28000, B4 32000/
    PN potencia nuclear base /7200/
    PE potencia eolica /7000/
    PE_vacio(k)
    PF(k) potencia fotovoltaica bloque k /B1 0, B2 4000, B3 6000, B4 0/
    RRT reserva rodante termica /0.2/
    PR(s) probabilidad escenario s /S1 0.36, S2 0.24, S3 0.16, S4 0.24/   
*parametros hidraulicos:
    PMT(j) potencia maxima de turbinacion /Tajo 3000, Duero 4400, Sil 3500/
    Rmax(j) reserva maxima /Tajo 4180, Duero 6790, Sil 2600/   
    Rmin(j) reserva minima /Tajo 4175, Duero 6785, Sil 2595/   
    Rin(j) reserva inicial /Tajo 4179, Duero 6789, Sil 2599/   
    Fl(j) fluyente /Tajo 160, Duero 440, Sil 200/
    XS_opt(i,k,s) valor de XS del modelo 4
    AS_opt(i,k,s) valor de AS del modelo 4
    PS_opt(i,k,s) valor de PS del modelo 4
    US_opt(i,k,s) valor de US del modelo 4
    ZS_opt(s) valor de ZS en el escenario s
    ZS_asterisco(s) valor de Z en el escenario s MODELO 3
    PES_media(k)
    Arrepentimiento(s)
;
table
    AP(j,k) aportaciones del grupo j al bloque k 
            B1   B2   B3   B4
    Tajo    1140 1200 1500 1080
    Duero   3000 3300 3600 2820
    Sil     1320 1500 1800 1200
;
table
    PES(k,s) potencia eólica en el bloque k y escenario s
            S1    S2    S3    S4
    B1      7000  7000  7000  7000
    B2      7000  7000  7000  7000
    B3      7400  7400  6400  6400
    B4      7400  6800  7150  6500
;
table
    PROCEDENCIAS(s,k) matriz de procedencias
            B1   B2   B3   B4
    S1      1     1    1    1
    S2      1     1    1    2
    S3      1     1    3    3
    S4      1     1    3    4
;

*variables de decision
variables
    X(i,k) potencia termica al grupo i en el bloque k
    Y(j,k) potencia hidraulica al grupo j en el bloque k
    U(i,k) si el grupo i está acoplado en el bloque k o no
    Re(j,k) reserva del grupo hidráulico j en el bloque k
    TLmax(k) máximo coste unitario de entre los grupos que están generando en el bloque k
    A(i,k) si arranca i en bloque k
    P(i,k) si para i en bloque k
    Z costo total (para la funcion objetivo)
    Z3 costo total (para la funcion objetivo) modelo 3
*variables estocasticas
    XS(i,k,s) potencia térmica al grupo i en el bloque k en el escenario s
    YS(j,k,s) potencia hidráulica al grupo j en el bloque k en el escenario s
    US(i,k,s) si el grupo i está acoplado en el bloque k en el escenario s
    ReS(j,k,s) reserva del grupo hidráulico j en el bloque k en el escenario s
    AS(i,k,s) si arranca i en bloque k en el escenario s
    PS(i,k,s) si para i en bloque k en el escenario s
    ZS costo esperado total
    alpha variable de maximo arrepentimiento

;
positive variables X,Y,Re,XS,YS,ReS;
binary variable U,A,P,US,AS,PS;


equations
OBJETIVO1 crear la función objetivo de coste del modelo 1
OBJETIVO2 crear la función objetivo de coste del modelo 2
OBJETIVO3 crear la función objetivo de coste del modelo 3
OBJETIVO4 crear la función objetivo de coste del modelo 4

BALPOT demanda
LIMTERM_MIN potencia térmica mínima
LIMTERM_MAX potencia térmica máxima
LIMHID_MIN potencia hidráulica mínima
LIMHID_MAX potencia hidráulica máxima
BALRESE_INI igualdad reserva hidráulica inicial
BALRESE igualdad reserva hidráulica
RAMPA_SUBIDA rampa de subida
RAMPA_BAJADA rampa de bajada
LIM_RE_MIN reserva hidráulica mínima
LIM_RE_MAX reserva hidráulica máxima
LIM_TL_MAX máximo término lineal para cada bloque
ARR_PAR para saber si está parado o acoplado
RES_ROD reserva rodante
RAMPA_SUBIDA_0 rampa subida 0
RAMPA_BAJADA_0 rampa bajada 0

*restricciones estocasticas:
BALPOTS demanda en el escenario s
LIMTERM_MINS potencia térmica mínima en el escenario s
LIMTERM_MAXS potencia térmica máxima en el escenario s
LIMHID_MINS potencia hidráulica mínima en el escenario s
LIMHID_MAXS potencia hidráulica máxima en el escenario s
BALRESE_INIS igualdad reserva hidráulica inicial en el escenario s
BALRESES igualdad reserva hidráulica en el escenario s
RAMPA_SUBIDAS rampa de subida en el escenario s
RAMPA_BAJADAS rampa de bajada en el escenario s
LIM_RE_MINS reserva hidráulica mínima en el escenario s
LIM_RE_MAXS reserva hidráulica máxima en el escenario s
ARR_PARS para saber si está parado o acoplado en el escenario s
RES_RODS reserva rodante en el escenario s
RAMPA_SUBIDA_0s rampa subida 0 en el escenario s
RAMPA_BAJADA_0s rampa bajada 0 en el escenario s

*restricciones de arrepentimiento:
ARREMP_MAX restricción que debe cumplir el arrepentimeinto máximo en cada escenario s

;

*funcion objetivo: minimizar los costes
OBJETIVO1.. Z =E= 6*sum((i,k), TL(i) * X(i,k));

*funcion objetivo: minimizar los costes
OBJETIVO2.. Z =E= 6*sum((k), TLmax(k) * D(k));

*funcion objetivo: minimizar los costes
OBJETIVO3.. Z3 =E= 6*sum((i,k), TL(i) * X(i,k)) + sum((i,k), CA(i) * A(i,k)) + sum((i,k), CP(i) * P(i,k)) + 6*sum((i,k), TI(i) * U(i,k));

*funcion objetivo: minimizar los costes
OBJETIVO4 .. ZS =E= sum(s, PR(s)*(6*sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), TL(i)*XS(i,k,sp)) + sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), CA(i)*AS(i,k,sp)) + sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), CP(i)*PS(i,k,sp)) + 6*sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), TI(i)*US(i,k,sp))));


*restricciones
LIMTERM_MIN(i,k) ..
    Pmin(i) * U(i,k) =L= X(i,k);

LIMTERM_MAX(i,k) ..
    X(i,k) =L= Pmax(i) * U(i,k);
    
BALPOT(k) ..
    (sum(i, X(i,k)) + sum(j, Y(j,k)) + PN + PE_vacio(k) + PF(k)) =E= D(k);

*uso $(ord(k) > 1) para que no empiece en 1
RAMPA_SUBIDA(i,k)$(ord(k) > 1) ..
    X(i,k) - X(i,k-1) =L= RS(i);
    
RAMPA_SUBIDA_0(i)..
    X(i,'B1') =L= RS(i);

RAMPA_BAJADA(i,k)$(ord(k) > 1) ..
    X(i,k-1) - X(i,k) =L= RB(i);
    
RAMPA_BAJADA_0(i)..
    -X(i,'B1') =L= RB(i);
    
LIMHID_MIN(j,k) ..
    0 =L= Y(j,k) + Fl(j);
LIMHID_MAX(j,k) ..
   Y(j,k) + Fl(j) =L= PMT(j);

BALRESE_INI(j)..
    Re(j,"B1") =E= ((Rin(j)*1000)/6)-Y(j,"B1")+(AP(j,"B1")/6);
BALRESE(j,k)$(ord(k) > 1) ..
    Re(j,k) =E= Re(j,k-1) + (AP(j,k)/6) - Y(j,k);

LIM_RE_MIN(j,k)..
    Re(j,k) =G= (Rmin(j)*1000)/6;
LIM_RE_MAX(j,k)..
    Re(j,k) =L= (Rmax(j)*1000)/6;
    
LIM_TL_MAX(i,k)..
    TLmax(k) =G= TL(i)*U(i,k);
    
ARR_PAR(i,k)..
    sum(ii$(ord(ii) <= ord(k)), A(i,ii) - P(i,ii)) =E= U(i,k);
    
RES_ROD(k)..
    RRT*sum(i, X(i,k)) =L= sum((i), Pmax(i)*U(i,k)-X(i,k));


*restricciones estocasticas:

LIMTERM_MINS(i,k,s) ..
    Pmin(i) * sum(sp$(ord(sp) = PROCEDENCIAS(s,k)), US(i,k,sp)) =L= sum(sp$(ord(sp) = PROCEDENCIAS(s,k)), XS(i,k,sp));


LIMTERM_MAXS(i,k,s) ..
    sum(sp$(ord(sp) = PROCEDENCIAS(s,k)),XS(i,k,sp)) =L= sum(sp$(ord(sp) = PROCEDENCIAS(s,k)),Pmax(i) * US(i,k,sp));

    
BALPOTS(k,s) ..
    sum(sp$(ord(sp) = PROCEDENCIAS(s,k)),sum(i, XS(i,k,sp)) +sum(j, YS(j,k,sp)) +PN +PES(k,sp) +PF(k)) =E= D(k);


RAMPA_SUBIDAS(i,k,s)$(ord(k) > 1) ..
    sum(sp$(ord(sp) = PROCEDENCIAS(s,k)), XS(i,k,sp)) - sum(sp$(ord(sp) = PROCEDENCIAS(s,k-1)), XS(i,k-1,sp)) =L= RS(i);

    
RAMPA_SUBIDA_0s(i,s) ..
    sum(sp$(ord(sp) = PROCEDENCIAS(s,'B1')),XS(i,'B1',sp)) =L= RS(i);


RAMPA_BAJADAS(i,k,s)$(ord(k) > 1) ..
    sum(sp$(ord(sp) = PROCEDENCIAS(s,k-1)), XS(i,k-1,sp)) - sum(sp$(ord(sp) = PROCEDENCIAS(s,k)), XS(i,k,sp)) =L= RB(i);

    
RAMPA_BAJADA_0s(i,s) ..
    -sum(sp$(ord(sp) = PROCEDENCIAS(s,'B1')), XS(i,'B1',sp)) =L= RB(i);

    
LIMHID_MINS(j,k,s) ..
    0 =L= sum(sp$(ord(sp) = PROCEDENCIAS(s,k)), YS(j,k,sp)) + Fl(j);
LIMHID_MAXS(j,k,s) ..
    sum(sp$(ord(sp) = PROCEDENCIAS(s,k)), YS(j,k,sp)) + Fl(j) =L= PMT(j);

BALRESE_INIS(j,s) ..
    sum(sp$(ord(sp) = PROCEDENCIAS(s,'B1')),ReS(j,"B1",sp)) =E= ((Rin(j)*1000)/6)- sum(sp$(ord(sp) = PROCEDENCIAS(s,'B1')), YS(j,"B1",sp))+ (AP(j,"B1")/6);

BALRESES(j,k,s)$(ord(k) > 1) ..
    sum(sp$(ord(sp) = PROCEDENCIAS(s,k)),ReS(j,k,sp)) =E= sum(sp$(ord(sp) = PROCEDENCIAS(s,k-1)),   ReS(j,k-1,sp))+ (AP(j,k)/6) - sum(sp$(ord(sp) = PROCEDENCIAS(s,k)),   YS(j,k,sp));


LIM_RE_MINS(j,k,s) ..
    sum(sp$(ord(sp) = PROCEDENCIAS(s,k)), ReS(j,k,sp)) =G= (Rmin(j)*1000)/6;
LIM_RE_MAXS(j,k,s) ..
    sum(sp$(ord(sp) = PROCEDENCIAS(s,k)), ReS(j,k,sp)) =L= (Rmax(j)*1000)/6;
       
ARR_PARS(i,k,s) ..
    sum(ii$(ord(ii) <= ord(k)),sum(sp$(ord(sp) = PROCEDENCIAS(s,ii)),AS(i,ii,sp) - PS(i,ii,sp))) =E= sum(sp$(ord(sp) = PROCEDENCIAS(s,k)), US(i,k,sp));

RES_RODS(k,s) ..
    RRT * sum((i,sp)$(ord(sp) = PROCEDENCIAS(s,k)), XS(i,k,sp)) =L= sum((i,sp)$(ord(sp) = PROCEDENCIAS(s,k)),Pmax(i)*US(i,k,sp) - XS(i,k,sp));



*RESTRICCIÓN ARREPENTIMIENTO MÁXIMO

ARREMP_MAX(s) ..
    alpha =G= (6*sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), TL(i)*XS(i,k,sp)) + sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), CA(i)*AS(i,k,sp)) + sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), CP(i)*PS(i,k,sp)) + 6*sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), TI(i)*US(i,k,sp))) - ZS_asterisco(s);
    
 


*====================================================================

PE_vacio(k)=PE

*modelo de optimizacion
MODEL MODELO1 /OBJETIVO1,LIMTERM_MIN,LIMTERM_MAX,BALPOT,RAMPA_SUBIDA,RAMPA_SUBIDA_0,RAMPA_BAJADA,RAMPA_BAJADA_0,LIMHID_MIN,LIMHID_MAX,BALRESE_INI,BALRESE,LIM_RE_MIN,LIM_RE_MAX,RES_ROD/;

*MIP porque es un problema mixto con variables continuas y binarias
SOLVE MODELO1 MINIMIZING Z USING MIP;
DISPLAY X.l,Y.l,Re.l, U.l, Z.l;


*====================================================================

*modelo de optimizacion
MODEL MODELO2 /OBJETIVO2,LIMTERM_MIN,LIMTERM_MAX,BALPOT,RAMPA_SUBIDA,RAMPA_SUBIDA_0,RAMPA_BAJADA,RAMPA_BAJADA_0,LIMHID_MIN,LIMHID_MAX,BALRESE_INI,BALRESE,LIM_RE_MIN,LIM_RE_MAX,RES_ROD,LIM_TL_MAX/;

*MIP porque es un problema mixto con variables continuas y binarias
SOLVE MODELO2 MINIMIZING Z USING MIP;
DISPLAY X.l,Y.l,Re.l, U.l,TLmax.l, Z.l;


*====================================================================

*modelo de optimizacion
MODEL MODELO3 /OBJETIVO3,LIMTERM_MIN,LIMTERM_MAX,BALPOT,RAMPA_SUBIDA,RAMPA_SUBIDA_0,RAMPA_BAJADA,RAMPA_BAJADA_0,LIMHID_MIN,LIMHID_MAX,BALRESE_INI,BALRESE,LIM_RE_MIN,LIM_RE_MAX,RES_ROD,ARR_PAR/;

*MIP porque es un problema mixto con variables continuas y binarias
SOLVE MODELO3 MINIMIZING Z3 USING MIP;
DISPLAY X.l,Y.l,Re.l, U.l,A.l,P.l, Z3.l;



loop(s,
    PE_vacio(k) = PES(k,s);
    solve MODELO3 minimizing Z3 using MIP;
    ZS_asterisco(s) = Z3.l;
);
 


*====================================================================

*modelo de optimizacion
MODEL MODELO4 /OBJETIVO4,LIMTERM_MINS,LIMTERM_MAXS,BALPOTS,RAMPA_SUBIDAS,RAMPA_SUBIDA_0s,RAMPA_BAJADAS,RAMPA_BAJADA_0s,LIMHID_MINS,LIMHID_MAXS,BALRESE_INIS,BALRESES,LIM_RE_MINS,LIM_RE_MAXS,RES_RODS,ARR_PARS/;

*MIP porque es un problema mixto con variables continuas y binarias
SOLVE MODELO4 MINIMIZING ZS USING MIP;
DISPLAY XS.l,YS.l,ReS.l, US.l,AS.l,PS.l, ZS.l;

XS_opt(i,k,s) = XS.l(i,k,s);
AS_opt(i,k,s) = AS.l(i,k,s);
PS_opt(i,k,s) = PS.l(i,k,s);
US_opt(i,k,s) = US.l(i,k,s);

ZS_opt(s) = 6*sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), TL(i)*XS_opt(i,k,sp)) + sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), CA(i)*AS_opt(i,k,sp)) + sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), CP(i)*PS_opt(i,k,sp)) + 6*sum((i,k,sp)$(ord(sp) = PROCEDENCIAS(s,k)), TI(i)*US_opt(i,k,sp));
display ZS_opt;


*==========================================

PES_media(k) = sum(s, PR(s) * PES(k,s));

PE_vacio(k) = PES_media(k);

MODEL MODELOMED /OBJETIVO3,LIMTERM_MIN,LIMTERM_MAX,BALPOT,RAMPA_SUBIDA,RAMPA_SUBIDA_0,RAMPA_BAJADA,RAMPA_BAJADA_0,LIMHID_MIN,LIMHID_MAX,BALRESE_INI,BALRESE,LIM_RE_MIN,LIM_RE_MAX,RES_ROD,ARR_PAR/;

solve MODELOMED minimizing Z3 using MIP;  
display X.l, Y.l, Re.l, U.l, A.l, P.l, Z3.l;

scalar EEV,VSS;
EEV = Z3.l;
VSS = ZS.l- EEV;
display VSS;

*Valor esperado con información perfecta:
scalar EVwPI;
EVwPI = sum(s, PR(s) * ZS_asterisco(s));
display EVwPI;

*Arrepentimiento
arrepentimiento(s) =  ZS_opt(s) - ZS_asterisco(s);
display arrepentimiento;

*Arrepentimiento esperado
scalar arrepentimiento_esperado;
arrepentimiento_esperado = sum(s,pr(s)*(ZS_opt(s) - ZS_asterisco(s)));
display arrepentimiento_esperado;


*====================================================================

*modelo de optimizacion
MODEL MODELO_ARREMPETIMIENTO /ARREMP_MAX,LIMTERM_MINS,LIMTERM_MAXS,BALPOTS,RAMPA_SUBIDAS,RAMPA_SUBIDA_0s,RAMPA_BAJADAS,RAMPA_BAJADA_0s,LIMHID_MINS,LIMHID_MAXS,BALRESE_INIS,BALRESES,LIM_RE_MINS,LIM_RE_MAXS,RES_RODS,ARR_PARS/;

*MIP porque es un problema mixto con variables continuas y binarias
SOLVE MODELO_ARREMPETIMIENTO MINIMIZING alpha USING MIP;
DISPLAY alpha.l;


*====================================================================