CONST NC = 12;
TYPE
    VETOR = PACKED ARRAY[1..10] OF CHAR;
VAR
    HORSE : ARRAY[1..NC] OF VETOR;
    CAVALO : ARRAY[1..NC] OF RECORD
                                NOME : VETOR;
                                BALIZA : INTEGER
                            END;
PARED, N, AUX, pRIM, SEG, TERC, NUM_CAV, IX, CAV, I : INTEGER;
LETRA, GANHOU : INTEGER;
VALC, VALDP, VAL1PL, VAL2PL : INTEGER;
VAL, P, TEMPO, MAIOR_12, TOTVC, TOTDP, TOTPL, TOTAL: REAL;
PISTA : PACKED ARRAY[1..12] OF CHAR; 
JOQUEI : ARRAY[1..NC] OF VETOR;
VEC,DIST,PLAC : ARRAY[1..NC] OF REAL;
DUPL : ARRAY[11..88] OF REAL;
POSICAO : ARRAY[0..NC] OF RECORD 
                            NUM : INTEGER;
                            POSIC : REAL;
                        END;

INITPROCEDURE;
 BEGIN 
    JOQUEI[1] := 'A. BARROSO  '; JOQUEI[2] := 'R. PENACHIO ';
    JOQUEI[3] := 'E. AMORIM   '; JOQUEI[4] := 'J. FAGUNDES ';
    JOQUEI[5] := 'J. M. AMORIM', JOQUEI[6] := 'A. MASSO    ';
    JOQUEI[7] := 'S. AZOCAR   '; JOQUEI[8] := 'J. DACOSTA  ';
    JOQUEI[9] := 'S. A. SANTOS'; JOQUEI[10] := 'R. RIBEIRO  ';
    JOGUEI[11] := 'J. ALMEIDA  '; JOGUEI[12] := 'I. QUINTANA ';
    PAREO := 1;
 END;

PROCEDURE RANDU(VAR X : INTEGER, VAR Z: REAL);
 BEGIN  
    X := X * 262147;
    IF (X <= 0 THEN X := X + 34359738367 + 1);
    Z = X*0.2910383E-10;
 END;

PROCEDURE GERA_NUM_CAV;
 BEGIN 
    REPEAT
        RANDU(IX,VAL);
        CAV := TRUNC(VAL * 20.0);
    UNTIL ((CAV > 0) AND (CAV <= NC));
 END;

PROCEDURE INICIALIZACAO;
 BEGIN 
    FOR I:=1 TO NC DO BEGIN
                        VENC[I] := 0.0;
                        DIST[I] := 0.0;
                        PLAC[I] := 0.0;
                      END;
    HORSE[1] := 'ADIL        '; HORSE[2] := 'ARNALDO     ';
    HORSE[3] := 'BIG POKER   '; HORSE[2] := 'DAIAO       ';
    HORSE[5] := 'DONETICA    '; HORSE[6] := 'ESCORIAL    ';
    HORSE[7] := 'FARWELL     '; HORSE[8] := 'GRANBY      ';
    HORSE[9] := 'GUARUS      '; HORSE[10] := 'NEARCO     ';
    HORSE[11]:='PHALLARIS   '; HORSE[12] := 'TURBILLON   ';
    NUM_CAV:=NC; I:=1; TOTVC:=0.0; TOTDP:=0.0; TOTPL:=0.0;
 END;

PROCURE PERGUNTA;
 BEGIN
    REPEAT
     WRITE(TTY,'VOCE QUER APOSTAR NUMA CORRIDA DE CAVALO?');
     BREAK;
     WRITELN(TTY,'ESCREVA 0 (SIM) OU 1 (NAO)'); BREAK;
     READ(TTY,LETRA); BREAK;
    UNTIL (LETRA = 0) OR (LETRA=1)
 END;

PROCEDURE QUANTO;
 BEGIN 
    REPEAT 
        WRITELN(TTY,'QUANTO VOCE QUER APOSTAR? (EM CR$!)');
        WRITELN(TTY, 'ESCREVA A QUANTIDADE EM FORMA DE NUMERO REAL, ',
        'I.E", X.Y (EX: 100.0)'); BREAK;
        BREAK; READ(TTY,VAL)
    UNTIL (VAL >= 5.0)
 END;

PROCEDURE VENCEDOR;
  BEGIN 
    REPEAT
        WRITELN(TTY,'EM QUE CAVALO VOCE QUER APOSTAR?'); BREAK;
        WRITELN(TTY,'ESCREVA O NUMERO CORRESPONDENTE:'); BREAK;
        READ(TTY,CAV)
    UNTIL (CAV > 0) AND (CAV <= NC);
    QUANTO;
    TOTVC := TOTVC + VAL;
    VENC[CAV] := VENC[CAV] + VAL
  END;
    
PROCEDURE DUPLA;
  BEGIN
    REPEAT
        WRITELN(TTY, 'EM QUE DUPLA VOCE QUER APOSTAR ?'); BREAK;
        READ(TTY,CAV)
    UNTIL (CAV IN [12..18,23..28,34..38,45..48,55..58,66..68]) OR (CAV = 77) OR (CAV = 78) OR (CAV=88);
    QUANTO;
    TOTDP := TOTDP + VAL;
    DUPL[CAV] := DUPL[CAV] + VAL
  END;

PROCEDURE PLACE;
  BEGIN 
    REPEAT
        WRITELN(TTY, 'EM QUE CAVALO VOCE QUER APOSTAR?'); BREAK;
        WRITELN(TTY, 'ESCREVA O NUMERO CORRESPONDENTE:'); BREAK;
        READ(TTY,CAV)
    UNTIL (CAV > 0) AND (CAV <= NC);
    QUANTO;
    TOTPL := TOTPL + VAL;
    PLAC[CAV] := PLAC[CAV] + VAL
  END;

  PROCEDURE APOSTAS;
    BEGIN
      WRITE(TTY); BREAK; WRITELN(TTY); BREAK;
      WRITELN(TTY,'A APOSTA MINIMA EH DE CR$5.0'); BREAK;
      WHILE(LETRA=0) DO 
        BEGIN
         REPEAT 
            WRITELN(TTY,'VOCE QUER APOSTAR NO VENCEDOR, DUPLA OU PLACE?');
            BREAK;
            WRITELN(TTY,'ESCREVA 2(VENCEDOR) OU 3(DUPLA) OU 4(PLACE)'); BREAK;
            READ(TTY,LETRA); BREAK
         UNTIL (LETRA=2) OR (LETRA=3) OR (LETRA=4);
         CASE LETRA OF
            2: VENCEDOR;
            3: DUPLA;
            4: PLACE
         END;
         REPEAT
            WRITELN(TTY,'VOCE QUER CONTINUAR APOSTANDO?'); BREAK;
            WRITELN(TTY,'ESCREVA 0(SIM) OU 1(NAO)'); BREAK;
            READ(TTY,LETRA); BREAK
         UNTIL(LETRA=0) OR (LETRA=1)
         END;
        TOTAL := TOTVC + TOTDP + TOTPL
        END;

    PROCEDURE GERA_POSIC_CAV;
    BEGIN
        RANDU(IX,VAL);
        VAL := 14.5325 - VAL;
        IF MAIOR_12 > VAL THEN MAIOR_12 := VAL;
        VAL = 24000.0/VAL
    END;

    PROCEDURE TRECHO;
     VAR ENTROU, CONT : INTEGER;
     BEGIN 
        CONT := 1; GERA_POSIC_CAV;
        DIST[1] := DIST[1] + VAL;
        POSICAO[1].POSIC := DIST[1];
        POSICAO[1].NUM := 1;
        FOR I:= 2 TO NC DO
            BEGIN
                ENTROU := 0; GERA_POSIC_CAV;
                DIST[I] := DIST[I] + VAL; AUX := CONT;
                WHILE((POSICAO[AUX].POSIC > DIST[I] AND (AUX > 0)) DO
                    BEGIN
                        P := POSICAO[AUX].POSIC := DIST[I]; N := POSICAO[AUX].NUM;
                        POSICAO[AUX].POSIC := DIST[I];
                        POSICAO[AUX].NUM := I;
                        POSICAO[AUX+1].POSIC := P;
                        POSICAO[AUX+1].NUM := N; AUX := AUX-1; ENTROU := 1
                    END;
                CONT := CONT + 1;
                IF(ENTROU = 0) THEN BEGIN 
                                        POSICAO[CONT].POSIC := DIST[I];
                                        POSICAO[CONT].NUM := I;
                                    END;                         
            END
    END;

PROCEDURE CALCULA_RATEIOS;
VAR AUX : REAL;
BEGIN
    IF(DUPL[GANHOU] = 0.0) THEN DUPL[GANHOU] := 5.0;
    IF(VENC[PRIM]=0.0) THEN VENC[PRIM] := 5.0;
    IF(PLAC[PRIM]=0.0) THEN PLAC[PRIM] := 5.0;
    IF(PLAC[SEG]=0.0) THEN PLAC[SEG] := 5.0;
    VALVC := TRUNC(((TOTVC*0.68)/VENC[PRIM]+0.05)*10.0);
    VALDP := TRUNC(((TOPTDP*0.625)/DUPL[GANHOU]+0.05)*10.0);
    AUX := TOTPL*0.81 - PLAC[SEG] - PLAC[PRIM];
    AUX := AUX/2.0;
    VAL1PL := TRUNC((AUX/PLAC[PRIM]+0.05)*10.0)+10;
    VAL2PL := TRUNC((AUX/PLAC[SEG]+0.05)*10.0)+10;
    IF(VALVC < 10) THEN VALVC := 10;
    IF(VAL1PL < 10) THEN VAL1PL := 10;
    IF(VAL2PL < 10) THEN VAL2PL := 10;
    IF(VALDP < 10) THEN VALDP := 10;
END;

PROCEDURE DISTANCIA_TOTAL:
    BEGIN
    WRITELN(TTY); BREAK; WRITELN(TTY); BREAK;
    WRITELN(TTY, 'CAVALO ',' ':8,'DISTANCIA CORRIDA'); BREAK;
    WRITELN(TTY); BREAK;
    FOR I:=1 TO NC DO
        BEGIN
            WRITELN(TTY, CAVALO[POSICAO[I].NUM].NOME, POSICAO[I].POSIC);
        END;
        WRITELN(TTY); BREAK; WRITE(TTY); BREAK
    END;