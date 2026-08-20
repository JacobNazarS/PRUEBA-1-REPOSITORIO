      ******************************************************************
      * Author:JACOB
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTIPLICACION-MATRICES.
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-CONTINUAR PIC X VALUE "S".

       01 LIMITES.
           05 LIM-F-A PIC 9(2).
           05 LIM-C-A PIC 9(2).
           05 LIM-F-B PIC 9(2).
           05 LIM-C-B PIC 9(2).

      *MATRIZ DE MAXIMO 10X10
       01 MATRIZ-A.
           05 FILA-A OCCURS 10 TIMES.
              10 VAL-A PIC 9(3) OCCURS 10 TIMES.

       01 MATRIZ-B.
           05 FILA-B OCCURS 10 TIMES.
              10 VAL-B PIC 9(3) OCCURS 10 TIMES.

       01 MATRIZ-C.
           05 FILA-C OCCURS 10 TIMES.
              10 VAL-C PIC 9(5) OCCURS 10 TIMES VALUE ZEROES.

       01 INDICES.
           05 X PIC 9(2) VALUE 1.
           05 Y PIC 9(2) VALUE 1.
           05 Z PIC 9(2) VALUE 1.

      *IMPRESION DINAMICA
       01 WS-DISPLAY.
           05 WS-LINEA PIC X(80) VALUE SPACES.
           05 WS-POS PIC 9(2) VALUE 1.
           05 WS-CELDA PIC ZZZZ9.


       PROCEDURE DIVISION.
      *BUCLE CORRECIÓN MI ERROR ANTERIOR DE FIN DEL PROGRAMA
       0000-PRINCIPAL.
           PERFORM UNTIL WS-CONTINUAR = "N" OR WS-CONTINUAR = "N"
               PERFORM 1000-CAPTURAR-LIMITES

               IF LIM-C-A = LIM-F-B
                   PERFORM 2000-CAPTURAR-DATOS
                   PERFORM 3000-MULTIPLICAR-MATRICES
                   PERFORM 4000-MOSTRAR-RESULTADO
               ELSE
                   DISPLAY " "
                   DISPLAY "ERROR: INCOMPATIBLE MATEMATICAMENTE"
                   DISPLAY "LAS COLUMNAS A DEBEN SER IGUAL A FILAS DE B"
               END-IF

               PERFORM 5000-PREGUNTAR-REINICIO
           END-PERFORM.

           DISPLAY "PROGRAMA FINALIZADO CON EXITO"
           STOP RUN.

       1000-CAPTURAR-LIMITES.
           DISPLAY " "
           DISPLAY " MULTIPLICACION DE MATRICES DINAMICA "

           DISPLAY "INGRESA FILAS DE MATRIZ A (MAX 10): "
           ACCEPT LIM-F-A
           DISPLAY "INGRESA COLUMNAS DE MATRIZ A (MAX 10): "
           ACCEPT LIM-C-A

           DISPLAY "INGRESA FILAS DE MATRIZ B (MAX 10): "
           ACCEPT LIM-F-B
           DISPLAY "INGRESA COLUMNAS DE MATRIZ B (MAX 10): "
           ACCEPT LIM-C-B.

       2000-CAPTURAR-DATOS.
           DISPLAY " "
           DISPLAY "--- LLENANDO MATRIZ A ---"
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > LIM-F-A
              PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > LIM-C-A
                 DISPLAY "VALOR A(" X "," Y "): "
                 ACCEPT VAL-A(X, Y)
              END-PERFORM
           END-PERFORM

           DISPLAY " "
           DISPLAY "--- LLENANDO MATRIZ B ---"
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > LIM-F-B
              PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > LIM-C-B
                 DISPLAY "VALOR B(" X "," Y "): "
                 ACCEPT VAL-B(X, Y)
              END-PERFORM
           END-PERFORM.

       3000-MULTIPLICAR-MATRICES.
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > LIM-F-A
              PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > LIM-C-B
                 MOVE 0 TO VAL-C(X, Y)
                 PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > LIM-C-A
                     COMPUTE VAL-C(X, Y) =
                     VAL-C(X, Y) + (VAL-A(X, Z) * VAL-B(Z, Y))
                 END-PERFORM
              END-PERFORM
           END-PERFORM.

       4000-MOSTRAR-RESULTADO.
           DISPLAY " "
           DISPLAY "--- RESULTADO DE LA MULTIPLICACION DE MATRICES ---"
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > LIM-F-A
              MOVE SPACES TO WS-LINEA
              MOVE 1 TO WS-POS
              PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > LIM-C-B
                  MOVE VAL-C(X, Y) TO WS-CELDA
                  STRING "[ " DELIMITED BY SIZE
                        WS-CELDA DELIMITED BY SIZE
                        " ]" DELIMITED BY SIZE
                        INTO WS-LINEA WITH POINTER WS-POS
              END-PERFORM
              DISPLAY WS-LINEA
           END-PERFORM.

       5000-PREGUNTAR-REINICIO.
           DISPLAY " "
           DISPLAY "DESEAS REALIZAR OTRA OPERACION? (S/N): "
           ACCEPT WS-CONTINUAR.

       END PROGRAM MULTIPLICACION-MATRICES.
