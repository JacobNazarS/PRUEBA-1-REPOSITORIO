      ******************************************************************
      * Author: JACOB
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTUALIZACION-ARCHIVOS-INDEX.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *EL ARCHIVO SECUENCIAL DADO
       SELECT ARCHIVO-SEC ASSIGN TO "PRODUCT_19082026.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-STATUS-SEC.

      *ARCHIVO INDEXADO CREADO TAREA ANTERIOR
       SELECT ARCHIVO-IND ASSIGN TO "PRODUCTOS.txt"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CODPRODU
           FILE STATUS IS WS-STATUS-IND.

       DATA DIVISION.
       FILE SECTION.

      *ESTRUCTURA DEL ARCHIVO SECUENCIAL
       FD ARCHIVO-SEC.
       01 REG-SEC.
           04 SEC-CODPRODU PIC X(30).
           04 SEC-FECINI PIC X(10).
           04 SEC-BASECALC PIC X(20).
           04 SEC-FRECPAG PIC X(20).
           04 SEC-CURVAFLO PIC X(25).
           04 FILLER PIC X(25).
           04 SEC-ORDIRREV PIC XX.
           04 SEC-NUMFASE PIC XX.
           04 SEC-TIPOPCIO PIC X(8).
           04 SEC-FECHOPCI PIC X(20).
           04 SEC-PERPETUA PIC XX.
           04 SEC-PORCENCU PIC X(8).
           04 SEC-PORCENTI PIC X(8).


      *ESTRUCTURA DEL ARCHIVO INDEXADO
       FD ARCHIVO-IND.
       01 PRODUCTO.
           04 CODPRODU    PIC X(30).
           04 FECINI      PIC X(10).
           04 HORINI      PIC X(8).
           04 HORFIN      PIC X(8).
           04 FECPAGO     PIC X(10).
           04 IMPMINSU    PIC X(50).
           04 IMPMAXSU    PIC X(50).
           04 TIPCUPON    PIC XX.
           04 BASECALC    PIC X(20).
           04 FRECPAG     PIC X(20).
           04 CURVAFLO    PIC X(25).
           04 DESTEMI     PIC X(150).
           04 TXTCUPON    PIC X(50).
           04 ORDIRREV    PIC XX.
           04 MULTFASE    PIC XX.
           04 NUMFASE     PIC XX.
           04 TIPOPCIO    PIC X(8).
           04 FECHOPCI    PIC X(20).
           04 PERPETUA    PIC XX.
           04 MAKWHOLE    PIC XX.
           04 FECMAWHO    PIC X(20).
           04 PORCENCU    PIC S9(4)V9(3) DISPLAY.
           04 PORCENTI    PIC S9(4)V9(3) DISPLAY.

       WORKING-STORAGE SECTION.
       01 WS-STATUS-SEC PIC XX.
       01 WS-STATUS-IND PIC XX.
       01 WS-FIN-SEC PIC X VALUE "N".

      *CONTADORES P/ LA ESTADISTICA
       01 WS-ESTADISTICAS.
           05 WS-LEIDOS PIC 9(4) VALUE ZEROES.
           05 WS-ACTUALIZADOS PIC 9(4) VALUE ZEROES.
           05 WS-NO-ENCONTRADOS PIC 9(4) VALUE ZEROES.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
      *LEECTURA DEL SECUENCIAL Y ACTUALIZACION DEL INDEXADO
           OPEN INPUT ARCHIVO-SEC
           OPEN I-O ARCHIVO-IND

           IF WS-STATUS-SEC NOT = "00"
               DISPLAY "ERROR AL ABRIR EL ARCHIVO SEC: " WS-STATUS-SEC
               STOP RUN
           END-IF.

           IF WS-STATUS-IND NOT = "00"
               DISPLAY "ERROR AL ABRIR LA BASE DE DATOS: " WS-STATUS-IND
               STOP RUN
           END-IF.

           DISPLAY "==================================================="
           DISPLAY " INICIANDO PROCESO BATCH DE ACTUALIZACION "
           DISPLAY "==================================================="

      *BUCLE DE LECTURA DEL ARCHIVO DE TEXTO
           PERFORM 1000-PROCESAR-REGISTRO UNTIL WS-FIN-SEC = "S".

      *CREACION DE LA ESTADÍSTICA
           DISPLAY " "
           DISPLAY "==================================================="
           DISPLAY "             ESTADISTICAS DEL PROCESO              "
           DISPLAY "==================================================="
           DISPLAY "TOTAL DE REG. LEIDOS        : " WS-LEIDOS
           DISPLAY "TOTAL DE REG. ACTUALIZADOS  : " WS-ACTUALIZADOS
           DISPLAY "TOTAL DE REG. NO ENCONTRADOS: " WS-NO-ENCONTRADOS

           CLOSE ARCHIVO-SEC
           CLOSE ARCHIVO-IND.
           STOP RUN.

       1000-PROCESAR-REGISTRO.
           READ ARCHIVO-SEC
               AT END
                  MOVE "S" TO WS-FIN-SEC
               NOT AT END
                  ADD 1 TO WS-LEIDOS

      *HOMOLOGAR LA LLAVE DEL SEC AL IND (EMPATAR)
                  MOVE SEC-CODPRODU TO CODPRODU
      *BUSCAR LA LLAVE EN LA BASE DE DATOS INDEX
                  READ ARCHIVO-IND
                  INVALID KEY
                    ADD 1 TO WS-NO-ENCONTRADOS

                  NOT INVALID KEY
      *SI EXISTE SE ACTUALIZA EN LA MEMORIA
                  MOVE SEC-FECINI TO FECINI
                  MOVE SEC-BASECALC TO BASECALC
                  MOVE SEC-FRECPAG TO FRECPAG
                  MOVE SEC-CURVAFLO TO CURVAFLO
                  MOVE SEC-ORDIRREV TO ORDIRREV
                  MOVE SEC-NUMFASE TO NUMFASE
                  MOVE SEC-TIPOPCIO TO TIPOPCIO
                  MOVE SEC-FECHOPCI TO FECHOPCI
                  MOVE SEC-PERPETUA TO PERPETUA

      *SOBREESCRIBIR LA INFO
                  REWRITE PRODUCTO
                     INVALID KEY
                       DISPLAY "ERROR INTERNO DE REWRITE: " CODPRODU
                     NOT INVALID KEY
                       ADD 1 TO WS-ACTUALIZADOS
                  END-REWRITE
              END-READ
           END-READ.



       END PROGRAM ACTUALIZACION-ARCHIVOS-INDEX.
