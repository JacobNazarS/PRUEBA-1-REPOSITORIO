      ******************************************************************
      * Author: JACOB
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DOCUMENTOS-INDEXADOS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL ARCHIVO-PRODUCTOS ASSIGN TO "PRODUCTOS.txt"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CODPRODU
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-PRODUCTOS.
       01 PRODUCTO.
           04 CODPRODU PIC X(30).
           04 FECINI PIC X(10).
           04 HORINI PIC X(8).
           04 HORFIN PIC X(8).
           04 FECPAGO PIC X(10).
           04 IMPMINSU PIC X(50).
           04 IMPMAXSU PIC X(50).
           04 TIPCUPON PIC XX.
           04 BASECALC PIC X(20).
           04 FRECPAG PIC X(20).
           04 CURVAFLO PIC X(25).
           04 DESTEMI PIC X(150).
           04 TXTCUPON PIC X(50).
           04 ORDIRREV PIC XX.
           04 MULTFASE PIC XX.
           04 NUMFASE  PIC XX.
           04 TIPOPCIO PIC X(8).
           04 FECHOPCI PIC X(20).
           04 PERPETUA PIC XX.
           04 MAKWHOLE PIC XX.
           04 FECMAWHO PIC X(20).
           04 PORCENCU PIC S9(4)V9(3) DISPLAY.
           04 PORCENTI PIC S9(4)V9(3) DISPLAY.

       WORKING-STORAGE SECTION.
       01 WS-STATUS PIC XX.
       01 WS-OPCION PIC 9 VALUE 0.
       01 WS-CONTINUAR PIC X VALUE "S".
       01 WS-FIN-ARCHIVO PIC X VALUE "N".

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM UNTIL WS-OPCION = 3
           DISPLAY "==================================================="
           DISPLAY "           SISTEMA BANCARIO DE PRUEBA              "
           DISPLAY "==================================================="
           DISPLAY "1- REGISRAR NUEVO PRODUCTO"
           DISPLAY "2- VER TODOS LOS PRODUCTOS"
           DISPLAY "3- SALIR DEL SISTEMA"
           DISPLAY "ELIGE UNA OPCION: "
           ACCEPT WS-OPCION

           EVALUATE WS-OPCION
              WHEN 1
                 PERFORM 1000-AGREGAR-DATOS
              WHEN 2
                 PERFORM 2000-VER-DATOS
              WHEN 3
                 DISPLAY "SALIENDO DEL SISTEMA..."
              WHEN OTHER
                 DISPLAY "OPCION NO VALIDA."
           END-EVALUATE
           END-PERFORM.

           STOP RUN.
       1000-AGREGAR-DATOS.
           MOVE "S" TO WS-CONTINUAR

           OPEN I-O ARCHIVO-PRODUCTOS
           IF WS-STATUS = "35"
               OPEN OUTPUT ARCHIVO-PRODUCTOS
               CLOSE ARCHIVO-PRODUCTOS
               OPEN I-O ARCHIVO-PRODUCTOS
           END-IF.

           PERFORM UNTIL WS-CONTINUAR = "N" OR WS-CONTINUAR = "n"
               INITIALIZE PRODUCTO

               DISPLAY " "
               DISPLAY "--- REGISTRO DE NUEVO PRODUCTO ---"
               DISPLAY "1. CODPRODU (LLAVE): "
               ACCEPT CODPRODU
               DISPLAY "2. FECINI (Fecha Inicio DD/MM/AAAA): "
               ACCEPT FECINI
               DISPLAY "3. HORINI (Hora Inicio HH:MM:SS): "
               ACCEPT HORINI
               DISPLAY "4. HORFIN (Hora Fin HH:MM:SS): "
               ACCEPT HORFIN
               DISPLAY "5. FECPAGO (Fecha Pago Cupon): "
               ACCEPT FECPAGO
               DISPLAY "6. IMPMINSU (Importe Minimo Suscripcion): "
               ACCEPT IMPMINSU
               DISPLAY "7. IMPMAXSU (Importe Maximo Suscripcion): "
               ACCEPT IMPMAXSU
               DISPLAY "8. TIPCUPON (Tipo Cupon - 2 letras): "
               ACCEPT TIPCUPON
               DISPLAY "9. BASECALC (Base de Calculo): "
               ACCEPT BASECALC
               DISPLAY "10. FRECPAG (Frecuencia Pago Cupon): "
               ACCEPT FRECPAG
               DISPLAY "11. CURVAFLO (Curva Flotante): "
               ACCEPT CURVAFLO
               DISPLAY "12. DESTEMI (Destinatario de la Emision): "
               ACCEPT DESTEMI
               DISPLAY "13. TXTCUPON (Texto Pago Cupon): "
               ACCEPT TXTCUPON
               DISPLAY "14. ORDIRREV (Orden Irrevocable - 2 letras): "
               ACCEPT ORDIRREV
               DISPLAY "15. MULTFASE (Multifase - 2 letras): "
               ACCEPT MULTFASE
               DISPLAY "16. NUMFASE (Numero de Fase Bono - 2 letras): "
               ACCEPT NUMFASE
               DISPLAY "17. TIPOPCIO (Tipo de Opcion): "
               ACCEPT TIPOPCIO
               DISPLAY "18. FECHOPCI (Fecha de Opcion): "
               ACCEPT FECHOPCI
               DISPLAY "19. PERPETUA (Perpetua - 2 letras): "
               ACCEPT PERPETUA
               DISPLAY "20. MAKWHOLE (Make Whole - 2 letras): "
               ACCEPT MAKWHOLE
               DISPLAY "21. FECMAWHO (Fecha Make Whole): "
               ACCEPT FECMAWHO
               DISPLAY "22. PORCENCU (Porcentaje Cupon Bloque): "
               ACCEPT PORCENCU
               DISPLAY "23. PORCENTI (Porcentaje TIR Bloque): "
               ACCEPT PORCENTI

               WRITE PRODUCTO
                   INVALID KEY
                      DISPLAY "----------------------------------------"
            DISPLAY
                "ERROR: YA EXISTE UN PRODUCTO CON LA LLAVE: " CODPRODU
                      DISPLAY "----------------------------------------"
                   NOT INVALID KEY
                      DISPLAY "----------------------------------------"
                       DISPLAY "PRODUCTO REGISTRADO EXITOSAMENTE"
                      DISPLAY "----------------------------------------"
               END-WRITE

               CLOSE ARCHIVO-PRODUCTOS
               OPEN I-O ARCHIVO-PRODUCTOS

               DISPLAY " "
               DISPLAY "¿Deseas agregar otro producto? (S/N): "
               ACCEPT WS-CONTINUAR

           END-PERFORM.

           CLOSE ARCHIVO-PRODUCTOS.

       2000-VER-DATOS.
           DISPLAY " "
           DISPLAY "========================================"
           DISPLAY "      LISTADO DE PRODUCTOS GUARDADOS    "
           DISPLAY "========================================"

           OPEN INPUT ARCHIVO-PRODUCTOS
           IF WS-STATUS = "35"
               DISPLAY "EL ARCHIVO ESTA VACIO."
               CLOSE ARCHIVO-PRODUCTOS
               GO TO 2000-FIN
           END-IF.

           MOVE "N" TO WS-FIN-ARCHIVO
           PERFORM UNTIL WS-FIN-ARCHIVO = "S"
               READ ARCHIVO-PRODUCTOS NEXT RECORD
                   AT END
                       MOVE "S" TO WS-FIN-ARCHIVO
                   NOT AT END
                      DISPLAY "1. LLAVE (CODPRODU) : " CODPRODU
                      DISPLAY "2. FECHA INICIO     : " FECINI
                      DISPLAY "3. HORA INICIO      : " HORINI
                      DISPLAY "4. HORA FIN         : " HORFIN
                      DISPLAY "5. FECHA PAGO       : " FECPAGO
                      DISPLAY "6. IMP MIN SUSCRIP  : " IMPMINSU
                      DISPLAY "7. IMP MAX SUSCRIP  : " IMPMAXSU
                      DISPLAY "8. TIPO CUPON       : " TIPCUPON
                      DISPLAY "9. BASE CALCULO     : " BASECALC
                      DISPLAY "10. FREC PAGO       : " FRECPAG
                      DISPLAY "11. CURVA FLOTANTE  : " CURVAFLO
                      DISPLAY "12. DESTINATARIO    : " DESTEMI
                      DISPLAY "13. TXT CUPON       : " TXTCUPON
                      DISPLAY "14. ORDEN IRREVOCA  : " ORDIRREV
                      DISPLAY "15. MULTIFASE       : " MULTFASE
                      DISPLAY "16. NUM FASE        : " NUMFASE
                      DISPLAY "17. TIPO OPCION     : " TIPOPCIO
                      DISPLAY "18. FECHA OPCION    : " FECHOPCI
                      DISPLAY "19. PERPETUA        : " PERPETUA
                      DISPLAY "20. MAKE WHOLE      : " MAKWHOLE
                      DISPLAY "21. FEC MAKE WHOLE  : " FECMAWHO
                      DISPLAY "22. % CUPON BLOQUE  : " PORCENCU
                      DISPLAY "23. % TIR BLOQUE    : " PORCENTI
                      DISPLAY "----------------------------------------"
               END-READ
           END-PERFORM.

           CLOSE ARCHIVO-PRODUCTOS.
       2000-FIN.
           DISPLAY "PRESIONA ENTER PARA VOLVER AL MENU..."
           ACCEPT WS-STATUS.

           END PROGRAM DOCUMENTOS-INDEXADOS.
