      ******************************************************************
      * Author: Johnathan Silva
      * Date: 02/01/2022
      * Purpose: Programa para ler notas de alunos
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO-LEITOR-DE-NOTAS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VARIAVEIS.
           03 WS-NOME                           PIC X(20).
           03 WS-MATERIA                        PIC X(20).
           03 WS-NOTA1                          PIC 99 VALUE 0.
           03 WS-NOTA2                          PIC 99 VALUE 0.
           03 WS-NOTA3                          PIC 99 VALUE 0.
           03 WS-NOTA4                          PIC 99 VALUE 0.
           03 WS-SOMANOTA1                      PIC 99 VALUE 0.
           03 WS-SOMANOTA2                      PIC 99 VALUE 0.
           03 WS-NOTAFINAL                      PIC 99 VALUE 0.
       77 WS-NOTAS                              PIC 99 VALUE 0.
       77 WS-IND                                PIC X(20).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM P001-INICIO.
           PERFORM P500-CALC.
           PERFORM P900-FIM.

       P001-INICIO.
           PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND = 'S'
               INITIALISE WS-VARIAVEIS

               DISPLAY '***CALCULE SUA NOTA***'

               DISPLAY 'DIGITE SEU NOME: '
               ACCEPT WS-NOME

               DISPLAY 'DIGITE A MATERIA: '
               ACCEPT WS-MATERIA

               DISPLAY 'DIGITE SUA PRIMEIRA NOTA: '
               ACCEPT WS-NOTA1
               IF WS-NOTA1 IS NOT NUMERIC OR WS-NOTA1 < 0
                   PERFORM P400-ERR
                   PERFORM P900-FIM
               ELSE
                   DISPLAY 'DIGITE SUA SEGUNDA NOTA: '
                   ACCEPT WS-NOTA2
                   IF WS-NOTA2 IS NOT NUMERIC OR WS-NOTA2 < 0
                   PERFORM P400-ERR
                   PERFORM P900-FIM
               ELSE
                   DISPLAY 'DIGITE SUA TERCEIRA NOTA: '
                   ACCEPT WS-NOTA3
                   IF WS-NOTA3 IS NOT NUMERIC OR WS-NOTA3 < 0
                   PERFORM P400-ERR
                   PERFORM P900-FIM
               ELSE
                   DISPLAY 'DIGITE SUA QUARTA NOTA: '
                   ACCEPT WS-NOTA4
                   IF WS-NOTA4 IS NOT NUMERIC OR WS-NOTA4 < 0
                   PERFORM P400-ERR
                   PERFORM P900-FIM
               ELSE
                   PERFORM P500-CALC
               END-IF
           END-PERFORM
               .
        P300-ERR.
               DISPLAY 'ERRO DE PROCESSAMENTO.'
               PERFORM P900-FIM
               .
        P400-ERR.
               DISPLAY 'ERRO DE PROCESSAMENTO DIGITE UM CARACTER VALIDO'
               PERFORM P900-FIM
               .
        P500-CALC.
               COMPUTE WS-SOMANOTA1 = WS-NOTA1 + WS-NOTA2
                                     ON SIZE ERROR PERFORM P400-ERR
               END-COMPUTE

               COMPUTE WS-SOMANOTA2 = WS-NOTA3 + WS-NOTA4
                                     ON SIZE ERROR PERFORM P400-ERR
               END-COMPUTE

               COMPUTE WS-NOTAS = WS-SOMANOTA1 + WS-SOMANOTA2
                                     ON SIZE ERROR PERFORM P400-ERR
               END-COMPUTE

               DIVIDE WS-NOTAS                     BY 4 GIVING WS-NOTAS
                                     ON SIZE ERROR PERFORM P400-ERR
               END-DIVIDE

               IF WS-NOTAS GREATER THAN 7
                   DISPLAY 'NOME DO ALUNO: ' WS-NOME
                   DISPLAY 'MATERIA: '       WS-MATERIA
                   DISPLAY 'PARABENS,APROVADO!'
               ELSE
                   DISPLAY 'NOME DO ALUNO: ' WS-NOME
                   DISPLAY 'MATERIA: '       WS-MATERIA
                   DISPLAY 'REPROVADO'
               END-IF

               DISPLAY 'DESEJA CONTINUAR? '
               ACCEPT WS-IND

               IF WS-IND = 'N'
                   DISPLAY 'PROGRAMA ENCERRADO.'
                   PERFORM P900-FIM
               END-IF
               .
        P900-FIM.

               STOP RUN.

       END PROGRAM DESAFIO-LEITOR-DE-NOTAS.
