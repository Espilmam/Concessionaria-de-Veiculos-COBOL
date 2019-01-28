      ******************************************************************
      * Author: PEDRO SPIELMANN
      * Date: 01/11/2017
      * OBS: ARQUIVO GERADO A PARTIR DA IDE OPENCOBOL.
      *    PASSOS PARA FUNCIONAR CODIGO:
      *     - DEPOIS DE INSTALAR A IDE, EXECUTAR SEMPRE COMO ADM
      *     - NA IDE, CLICAR EM Edit > Preferences > Run > MARCAR Run in external terminal
      *     - PARA ABRIR QUALQUER PROGRAMA COBOL NESSA IDE, DEVE PRIMEIRO ABRIR
      *       ELA E DEPOIS ABRIR O PROGRAMA. NUNCA ABRIR DIRETO.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172901.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADMARCA ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CODIGO
               FILE STATUS IS ERRO
               ALTERNATE RECORD KEY MARCA WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

           FD CADMARCA
              LABEL  RECORD IS STANDARD
              VALUE OF FILE-ID IS "CADMARCA.DAT".
           01 REGMARCA.
               05 CODIGO PIC 9(3) VALUE ZEROS.
               05 MARCA PIC X(30) VALUE SPACES.
               05 ORIGEM PIC X(1) VALUE SPACES.

       WORKING-STORAGE SECTION.

           01 ERRO PIC X(2) VALUE "00".
           01 OPC PIC X(1) VALUE SPACES.
           01 EFE PIC 9(2).
           01 BITVALIDA PIC 9(1).

       SCREEN SECTION.

      *Telas geradas no DOSBOX 0.74 (possui um gerador de telas proprio)
       01  TELAMARCA.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "ษอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 01  COLUMN 41
               VALUE  "ออออป".
           05  LINE 02  COLUMN 01
               VALUE  "บ           CADASTRO DE MARCA".
           05  LINE 02  COLUMN 41
               VALUE  "    บ".
           05  LINE 03  COLUMN 01
               VALUE  "บ".
           05  LINE 03  COLUMN 41
               VALUE  "    บ".
           05  LINE 04  COLUMN 01
               VALUE  "บ   CODIGO:".
           05  LINE 04  COLUMN 41
               VALUE  "    บ".
           05  LINE 05  COLUMN 01
               VALUE  "บ".
           05  LINE 05  COLUMN 41
               VALUE  "    บ".
           05  LINE 06  COLUMN 01
               VALUE  "บ   MARCA:".
           05  LINE 06  COLUMN 41
               VALUE  "    บ".
           05  LINE 07  COLUMN 01
               VALUE  "บ".
           05  LINE 07  COLUMN 41
               VALUE  "    บ".
           05  LINE 08  COLUMN 01
               VALUE  "บ   ORIGEM:".
           05  LINE 08  COLUMN 41
               VALUE  "    บ".
           05  LINE 09  COLUMN 01
               VALUE  "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 09  COLUMN 41
               VALUE  "ฤฤฤฤถ".
           05  LINE 10  COLUMN 01
               VALUE  "บ".
           05  LINE 10  COLUMN 41
               VALUE  "    บ".
           05  LINE 11  COLUMN 01
               VALUE  "บ".
           05  LINE 11  COLUMN 41
               VALUE  "    บ".
           05  LINE 12  COLUMN 01
               VALUE  "บ".
           05  LINE 12  COLUMN 41
               VALUE  "    บ".
           05  LINE 13  COLUMN 01
               VALUE  "บ".
           05  LINE 13  COLUMN 41
               VALUE  "    บ".
           05  LINE 14  COLUMN 01
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 14  COLUMN 41
               VALUE  "ออออผ".
           05  TCODIGO
               LINE 04  COLUMN 13  PIC 9(3)
               USING  CODIGO.
           05  TMARCA
               LINE 06  COLUMN 12  PIC X(30)
               USING  MARCA
               HIGHLIGHT.
           05  TORIGEM
               LINE 08  COLUMN 13  PIC X(01)
               USING  ORIGEM
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

      *Cria o arquio "CADMARCA.dat"
       TELA-MARCA.

           DISPLAY TELAMARCA
           OPEN I-O CADMARCA
           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = 35
                   OPEN OUTPUT CADMARCA
                   CLOSE CADMARCA
                  DISPLAY "ARQUIVO CADMARCA SENDO CRIADO" AT 1005
                   GO TO TELA-MARCA
               ELSE
                  DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADMARCA" AT 1005
           ELSE
               CONTINUE.

      *Cadastra o codigo da marca
       CAD-CODIGO.

           DISPLAY TELAMARCA
           DISPLAY "F2 - SAIR" AT 1501
           ACCEPT CODIGO AT 0413
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 02
                   GO TO SAIR
           END-IF

               IF CODIGO = ZERO
                  DISPLAY "NAO PODE CONTER APENAS VALORES ZEROS" AT 1005
                  GO TO CAD-CODIGO
               ELSE
                   PERFORM LERCHAVE

                   CONTINUE.

      *Cadastra o nome da marca
       CAD-MARCA.

           DISPLAY "F1 - VOLTA AO CAMPO ANTERIOR" AT 1510

           ACCEPT MARCA AT 0612
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-CODIGO
               ELSE IF EFE = 02
                   GO TO SAIR
           END-IF

               IF MARCA = SPACES
                   DISPLAY "DIGITE ALGO NA MARCA"
                   GO TO CAD-MARCA
               ELSE

                   CONTINUE.

      *Cadastra a origem da marca
       CAD-ORIGEM.

           PERFORM MOSTRAORIGEM
           ACCEPT ORIGEM AT 0813
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-MARCA
               ELSE IF EFE = 02
                   GO TO SAIR
           END-IF

               IF ORIGEM NOT = "N" AND NOT = "I" OR ORIGEM = SPACES
                   DISPLAY "DIGITE UMA OPCAO VALIDA" AT 1005
                   GO TO CAD-ORIGEM
               ELSE
                   PERFORM LIMPAORIGEM
                   CONTINUE.

      *Permite ao usuario validar os dados
       VERIFICA.

           DISPLAY "OS VALORES ESTAO MESMO CORRETOS? "AT 1005
           DISPLAY "S/N ?" AT 1105

           ACCEPT OPC AT 1111

              IF OPC = "S" OR "s"
                  IF BITVALIDA = 1
                      GO TO ALTERAR
                  ELSE
                      GO TO GRAVARCHAVE
              ELSE
                  GO TO LIMPAVARIAVEL
                  GO TO TELA-MARCA.

      *Grava os dados no registro
       GRAVARCHAVE.

           WRITE REGMARCA

           IF ERRO = "00" OR "02"
               DISPLAY "DADOS GRAVADOS" AT 1005
               GO TO SAIR
           ELSE
               DISPLAY "ERRO NA GRAVACAO DO ARQUIVO" AT 1005
               GO TO SAIR.

      *Verificao da entrada da marca
       LERCHAVE.

           READ CADMARCA
           DISPLAY TELAMARCA
               IF ERRO NOT = "23"
                 IF ERRO = "00"
                   DISPLAY "JA ESTA CADASTRAD0" AT 1005
                   DISPLAY "DESEJA MUDAR A MARCA ?" AT 1105
                   DISPLAY "S/N ?" AT 1205
                   ACCEPT OPC AT 1211

                       IF OPC = "S" OR "s"
                         PERFORM LIMPAVARIAVEL
                         MOVE 1 TO BITVALIDA
                         GO TO CAD-MARCA
                       ELSE
                         PERFORM LIMPAVARIAVEL
                         MOVE 0 TO BITVALIDA
                         GO TO TELA-MARCA
                 ELSE
                   DISPLAY "ERRO NA LEITURA DO ARQUIVO CADMARCA" AT 1005
                   GO TO TELA-MARCA.

      *Altera a marca
       ALTERAR.

           REWRITE REGMARCA

           IF ERRO = "00" OR "02"
               DISPLAY "MARCA ALTERADA" AT 1005
               GO TO CAD-CODIGO
           ELSE
               DISPLAY "ERRO AO ALTERAR A MARCA" AT 1005
               GO TO CAD-CODIGO.

      *Limpa os dados das variaveis na tela do usuแrio
       LIMPAVARIAVEL.

           MOVE SPACES TO OPC MARCA ORIGEM.
           DISPLAY "                " AT 0612
           DISPLAY " " AT 0813
           DISPLAY "            " AT 0815.

      *Limpa as mensagens de erro na tela do usuario
       LIMPAERRO.

           DISPLAY "                                     " AT 1005
           DISPLAY "                                     " AT 1105
           DISPLAY "                                     " AT 1205.
       MOSTRAORIGEM.

           DISPLAY "N - NACIONAL" AT 0647
           DISPLAY "I - INTERNACIONAL" AT 0847.
       LIMPAORIGEM.
           DISPLAY "            " AT 0647
           DISPLAY "                 " AT 0847.

      *Fecha o programa
       SAIR.

           CLOSE CADMARCA.

       END PROGRAM P172901.
