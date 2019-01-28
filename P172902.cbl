      ******************************************************************
      * Author: PEDRO SPIELMANN
      * Date: 08/11/2017
      * OBS: ARQUIVO GERADO A PARTIR DA IDE OPENCOBOL.
      *    PASSOS PARA FUNCIONAR CODIGO:
      *     - DEPOIS DE INSTALAR A IDE, EXECUTAR SEMPRE COMO ADM
      *     - NA IDE, CLICAR EM Edit > Preferences > Run > MARCAR Run in external terminal
      *     - PARA ABRIR QUALQUER PROGRAMA COBOL NESSA IDE, DEVE PRIMEIRO ABRIR
      *       ELA E DEPOIS ABRIR O PROGRAMA. NUNCA ABRIR DIRETO.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172902.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADMODEL ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CADMOD-CODIGO
               FILE STATUS IS ERRO
               ALTERNATE RECORD KEY MODELO DUPLICATES.
           SELECT CADMARCA ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CODIGO
               FILE STATUS IS ERRO
               ALTERNATE RECORD KEY MARCA DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

           FD CADMODEL
              LABEL  RECORD IS STANDARD
              VALUE OF FILE-ID IS "CADMODEL.DAT".
           01 REGMODEL.
               02 CADMOD-CODIGO.
                   05 CONTCODIGO PIC 9(3) VALUE ZEROS.
                   05 CODIGOMODELO PIC 9(3) VALUE ZEROS.
               02 MODELO PIC X(20) VALUE SPACES.
               02 CADMOD-MARCA PIC X(20) VALUE SPACES.

           FD CADMARCA
              LABEL  RECORD IS STANDARD
              VALUE OF FILE-ID IS "CADMARCA.DAT".
           01 REGMARCA.
               02 CODIGO PIC 9(3).
               02 MARCA PIC X(30).

       WORKING-STORAGE SECTION.

           01 ERRO PIC X(2) VALUE SPACES.
           01 OPC PIC X(1) VALUE SPACES.
           01 EFE PIC 9(2).
           01 BITVALIDA PIC 9(1).

       SCREEN SECTION.

      *Telas geradas no DOSBOX 0.74 (possui um gerador de telas proprio)
       01  TELAMODELO.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "ษอออออออออออออออออCADASTRO DE MODELOออออ".
           05  LINE 01  COLUMN 41
               VALUE  "ออออออออออป".
           05  LINE 02  COLUMN 01
               VALUE  "บ".
           05  LINE 02  COLUMN 41
               VALUE  "          บ".
           05  LINE 03  COLUMN 01
               VALUE  "บ".
           05  LINE 03  COLUMN 41
               VALUE  "          บ".
           05  LINE 04  COLUMN 01
               VALUE  "บ   CODIGO:   .".
           05  LINE 04  COLUMN 41
               VALUE  "          บ".
           05  LINE 05  COLUMN 01
               VALUE  "บ".
           05  LINE 05  COLUMN 41
               VALUE  "          บ".
           05  LINE 06  COLUMN 01
               VALUE  "บ   MARCA:".
           05  LINE 06  COLUMN 41
               VALUE  "          บ".
           05  LINE 07  COLUMN 01
               VALUE  "บ".
           05  LINE 07  COLUMN 41
               VALUE  "          บ".
           05  LINE 08  COLUMN 01
               VALUE  "บ   MODELO:".
           05  LINE 08  COLUMN 41
               VALUE  "          บ".
           05  LINE 09  COLUMN 01
               VALUE  "บ".
           05  LINE 09  COLUMN 41
               VALUE  "          บ".
           05  LINE 10  COLUMN 01
               VALUE  "บ".
           05  LINE 10  COLUMN 41
               VALUE  "          บ".
           05  LINE 11  COLUMN 01
               VALUE  "บอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 11  COLUMN 41
               VALUE  "ออออออออออบ".
           05  LINE 12  COLUMN 01
               VALUE  "บ".
           05  LINE 12  COLUMN 41
               VALUE  "          บ".
           05  LINE 13  COLUMN 01
               VALUE  "บ".
           05  LINE 13  COLUMN 41
               VALUE  "          บ".
           05  LINE 14  COLUMN 01
               VALUE  "บ".
           05  LINE 14  COLUMN 41
               VALUE  "          บ".
           05  LINE 15  COLUMN 01
               VALUE  "บ".
           05  LINE 15  COLUMN 41
               VALUE  "          บ".
           05  LINE 16  COLUMN 01
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 16  COLUMN 41
               VALUE  "ออออออออออผ".
           05  TCONCODIGO
               LINE 04  COLUMN 12  PIC 9(03)
               USING  CONTCODIGO
               HIGHLIGHT.
           05  TCODIGOMODELO
               LINE 04  COLUMN 16  PIC 9(03)
               USING  CODIGOMODELO
               HIGHLIGHT.
           05  TMODELO
               LINE 08  COLUMN 12  PIC X(20)
               USING  MODELO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

      *Cria o arquio "CADMODEL.dat"
       TELA-MODELO.

           DISPLAY TELAMODELO
           PERFORM LIMPAERRO

           OPEN I-O CADMODEL

           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = 35
                   OPEN OUTPUT CADMODEL
                   CLOSE CADMODEL
                   DISPLAY "ARQUIVO CADMODEL SENDO CRIADO" AT 1305
                   GO TO TELA-MODELO
               ELSE
                 DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADMODEL" AT 1305
           ELSE
               CONTINUE.

      *Cadastra o codigo do modelo
       CAD-CODIGO.

           DISPLAY "F2 - SAIR" AT 1705

           ACCEPT TCONCODIGO
           ACCEPT TCODIGOMODELO
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 02
                   GO TO SAIR
           END-IF

               IF CONTCODIGO AND CODIGOMODELO = ZERO
                  DISPLAY "NAO PODE CONTER APENAS VALORES ZEROS" AT 1305
                   GO TO CAD-CODIGO
               ELSE
                   PERFORM LERMODELO
                   PERFORM LERCHAVE
                   GO TO CAD-MODELO.

      *Le o arquivo "CADMARCA.dat"
       LERCHAVE.

           MOVE CONTCODIGO TO CODIGO
           OPEN INPUT CADMARCA
           READ CADMARCA

           IF ERRO NOT = "23"
               IF ERRO = "00"
                   DISPLAY MARCA AT 0611
                   CLOSE CADMARCA
               ELSE
                   DISPLAY "ERRO AO LER O CADMARCA" AT 1305
                   GO TO CAD-CODIGO
               END-IF
           ELSE
               DISPLAY "MARCA NAO ENCONTRADA" AT 1305
               PERFORM LIMPAVARIAVEL
               GO TO CAD-CODIGO
           END-IF.

      *Cadastra o modelo
       CAD-MODELO.

           DISPLAY "F1 - VOLTA AO CAMPO ANTERIOR" AT 1715

           ACCEPT MODELO AT 0812
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-CODIGO
               ELSE IF EFE = 02
                   GO TO SAIR
           END-IF

               IF MODELO = SPACES
                  DISPLAY "DIGITE ALGUM MODELO" AT 1305
                  GO TO CAD-MODELO
               ELSE
                   CONTINUE.

      *Permite ao usuแrio validar os dados
       VERIFICA.

           DISPLAY "OS VALORES ESTAO MESMO CORRETOS? " AT 1305
           DISPLAY "S/N ?" AT 1405

           ACCEPT OPC AT 1411

              IF OPC = "S" OR "s"
                  IF BITVALIDA = 1
                      GO TO ALTERAR
                  ELSE
                      GO TO GRAVARCHAVE
              ELSE
                  CONTINUE.

      *Grava os dados no registro
       GRAVARCHAVE.

           WRITE REGMODEL
           PERFORM LIMPAERRO

           IF ERRO = "00" OR "02"
               DISPLAY "DADOS GRAVADOS" AT 1305
               GO TO CAD-CODIGO
           ELSE
               DISPLAY "ERRO NA GRAVACAO DO ARQUIVO" AT 1305
               GO TO CAD-CODIGO.

      *Verificao da entrada do modelo
       LERMODELO.

           READ CADMODEL

               IF ERRO NOT = "23"
                 IF ERRO = "00"
                     DISPLAY TELAMODELO
                     PERFORM LERCHAVE
           DISPLAY "JA ESTA CADASTRAD0, DESEJA MUDAR O MODELO ?" AT 1302
                   DISPLAY "S/N ?" AT 1402
                   ACCEPT OPC AT 1406

                       IF OPC = "S" OR "s"
                         MOVE 1 TO BITVALIDA
                         GO TO CAD-MODELO
                       ELSE
                         PERFORM LIMPAVARIAVEL
                         MOVE 0 TO BITVALIDA
                         GO TO SAIR
                 ELSE
                  DISPLAY "ERRO NA LEITURA DO ARQUIVO CADMODELO" AT 1305
                   GO TO CAD-CODIGO.

      *Altera o modelo
       ALTERAR.

           PERFORM LIMPAERRO
           REWRITE REGMODEL

           IF ERRO = "00" OR "02"
               DISPLAY "MODELO ALTERADA" AT 1305
               GO TO CAD-CODIGO
           ELSE
               DISPLAY "ERRO AO ALTERAR A MODELO" AT 1305
               GO TO CAD-CODIGO.

      *Limpa os dados das variแveis na tela do usuแrio
       LIMPAVARIAVEL.

           MOVE SPACES TO OPC MODELO
           MOVE ZEROS TO CONTCODIGO CODIGOMODELO BITVALIDA.

      *Limpa as mensagens de erro na tela do usuแrio
       LIMPAERRO.

           DISPLAY "                                           " AT 1302
           DISPLAY "                                           "AT 1402.

      *Fecha o programa
       SAIR.

           CLOSE CADMODEL
           CLOSE CADMARCA.

       END PROGRAM P172902.
