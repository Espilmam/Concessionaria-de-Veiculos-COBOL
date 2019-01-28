      ******************************************************************
      * Author: PEDRO SPIELMANN
      * Date: 16/11/2017
      * OBS: ARQUIVO GERADO A PARTIR DA IDE OPENCOBOL.
      *    PASSOS PARA FUNCIONAR CODIGO:
      *     - DEPOIS DE INSTALAR A IDE, EXECUTAR SEMPRE COMO ADM
      *     - NA IDE, CLICAR EM Edit > Preferences > Run > MARCAR Run in external terminal
      *     - PARA ABRIR QUALQUER PROGRAMA COBOL NESSA IDE, DEVE PRIMEIRO ABRIR
      *       ELA E DEPOIS ABRIR O PROGRAMA. NUNCA ABRIR DIRETO.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172903.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADPROPR ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CPF
               FILE STATUS IS ERRO
               ALTERNATE RECORD KEY EMAIL DUPLICATES.
           SELECT CADCEP ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CEP
               FILE STATUS IS ERRO
               ALTERNATE RECORD KEY IS LOGRADOURO WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

           FD CADPROPR
              LABEL  RECORD IS STANDARD
              VALUE OF FILE-ID IS "CADPROPR.DAT".
           01 REGPROPR.
               02 CPF PIC 9(11) VALUE ZEROS.
               02 EMAIL PIC X(30) VALUE SPACES.
               02 NOME PIC X(20) VALUE SPACES.
               02 COMPLEMENTO PIC X(30) VALUE SPACES.
               02 TELEFONE.
                   05 DDD PIC 9(2) VALUE ZEROS.
                   05 NUMERO PIC 9(9) VALUE ZEROS.
               02 DATANASC.
                   05 DIA PIC 9(2) VALUE ZEROS.
                   05 MES PIC 9(2) VALUE ZEROS.
                   05 ANO PIC 9(4) VALUE ZEROS.

           FD CADCEP
              LABEL  RECORD IS STANDARD
              VALUE OF FILE-ID IS "CADCEP.DAT".
           01 REGCEP.
               02 CEP PIC 9(8).
               02 LOGRADOURO PIC X(35) VALUE SPACES.
               02 BAIRRO PIC X(26) VALUE SPACES.
               02 CIDADE PIC X(20) VALUE SPACES.
               02 UF PIC X(2) VALUE SPACES.
               02 REFERENCIA PIC X(35) VALUE SPACES.
               02 LATITUDE PIC X(15) VALUE SPACES.
               02 LONGITUDE PIC X(15) VALUE SPACES.

       WORKING-STORAGE SECTION.

           01 NUM PIC 9(4) VALUE ZEROS.
           01 ERRO PIC X(2) VALUE "00".
           01 OPC PIC X(1) VALUE SPACES.
           01 EFE PIC 9(2).
           01 BITVALIDA PIC 9(1) VALUE ZEROES.

       SCREEN SECTION.

      *Telas geradas no DOSBOX 0.74 (possui um gerador de telas proprio)
       01  TELAPROP.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "ษอออออออออออออออออออออCADASTRO DE PROPRI".
           05  LINE 01  COLUMN 41
               VALUE  "ETARIOออออออออออออออออออออออออออป".
           05  LINE 02  COLUMN 01
               VALUE  "บ".
           05  LINE 02  COLUMN 41
               VALUE  "                                บ".
           05  LINE 03  COLUMN 01
               VALUE  "บ   CPF:                DATA NASCIMENTO:".
           05  LINE 03  COLUMN 41
               VALUE  "                                บ".
           05  LINE 04  COLUMN 01
               VALUE  "บ   NOME:".
           05  LINE 04  COLUMN 41
               VALUE  "                                บ".
           05  LINE 05  COLUMN 01
               VALUE  "บ".
           05  LINE 05  COLUMN 41
               VALUE  "                                บ".
           05  LINE 06  COLUMN 01
               VALUE  "บอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 06  COLUMN 41
               VALUE  "ออออออออออออออออออออออออออออออออบ".
           05  LINE 07  COLUMN 01
               VALUE  "บ   CEP:                LOGRADOURO:".
           05  LINE 07  COLUMN 41
               VALUE  "                                บ".
           05  LINE 08  COLUMN 01
               VALUE  "บ   BAIRRO:".
           05  LINE 08  COLUMN 41
               VALUE  "CIDADE:                         บ".
           05  LINE 09  COLUMN 01
               VALUE  "บ   UF:      COMPLEMENTO:".
           05  LINE 09  COLUMN 41
               VALUE  "                                บ".
           05  LINE 10  COLUMN 01
               VALUE  "บอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 10  COLUMN 41
               VALUE  "ออออออออออออออออออออออออออออออออบ".
           05  LINE 11  COLUMN 01
               VALUE  "บ".
           05  LINE 11  COLUMN 41
               VALUE  "                                บ".
           05  LINE 12  COLUMN 01
               VALUE  "บ   TELEFONE:".
           05  LINE 12  COLUMN 41
               VALUE  "                                บ".
           05  LINE 13  COLUMN 01
               VALUE  "บ   E-MAIL:".
           05  LINE 13  COLUMN 41
               VALUE  "                                บ".
           05  LINE 14  COLUMN 01
               VALUE  "บ".
           05  LINE 14  COLUMN 41
               VALUE  "                                บ".
           05  LINE 15  COLUMN 01
               VALUE  "บอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 15  COLUMN 41
               VALUE  "ออออออออออออออออออออออออออออออออบ".
           05  LINE 16  COLUMN 01
               VALUE  "บ".
           05  LINE 16  COLUMN 41
               VALUE  "                                บ".
           05  LINE 17  COLUMN 01
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 17  COLUMN 41
               VALUE  "ออออออออออออออออออออออออออออออออผ".
           05  TCPF
               LINE 03  COLUMN 10  PIC 9(11)
               USING  CPF
               HIGHLIGHT.
           05  TDATANASC
               LINE 03  COLUMN 42  PIC 9(08)
               USING  DATANASC
               HIGHLIGHT.
           05  TNOME
               LINE 04  COLUMN 11  PIC X(20)
               USING  NOME
               HIGHLIGHT.
           05  TCEP
               LINE 07  COLUMN 10  PIC 9(08)
               USING  CEP
               HIGHLIGHT.
           05  TTELEFONE
               LINE 12  COLUMN 15  PIC 99.99999.9999
               USING  TELEFONE
               HIGHLIGHT.
           05  TEMAIL
               LINE 13  COLUMN 13  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

      *Cria o arquivo "CADPROPR.dat"
       TELA-PROPR.

           DISPLAY TELAPROP
           OPEN I-O CADPROPR

           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = 35
                   OPEN OUTPUT CADPROPR
                   CLOSE CADPROPR
                  DISPLAY "ARQUIVO CADPROPR SENDO CRIADO" AT 1612
                   GO TO TELA-PROPR
               ELSE
                  DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADPROPR" AT 1612
           ELSE
               CONTINUE.

      *Cadastra o CPF
       CAD-CPF.
           PERFORM LIMPAVARIAVEL
           DISPLAY "F2 - SAIR" AT 1820

           ACCEPT TCPF
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 02
                   GO TO SAIR
           END-IF

               IF CPF = ZEROS
                   DISPLAY "DIGITE UM CPF" AT 1612
                   GO TO CAD-CPF
               ELSE
                   PERFORM LERCHAVE
                   CONTINUE.

      *Cadastra o nome
       CAD-NOME.

           DISPLAY "F1 - VOLTA AO CAMPO ANTERIOR" AT 1840

           ACCEPT TNOME
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-CPF
               ELSE IF EFE = 02
                   GO TO SAIR
           END-IF

               IF NOME = SPACES
                   DISPLAY "DIGITE ALGO NO NOME" AT 1612
                   GO TO CAD-NOME
               ELSE
                   CONTINUE.

      *Cadastra a data de nascimento
       CAD-DATANASC.

           ACCEPT TDATANASC
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-NOME
               ELSE IF EFE = 2
                   GO TO SAIR
           END-IF

               IF DATANASC = ZEROS
                   DISPLAY "DIGITE ALGO NA DATA DE NASCIMENTO" AT 1612
                   GO TO CAD-DATANASC
               ELSE
                   CONTINUE.

      *Recebe dados do arquivo "CADCEP.dat"
       CAD-CEP.

           ACCEPT TCEP
           PERFORM LIMPAERRO

               IF CEP = ZEROS
                   DISPLAY "DIGITE UM CEP" AT 1612
                   GO TO CAD-CEP
               ELSE
                   OPEN I-O CADCEP
                   READ CADCEP

                   IF ERRO NOT = "23"
                       IF ERRO = "00"
                           DISPLAY CEP AT 0710
                           DISPLAY LOGRADOURO AT 0737
                           DISPLAY BAIRRO AT 0813
                           DISPLAY CIDADE AT 0849
                           DISPLAY UF AT 0909
                           ACCEPT COMPLEMENTO AT 0927

                           ACCEPT EFE FROM ESCAPE KEY

                               IF EFE = 01
                                   GO TO CAD-DATANASC
                               END-IF
                       ELSE
                           DISPLAY "ERRO AO LER O CADCEP" AT 1612
                           GO TO TELA-PROPR
                       END-IF
                   ELSE
                       DISPLAY "CEP NAO ENCONTRADO" AT 1612
                       GO TO CAD-CEP
                   END-IF.

      *Cadastra o telefone
       CAD-TELEFONE.

           ACCEPT TTELEFONE
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-CEP
               ELSE IF EFE = 2
                   GO TO SAIR
           END-IF

               IF TELEFONE = ZEROS
                   DISPLAY "DIGITE UM TELEFONE" AT 1612
                   GO TO CAD-TELEFONE
               ELSE
                   CONTINUE.

      *Cadastra o e-mail
       CAD-EMAIL.

           ACCEPT TEMAIL
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-TELEFONE
           END-IF

               IF EMAIL = SPACES
                   DISPLAY "DIGITE UM E-MAIL" AT 1612
                   GO TO CAD-EMAIL
               ELSE
                   CONTINUE.

      *Permite ao usuแrio validar os dados
       VERIFICA.

           DISPLAY "OS VALORES ESTรO MESMO CORRETOS ?" AT 1612
           DISPLAY "S/N ?" AT 1650

           ACCEPT OPC AT 1656

               IF OPC = "S" OR "s"
                   IF BITVALIDA = 1
                       GO TO ALTERAR
                   ELSE
                       GO TO GRAVARCHAVE
               ELSE
                   GO TO CAD-CPF.

      *Grava os dados no registro
       GRAVARCHAVE.

           WRITE REGPROPR

           IF ERRO = "00" OR "02"
               DISPLAY "DADOS GRAVADOS" AT 1612
               GO TO SAIR
           ELSE
               DISPLAY "ERRO NA GRAVACAO DO ARQUIVO" AT 1612
               GO TO SAIR.

      *Verificao da entrada do CPF
       LERCHAVE.

           READ CADPROPR
           DISPLAY ERRO
               IF ERRO NOT = "23"
                 IF ERRO = "00"
                   DISPLAY TELAPROP
                   DISPLAY "JA ESTA CADASTRAD0. DESEJA MUDAR ?" AT 1612
                   DISPLAY "S/N ?" AT 1650
                   ACCEPT OPC AT 1656

                       IF OPC = "S" OR "s"
                         MOVE 1 TO BITVALIDA
                         GO TO CAD-NOME
                       ELSE
                         PERFORM LIMPAVARIAVEL
                         MOVE 0 TO BITVALIDA
                         GO TO TELA-PROPR
                 ELSE
                   DISPLAY "ERRO NA LEITURA DO ARQUIVO CADVEIC" AT 1402
                   GO TO TELA-PROPR.

      *Altera o CPF
       ALTERAR.

           PERFORM LIMPAERRO
           REWRITE REGPROPR

           IF ERRO = "00" OR "02"
               DISPLAY "CPF ALTERADO" AT 1402
               GO TO SAIR
           ELSE
               DISPLAY "ERRO AO ALTERAR O MODELO" AT 1402
               GO TO SAIR.

      *Limpa os dados das variแveis na tela do usuario
       LIMPAVARIAVEL.

           MOVE ZEROS TO CPF TELEFONE DATANASC CEP BITVALIDA
           MOVE SPACES TO NOME EMAIL LOGRADOURO BAIRRO CIDADE UF.

      *Limpa as mensagens de erro na tela do usuario
       LIMPAERRO.

       DISPLAY "                                              " AT 1612.

      *Fecha o programa
       SAIR.

           CLOSE CADPROPR.
           CLOSE CADCEP.

       END PROGRAM P172903.
