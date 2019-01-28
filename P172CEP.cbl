      ******************************************************************
      * Author: PEDRO SPIELMANN
      * Date: 13/09/2017
      * OBS: ARQUIVO GERADO A PARTIR DA IDE OPENCOBOL.
      *    PASSOS PARA FUNCIONAR CODIGO:
      *     - DEPOIS DE INSTALAR A IDE, EXECUTAR SEMPRE COMO ADM
      *     - NA IDE, CLICAR EM Edit > Preferences > Run > MARCAR Run in external terminal
      *     - PARA ABRIR QUALQUER PROGRAMA COBOL NESSA IDE, DEVE PRIMEIRO ABRIR
      *       ELA E DEPOIS ABRIR O PROGRAMA. NUNCA ABRIR DIRETO.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172CEP.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCEP ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CEP
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS LOGRADOURO WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

           FD CADCEP
               LABEL RECORD IS STANDARD
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

           01 TABUF PIC X(54)
          VALUE"ACALAPAMBACEDFESGOMAMTMSMGPAPBPRPEPIRJRNRSRORRSCSPSETO".
           01 TUF REDEFINES TABUF.
              03 TUFP PIC X(2) OCCURS 27 TIMES.
           01 IND PIC 9(2) VALUE ZEROS.
           01 ERRO PIC X(2) VALUE "00".
           01 ALT PIC X(1).
           01 SEL PIC 9(01) VALUE ZEROS.
           01 MASK PIC 99999.999.
           01 AUX PIC X(25).
           01 EFE PIC 9(2).
           01 BITVALIDA PIC 9(1) VALUE ZEROS.

       SCREEN SECTION.

       01 LIMPATELA.
           05 BLANK SCREEN.

      *Telas geradas no DOSBOX 0.74 (possui um gerador de telas proprio)
       01  TELAMENU.

           05  LINE 01  COLUMN 01
               VALUE  "ษออออออออออออออออออออออออออออออMENU CADA".
           05  LINE 01  COLUMN 41
               VALUE  "STRO ออออออออออออออออออออออออออออออออออป".
           05  LINE 02  COLUMN 01
               VALUE  "บ".
           05  LINE 02  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 03  COLUMN 01
               VALUE  "บ  1 - CADASTRA       2 - APAGAR       3".
           05  LINE 03  COLUMN 41
               VALUE  " - MOSTRAR       4 - SAIR              บ".
           05  LINE 04  COLUMN 01
               VALUE  "บ".
           05  LINE 04  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 05  COLUMN 01
               VALUE  "บ              ษออออออออออออออออออออออออ".
           05  LINE 05  COLUMN 41
               VALUE  "ออออออออออออออออออป                    บ".
           05  LINE 06  COLUMN 01
               VALUE  "บ              บ".
           05  LINE 06  COLUMN 41
               VALUE  "                  บ                    บ".
           05  LINE 07  COLUMN 01
               VALUE  "ฬออออออออออออออสออออออออออออออออออออออออ".
           05  LINE 07  COLUMN 41
               VALUE  "ออออออออออออออออออสออออออออออออออออออออน".
           05  LINE 08  COLUMN 01
               VALUE  "บ".
           05  LINE 08  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 09  COLUMN 01
               VALUE  "บ".
           05  LINE 09  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 10  COLUMN 01
               VALUE  "บ".
           05  LINE 10  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 11  COLUMN 01
               VALUE  "บ".
           05  LINE 11  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 12  COLUMN 01
               VALUE  "บ".
           05  LINE 12  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 13  COLUMN 01
               VALUE  "บ".
           05  LINE 13  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 14  COLUMN 01
               VALUE  "บ".
           05  LINE 14  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 15  COLUMN 01
               VALUE  "บ".
           05  LINE 15  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 16  COLUMN 01
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 16  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออผ".
           05  TALT
               LINE 06  COLUMN 18  PIC 9(01)
               USING  ALT
               HIGHLIGHT.

       01  TELACAD.

           05  LINE 01  COLUMN 01
               VALUE  "ษอออออออออออออออออออออออออออCADASTROออออ".
           05  LINE 01  COLUMN 41
               VALUE  "ออออออออออออออออออออออออออป".
           05  LINE 02  COLUMN 01
               VALUE  "บ".
           05  LINE 02  COLUMN 41
               VALUE  "                          บ".
           05  LINE 03  COLUMN 01
               VALUE  "บ   1 - CEP:".
           05  LINE 03  COLUMN 41
               VALUE  "                          บ".
           05  LINE 04  COLUMN 01
               VALUE  "บ   2 - LOGRADOURO:".
           05  LINE 04  COLUMN 41
               VALUE  "                          บ".
           05  LINE 05  COLUMN 01
               VALUE  "บ   3 - BAIRRO:".
           05  LINE 05  COLUMN 41
               VALUE  "                          บ".
           05  LINE 06  COLUMN 01
               VALUE  "บ   4 - CIDADE:".
           05  LINE 06  COLUMN 41
               VALUE  "                          บ".
           05  LINE 07  COLUMN 01
               VALUE  "บ   5 - UF:".
           05  LINE 07  COLUMN 41
               VALUE  "                          บ".
           05  LINE 08  COLUMN 01
               VALUE  "บ   6 - REFERENCIA:".
           05  LINE 08  COLUMN 41
               VALUE  "                          บ".
           05  LINE 09  COLUMN 01
               VALUE  "บ   7 - LATITUDE:".
           05  LINE 09  COLUMN 41
               VALUE  "                          บ".
           05  LINE 10  COLUMN 01
               VALUE  "บ   8 - LONGITUDE:".
           05  LINE 10  COLUMN 41
               VALUE  "                          บ".
           05  LINE 11  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 11  COLUMN 41
               VALUE  "ออออออออออออออออออออออออออน".
           05  LINE 12  COLUMN 01
               VALUE  "บ".
           05  LINE 12  COLUMN 41
               VALUE  "                          บ".
           05  LINE 13  COLUMN 01
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 13  COLUMN 41
               VALUE  "ออออออออออออออออออออออออออผ".
           05  TCEP
               LINE 03  COLUMN 15  PIC 99999.999
               USING  CEP
               HIGHLIGHT
               BLANK ZEROS.
           05  TLOGRADOURO
               LINE 04  COLUMN 20  PIC X(35)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 05  COLUMN 16  PIC X(26)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 06  COLUMN 16  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TELAUF
               LINE 07  COLUMN 12  PIC X(2)
               USING  UF
               HIGHLIGHT.
           05  TREFERENCIA
               LINE 08  COLUMN 20  PIC X(35)
               USING  REFERENCIA
               HIGHLIGHT.
           05  TLATITUDE
               LINE 09  COLUMN 18  PIC X(15)
               USING  LATITUDE
               HIGHLIGHT.
           05  TLONGITUDE
               LINE 10  COLUMN 19  PIC X(15)
               USING  LONGITUDE
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

      *Abre o arquivo de registro "CADCEP"
       ARQUIVO.
           OPEN I-O CADCEP

           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = "35"
                   OPEN OUTPUT CADCEP
                   CLOSE CADCEP
                  DISPLAY "ARQUIVO CADCEP SENDO CRIADO" AT 0622
                   GO TO MENU
               ELSE
                  DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADCEP" AT 0622
           ELSE
               CONTINUE.

      *Mostra um menu de opcoes para o usuario (as opcoes do menu estao nas telas criadas, como mostrado acima do codigo)
       MENU.

           DISPLAY TELAMENU

           ACCEPT ALT AT 0618
           DISPLAY "                          " AT 0622

               IF ALT = "1"
                   DISPLAY LIMPATELA
                   GO TO CAD-CEP
               ELSE IF ALT = "2"
                   DISPLAY "DIGITE O CEP: " AT 0622
                   ACCEPT CEP
                   PERFORM APAGAR
                   GO TO MENU
               ELSE IF ALT = "3"
                   PERFORM MOSTRAR
                   GO TO MENU
               ELSE IF ALT = "4"
                   GO TO SAIR
               ELSE
                   DISPLAY "OPCAO INVALIDA" AT 0622
                   GO TO MENU.
      *Cadastra um novo CEP
       CAD-CEP.

           PERFORM LIMPAVARIAVEL
           DISPLAY TELACAD

           ACCEPT TCEP
           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   DISPLAY LIMPATELA
                   GO TO MENU
           END-IF
           DISPLAY "             " AT 1205

               IF CEP = ZEROS OR CEP = "0000000"
                   DISPLAY "DIGITE ALGO" AT 1205
                   GO TO CAD-CEP
               END-IF

               CONTINUE.

      *Verificao da entrada do CEP
       LERCEP.

           READ CADCEP

                IF ERRO NOT = "23"
                 IF ERRO = "00"
                   DISPLAY TELACAD
                 DISPLAY "CEP JA CADASTRAD0, DESEJA MUDAR? S/N?" AT 1205
                   ACCEPT ALT AT 1243

                     IF ALT = "S" OR "s"
                       PERFORM LIMPAERRO
                       MOVE 1 TO BITVALIDA
                       GO TO CAD-LOGRADOURO
                     ELSE
                       DISPLAY LIMPATELA
                       DISPLAY TELAMENU
                       GO TO MENU
                 ELSE
                   DISPLAY "ERRO NA LEITURA ARQUIVO CADCEP" AT 1205
                   GO TO SAIR
                 ELSE

                 CONTINUE.

      *Cadastra o logradouro
       CAD-LOGRADOURO.

           ACCEPT TLOGRADOURO
           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   DISPLAY LIMPATELA
                   GO TO CAD-CEP
           END-IF

               IF LOGRADOURO = SPACES
                   DISPLAY "DIGITE ALGO" AT 1205
                   GO TO CAD-LOGRADOURO
               END-IF

               CONTINUE.

      *Cadastra o bairro
       CAD-BAIRRO.

           ACCEPT BAIRRO AT 0516
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-LOGRADOURO
           END-IF

               IF BAIRRO = SPACES
                   DISPLAY "DIGITE ALGO" AT 1205
                   GO TO CAD-BAIRRO
               END-IF

               CONTINUE.

      *Cadastra a cidade
       CAD-CIDADE.

           ACCEPT CIDADE AT 0616
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-BAIRRO
           END-IF

               IF CIDADE = SPACES
                   DISPLAY "DIGITE ALGO" AT 1205
                   GO TO CAD-CIDADE
               END-IF

               CONTINUE.

      *Cadastra o estado
       CAD-ESTADO.

           ACCEPT UF AT 0712
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-CIDADE
           END-IF

               IF UF = SPACES
                   DISPLAY "DIGITE ALGO" AT 1205
                   GO TO CAD-ESTADO
               ELSE
                   MOVE 1 TO IND
                   GO TO ESTADOVALIDA
               END-IF.

      *Valida a entrada do estado
       ESTADOVALIDA.

           IF IND > 27
               DISPLAY "ESTADO NAO ENCONTRADO" AT 1205
               MOVE 1 TO IND
               GO TO CAD-ESTADO
           ELSE
               IF UF = TUFP(IND)
                   GO TO CAD-REFERENCIA
               ELSE
                   ADD 1 TO IND
                   GO TO ESTADOVALIDA
               END-IF
           END-IF

               CONTINUE.

      *Cadastra alguma possํvel referencia do local
       CAD-REFERENCIA.

           ACCEPT REFERENCIA AT 0820
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-ESTADO
           END-IF

               IF REFERENCIA= SPACES
                   DISPLAY "DIGITE ALGO" AT 1205
                   GO TO CAD-REFERENCIA
               END-IF

               CONTINUE.

      *Cadastra a latitude
       CAD-LATITUDE.

           ACCEPT LATITUDE AT 0918
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-REFERENCIA
           END-IF

               IF LATITUDE = SPACES
                   DISPLAY "DIGITE ALGO" AT 1205
                   GO TO CAD-LATITUDE
               END-IF

               CONTINUE.

      *Cadastra a longitude
       CAD-LONGITUDE.

           ACCEPT LONGITUDE AT 1019
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-LATITUDE
           END-IF

               IF LONGITUDE = SPACES
                   DISPLAY "DIGITE ALGO" AT 1205
                   GO TO CAD-LONGITUDE
               ELSE
                   DISPLAY LIMPATELA

               IF BITVALIDA = 1
                   GO TO ALTERAR
               ELSE
                   CONTINUE.

      *Grava os arquivos no aquivo "CADCEP.dat"
       ESCRITA.

           WRITE REGCEP

                IF ERRO = "00" OR "02"
                   DISPLAY "DADOS GRAVADOS" AT 0622
                   GO TO MENU
                ELSE
                   DISPLAY "ERRO NA GRAVACAO DO ARQUIVO" AT 0622
                   GO TO MENU.

      *Apaga o registro
       APAGAR.

           DELETE CADCEP RECORD

               IF ERRO = "00"
                   DISPLAY "REGISTRO EXCLUIDO" AT 0622
                   GO TO MENU
               ELSE
                   DISPLAY "ERRO NA EXCLUSAO" AT 0622
                   GO TO MENU.
      *Altera o registro
       ALTERAR.

           REWRITE REGCEP

           IF ERRO = "00" OR "02"
               DISPLAY "CEP ALTERADO" AT 0622
               GO TO MENU
           ELSE
               DISPLAY "ERRO AO ALTERAR O CEP" AT 0622
               GO TO MENU.

      *Mostra ao usuario os dados cadastrados
       MOSTRAR.

           OPEN INPUT CADCEP
           READ CADCEP
           MOVE CEP TO MASK
           DISPLAY MASK AT 0806
           DISPLAY LOGRADOURO AT 0906
           DISPLAY BAIRRO AT 1006
           DISPLAY CIDADE AT 1106
           DISPLAY UF AT 1206
           DISPLAY REFERENCIA AT 1306
           DISPLAY LATITUDE AT 1406
           DISPLAY LONGITUDE AT 1506
           CLOSE CADCEP.

      *Limpa os dados das variแveis na tela do usuแrio
       LIMPAVARIAVEL.

           MOVE ZEROS TO CEP
           MOVE SPACES TO LOGRADOURO
           MOVE SPACES TO BAIRRO
           MOVE SPACES TO CIDADE
           MOVE SPACES TO UF
           MOVE SPACES TO REFERENCIA
           MOVE ZEROS TO LATITUDE
           MOVE ZEROS TO LONGITUDE.

      *Limpa as mensagens de erro na tela do usuario
       LIMPAERRO.

           DISPLAY "                                         " AT 1205.

      *Fecha o programa
       SAIR.

           CLOSE CADCEP.

       END PROGRAM P172CEP.
