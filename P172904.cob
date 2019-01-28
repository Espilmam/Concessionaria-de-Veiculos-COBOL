      ******************************************************************
      * Author: PEDRO SPIELMANN
      * Date: 22/11/2017
      * Purpose:
      * OBS: ARQUIVO GERADO A PARTIR DA IDE OPENCOBOL.
      *    PASSOS PARA FUNCIONAR CODIGO:
      *     - DEPOIS DE INSTALAR A IDE, EXECUTAR SEMPRE COMO ADM
      *     - NA IDE, CLICAR EM Edit > Preferences > Run > MARCAR Run in external terminal
      *     - PARA ABRIR QUALQUER PROGRAMA COBOL NESSA IDE, DEVE PRIMEIRO ABRIR
      *       ELA E DEPOIS ABRIR O PROGRAMA. NUNCA ABRIR DIRETO.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172904.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADVEIC ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS VEICULO
               FILE STATUS IS ERRO
               ALTERNATE RECORD KEY ANO-MODEL DUPLICATES.
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
           SELECT CADPROPR ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CPF
               FILE STATUS IS ERRO
               ALTERNATE RECORD KEY EMAIL DUPLICATES.
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

           FD CADMODEL
              LABEL  RECORD IS STANDARD
              VALUE OF FILE-ID IS "CADMODEL.DAT".
           01 REGMODELO.
               02 CADMOD-CODIGO.
                   05 CONTCODIGO PIC 9(3) VALUE ZEROS.
                   05 CODIGOMODELO PIC 9(3) VALUE ZEROS.
               02 MODELO PIC X(20) VALUE SPACES.
               02 CADMOD-MARCA PIC X(20) VALUE SPACES.

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
                   05 NUMERO PIC 9(11) VALUE ZEROS.
               02 DATANASC.
                   05 DIA PIC 9(2) VALUE ZEROS.
                   05 MES PIC 9(2) VALUE ZEROS.
                   05 ANO PIC 9(4) VALUE ZEROS.

           FD CADVEIC
               LABEL  RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADVEIC.DAT".
           01 REGVEIC.
               02 VEICULO.
                   03 VEIC-MARCA PIC 9(3) VALUE ZEROES.
                   03 VEIC-MODEL PIC 9(3) VALUE ZEROES.
                   03 VEIC-CPF PIC 9(11) VALUE ZEROS.
               02 ANOMODELO.
                   05 ANO-FABRIC PIC 9(4) VALUE ZEROS.
                   05 ANO-MODEL PIC 9(4) VALUE ZEROS.
               02 COR PIC 9(2) VALUE ZEROS.
               02 COMISSAO PIC 9(2)V9(2) VALUE ZEROS.
               02 VALORVENDA PIC 9(8)V99 VALUE ZEROS.
               02 VRPGTO PIC 9(8)V99 VALUE ZEROS.
               02 VRCOM PIC 9(8)V99 VALUE ZEROS.

       WORKING-STORAGE SECTION.

           01 ERRO PIC X(2) VALUE "00".
           01 EFE PIC 9(2).
           01 OPC PIC X(1).
           01 BITVALIDA PIC 9(1) value zeroes.

       SCREEN SECTION.

      *Telas geradas no DOSBOX 0.74 (possui um gerador de telas proprio)
       01  TELAVEICULO.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "ษอออออออออออออCADASTRO DE VEICULOSออออออ".
           05  LINE 01  COLUMN 41
               VALUE  "ออออออป".
           05  LINE 02  COLUMN 01
               VALUE  "บ".
           05  LINE 02  COLUMN 41
               VALUE  "      บ".
           05  LINE 03  COLUMN 01
               VALUE  "บ  MARCA:".
           05  LINE 03  COLUMN 41
               VALUE  "      บ".
           05  LINE 04  COLUMN 01
               VALUE  "บ  MODELO:".
           05  LINE 04  COLUMN 41
               VALUE  "      บ".
           05  LINE 05  COLUMN 01
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 05  COLUMN 41
               VALUE  "ฤฤฤฤฤฤถ".
           05  LINE 06  COLUMN 01
               VALUE  "บ  CPF PROPRIETARIO:".
           05  LINE 06  COLUMN 41
               VALUE  "      บ".
           05  LINE 07  COLUMN 01
               VALUE  "บ".
           05  LINE 07  COLUMN 41
               VALUE  "      บ".
           05  LINE 08  COLUMN 01
               VALUE  "บ  NOME:".
           05  LINE 08  COLUMN 41
               VALUE  "      บ".
           05  LINE 09  COLUMN 01
               VALUE  "บ  TELEFONE:".
           05  LINE 09  COLUMN 41
               VALUE  "      บ".
           05  LINE 10  COLUMN 01
               VALUE  "บ  E-MAIL:".
           05  LINE 10  COLUMN 41
               VALUE  "      บ".
           05  LINE 11  COLUMN 01
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 11  COLUMN 41
               VALUE  "ฤฤฤฤฤฤถ".
           05  LINE 12  COLUMN 01
               VALUE  "บ  ANO FABRICACAO:".
           05  LINE 12  COLUMN 41
               VALUE  "      บ".
           05  LINE 13  COLUMN 01
               VALUE  "บ  ANO MODELO:".
           05  LINE 13  COLUMN 41
               VALUE  "      บ".
           05  LINE 14  COLUMN 01
               VALUE  "บ  COR:".
           05  LINE 14  COLUMN 41
               VALUE  "      บ".
           05  LINE 15  COLUMN 01
               VALUE  "บ  VALOR DE VENDA:".
           05  LINE 15  COLUMN 41
               VALUE  "      บ".
           05  LINE 16  COLUMN 01
               VALUE  "บ  COMISSAO (%):".
           05  LINE 16  COLUMN 41
               VALUE  "      บ".
           05  LINE 17  COLUMN 01
               VALUE  "บ  VALOR DE PAGAMENTO:".
           05  LINE 17  COLUMN 41
               VALUE  "      บ".
           05  LINE 18  COLUMN 01
               VALUE  "บ  VALOR DE COMISSAO:".
           05  LINE 18  COLUMN 41
               VALUE  "      บ".
           05  LINE 19  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 19  COLUMN 41
               VALUE  "ออออออน".
           05  LINE 20  COLUMN 01
               VALUE  "บ".
           05  LINE 20  COLUMN 41
               VALUE  "      บ".
           05  LINE 21  COLUMN 01
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 21  COLUMN 41
               VALUE  "ออออออผ".
           05  TCODIGO
               LINE 03  COLUMN 11  PIC 9(03)
               USING  VEIC-MARCA
               HIGHLIGHT.
           05  TCODIGOMODELO
               LINE 04  COLUMN 12  PIC 9(03)
               USING  VEIC-MODEL
               HIGHLIGHT.
           05  TCPF
               LINE 06  COLUMN 22  PIC 9(11)
               USING  VEIC-CPF
               HIGHLIGHT.
           05  TANOFABRIC
               LINE 12  COLUMN 20  PIC 9(04)
               USING  ANO-FABRIC
               HIGHLIGHT.
           05  TANOMODEL
               LINE 13  COLUMN 16  PIC 9(04)
               USING  ANO-MODEL
               HIGHLIGHT.
           05  TCOR
               LINE 14  COLUMN 09  PIC 9(2)
               USING  COR
               HIGHLIGHT.
           05  TVALORVENDA
               LINE 15  COLUMN 20  PIC Z(8),99
               USING  VALORVENDA
               HIGHLIGHT.
           05  TCOMISSAO
               LINE 16  COLUMN 18  PIC ZZ,99
               USING  COMISSAO
               HIGHLIGHT.
           05  TVRPGTO
               LINE 17  COLUMN 24  PIC Z(8),99
               USING  VRPGTO
               HIGHLIGHT.
           05  TVRCOM
               LINE 18  COLUMN 23  PIC Z(8),99
               USING  VRCOM
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

      *Cria o arquivo "CADVEIC.dat"
       TELA-VEIC.

           DISPLAY TELAVEICULO
           OPEN I-O CADVEIC

           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = 35
                   OPEN OUTPUT CADVEIC
                   CLOSE CADVEIC
                  DISPLAY "ARQUIVO CADVEIC SENDO CRIADO" AT 2003
                   GO TO TELA-VEIC
               ELSE
                   DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADVEIC" AT 2003
           ELSE
               CONTINUE.

      *Recebe dados do arquivo "CADMARCA.dat"
       VER-MARCA.

           DISPLAY "F2 - SAIR" AT 2234

           ACCEPT TCODIGO
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 02
                   GO TO SAIR
           END-IF

               IF VEIC-MARCA = ZEROS
                   DISPLAY "DIGITE ALGO NO CODIGO" AT 2003
                   GO TO VER-MARCA
               ELSE
                   MOVE VEIC-MARCA TO CODIGO
                   OPEN I-O CADMARCA
                   READ CADMARCA
                   IF ERRO NOT = "23"
                       IF ERRO = "00"
                           DISPLAY MARCA AT 0315
                       ELSE
                           DISPLAY "ERRO AO LER O CADMARCA" AT 2003
                           GO TO VER-MARCA
                       END-IF
                   ELSE
                       DISPLAY "MARCA NAO ENCONTRADA" AT 2003
                       GO TO VER-MARCA
                   END-IF.
           CONTINUE.

      *Recebe dados do arquivo "CADMODEL.dat"
       VER-MODELO.

           DISPLAY "F1 - VOLTA AO CAMPO ANTERIOR" AT 2205

           ACCEPT TCODIGOMODELO
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO VER-MARCA
               ELSE
                   IF EFE = 02
                   GO TO SAIR
           END-IF

               IF VEIC-MODEL = ZEROS
                   DISPLAY "DIGITE UM CODIGO" AT 2003
                   GO TO VER-MODELO
               ELSE
                   MOVE VEIC-MARCA TO CONTCODIGO
                   MOVE VEIC-MODEL TO CODIGOMODELO
                   OPEN INPUT CADMODEL
                   READ CADMODEL

                   IF ERRO NOT = "23"
                       IF ERRO = "00"
                           DISPLAY MODELO AT 0416
                           CLOSE CADMODEL
                       ELSE
                           DISPLAY "ERRO AO LER O CADMODEL" AT 2003
                           GO TO VER-MODELO
                           CLOSE CADMODEL
                       END-IF
                   ELSE
                       DISPLAY "MODELO NAO ENCONTRADO" AT 2003
                       GO TO VER-MODELO
                       CLOSE CADMODEL
                   END-IF.
           CONTINUE.

      *Recebe dados do arquivo "CADPROPR.dat"
       VER-CPF.

           ACCEPT TCPF
           PERFORM LIMPAERRO
           MOVE VEIC-CPF TO CPF
           PERFORM LERCHAVE

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO VER-MODELO
               ELSE IF EFE = 02
                   GO TO SAIR
           END-IF

               IF TCPF = ZEROS
                   DISPLAY "DIGITE ALGO NO CPF" AT 2003
                   GO TO VER-CPF
               ELSE
                   OPEN I-O CADPROPR
                   READ CADPROPR

                   IF ERRO NOT = "23"
                       IF ERRO = "00"
                           DISPLAY NOME AT 0810
                           DISPLAY TELEFONE AT 0914
                           DISPLAY EMAIL AT 1012
                       ELSE
                           DISPLAY "ERRO AO LER O CADPROPR" AT 2003
                           GO TO VER-CPF
                       END-IF
                   ELSE
                       DISPLAY "CPF NAO ENCONTRADO" AT 2003
                       GO TO VER-CPF
                   END-IF.
           CONTINUE.

      *Cadastra o ano de fabricacao
       CAD-FABRIC.

           ACCEPT TANOFABRIC
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO VER-CPF
               ELSE IF EFE = 02
                   GO TO SAIR
               END-IF

               IF TANOFABRIC = ZEROS OR TANOFABRIC <= 1890
                   DISPLAY "DIGITE UM ANO DE FABRICACAO VALIDO" AT 2003
                   GO TO CAD-FABRIC
               ELSE
                   CONTINUE.

      *Cadastra o ano do modelo
       CAD-ANOMODELO.

           PERFORM LIMPAERRO
           ACCEPT TANOMODEL
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-FABRIC
               ELSE IF EFE = 02
                   GO TO SAIR
               END-IF

               IF TANOMODEL = ZEROS OR TANOMODEL <= 1890
                   DISPLAY "DIGITE UM NAO DE MODELO VALIDO" AT 2003
                   GO TO CAD-ANOMODELO
               ELSE
                   CONTINUE.

      *Cadastra a cor
       CAD-COR.

           PERFORM CORES
           ACCEPT TCOR
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-ANOMODELO
               ELSE IF EFE = 02
                   GO TO SAIR
               END-IF

               IF TCOR <= ZEROS OR TCOR > 10
                 DISPLAY "DIGITE UM CODIGO DE COR VALIDO" AT 2003
                   GO TO CAD-COR
               ELSE
                   PERFORM LIMPACORES
                   CONTINUE.

      *Cadastra o valor da venda
       CAD-VALORVENDA.

           ACCEPT TVALORVENDA
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-COR
               ELSE IF EFE = 02
                   GO TO SAIR
               END-IF

               IF VALORVENDA <= 0
                   DISPLAY "DIGITE UM VALOR MAIOR QUE ZERO" AT 2003
                   GO TO CAD-VALORVENDA
               ELSE
                   CONTINUE.

      *Cadastra o valor da comissao
       CAD-COMISSAO.

           ACCEPT TCOMISSAO
           PERFORM LIMPAERRO

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-VALORVENDA
               ELSE IF EFE = 02
                   GO TO SAIR
               END-IF

               IF COMISSAO <= 0
                   DISPLAY "DIGITE UM VALOR VALIDO" AT 2003
                   GO TO CAD-COMISSAO
               ELSE
                   CONTINUE.

      *Realiza o valor de pagamento
       CALCULA.

           ACCEPT EFE FROM ESCAPE KEY

               IF EFE = 01
                   GO TO CAD-VALORVENDA
               ELSE IF EFE = 02
                   GO TO SAIR
               END-IF

           COMPUTE VRCOM = VALORVENDA * (COMISSAO/100)
               DISPLAY TVRCOM
           COMPUTE VRPGTO = VALORVENDA - VRCOM
               DISPLAY TVRPGTO

               CONTINUE.

      *Permite ao usuario validar os dados
       VERIFICA.

           DISPLAY "OS VALORES ESTรO MESMO CORRETOS ?" AT 2003
           DISPLAY "S/N ?" AT 2037

           ACCEPT OPC AT 2043

               IF OPC = "S" OR "s"
                   IF BITVALIDA = 1
                       GO TO ALTERAR
                   ELSE
                       GO TO GRAVARCHAVE
               ELSE
                   GO TO VER-MARCA.

      *Grava os dados no registro
       GRAVARCHAVE.

           WRITE REGVEIC
           DISPLAY ERRO
           IF ERRO = "00" OR "02"
               DISPLAY "DADOS GRAVADOS" AT 2003
               GO TO SAIR
           ELSE
               DISPLAY "ERRO NA GRAVACAO DO ARQUIVO" AT 2003
               GO TO SAIR.

      *Verificacao da entrada do CPF
       LERCHAVE.

           READ CADVEIC
               IF ERRO NOT = "23"
                 IF ERRO = "00"
                   MOVE VEIC-CPF TO CPF

                   OPEN I-O CADPROPR
                   READ CADPROPR

                   DISPLAY TELAVEICULO
                   DISPLAY NOME AT 0810
                   DISPLAY TELEFONE AT 0914
                   DISPLAY EMAIL AT 1012
                   DISPLAY "JA ESTA CADASTRAD0. DESEJA MUDAR ?" AT 2003
                   DISPLAY "S/N ?" AT 2040
                   ACCEPT OPC AT 2045

                       IF OPC = "S" OR "s"
                         MOVE 1 TO BITVALIDA
                         GO TO CAD-FABRIC
                       ELSE
                         PERFORM LIMPAVARIAVEL
                         MOVE 0 TO BITVALIDA
                         GO TO VER-MARCA
                 ELSE
                   DISPLAY "ERRO NA LEITURA DO ARQUIVO CADVEIC" AT 2003
                   GO TO TELA-VEIC.

      *Altera o veiculo
       ALTERAR.

           PERFORM LIMPAERRO
           REWRITE REGVEIC

           IF ERRO = "00" OR "02"
               DISPLAY "VEICULO ALTERADO" AT 2003
               GO TO SAIR
           ELSE
               DISPLAY "ERRO AO ALTERAR O VEICULO" AT 2003
               GO TO SAIR.

      *Limpa os dados das variaveis na tela do usuario
       LIMPAVARIAVEL.

           MOVE ZEROS TO CODIGO CODIGOMODELO CPF TELEFONE ANO-FABRIC COR
           MOVE ZEROS TO VALORVENDA COMISSAO VRCOM VRPGTO VEIC-MODEL
           MOVE ZEROES TO VEIC-MARCA ANOMODELO BITVALIDA
           MOVE SPACES TO MARCA MODELO NOME EMAIL.

      *Limpa as mensagens de erro na tela do usuario
       LIMPAERRO.

           DISPLAY"                                           " AT 2003.
       CORES.

           DISPLAY "1 - VERMELHO" AT 0349
           DISPLAY "2 - VERDE" AT 0449
           DISPLAY "3 - AZUL" AT 0549
           DISPLAY "4 - AMARELO" AT 0649
           DISPLAY "5 - PRETO" AT 0749
           DISPLAY "6 - PRATA" AT 0849
           DISPLAY "7 - BRANCO" AT 0949
           DISPLAY "8 - CINZA" AT 1049
           DISPLAY "9 - LARANJA" AT 1149
           DISPLAY "10 - VINHO" AT 1249.
       LIMPACORES.

           DISPLAY "            " AT 0349
           DISPLAY "            " AT 0449
           DISPLAY "            " AT 0549
           DISPLAY "            " AT 0649
           DISPLAY "            " AT 0749
           DISPLAY "            " AT 0849
           DISPLAY "            " AT 0949
           DISPLAY "            " AT 1049
           DISPLAY "            " AT 1149
           DISPLAY "            " AT 1249.

      *Fecha o programa
       SAIR.

           CLOSE CADVEIC
           CLOSE CADMARCA
           CLOSE CADPROPR.

       END PROGRAM P172904.
