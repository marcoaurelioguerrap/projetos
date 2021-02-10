 **Info**
 
  O codigo "compila_dados_arquivos_B3.R" lê os arquivos baixados do site da B3 (http://www.b3.com.br/pt_br/market-data-e-indices/servicos-de-dados/market-data/historico/mercado-a-vista/series-historicas/)
e salva os dados em 3 arquivos ( "lista_das_acoes.RData" , "lista_dos_CALL.RData", "lista_dos_PUT.RData" ). Os dados gerados são as ações e opções negociadas.
OBS 1.: Não carreguei todos os arquivos no github pois eles são maiores que 25 mb (59 , 43 respectivamente ) 
OBS 2.: O código demora em torno de 4 horas para compilar e extrair as dados das ações e opções negociadas por dia.

Informações extraidas:
>[1] "TIPREG - TIPO DE REGISTRO"                                                                                                    
 [2] "DATA DO PREGÃO"                                                                                                               
 [3] "CODBDI - CÓDIGO BDI"                                                                                                          
 [4] "CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL"                                                                                       
 [5] "TPMERC - TIPO DE MERCADO"                                                                                                     
 [6] "NOMRES - NOME RESUMIDO DA EMPRESA EMISSORA DO PAPEL"                                                                          
 [7] "ESPECI - ESPECIFICAÇÃO DO PAPEL"                                                                                              
 [8] "PRAZOT - PRAZO EM DIAS DO MERCADO A TERMO"                                                                                    
 [9] "MODREF - MOEDA DE REFERÊNCIA"                                                                                                 
[10] "PREABE - PREÇO DE ABERTURA DO PAPELMERCADO NO PREGÃO "                                                                        
[11] "PREMAX - PREÇO MÁXIMO DO PAPELMERCADO NO PREGÃO "                                                                             
[12] "PREMIN - PREÇO MÍNIMO DO PAPELMERCADO NO PREGÃO"                                                                              
[13] "PREMED - PREÇO MÉDIO DO PAPELMERCADO NO PREGÃO"                                                                               
[14] "PREULT - PREÇO DO ÚLTIMO NEGÓCIO DO PAPEL-MERCADO NO PREGÃO"                                                                  
[15] "PREOFC - PREÇO DA MELHOR OFERTA DE COMPRA DO PAPELMERCADO"                                                                    
[16] "PREOFV - PREÇO DA MELHOR OFERTA DE VENDA DO PAPELMERCADO"                                                                     
[17] "TOTNEG - NEG. - NÚMERO DE NEGÓCIOS EFETUADOS COM O PAPEL- MERCADO NO PREGÃO"                                                  
[18] "QUATOT - QUANTIDADE TOTAL DE TÍTULOS NEGOCIADOS NESTE PAPEL- MERCADO"                                                         
[19] "VOLTOT - VOLUME TOTAL DE TÍTULOS NEGOCIADOS NESTE PAPEL- MERCADO"                                                             
[20] "PREEXE - PREÇO DE EXERCÍCIO PARA O MERCADO DE OPÇÕES OU VALOR DO CONTRATO PARA O MERCADO DE TERMO SECUNDÁRIO"                 
[21] "INDOPC - INDICADOR DE CORREÇÃO DE PREÇOS DE EXERCÍCIOS OU VALORES DE CONTRATO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO" 
[22] "DATVEN - DATA DO VENCIMENTO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO"                                                   
[23] "FATCOT - FATOR DE COTAÇÃO DO PAPEL"                                                                                           
[24] "PTOEXE - PREÇO DE EXERCÍCIO EM PONTOS PARA OPÇÕES REFERENCIADAS EM DÓLAR OU VALOR DE CONTRATO EM PONTOS PARA TERMO SECUNDÁRIO"
[25] "CODISI - CÓDIGO DO PAPEL NO SISTEMA ISIN OU CÓDIGO INTERNO DO PAPEL"                                                          
[26] "DISMES - NÚMERO DE DISTRIBUIÇÃO DO PAPEL"
