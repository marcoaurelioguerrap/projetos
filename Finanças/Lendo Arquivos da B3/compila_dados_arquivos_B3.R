library(xts)
require(tidyverse)

# Le os dados dos arquivos da B3 


############################################################################################################


                                      ########################
                                      # AVISO : DEMORADO !!! #
                                      ########################


############################################################################################################

# TODO: salva os arquivos lidos da B3, pra nao ter trabalho de separar tudo de novo

# TODO: NAO AJUSTAR OS PRECOS DE FECHAMENTO !!!!!!!!


###### 0) lista das acoes e os codigos ISIN ######

acoes.estudadas <- c( "ABEV3","AZUL4","B3SA3","BBAS3","BBDC3",
                      "BBDC4","BBSE3","BOVA11","BRAP4","BRFS3",
                      "BRKM5","BRML3","CCRO3","CIEL3","CMIG4",
                      "COGN3","CSNA3","CVCB3","CYRE3","ELET3",
                      "ELET6","EMBR3","EQTL3","GGBR4","GOAU4",
                      "HYPE3","IRBR3","ITSA4","ITUB4","JBSS3",
                      "LAME4","LREN3","MGLU3","MRFG3","MULT3",
                      "PETR4","RADL3","RAIL3","SBSP3","SUZB3",
                      "UGPA3","USIM5","VALE3","VVAR3","WEGE3",
                      "YDUQ3")


cod_isin <- list( ABEV3 = "BRABEVACNOR1", AZUL4 = "BRAZULACNPR4", B3SA3 = "BRB3SAACNOR6", BBAS3 = "BRBBASACNOR3", BBDC3 = "BRBBDCACNOR1",
                  BBDC4 = "BRBBDCACNPR8", BBSE3 = "BRBBSEACNOR5", BOVA11 = "BRBOVACTF003", BRAP4 = "BRBRAPACNPR2", BRFS3 = "BRBRFSACNOR8",
                  BRKM5 = "BRBRKMACNPA4", BRML3 = "BRBRMLACNOR9", CCRO3 = "BRCCROACNOR2", CIEL3 = "BRCIELACNOR3", CMIG4 = "BRCMIGACNPR3",
                  COGN3 = "BRCOGNACNOR2", CSNA3 = "BRCSNAACNOR6", CVCB3 = "BRCVCBACNOR1", CYRE3 = "BRCYREACNOR7", ELET3 = "BRELETACNOR6",
                  ELET6 = "BRELETACNPB7", EMBR3 = "BREMBRACNOR4", EQTL3 = "BREQTLACNOR0", GGBR4 = "BRGGBRACNPR8", GOAU4 = "BRGOAUACNPR8",
                  HYPE3 = "BRHYPEACNOR0", IRBR3 = "BRIRBRACNOR4", ITSA4 = "BRITSAACNPR7", ITUB4 = "BRITUBACNPR1", JBSS3 = "BRJBSSACNOR8",
                  LAME4 = "BRLAMEACNPR6", LREN3 = "BRLRENACNOR1", MGLU3 = "BRMGLUACNOR2", MRFG3 = "BRMRFGACNOR0", MULT3 = "BRMULTACNOR5",
                  PETR4 = "BRPETRACNPR6", RADL3 = "BRRADLACNOR0", RAIL3 = "BRRAILACNOR9", SBSP3 = "BRSBSPACNOR5", SUZB3 = "BRSUZBACNOR0",
                  UGPA3 = "BRUGPAACNOR8", USIM5 = "BRUSIMACNPA6", VALE3 = "BRVALEACNOR0", VVAR3 = "BRVVARACNOR1", WEGE3 = "BRWEGEACNOR0",
                  YDUQ3 = "BRYDUQACNOR3")



######



###### 1 le o arquivo e separa as acoes da lista que eu quero ######

#### 1.1) le os arquivos da B3 ####



# setup para o loop que le os dados da B3

anos <- seq(2000,2020,1)

lista_das_acoes <- data.frame()

lista_dos_CALL <- data.frame()

lista_dos_PUT <- data.frame()




for ( ano in anos ){
  print(paste(ano, "Começo" ))
  dados_bov <- read.fwf( unz( paste(".//arquivos B3/COTAHIST_A",ano,".zip",sep =""),
                              paste("COTAHIST_A",ano,".TXT",sep ="")), skip = 1 , 
                         widths = c( 2, 8, 2, 12, 3, 12, 10, 3, 4,
                                     13, 13, 13, 13 ,13 ,13, 13, 
                                     5, 18, 18, 13, 1,8,7,13,12,3)) 
  print(paste(ano, "leu" ))
  
  cabecalho <- c('TIPREG - TIPO DE REGISTRO' ,
                 'DATA DO PREGÃO' , 'CODBDI - CÓDIGO BDI', 
                 'CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL',
                 'TPMERC - TIPO DE MERCADO',
                 'NOMRES - NOME RESUMIDO DA EMPRESA EMISSORA DO PAPEL',
                 'ESPECI - ESPECIFICAÇÃO DO PAPEL' ,
                 'PRAZOT - PRAZO EM DIAS DO MERCADO A TERMO',
                 'MODREF - MOEDA DE REFERÊNCIA',
                 'PREABE - PREÇO DE ABERTURA DO PAPELMERCADO NO PREGÃO ',
                 'PREMAX - PREÇO MÁXIMO DO PAPELMERCADO NO PREGÃO ',
                 'PREMIN - PREÇO MÍNIMO DO PAPELMERCADO NO PREGÃO',
                 'PREMED - PREÇO MÉDIO DO PAPELMERCADO NO PREGÃO',
                 'PREULT - PREÇO DO ÚLTIMO NEGÓCIO DO PAPEL-MERCADO NO PREGÃO',
                 'PREOFC - PREÇO DA MELHOR OFERTA DE COMPRA DO PAPELMERCADO',
                 'PREOFV - PREÇO DA MELHOR OFERTA DE VENDA DO PAPELMERCADO',
                 'TOTNEG - NEG. - NÚMERO DE NEGÓCIOS EFETUADOS COM O PAPEL- MERCADO NO PREGÃO',
                 'QUATOT - QUANTIDADE TOTAL DE TÍTULOS NEGOCIADOS NESTE PAPEL- MERCADO',
                 'VOLTOT - VOLUME TOTAL DE TÍTULOS NEGOCIADOS NESTE PAPEL- MERCADO',
                 'PREEXE - PREÇO DE EXERCÍCIO PARA O MERCADO DE OPÇÕES OU VALOR DO CONTRATO PARA O MERCADO DE TERMO SECUNDÁRIO',
                 'INDOPC - INDICADOR DE CORREÇÃO DE PREÇOS DE EXERCÍCIOS OU VALORES DE CONTRATO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO',
                 'DATVEN - DATA DO VENCIMENTO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO',
                 'FATCOT - FATOR DE COTAÇÃO DO PAPEL',
                 'PTOEXE - PREÇO DE EXERCÍCIO EM PONTOS PARA OPÇÕES REFERENCIADAS EM DÓLAR OU VALOR DE CONTRATO EM PONTOS PARA TERMO SECUNDÁRIO',
                 'CODISI - CÓDIGO DO PAPEL NO SISTEMA ISIN OU CÓDIGO INTERNO DO PAPEL',
                 'DISMES - NÚMERO DE DISTRIBUIÇÃO DO PAPEL')
  
  colnames(dados_bov)<-cabecalho
  
  
  ### formatando dados_bov
  
  # 1) transforma as datas de 20200101 -> 2020-01-01
  
  dados_bov$`DATA DO PREGÃO` <- gsub("(\\d{2})(?<=\\d{4})(?!\\d?$)", "\\1-", dados_bov$`DATA DO PREGÃO`, perl = TRUE)
  dados_bov$`DATVEN - DATA DO VENCIMENTO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO` <- gsub("(\\d{2})(?<=\\d{4})(?!\\d?$)"
                                                                                                 , "\\1-", dados_bov$`DATVEN - DATA DO VENCIMENTO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO` 
                                                                                                 , perl = TRUE)
  
  dados_bov$`DATA DO PREGÃO` <- as.Date(dados_bov$`DATA DO PREGÃO`)
  dados_bov$`DATVEN - DATA DO VENCIMENTO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO` <- as.Date(dados_bov$`DATVEN - DATA DO VENCIMENTO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO`)
  
  
  
  
  # 2) divide os preços por 100
  
  dados_bov$`PREABE - PREÇO DE ABERTURA DO PAPELMERCADO NO PREGÃO ` <- dados_bov$`PREABE - PREÇO DE ABERTURA DO PAPELMERCADO NO PREGÃO `/100
  dados_bov$`PREMAX - PREÇO MÁXIMO DO PAPELMERCADO NO PREGÃO ` <- dados_bov$`PREMAX - PREÇO MÁXIMO DO PAPELMERCADO NO PREGÃO `/100
  dados_bov$`PREMIN - PREÇO MÍNIMO DO PAPELMERCADO NO PREGÃO` <- dados_bov$`PREMIN - PREÇO MÍNIMO DO PAPELMERCADO NO PREGÃO` /100
  dados_bov$`PREMED - PREÇO MÉDIO DO PAPELMERCADO NO PREGÃO` <- dados_bov$`PREMED - PREÇO MÉDIO DO PAPELMERCADO NO PREGÃO` /100
  dados_bov$`PREULT - PREÇO DO ÚLTIMO NEGÓCIO DO PAPEL-MERCADO NO PREGÃO` <- dados_bov$`PREULT - PREÇO DO ÚLTIMO NEGÓCIO DO PAPEL-MERCADO NO PREGÃO` /100
  dados_bov$`PREOFC - PREÇO DA MELHOR OFERTA DE COMPRA DO PAPELMERCADO` <- dados_bov$`PREOFC - PREÇO DA MELHOR OFERTA DE COMPRA DO PAPELMERCADO` /100
  dados_bov$`PREOFV - PREÇO DA MELHOR OFERTA DE VENDA DO PAPELMERCADO` <- dados_bov$`PREOFV - PREÇO DA MELHOR OFERTA DE VENDA DO PAPELMERCADO` /100
  dados_bov$`PREEXE - PREÇO DE EXERCÍCIO PARA O MERCADO DE OPÇÕES OU VALOR DO CONTRATO PARA O MERCADO DE TERMO SECUNDÁRIO` <- dados_bov$`PREEXE - PREÇO DE EXERCÍCIO PARA O MERCADO DE OPÇÕES OU VALOR DO CONTRATO PARA O MERCADO DE TERMO SECUNDÁRIO` /100
  
  #### lista as acoes
    
  lista_das_acoes.temp <- dados_bov[which(dados_bov$`TPMERC - TIPO DE MERCADO` == 10),]   
  
  lista_das_acoes <- rbind(lista_das_acoes,lista_das_acoes.temp)
  
  # Salva os dias de trade, retirados dos arquivos da B3
  dia.de.trade <- unique(lista_das_acoes$`DATA DO PREGÃO`)
  
  dia.de.trade <- dia.de.trade[ ( order( dia.de.trade ) )]
  
  dia.de.trade <- dia.de.trade[complete.cases(dia.de.trade)]
  
  
  rm(lista_das_acoes.temp)
  
  #### lista as opcoes de compra ( CALL )
  
  lista_dos_CALL.temp <- dados_bov[which(dados_bov$`TPMERC - TIPO DE MERCADO` == 70),]          
  
  lista_dos_CALL <- rbind(lista_dos_CALL,lista_dos_CALL.temp)
  
  rm(lista_dos_CALL.temp)
  
  #### lista as opcoes de venda ( PUT )
  
  lista_dos_PUT.temp <- dados_bov[which(dados_bov$`TPMERC - TIPO DE MERCADO` == 80),]
  
  lista_dos_PUT <- rbind(lista_dos_PUT,lista_dos_PUT.temp)
  
  rm(lista_dos_PUT.temp)
  
  rm(dados_bov)
  
  print(paste(ano, "fim" ))
  
}  

rm(ano)
rm(anos)


# Salva os dados

saveRDS( lista_das_acoes , file = ".//arquivos B3/lista_das_acoes.RData")  

saveRDS( lista_dos_CALL , file = ".//arquivos B3/lista_dos_CALL.RData")

saveRDS( lista_dos_PUT , file = ".//arquivos B3/lista_dos_PUT.RData")


