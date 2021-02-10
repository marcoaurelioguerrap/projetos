require(BatchGetSymbols)
library("rjson")

# le os dados de dividendos e splits salvos do PYTHON yfinance

# TODO : fazer um loop que gera os precos ajustado para todas as acoes ( as regras para cada estão descritas mais a baixo)


json_data <- fromJSON(file = ".//dados de desdobramentos e dividendos/dados_split_div.txt")

tickers_acoes[33]

# obs.: 1) BBDC4 usa os splits de BBDC3 ( é mais completo ); 2) BRAP4 nao usa a reducao de fator f.BRAP4 ;
# 3) as acoes normativas usam DIV ; 4) nao usei CMIG3, usei CMIG4 mas ta diferente do site br.trading ; 
# 5) LREN nao usa as divisoes 3,4 ; 6) UGPA3 nao tem mto dados, nao peguei;7) EQTL3 ta zoado   ;
# 8) ABEV3 fiz umas mudanças ver 9) B3SA3 nao usa os splits

acao <- "PETR4"

# le os dividendos para "acao"
div <- data.frame( date = as.Date(names(json_data[[acao]]$Dividends)),
                   dividendos = unlist(json_data[[acao]]$Dividends),
                   row.names = as.Date(names(json_data[[acao]]$Dividends)))
# le os splits para "acao"
splits <- data.frame(date = as.Date(names(json_data[[acao]]$`Stock Splits`)),
                     splits = unlist(json_data[[acao]]$`Stock Splits`),
                     row.names = as.Date(names(json_data[[acao]]$`Stock Splits`)))

# splits
teste1 <- lista_das_acoes.serie$p.PETR4
teste2 <- lista_das_acoes.serie$f.PETR4 # fator de correção do preço 
teste <- teste1/teste2

# Calculo para cada ação #####
# OBS.: preciso fazer incorporar isso no código para ser feito automáticamente !

#### ITUB4,  ITAU4 ####

#teste1 <- rbind(lista_das_acoes.serie$p.ITAU4[complete.cases(lista_das_acoes.serie$p.ITAU4) ],
#              lista_das_acoes.serie$p.ITUB4[complete.cases(lista_das_acoes.serie$p.ITUB4) ])

#teste2 <- rbind(lista_das_acoes.serie$f.ITAU4[complete.cases(lista_das_acoes.serie$f.ITAU4) ],
#               lista_das_acoes.serie$f.ITUB4[complete.cases(lista_das_acoes.serie$f.ITUB4) ])

#teste <- teste1/teste2

#####


#### CMIG4 ####


#splits2 <- data.frame(date = as.Date(c( "2002-05-02" , "2007-04-27" , "2008-04-28" , "2009-04-30", "2017-10-27")),
#                     splits = unlist(c( 511/500 , 3/2 , 51/50 , 5/4 ,1031/1000)),
#                     row.names = as.Date( c( "2002-05-02" , "2007-04-27" , "2008-04-28" , "2009-04-30" , "2017-10-27" ) ))

#splits <- rbind(splits2,splits)

#####


#### WEGE3,  WEGE4 ####



#teste1 <- rbind(lista_das_acoes.serie$p.WEGE3[complete.cases(lista_das_acoes.serie$p.WEGE3) ],
#               lista_das_acoes.serie$p.WEGE4[complete.cases(lista_das_acoes.serie$p.WEGE4) ])

#teste2 <- rbind(lista_das_acoes.serie$f.WEGE3[complete.cases(lista_das_acoes.serie$f.WEGE3) ],
#               lista_das_acoes.serie$f.WEGE4[complete.cases(lista_das_acoes.serie$f.WEGE4) ])

#teste <- teste1/teste2

#####

#### RADL3, DROG3  ####



#teste1 <- rbind(lista_das_acoes.serie$p.RADL3[complete.cases(lista_das_acoes.serie$p.RADL3) ],
#               lista_das_acoes.serie$p.DROG3[complete.cases(lista_das_acoes.serie$p.DROG3) ])

#teste2 <- rbind(lista_das_acoes.serie$f.RADL3[complete.cases(lista_das_acoes.serie$f.RADL3) ],
#               lista_das_acoes.serie$f.DROG3[complete.cases(lista_das_acoes.serie$f.DROG3) ])

#teste <- teste1/teste2

#####


#### ABEV3,  AMBV3 ####

#teste1 <- rbind(lista_das_acoes.serie$p.AMBV3[complete.cases(lista_das_acoes.serie$p.AMBV3) ],
#                lista_das_acoes.serie$p.ABEV3[complete.cases(lista_das_acoes.serie$p.ABEV3) ])

#teste2 <- rbind(lista_das_acoes.serie$f.AMBV3[complete.cases(lista_das_acoes.serie$f.AMBV3) ],
#               lista_das_acoes.serie$f.ABEV3[complete.cases(lista_das_acoes.serie$f.ABEV3) ])

#teste <- teste1/teste2
#splits[1,1] <- "2000-10-21"
#splits[1,2] <- 5/1
#splits[2,2] <- 5/6
#splits[3,2] <- 1/100
#splits[5,2] <- 5/1
#splits[6,2] <- 1

#####

#### BRFS3,  PRGA3 ####

#teste1 <- rbind(lista_das_acoes.serie$p.PRGA3[complete.cases(lista_das_acoes.serie$p.PRGA3) ],
#               lista_das_acoes.serie$p.BRFS3[complete.cases(lista_das_acoes.serie$p.BRFS3) ])

#teste2 <- rbind(lista_das_acoes.serie$f.PRGA3[complete.cases(lista_das_acoes.serie$f.PRGA3) ],
#               lista_das_acoes.serie$f.BRFS3[complete.cases(lista_das_acoes.serie$f.BRFS3) ])

#teste <- teste1/teste2

#####


#### B3SA3,  BVMF3 ####

#teste1 <- rbind(lista_das_acoes.serie$p.BVMF3[complete.cases(lista_das_acoes.serie$p.BVMF3) ],
#               lista_das_acoes.serie$p.B3SA3[complete.cases(lista_das_acoes.serie$p.B3SA3) ])

#teste2 <- rbind(lista_das_acoes.serie$f.BVMF3[complete.cases(lista_das_acoes.serie$f.BVMF3) ],
#               lista_das_acoes.serie$f.B3SA3[complete.cases(lista_das_acoes.serie$f.B3SA3) ])

#teste <- teste1/teste2

#####

#### COGN3,  KROT3 , KROT4 ####

#teste1 <- rbind(lista_das_acoes.serie$p.KROT3[complete.cases(lista_das_acoes.serie$p.KROT3) ],
#               lista_das_acoes.serie$p.COGN3[complete.cases(lista_das_acoes.serie$p.COGN3) ])

#teste2 <- rbind(lista_das_acoes.serie$f.KROT3[complete.cases(lista_das_acoes.serie$f.KROT3) ],
#               lista_das_acoes.serie$f.COGN3[complete.cases(lista_das_acoes.serie$f.COGN3) ])

#teste <- teste1/teste2


#splits <- data.frame(date = as.Date(c( "2012-12-05" , "2013-03-06" , "2014-09-12" )),
#           splits = c( 1/6.993006993007 , 2/1 , 4/1 ),
#           row.names = c( "2014-09-12" , "2013-06-03" , "2012-12-05" ))

#####

#### PETR4 ####

splits[1,1] <- "2000-06-22"

#####

#### GGBR4 ####

#splits[2,2] <- 1/1000

#####

# Loops que fazem o ajuste do preço ! #####

# OBS.: dividendos nao desconta em 4

#i <- 1
#while ( div[i,1]<= as.Date(last(index(teste)))  ){
#  ate_data <- paste("/",(div[i,1]-1) , sep ="")
#  teste[ate_data] <- (teste[ate_data]  *  (1-div[i,2]/as.numeric(teste[div[i,1]])  ))  
#  print((1-div[i,2]/as.numeric(teste[div[i,1]])))
  
#  i <- 1 + i
#}

  #plot.xts(teste)
#plot.xts(teste)
### quando é de 1:100 multiplica, quando é de 100:1 divide
for( i in 1:(nrow(splits))){
  print(splits[i,1])
  ate_data <- paste("/",(splits[i,1]-1) , sep ="")
  teste[ate_data] <- (teste[ate_data]  / splits[i,2]) 
  
  #plot.xts(teste , main = splits[i,1] )
  #plot(teste[ate_data] )
  i<-i+1
}

#plot(teste )



#### cria as acoes ####
preco.acoes.nivel <- list()

#### carrega preco.acoes.nivel ####  
#preco.acoes.nivel <- readRDS("preco.acoes.nivel_preview.RData")

colnames(teste) <- "Preço_ajustado"
colnames(teste1) <- "Preço_bruto"


preco.acoes.nivel[[acao]] <- list("Preco_ajustado" = teste,
                                 "Preco_bruto" = teste1)



saveRDS( preco.acoes.nivel , file = "preco.acoes.nivel.RData")  
#preco.acoes.nivel <- readRDS("preco.acoes.nivel.RData")

rm(teste)
rm(teste1)
rm(teste2)
rm(div)
rm(splits)
rm(cabecalho)

