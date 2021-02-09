library(xts)
require(tidyverse)

# Prepara os dados para uso

# le os arquivos lista_das_acoes

lista_das_acoes <- readRDS( file = ".//arquivos B3/lista_das_acoes.RData" )

lista_dos_CALL <- readRDS( file = ".//arquivos B3/lista_dos_CALL.RData" )

lista_dos_PUT <- readRDS( file = ".//arquivos B3/lista_dos_PUT.RData" )



# Limpando a lista para os dados que desejo 

lista_das_acoes.unstacked <- cbind(as.data.frame.Date(lista_das_acoes$`DATA DO PREGÃO`),
                                   lista_das_acoes$`PREULT - PREÇO DO ÚLTIMO NEGÓCIO DO PAPEL-MERCADO NO PREGÃO`,
                                   lista_das_acoes$`CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL`,
                                   lista_das_acoes$`FATCOT - FATOR DE COTAÇÃO DO PAPEL` )

rm(lista_das_acoes)

colnames(lista_das_acoes.unstacked) <- c("date","price","ticker","fator cotacao")


# desempilhando os dados empilhandos 

lista_das_acoes.stacked <- reshape(lista_das_acoes.unstacked, idvar = "date",
                 timevar = "ticker", direction = "wide", v.names = c("price","fator cotacao")  )


rm(lista_das_acoes.unstacked)

# renomea as colunas para o nome das ações
colnames(lista_das_acoes.stacked) <- str_replace(colnames(lista_das_acoes.stacked),"price.","p.")

colnames(lista_das_acoes.stacked) <- str_replace(colnames(lista_das_acoes.stacked),"fator cotacao.","f.")

# dropa a linha que nao tem data
lista_das_acoes.stacked <- lista_das_acoes.stacked[complete.cases(lista_das_acoes.stacked[,1]), ]

row.names(lista_das_acoes.stacked) <- lista_das_acoes.stacked$date

lista_das_acoes.serie <- as.xts(lista_das_acoes.stacked[-2479,-1])

rm(lista_das_acoes.stacked)

####
