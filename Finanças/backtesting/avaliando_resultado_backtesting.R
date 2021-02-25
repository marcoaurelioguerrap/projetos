library(PerformanceAnalytics)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)
library(ggalt)
library(ggrepel)
library(nvmix)
library(xts)
library(qrmtools)
library(bizdays)
require(tidyverse)

# utilizado para gera as tabelas do kable
# install.packages("magick")
# install.packages("webshot")
# webshot::install_phantomjs()



######

# preparando para avaliar as melhores estrategias na janela "2006-01-13 / 2010-02-04"

# Carregando dados de retorno para todas estrategias de "2006-01-13 / 2010-02-04" ####


id_estrategias <- rbind(c("estrategia_1_1","estrategia_9_4"),
                        c("estrategia_10_1","estrategia_16_6"),
                        c("estrategia_17_1","estrategia_22_6"))


lista_indices_performance <- list()

lista_retorno_indices_performance <- list()


for (i in 1:3) {
  if (i == 1){
    indices_performance_todas_estrategias_2006 <- readRDS(file = paste0(".//estrategias/dados testes/indices_performance_final_",
                                                                        id_estrategias[i,1],"_",id_estrategias[i,2],
                                                                        ".RData"))
    
    retorno_indices_performance_todas_estrategias_2006 <- readRDS(file = paste0(".//estrategias/dados testes/retorno_indices_performance_final_",
                                                                                id_estrategias[i,1],"_",id_estrategias[i,2],
                                                                                ".RData"))
    
  } else {
    
    indices_performance_todas_estrategias_2006 <- cbind(indices_performance_todas_estrategias_2006,
                                                        readRDS(file = paste0(".//estrategias/dados testes/indices_performance_final_",
                                                                              id_estrategias[i,1],"_",id_estrategias[i,2],".RData"))[,-1])
    
    retorno_indices_performance_todas_estrategias_2006 <- cbind(retorno_indices_performance_todas_estrategias_2006,
                                                                readRDS(file = paste0(".//estrategias/dados testes/retorno_indices_performance_final_",
                                                                                      id_estrategias[i,1],"_",id_estrategias[i,2],".RData"))[,-1])
  }
}


lista_indices_performance[["2006_01_13"]] <- indices_performance_todas_estrategias_2006 

lista_retorno_indices_performance[["2006_01_13"]] <- retorno_indices_performance_todas_estrategias_2006

########

# separando as estrategias selecionadas ######


estrategias_selecionadas <- list('estrategia_13_6','estrategia_10_4','estrategia_10_5', 'estrategia_13_5' , 
                                 'estrategia_14_6'	, 'estrategia_13_4' , 'estrategia_12_7'	,'estrategia_10_3' ,
                                 'estrategia_12_6'	, 'estrategia_8_2' , 'estrategia_17_6' , 'estrategia_12_5' ,
                                 'estrategia_12_1'	, 'estrategia_15_1' , 'estrategia_8_1' , 'estrategia_14_5',
                                 'estrategia_15_4',  'estrategia_13_3', 'estrategia_12_4', 'estrategia_13_2',
                                 'estrategia_15_5' , 'estrategia_15_3', 'estrategia_12_3')

#####


# calculando os indicadores de risco para as estrategias de "2006-01-13 / 2010-02-04" #####

risco_retorno <- data.frame( id = '',
                             data_inicial = format(as.Date("2006-01-01"), format = "%Y"),
                             retorno = as.numeric(0), 
                             des.pad. = as.numeric(0),
                             sharpe = as.numeric(0),
                             max_drawdown = as.numeric(0) , 
                             Sortino = as.numeric(0),
                             Risk_return_adjusted =as.numeric(0),
                             Pain_index = as.numeric(0),
                             Pain_ratio = as.numeric(0))

for (janela in names(lista_retorno_indices_performance)) { 
  
  print(janela)
  retorno_indices_performance <- lista_retorno_indices_performance[[janela]]
  
  risco_retorno_temp <- data.frame( id = colnames(retorno_indices_performance) ,
                                    data_inicial = format(first(index(retorno_indices_performance)), format = "%Y"),
                                    retorno = colMeans(retorno_indices_performance[which(complete.cases(retorno_indices_performance)),]),
                                    des.pad. = colStdevs(retorno_indices_performance),
                                    sharpe = t(SharpeRatio.annualized(retorno_indices_performance,geometric = FALSE)),
                                    max_drawdown = t(maxDrawdown(retorno_indices_performance,geometric = FALSE)) ,
                                    Sortino = t(SortinoRatio(retorno_indices_performance)),
                                    Risk_return_adjusted = t(((1+colMeans(retorno_indices_performance))^252 -1)/-VaR(retorno_indices_performance,
                                                                                         p = .95,method = 'historical')) ,
                                    Pain_index = t(PainIndex(retorno_indices_performance) ),
                                    Pain_ratio = t(PainRatio(retorno_indices_performance) ))
  
  
  colnames(risco_retorno_temp) <- c('id','data_inicial','retorno',
                                    'des.pad.','sharpe','max_drawdown',
                                    'Sortino','Risk_return_adjusted','Pain_index',
                                    'Pain_ratio')
  
  risco_retorno <- rbind(risco_retorno,risco_retorno_temp)
  
}

risco_retorno <- risco_retorno[-1,]

#risco_retorno[-which(substr(risco_retorno$id,1,10) == "Papel_base")[-1],]

# anualizando retorno e risco
risco_retorno$retorno <- ((1+risco_retorno$retorno )^252 -1)

risco_retorno$des.pad. <- (risco_retorno$des.pad. )*sqrt(252)

####

####### tabelas com os dados das estratégias #######

risco_retorno[,c(-1,-2)] %>%
  kbl(caption = "Tabela - Indicadores para todas estratégias", digits = 4, 'html') %>%
  kable_classic(full_width = F, html_font = "Cambria") 
  #save_kable(file = './imagens/Indicadores.pdf')

risco_retorno[c(1, which(risco_retorno$id %in% estrategias_selecionadas)), c(-1,-2)] %>%
  kbl(caption = "Tabela - Indicadores para estratégias selecionadas", digits = 4) %>%
  kable_classic(full_width = F, html_font = "Cambria")


######

# TODO : tirar o Var.Cov. !!!!!

# Tabela com os VaRs e ES #####
riscos_estrategias <- data.frame()

# loop para gera os VaR e ES de todas estrategias #####
for (id_estrategia in names(lista_retorno_indices_performance$`2006_01_13`))  {
  
  retornos <- lista_retorno_indices_performance$`2006_01_13`[,id_estrategia]
  
  riscos <- list()
  
  for (alpha in c(.95,.99) ){
    
    # funcoes definidas no "estimando_var_nao_condicional.R
    SimHist <- VaR_ES_SimHist(retornos,alpha)
    #VarCov <- VaR_ES_VarCov(retornos,alpha)
    MC_normal <-VaR_ES_MC_normal(retornos,alpha,N)
    MC_student <-VaR_ES_MC_student(retornos,alpha,N)
    POT <- VaR_ES_POT(retornos,alpha,.9)
    #BMM <- VaR_ES_BMM(retornos,alpha) # TODO 
    
    rm <- rbind("Sim Hist"   = unlist(SimHist[c('VaR','ES')]),
                #"Var. Cov."  = unlist(VarCov[c('VaR','ES')]), 
                "MC_normal"  = unlist(MC_normal[c('VaR','ES')]),
                "MC_student" = unlist(MC_student[c('VaR','ES')]),
                "POT"        = unlist(POT[c('VaR','ES')]))
    
    riscos[[as.character(alpha)]] <- rm
    
    
  }
  
  riscos_temp <- data.frame(id_estrategia = id_estrategia ,
                            t(riscos$`0.95`[,'VaR']),
                            t(riscos$`0.99`[,'VaR']),
                            t(riscos$`0.95`[,'ES']),
                            t(riscos$`0.99`[,'ES']))
  
  riscos_estrategias <- rbind(riscos_estrategias,riscos_temp)
    
}
#####

# tabelas com os VaR e os ES por modelo para todas as estrategia ######

nomes_colunas <-c('id estrategia',
                  paste0('Sim.Hist', footnote_marker_number(1)),
                  #'Var.Cov.',
                  paste0('MC normal', footnote_marker_number(2)),
                  paste0('MC t student', footnote_marker_number(3)),
                  paste0('POT', footnote_marker_number(4)),
                  'Sim.Hist',
                  #'Var.Cov.',
                  'MC normal','MC t student', 'POT')


riscos_estrategias[which(riscos_estrategias$id %in% estrategias_selecionadas), 1:9 ] %>%
  kbl(caption = "Tabela - Value at risk para estratégias selecionadas", digits = 4,
      row.names = FALSE,col.names = nomes_colunas , escape = F) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c(" ", "$\\alpha$ = .95" = 4, "$\\alpha$ = .99" = 4)) %>%
  add_header_above(c(" ", "Value at Risk" = 8)) %>%
  footnote(#general = "Here is a general comments of the table. ",
    number = c("Sim.Hist - Value at risk calculado através da distribuição histórica dos logs-retornos ; ",
               "MC normal - Value at risk estimado por um processo Monte Carlo onde são utilizados a média e a variancia observada; ",
               "MC t student - Semelhante ao MC normal no entanto supõe uma distribuição t-student dos log-retornos ; ",
               "POT - Modelo Peaks-over-threshold, estimação do Value at risk onde os excessos das perdas são ajustadas a uma distribuição de Pareto Generalizada (GPD) ; "),
    #alphabet = c("Footnote A; ", "Footnote B; "),
    #symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2")
  )

riscos_estrategias[which(riscos_estrategias$id %in% estrategias_selecionadas), c(1,10:17) ] %>%
  kbl(caption = "Tabela -  Expected Shortfall para estratégias selecionadas", digits = 4,
      row.names = FALSE,col.names = nomes_colunas, escape = F ) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c(" ", "$\\alpha$ = .95" = 4, "$\\alpha$ = .99" = 4)) %>%
  add_header_above(c(" ", "Expected Shortfall" = 8)) %>%
  footnote(#general = "Here is a general comments of the table. ",
           number = c("Sim.Hist - Expected Shortfall calculado através da distribuição histórica dos logs-retornos ; ",
                      "MC normal - Expected Shortfall estimado por um processo Monte Carlo onde são utilizados a média e a variancia observada; ",
                      "MC t student - Semelhante ao MC normal no entanto supõe uma distribuição t-student dos log-retornos ; ",
                      "POT - Modelo Peaks-over-threshold, estimação do Expected Shortfall onde os excessos das perdas são ajustadas a uma distribuição de Pareto Generalizada (GPD) ; "),
           #alphabet = c("Footnote A; ", "Footnote B; "),
           #symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2")
  )
########


# deletar?! essa parte de baixo é para visualizar as estrategias escolhidas no ano inicial #######

risco_retorno_melhores <- risco_retorno[(duplicated(risco_retorno$id,fromLast  = FALSE) | duplicated(risco_retorno$id,fromLast  = TRUE)) , ]

risco_retorno_melhores$retorno <- ((1+risco_retorno_melhores$retorno )^252 -1)

# média do retorno por estrategia
aggregate(risco_retorno_melhores[, 'retorno'], list(risco_retorno_melhores$id), mean)

######


# 

# teste incluindo os dados de VaR e ES  #####

df_teste <- cbind(risco_retorno,riscos_estrategias)

ggplot(data = df_teste,aes(x=des.pad. ,y=retorno )) + 
  geom_point(aes(fill=Pain_ratio),shape = 21,stroke = 1,size = 2) +
  scale_y_continuous(name = 'Retorno Anualizado') + 
  scale_x_continuous(name = 'Risco Anualizado') +  
  scale_fill_distiller(name = "Pain Ratio",palette = 'Spectral',direction = 1) +
  #ggrepel::geom_text_repel(data=df_teste[head(order(df_teste$POT),23), ], aes(label=id),
  #                         arrow = arrow(length = unit(2, "mm")) ,
  #                         box.padding = .6, max.overlaps = Inf , min.segment.length = 0,
  #                         
  #) +
  labs(title = "Risco-Retorno de todas estratégias - 2006-2010") +
  theme_minimal()+ 
  geom_abline(aes(slope = 1,intercept = 0,linetype = "Sharpe = 1")  ) + 
  geom_abline(aes(slope = 2,intercept = 0,linetype = "Sharpe = 2") ) + 
  geom_hline(yintercept = 0)+
  scale_linetype_manual("Nível Sharpe", values = c("dashed","dotted"),
                        breaks = c("Sharpe = 1","Sharpe = 2")) +
  scale_shape_manual(values = c(17, 19)) +
  guides(colour = guide_legend(order = 1), 
         shape = guide_legend(order = 1),
         linetype = guide_legend(order = 2)) + 
  theme_classic() +
  theme( 
    legend.justification = "center", 
    legend.spacing = unit(0.1, "cm"), 
    legend.spacing.y = unit(0.05, "cm"), 
    legend.margin = margin(0, 0, 0, 0), 
    legend.box.margin = margin(0, 0, 0, 0))


######



# scatter plot risco-retorno das estratégias com os melhores indice de sharpe #####

ggplot(data = risco_retorno , aes(x=des.pad.,y=retorno )) + 
  geom_point(aes(fill=Pain_ratio),shape = 21,stroke = 1,size = 2) +
  scale_fill_distiller(name = "Pain Ratio",palette = 'Spectral') +
  scale_y_continuous(name = 'Retorno Anualizado') + 
  scale_x_continuous(name = 'Risco Anualizado') + 
  #theme(legend.position="none") +
  ggrepel::geom_text_repel(data=risco_retorno[tail(order(risco_retorno$Risk_return_adjusted),23), ], aes(label=id),
                           arrow = arrow(length = unit(2, "mm")) ,
                           box.padding = .6, max.overlaps = Inf , min.segment.length = 0,
  ) +
  labs(title = "Risco-Retorno de todas estratégias - 2006-2010") +
  theme_minimal()+ 
  geom_abline(aes(slope = 1,intercept = 0,linetype = "Sharpe = 1")  ) + 
  geom_abline(aes(slope = 2,intercept = 0,linetype = "Sharpe = 2") ) + 
  scale_linetype_manual("Nível Sharpe", values = c("dashed","dotted"),
                        breaks = c("Sharpe = 1","Sharpe = 2")) +
  scale_shape_manual(values = c(17, 19)) +
  guides(colour = guide_legend(order = 1), 
         shape = guide_legend(order = 1),
         linetype = guide_legend(order = 2)) + 
  theme_classic() +
  theme( 
    legend.justification = "center", 
    legend.spacing = unit(0.1, "cm"), 
    legend.spacing.y = unit(0.05, "cm"), 
    legend.margin = margin(0, 0, 0, 0), 
    legend.box.margin = margin(0, 0, 0, 0))

#####

# scatter plot risco-retorno das estratégias escolhidas para outros testes  ######

ggplot(data = risco_retorno ,aes(x=des.pad. ,y=retorno )) + 
  geom_point(aes(fill=Pain_ratio),shape = 21,stroke = 1,size = 2) +
  scale_y_continuous(name = 'Retorno Anualizado') + 
  scale_x_continuous(name = 'Risco Anualizado') +  
  scale_fill_distiller(name = "Pain Ratio",palette = 'Spectral',direction = 1) +
  ggrepel::geom_text_repel(data=risco_retorno[which(risco_retorno$id %in% estrategias_selecionadas), ], aes(label=id),
                           arrow = arrow(length = unit(2, "mm")) ,
                           box.padding = .6, max.overlaps = Inf , min.segment.length = 0,
                           
                           ) +
  labs(title = "Risco-Retorno de todas estratégias - 2006-2010") +
  theme_minimal()+ 
  geom_abline(aes(slope = 1,intercept = 0,linetype = "Sharpe = 1")  ) + 
  geom_abline(aes(slope = 2,intercept = 0,linetype = "Sharpe = 2") ) + 
  geom_hline(yintercept = 0)+
  scale_linetype_manual("Nível Sharpe", values = c("dashed","dotted"),
                        breaks = c("Sharpe = 1","Sharpe = 2")) +
  scale_shape_manual(values = c(17, 19)) +
  guides(colour = guide_legend(order = 1), 
         shape = guide_legend(order = 1),
         linetype = guide_legend(order = 2)) + 
  theme_classic() +
  theme( 
        legend.justification = "center", 
        legend.spacing = unit(0.1, "cm"), 
        legend.spacing.y = unit(0.05, "cm"), 
        legend.margin = margin(0, 0, 0, 0), 
        legend.box.margin = margin(0, 0, 0, 0))

######

#### line plot dos retornos das estratégias selecionadas

df <- fortify(lista_indices_performance$`2006_01_13`)

df$Index <- as.Date(df$Index)

df2 <- data.frame(df[1],stack(df[2:ncol(df)]))


# calculando drawdown #####

drawdown <- function(perfomance_nivel){
  NAV <- perfomance_nivel[complete.cases(perfomance_nivel),]
  # Max Drawdown , a funcao nao retorna ele, depois vejo o q faco com ele
  MDD <- 0
  pico <- -99999
  
  DD <- NAV
  
  for ( i in 1:nrow(NAV)){
    
    if (as.numeric(NAV[i]) > as.numeric(pico)){
      pico <- NAV[i]
    }
    DD[i] <- - (as.numeric(pico) - as.numeric(NAV[i]))/as.numeric(pico)
    if ( as.numeric(DD[i]) > as.numeric(MDD) ){
      MDD <- DD[i]
    }
    
    
  }
  return(DD)
  
}

drawdown_todas_estrategias <- lista_indices_performance$`2006_01_13`[complete.cases(lista_indices_performance$`2006_01_13`),]


for (id_estrategia in names(lista_indices_performance$`2006_01_13`)) {
  
  NAV <- lista_indices_performance$`2006_01_13`[complete.cases(lista_indices_performance$`2006_01_13`),id_estrategia]
  
  drawdown_todas_estrategias[,id_estrategia] <- drawdown(NAV)

  
  
}

# empilhando os dados para o ggplot
df_dd <- fortify(drawdown_todas_estrategias)

df_dd$Index <- as.Date(df_dd$Index)

df2_dd <- data.frame(df_dd[1],stack(df_dd[2:ncol(df_dd)]))



#####

# estrategias_selecionadas
# as.character(risco_retorno[tail(order(risco_retorno$sharpe),5),'id' ] ) )
 
nivel_plot <- ggplot() +
              geom_line(data = dplyr::filter(df2 , ind %in% estrategias_selecionadas[16:23] ),
                        aes(x = Index, y = values, color = ind), size = .7) + 
              geom_line(data = dplyr::filter(df2 , ind %in% 'Papel_base' ) ,
                        aes(x = Index, y = values ,linetype = 'Papel_base'), size = .7) +
  
  # define as marcações no eixo x            
  scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = scales::date_breaks("6 month"),
               limits = c( as.Date("2006-01-13") , as.Date("2010-02-04") ),
               expand = c(0,0))+
  
  # define o titulo e subtitulo            
  labs(title = "Retorno de todas estratégias - 2006-2010",
                   subtitle = "2006-01-12 definido como 100") +
  
  # define o tema basico
  theme_classic() +
  
  # configurando a legenda das linhas manualmente
  scale_linetype_manual("", values = c(1),
                                    breaks = c("Papel_base")) +
  scale_shape_manual(values = c(17, 19)) +
  guides(colour = guide_legend(order = 1),
         shape = guide_legend(order = 1),
         linetype = guide_legend(order = 2)) +
  
  # define estica geral do grafico
  theme( 
        panel.grid.major.x = element_line(colour = "grey50"),
        #panel.grid.major.y = element_line(colour = "grey50"),
        panel.border = element_rect(linetype = 1, fill = NA) ,
        panel.background = element_rect(fill = 'white',colour = 'black'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.title=element_blank(),
        legend.justification = "center", 
        legend.spacing = unit(0., "cm"),
        legend.spacing.y = unit(0.0, "cm"), 
        legend.margin = margin(0, 0, 0, 0), 
        legend.box.margin = margin(0, 0, 0, 0),
        legend.position = c(.1,.67))


drawdown_plot <- ggplot() +
                  geom_line(data = dplyr::filter(df2_dd , ind %in% estrategias_selecionadas[16:23]),
                            aes(x = Index, y = values, color = ind), size = .7,show.legend = FALSE) + 
                  geom_line(data = dplyr::filter(df2_dd , ind %in% 'Papel_base' ) ,
                            aes(x = Index, y = values), size = .7,show.legend = FALSE) +
                  #geom_line(data = dplyr::filter(df2 ,! ind %in% as.character(risco_retorno[tail(order(risco_retorno$sharpe),23),'id' ] ) ),
                  #          aes(x = Index, y = values, alpha = .01, group = ind), linetype = 3, show.legend = FALSE) +
                  scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = scales::date_breaks("6 month"),
                               limits = c( as.Date("2006-01-13") , as.Date("2010-02-04") ),
                               expand = c(0,0))+
                  labs(title = "Drawdown") +
                  #theme_classic() +
                  theme( 
                    panel.grid.major.x = element_line(colour = "grey50"),
                    panel.grid.major.y = element_line(colour = "grey50"),
                    panel.border = element_rect(linetype = 1, fill = NA) ,
                    panel.background = element_rect(fill = 'white',colour = 'black'),
                    legend.justification = "center", 
                    legend.spacing = unit(0.1, "cm"), 
                    legend.spacing.y = unit(0.05, "cm"), 
                    legend.margin = margin(0, 0, 0, 0), 
                    legend.box.margin = margin(0, 0, 0, 0),
                    axis.text.x = element_text( vjust = 0, hjust=1))




#gs <-  arrangeGrob(nivel_plot ,drawdown_plot,nrow = 2 )
#gList(nivel_plot,drawdown_plot)

grid.arrange( nivel_plot ,drawdown_plot,
             layout_matrix = rbind(c(1,1,1),
                                   c(1,1,1),
                                   c(1,1,1),
                                   c(2,2,2)) )


#####
