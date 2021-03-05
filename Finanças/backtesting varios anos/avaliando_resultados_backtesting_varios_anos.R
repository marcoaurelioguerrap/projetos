library(plyr)
library(xts)
library(qrmtools)
library(PerformanceAnalytics)
library(knitr)
library(kableExtra)
library(gridExtra)
library(grid)
library(lattice)
library(ggalt)
library(ggrepel)
library(nvmix)
library(dplyr)
library(timeSeries)
library(ggplot2)
library(nvmix)
library(fBasics)
library(MASS)
library(reshape2)
# utilizado para gera as tabelas do kable
# install.packages("magick")
# install.packages("webshot")
# webshot::install_phantomjs()

######## avaliando resultados backtesting varios anos ##########

# Carregando os dados ######

# 1) carregando os dados do primeiro backtesting ( 2006-2010 ) ######

id_estrategias <- rbind(c("estrategia_1_1","estrategia_9_4"),
                        c("estrategia_10_1","estrategia_16_6"),
                        c("estrategia_17_1","estrategia_22_6"))


lista_indices_performance <- list()

lista_retorno_indices_performance <- list()

for (i in 1:3) {
  if (i == 1){
    indices_performance_todas_estrategias_2006 <- readRDS(file = paste0(".//dados testes/indices_performance_final_",
                                                                        id_estrategias[i,1],"_",id_estrategias[i,2],
                                                                        ".RData"))
    
    retorno_indices_performance_todas_estrategias_2006 <- readRDS(file = paste0(".//dados testes/retorno_indices_performance_final_",
                                                                                id_estrategias[i,1],"_",id_estrategias[i,2],
                                                                                ".RData"))
    
  } else {
    
    indices_performance_todas_estrategias_2006 <- cbind(indices_performance_todas_estrategias_2006,
                                                        readRDS(file = paste0(".//dados testes/indices_performance_final_",
                                                                              id_estrategias[i,1],"_",id_estrategias[i,2],".RData"))[,-1])
    
    retorno_indices_performance_todas_estrategias_2006 <- cbind(retorno_indices_performance_todas_estrategias_2006,
                                                                readRDS(file = paste0(".//dados testes/retorno_indices_performance_final_",
                                                                                      id_estrategias[i,1],"_",id_estrategias[i,2],".RData"))[,-1])
  }
}


lista_indices_performance[["2006_01_13"]] <- indices_performance_todas_estrategias_2006 

lista_retorno_indices_performance[["2006_01_13"]] <- retorno_indices_performance_todas_estrategias_2006

#####


# 2) carregando os dados dos backtestings para varias janelas  ######

# separando as estrategias selecionadas 


estrategias_selecionadas <- list('estrategia_13_6','estrategia_10_4','estrategia_10_5', 'estrategia_13_5' , 
                                 'estrategia_14_6'	, 'estrategia_13_4' , 'estrategia_12_7'	,'estrategia_10_3' ,
                                 'estrategia_12_6'	, 'estrategia_8_2' , 'estrategia_17_6' , 'estrategia_12_5' ,
                                 'estrategia_12_1'	, 'estrategia_15_1' , 'estrategia_8_1' , 'estrategia_14_5',
                                 'estrategia_15_4',  'estrategia_13_3', 'estrategia_12_4', 'estrategia_13_2',
                                 'estrategia_15_5' , 'estrategia_15_3', 'estrategia_12_3')



janelas = list( 
  #dias.de.trade[1500:2500],
  dias.de.trade[2000:3000],
  dias.de.trade[2500:3500],
  dias.de.trade[3000:4000],
  dias.de.trade[3500:4500],
  dias.de.trade[3964:4962])


for (janela in janelas) {
  
  
  lista_indices_performance[[as.character(first(janela))]] <- readRDS(file = paste0(".//dados testes/indices_performance_varios_anos_",first(janela),
                                                                                    ".RData"))
  
  lista_retorno_indices_performance[[as.character(first(janela))]] <- readRDS(file = paste0(".//dados testes/retorno_indices_performance_varios_anos_",
                                                                                            first(janela),".RData"))
}
#####


# 3) montando tabela de indicadores para todas as janelas #######


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

# loop que itera sobre todas as estrategias 
for (janela in names(lista_retorno_indices_performance)) { 
  
  print(janela)
  retorno_indices_performance <- lista_retorno_indices_performance[[janela]][complete.cases(lista_retorno_indices_performance[[janela]])]
  
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

#####


# 4) scatter plot risco-retorno, varios anos ######

ggplot(data = risco_retorno[which(risco_retorno$id %in% estrategias_selecionadas), ] , aes(x=des.pad.,y=retorno )) + 
  geom_point(aes(color=id)) +
  #scale_fill_distiller(name = "Pain Ratio",palette = 'Spectral') +
  scale_y_continuous(name = 'Retorno Anualizado') + 
  scale_x_continuous(name = 'Risco Anualizado') + 
  #theme(legend.position="none") +
  #ggrepel::geom_text_repel(data=risco_retorno[which(risco_retorno$id %in% estrategias_selecionadas), ], aes(label=id),
  #                         arrow = arrow(length = unit(2, "mm")) ,
  #                         box.padding = .6, max.overlaps = Inf , min.segment.length = 0,
  #) +
  labs(title = "Risco-Retorno de estratégias selecionadas - Todos os anos",
       color = "id estrategias",
       caption = "Periodo Backtesting : 2006 - de 2006-01-12 até 2010-02-05,2008 - de 2008-01-28 até 2012-02-09, de 2010 - 2010-02-05 até 2014-02-18,
       2012 - de 2012-02-09 até 2016-03-02, 2014 - de 2014-02-18 até 2018-03-09, 2016 - de 2016-01-07 até 2020-01-22" ) +
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
    panel.grid.major.x = element_line(colour = "grey"),
    panel.grid.major.y = element_line(colour = "grey"),
    legend.justification = "center", 
    legend.spacing = unit(0.1, "cm"), 
    legend.spacing.y = unit(0.05, "cm"), 
    legend.margin = margin(0, 0, 0, 0), 
    legend.box.margin = margin(0, 0, 0, 0),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1))+
  facet_wrap(~data_inicial)



#####


# 5) tabelas com os dados das estratégias #######

# separando os riscos retornos das estratégias selecionadas
risco_retorno_melhores <- risco_retorno[which(risco_retorno$id %in% c(unlist(estrategias_selecionadas),"Papel_base")),]

# ordenando o risco_retorno_melhores para poder fazer a tabela com os dados de cada estrategia 
# organizado por ano
risco_retorno_melhores <- risco_retorno_melhores[order(risco_retorno_melhores$id,decreasing = TRUE),]

# Separa os numeros de linhas para cada grup
tabela_classe <- table(risco_retorno_melhores$id)
tabela_classe <- tabela_classe[which(table(risco_retorno_melhores$id) != 0)]
tabela_classe[seq(24,1,-1)]

# cria tabela com os dados de risco
risco_retorno_melhores[,-1] %>%
  kbl(caption = "Tabela - Indicadores por ano para estratégias selecionadas", digits = 4, 'html', row.names = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%

  pack_rows(index = tabela_classe[seq(24,1,-1)],label_row_css = "border-bottom: 1.2px solid;border-top: 1.2px solid;",
            bold = FALSE) %>%
  save_kable(file = './imagens/Indicadores_melhores_est_por_ano.png')

#####


# 6) Tabela com os VaRs e ES #####
riscos_estrategias_var <- data.frame()

# loop para gera os VaR e ES de todas estrategias 
for (retornos_ano in lista_retorno_indices_performance) {
  
  retornos_ano<- retornos_ano[complete.cases(retornos_ano),]
  
  for (id_estrategia in c("Papel_base",unlist(estrategias_selecionadas) ) ){
    
    retornos <- retornos_ano[,id_estrategia]
    
    riscos <- list()
    
    riscos_temp <- 0
    
    for (alpha in c(.95,.99) ){
      
      # funcoes definidas no "estimando_var_nao_condicional.R
      SimHist <- VaR_ES_SimHist(retornos,alpha)
      #VarCov <- VaR_ES_VarCov(retornos,alpha)
      MC_normal <-VaR_ES_MC_normal(retornos,alpha,N)
      MC_student <- tryCatch(VaR_ES_MC_student(retornos,alpha,N), warning=function(w) returns(NA) )
      POT <- tryCatch(VaR_ES_POT(retornos,alpha,.9), error=function(e) return(NA) )
      #BMM <- VaR_ES_BMM(retornos,alpha) # TODO 
      
      rm <- rbind("Ano" = format(first(index(retornos_ano)),"%Y"),
                  "Sim Hist"   = unlist(SimHist[c('VaR','ES')]),
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
    
    riscos_estrategias_var <- rbind(riscos_estrategias_var,riscos_temp)
    
  }

}

# 6.2) 

View(riscos_estrategias_var)


#####


# 7) tabelas com os diferentes VaR e ES das estratégias selecionadas #######

# ordenando o risco_retorno_melhores para poder fazer a tabela com os dados de cada estrategia 
# organizado por ano
riscos_estrategias_var_ordenado <- riscos_estrategias_var[order(riscos_estrategias_var$id,decreasing = FALSE),]

indx <- sapply(riscos_estrategias_var_ordenado[,3:21], is.factor)

riscos_estrategias_var_ordenado[,3:21][indx] <- lapply(riscos_estrategias_var_ordenado[,3:21][indx], function(x) as.numeric(as.character(x)))

# Separa os numeros de linhas para cada grup
tabela_classe <- table(riscos_estrategias_var_ordenado$id_estrategia)
#tabela_classe <- tabela_classe[which(table(riscos_estrategias_var_ordenado$id_estrategia) != 0)]
#tabela_classe[seq(24,1,-1)]

riscos_estrategias_var_ordenado <- riscos_estrategias_var_ordenado[,c(-7,-12,-17)]

nomes_colunas <-c(#'id estrategia',
                  'ano',
                  paste0('Sim.Hist', footnote_marker_number(1)),
                  #'Var.Cov.',
                  paste0('MC normal', footnote_marker_number(2)),
                  paste0('MC t student', footnote_marker_number(3)),
                  paste0('POT', footnote_marker_number(4)),
                  'Sim.Hist',
                  #'Var.Cov.',
                  'MC normal','MC t student', 'POT')


# cria tabela com os dados de risco
riscos_estrategias_var_ordenado[,2:10] %>%
  kbl(caption = "Tabela - Indicadores por ano para estratégias selecionadas", digits = 4, 'html',
      row.names = FALSE,col.names = nomes_colunas , escape = F)  %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  
  pack_rows(index = tabela_classe,label_row_css = "border-bottom: 1.2px solid;border-top: 1.2px solid;",
            bold = FALSE) %>%
  add_header_above(c(" ", "$\\alpha$ = .95" = 4, "$\\alpha$ = .99" = 4)) %>%
  add_header_above(c( "Value at Risk" = 9)) %>%
  footnote(#general = "Here is a general comments of the table. ",
    number = c("Sim.Hist - Value at risk calculado através da distribuição histórica dos logs-retornos ; ",
               "MC normal - Value at risk estimado por um processo Monte Carlo onde são utilizados a média e a variancia observada; ",
               "MC t student - Semelhante ao MC normal no entanto supõe uma distribuição t-student dos log-retornos ; ",
               "POT - Modelo Peaks-over-threshold, estimação do Value at risk onde os excessos das perdas são ajustadas a uma distribuição de Pareto Generalizada (GPD) ; "),
    #alphabet = c("Footnote A; ", "Footnote B; "),
    #symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2")
  ) %>%
  save_kable(file = './imagens/VaR_melhores_est_por_ano.png')

riscos_estrategias_var_ordenado[,c(1,11:18)] %>%
  kbl(caption = "Tabela - Indicadores por ano para estratégias selecionadas", digits = 4, 'html',
      row.names = FALSE,col.names = nomes_colunas , escape = F)  %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  
  pack_rows(index = tabela_classe,label_row_css = "border-bottom: 1.2px solid;border-top: 1.2px solid;",
            bold = FALSE) %>%
  add_header_above(c(" ", "$\\alpha$ = .95" = 4, "$\\alpha$ = .99" = 4)) %>%
  add_header_above(c( "Expected Shorfall" = 9)) %>%
  footnote(#general = "Here is a general comments of the table. ",
    number = c("Sim.Hist - Expected Shorfall calculado através da distribuição histórica dos logs-retornos ; ",
               "MC normal - Expected Shorfall estimado por um processo Monte Carlo onde são utilizados a média e a variancia observada; ",
               "MC t student - Semelhante ao MC normal no entanto supõe uma distribuição t-student dos log-retornos ; ",
               "POT - Modelo Peaks-over-threshold, estimação do Expected Shorfall onde os excessos das perdas são ajustadas a uma distribuição de Pareto Generalizada (GPD) ; "),
    #alphabet = c("Footnote A; ", "Footnote B; "),
    #symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2")
  ) %>%
  save_kable(file = './imagens/ES_melhores_est_por_ano.png')

  
#####


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


#####

# 8) line plot performances das estratégias selecionadas ####

lista_estrategias_selecionadas <- list( estrategias_selecionadas[1:5],
                                        estrategias_selecionadas[6:10],
                                        estrategias_selecionadas[11:15],
                                        estrategias_selecionadas[16:23])


for (grupo in lista_estrategias_selecionadas)  {# loop para gera os VaR e ES de todas estrategias 
  for (nivel_ano in lista_indices_performance) {
    #nivel_ano <- lista_indices_performance[[2]]
    nivel_ano <- nivel_ano[complete.cases(nivel_ano),]
    
    #### line plot dos retornos das estratégias selecionadas
    
    df <- fortify(nivel_ano)
    
    df$Index <- as.Date(df$Index)
    
    df2 <- data.frame(df[1],stack(df[2:ncol(df)]))
   
    #df2 <- dplyr::filter(df2 , ind %in% estrategias_selecionadas )
    
  
    # estrategias_selecionadas
    # as.character(risco_retorno[tail(order(risco_retorno$sharpe),5),'id' ] ) )
    
    nivel_plot <- ggplot() +
      geom_line(data = dplyr::filter(df2 , ind %in% grupo ),
                aes(x = Index, y = values, color = ind), size = .7) + 
      geom_line(data = dplyr::filter(df2 , ind %in% 'Papel_base' ) ,
                aes(x = Index, y = values ,linetype = 'Papel_base'), size = .7) +
      
      # define as marcações no eixo x            
      scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = scales::date_breaks("6 month"),
                   limits = c( as.Date(first(index(nivel_ano))) , as.Date(last(index(nivel_ano))) ),
                   expand = c(0,0))+
      
      # define o titulo e subtitulo            
      labs(title = paste0("Retorno de todas estratégias - ",
                          format(first(index(nivel_ano)), "%Y") , 
                          " / ", 
                          format(last(index(nivel_ano)), "%Y")),
           subtitle = paste0(as.Date(first(index(nivel_ano)))," definido como 100") ) +
      
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
    
    
    # calculando drawdown das estrategias
    drawdown_todas_estrategias <- nivel_ano
    
    
    for (id_estrategia in names(nivel_ano)) {
      
      NAV <- nivel_ano[complete.cases(nivel_ano),id_estrategia]
      
      drawdown_todas_estrategias[,id_estrategia] <- drawdown(NAV)
      
      
      
    }
    
    # empilhando os dados para o ggplot
    df_dd <- fortify(drawdown_todas_estrategias)
    
    df_dd$Index <- as.Date(df_dd$Index)
    
    df2_dd <- data.frame(df_dd[1],stack(df_dd[2:ncol(df_dd)]))
    
    
    
    drawdown_plot <- ggplot() +
      geom_line(data = dplyr::filter(df2_dd , ind %in% grupo),
                aes(x = Index, y = values, color = ind), size = .7,show.legend = FALSE) + 
      geom_line(data = dplyr::filter(df2_dd , ind %in% 'Papel_base' ) ,
                aes(x = Index, y = values), size = .7,show.legend = FALSE) +
      #geom_line(data = dplyr::filter(df2 ,! ind %in% as.character(risco_retorno[tail(order(risco_retorno$sharpe),23),'id' ] ) ),
      #          aes(x = Index, y = values, alpha = .01, group = ind), linetype = 3, show.legend = FALSE) +
      scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = scales::date_breaks("6 month"),
                   limits =c( as.Date(first(index(nivel_ano))) , as.Date(last(index(nivel_ano))) ),
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
    
    plot <- grid.arrange( nivel_plot ,drawdown_plot,
                  layout_matrix = rbind(c(1,1,1),
                                        c(1,1,1),
                                        c(1,1,1),
                                        c(2,2,2)) )
    
    # Tenho q corrigir como salva os tamanhos das linhas
    #ggsave(file=paste0(".//imagens/", format(first(index(nivel_ano))) ,".png"),dpi = 300,
    #       plot , width = 894/300, height = 604/300 , units = 'in',scale = 3)
    #####
  }
  #####
  
}


#####

# 9) VaR e ES rolling window ####

################################################################
# OBS.: DEMORADO, rodar uma estratégia 2500 dias demora 1 hora #
################################################################

#######
resultados_VaR_ES_para_todos_anos_e_estrategias <- list()

start.time <- Sys.time()
for (id_estrategia in names(lista_retorno_indices_performance$`2008-01-2`)[20:21]){
  print(id_estrategia)
  
  # Agrupa os retornos por periodos das estrategias. #####
  # OBS.: algumas das estrategias são sensiveis ao momento inicial, então isso
  # pode gerar distorcoes no retorno real da estratégia. Só estou fazendo isso para demonstrar
  # as diferentes medidas de risco ... ( na hora de fazer o backtesting só coloquei 1000 dias , preciso de 1000
  # mais ou menos para ser possivel calcular os estimadores por GPD)
  
  retorno_sintetico_todo_periodo <-  cbind(lista_retorno_indices_performance[[1]][,id_estrategia],
                                           lista_retorno_indices_performance[[2]][,id_estrategia],
                                           lista_retorno_indices_performance[[3]][,id_estrategia],
                                           lista_retorno_indices_performance[[4]][,id_estrategia],
                                           lista_retorno_indices_performance[[5]][,id_estrategia],
                                           lista_retorno_indices_performance[[6]][,id_estrategia])
  
  #max <- xts(apply(retorno_sintetico_todo_periodo,1,maximo),
  #           order.by = as.Date(index(retorno_sintetico_todo_periodo)))
  
  #min <- xts(apply(retorno_sintetico_todo_periodo,1,minimo),
  #           order.by = as.Date(index(retorno_sintetico_todo_periodo)))
  
  media <- xts(rowMeans(as.matrix(retorno_sintetico_todo_periodo),na.rm = TRUE),
               order.by = as.Date(index(retorno_sintetico_todo_periodo)))
  
  #####
  
  
  # estimacao dos VaR e ES #####
  
  #resultado_estimacao_max <- estimacao_VaR_ES(max, 1000 ,.9) 
  #resultado_estimacao_min <- estimacao_VaR_ES(min, 1000 ,.9) 
  resultado_estimacao_media <- tryCatch(estimacao_VaR_ES(media, 1000 ,.9) , error=function(e){return('bugou')} )
  
  #####
  
  resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]] <- list( resultado_estimacao_media = resultado_estimacao_media)
  
  
  #list( #resultado_estimacao_max = resultado_estimacao_max,
  #resultado_estimacao_min = resultado_estimacao_min,
  #resultado_estimacao_media = resultado_estimacao_media)
  
}
end <- Sys.time()
tempo.total <- end- start.time 

##### quebra galho pq esqueci de colar no rolling windo do VaR e ES

for (id_estrategia in names(resultados_VaR_ES_para_todos_anos_e_estrategias)){
  print(id_estrategia)
  
  # Agrupa os retornos por periodos das estrategias. #####
  # OBS.: algumas das estrategias são sensiveis ao momento inicial, então isso
  # pode gerar distorcoes no retorno real da estratégia. Só estou fazendo isso para demonstrar
  # as diferentes medidas de risco ... ( na hora de fazer o backtesting só coloquei 1000 dias , preciso de 1000
  # mais ou menos para ser possivel calcular os estimadores por GPD)
  
  retorno_sintetico_todo_periodo <-  cbind(lista_retorno_indices_performance[[1]][,id_estrategia],
                                           lista_retorno_indices_performance[[2]][,id_estrategia],
                                           lista_retorno_indices_performance[[3]][,id_estrategia],
                                           lista_retorno_indices_performance[[4]][,id_estrategia],
                                           lista_retorno_indices_performance[[5]][,id_estrategia],
                                           lista_retorno_indices_performance[[6]][,id_estrategia])
  
  #max <- xts(apply(retorno_sintetico_todo_periodo,1,maximo),
  #           order.by = as.Date(index(retorno_sintetico_todo_periodo)))
  
  #min <- xts(apply(retorno_sintetico_todo_periodo,1,minimo),
  #           order.by = as.Date(index(retorno_sintetico_todo_periodo)))
  
  media <- xts(rowMeans(as.matrix(retorno_sintetico_todo_periodo),na.rm = TRUE),
               order.by = as.Date(index(retorno_sintetico_todo_periodo)))
  
  
  media_rw <- rollapply(media,1000,FUN = mean)
  
  des_pad_rw <- rollapply(media,1000,FUN = stdev)
  
  resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]]$resultado_estimacao_media$media_rw <- media_rw
  
  resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]]$resultado_estimacao_media$des_pad_rw <- des_pad_rw
}

#saveRDS(resultados_VaR_ES_para_todos_anos_e_estrategias,file = ".//estrategias/estimacoes_VaR_ES_resultados_final_quasetodos.RData")



#########

#
#seguro111 <- resultados_VaR_ES_para_todos_anos_e_estrategias


#teste <- readRDS(".//estrategias/estimacoes_VaR_ES_resultados_1_6_teste.RData")

#resultados_VaR_ES_para_todos_anos_e_estrategias <- c(teste,seguro111)
#

# tabelas com os testes para as estimações VaR e ES para todas estrategias
for (id_estrategia in names(resultados_VaR_ES_para_todos_anos_e_estrategias)[c(-11,-14,-21)]){
  
  # tabela com teste de violações VaR 95%

  resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]]$resultado_estimacao_media$`estatisticas do VaR por ano`[,c(1:6)] %>%
    kbl(caption = paste0("Tabela - Violações do Value at Risk a 95% - ",id_estrategia), digits = 4, 'html',
        row.names = FALSE,
        col.names = c("Modelo", "n","Violações esperadas","Violações do VaR a 95%", "p-valor Binomial¹","p-valor Binomial cond.²") ,
        escape = F)  %>%
    kable_classic(full_width = F, html_font = "Cambria") %>%
    column_spec(1,width_min  = '1.7in') %>%
    footnote(#general = "Here is a general comments of the table. ",
      number = c("p-Valor do teste binomial não condicional número de violações = número de violações esperado,
                 H0 : número de violações correto ",
                 "p-Valor do teste binomial condicional (independência das violações) número de violações = número de violações esperado,
                 H0 : número de violações correto e independente")) %>%
    save_kable(file = paste0('./imagens/violacoes_var/violacoes_var_95',id_estrategia,".png" ) )
    
  # tabela com teste de violações VaR 99%
  
  resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]]$resultado_estimacao_media$`estatisticas do VaR por ano`[,c(1,2,7:10)]%>%
    kbl(caption = paste0("Tabela - Violações do Value at Risk a 99% - ",id_estrategia), digits = 4, 'html',
        row.names = FALSE,
        col.names = c("Modelo", "n","Violações esperadas","Violações do VaR a 99%", "p-valor Binomial¹","p-valor Binomial cond.²") ,
        escape = F)  %>%
    kable_classic(full_width = F, html_font = "Cambria") %>%
    column_spec(1,width_min  = '1.7in') %>%
    footnote(#general = "Here is a general comments of the table. ",
      number = c("p-Valor do teste binomial não condicional número de violações = número de violações esperado,
                 H0 : número de violações correto ",
                 "p-Valor do teste binomial condicional (independência das violações) número de violações = número de violações esperado,
                 H0 : número de violações correto e independente")) %>%
    save_kable(file = paste0('./imagens/violacoes_var/violacoes_var_99',id_estrategia,".png" ) )

  
  # tabela com teste de média ES 95%
  resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]]$resultado_estimacao_media$`estatisticas do ES por ano`[,c(1:4)] %>%
    kbl(caption = paste0("Tabela - Violações do Value at Risk a 95% - ",id_estrategia), digits = 4, 'html',
        row.names = FALSE,
        col.names = c("Modelo", "n","p-valor da média esperada¹", "p-valor da média esperada por boot²") ,
        escape = F)%>%
    kable_classic(full_width = F, html_font = "Cambria") %>%
    column_spec(1,width_min  = '1.7in') %>%
    footnote(#general = "Here is a general comments of the table. ",
      number = c("p-Valor do teste que a média das violações do VaR a 95% é igual ao Expected Shortfall,
                 H0 : média das violações = ES(perdas) à 95%",
                 "p-Valor do teste por bootstrap que a média das violações do VaR a 95% é igual ao Expected Shortfall,
                 H0 : média das violações = ES(perdas) à 95%")) %>%
    save_kable(file = paste0('./imagens/violacoes_es/violacoes_ES_95',id_estrategia,".png" ) )
    
    # tabela com teste de média ES 99%
    resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]]$resultado_estimacao_media$`estatisticas do ES por ano`[,c(1,2,5,6)] %>%
      kbl(caption = paste0("Tabela - Violações do Expected Shortfall à 99% - ",id_estrategia), digits = 4, 'html',
          row.names = FALSE,
          col.names = c("Modelo", "n","p-valor da média esperada¹", "p-valor da média esperada por boot²") ,
          escape = F)%>%
      kable_classic(full_width = F, html_font = "Cambria") %>%
      column_spec(1,width_min  = '1.7in') %>%
      footnote(#general = "Here is a general comments of the table. ",
        number = c("p-Valor do teste que a média das violações do VaR a 99% é igual ao Expected Shortfall,
                 H0 : média das violações = ES(perdas) à 99%",
                   "p-Valor do teste por bootstrap que a média das violações do VaR a 99% é igual ao Expected Shortfall,
                 H0 : média das violações = ES(perdas) à 99%")) %>%
    save_kable(file = paste0('./imagens/violacoes_es/violacoes_ES_99',id_estrategia,".png" ) )
  
  
}



#### 10) Graficos dos resultados ######

avaliando_var_es <- resultados_VaR_ES_para_todos_anos_e_estrategias$estrategia_14_6$resultado_estimacao_media

losses <- xts(avaliando_var_es$`VaR e ES estimados`$`Simulacao Historica`$actual_log_loss,
              order.by = avaliando_var_es$`VaR e ES estimados`$`Simulacao Historica`$dia)

SH_VaR <- xts(avaliando_var_es$`VaR e ES estimados`$`Simulacao Historica`$VaR_95,
              order.by = avaliando_var_es$`VaR e ES estimados`$`Simulacao Historica`$dia)

SH_CONDEVT <- xts(avaliando_var_es$`VaR e ES estimados`$`SH-CONDEVT`$VaR_95,
                  order.by = avaliando_var_es$`VaR e ES estimados`$`SH-CONDEVT`$dia)


# legacy !!!
#grafico_var_SH_vs_CONDEVT <- xts(cbind(avaliando_var_es$`VaR e ES estimados`$`Simulacao Historica`$actual_log_loss,
#                                       avaliando_var_es$`VaR e ES estimados`$`Simulacao Historica`$VaR_95,
#                                       avaliando_var_es$`VaR e ES estimados`$`SH-GARCH t`$VaR_95),
#                                 order.by = avaliando_var_es$`VaR e ES estimados`$`Simulacao Historica`$dia )


grafico_var_SH_vs_CONDEVT <- cbind(losses,SH_VaR,SH_CONDEVT)

plot.xts(grafico_var_SH_vs_CONDEVT['2010/2012'],
     lty = c(1,2,3),lwd=1.7,col = 'black' , major.ticks = NULL, main = "VaR 95% do log das perdas",
     grid.col =NA)

points(grafico_var_SH_vs_CONDEVT[which(grafico_var_SH_vs_CONDEVT[,1]>grafico_var_SH_vs_CONDEVT[,2]),1],
       pch =3, lwd = 1.9,cex=1.5)

points(grafico_var_SH_vs_CONDEVT[which(grafico_var_SH_vs_CONDEVT[,1]>grafico_var_SH_vs_CONDEVT[,3]),1],
       pch = 1, lwd = 1.9,cex=1.5)


addLegend("topright", on=1, 
          legend.names = c("Log das perdas", "SH VaR 95%" , "SH-GARCH VaR 95%"), 
          lty=c(1,2,3), lwd=1.7,
          col="black")

addLegend("bottomright", on=1, 
          legend.names = c( "Violação do SH VaR 95%" , "Violação do SH-CONDEVT VaR 95%"), 
          pch=c(3,1),
          col="black")



#####



# 11) Mapa de calor risco retorno ######


mapa_de_calor_risco_retorno <- data.frame(media_rw = 0, des_pad_rw =0, VaR_95_rw = 0, ES_95_rw = 0 , loss = 0, id = 'modelo')


# 11.1) gera o data.frame utilizado para o scatter-plot com mapa de calor retorno-risco rolling window 

#names(resultados_VaR_ES_para_todos_anos_e_estrategias)[c(-1,-11,-14)]

for (id_estrategia in names(resultados_VaR_ES_para_todos_anos_e_estrategias)[c(-11,-14,-21)]){
  print(id_estrategia)
  indices_do_var_estimado <- resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]]$resultado_estimacao_media$`VaR e ES estimados`$`SH-CONDEVT`[,1]
  
  testando1 <- resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]]$resultado_estimacao_media$media_rw 
  
  des_pad <- resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]]$resultado_estimacao_media$des_pad_rw
  
  testando2 <- xts(resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]]$resultado_estimacao_media$`VaR e ES estimados`$`SH-CONDEVT`[,c(2,3)],
                   indices_do_var_estimado)
  
  actual <- xts(resultados_VaR_ES_para_todos_anos_e_estrategias[[id_estrategia]]$resultado_estimacao_media$`VaR e ES estimados`$`SH-CONDEVT`[,6],
                indices_do_var_estimado)
  
  
  testando3 <- cbind(testando1,des_pad,testando2)
  testando3 <- cbind(testando3,actual)
  testando3 <- testando3[complete.cases(testando3),]
  testando3 <- data.frame(media_rw = testando3$testando1,
                          des_pad_rw = testando3$des_pad,
                          VaR_95_rw = testando3$VaR_95,
                          ES_95_rw = testando3$ES_95,
                          loss = testando3$actual,
                          id = id_estrategia )
  
  
  #plot(as.numeric(testando3$testando1),as.numeric(testando3$testando2))
  
  colnames(testando3) <- c('media_rw','des_pad_rw','VaR_95_rw','ES_95_rw','loss','id')
  #testando3[,'id'] <- id_estrategia
  mapa_de_calor_risco_retorno <- rbind(mapa_de_calor_risco_retorno,testando3)
}

mapa_de_calor_risco_retorno <- mapa_de_calor_risco_retorno[-1,]


# 11.2) separando os n maiores valores por grupo 

violacoes <- mapa_de_calor_risco_retorno[which(mapa_de_calor_risco_retorno$loss>mapa_de_calor_risco_retorno$VaR_95_rw), ]

violacoes$diff_violacao <- violacoes$loss - violacoes$VaR_95_rw

df <- violacoes %>% 
  group_by(violacoes$id) %>% 
  arrange(desc(diff_violacao)) %>%
  slice(1:20)

df <- as.data.frame(df)


# 11.3) mapa de calor usando o metodo de estimação de densidade kernel ####

dc_todos <- data.frame()

dc_ES_todos <- data.frame()

dc_des_pad_todos <- data.frame()

for( id_estrategia in names(resultados_VaR_ES_para_todos_anos_e_estrategias)[c(-11,-14,-21)]){

  testando_kde <- mapa_de_calor_risco_retorno[which(mapa_de_calor_risco_retorno$id == id_estrategia),]
  testando_kde$media_rw <- ((1+testando_kde$media_rw)^252)-1
  testando_kde$des_pad_rw <- testando_kde$des_pad_rw*sqrt(252)
  
  # a) gerando o mapping das "probabilidades" para o VaR ######
  
  mv.kde <- kde2d(testando_kde$VaR_95_rw,testando_kde$media_rw, n = 500,
                                  h = c(MASS::width.SJ(testando_kde$VaR_95_rw, method = "dpi")*1.5,
                                        MASS::width.SJ(testando_kde$media_rw)*1.5))
  #
  dx <- diff(mv.kde$x[1:2])  # lifted from emdbook::HPDregionplot()
  dy <- diff(mv.kde$y[1:2])
  sz <- sort(mv.kde$z)
  c1 <- cumsum(sz) * dx * dy
  
  # specify desired contour levels:
  prob <- c(.99,0.95,0.90,0.5,0)
  
  # plot:
  dimnames(mv.kde$z) <- list(mv.kde$x,mv.kde$y)
  dc <- melt(mv.kde$z)
  dc$prob <- approx(sz,1-c1,dc$value)$y
  dc$id <- id_estrategia
  
  dc_todos <- rbind(dc_todos,dc)
  

  
  
  # b) gerando o mapping das "probabilidades" para o ES 
  
  mv.kde_ES <- kde2d(testando_kde$ES_95_rw,testando_kde$media_rw, n = 500,
                  h = c(MASS::width.SJ(testando_kde$ES_95_rw, method = "dpi")*1.5,
                        MASS::width.SJ(testando_kde$media_rw)*1.5))
  #
  dx_ES <- diff(mv.kde_ES$x[1:2])  # lifted from emdbook::HPDregionplot()
  dy_ES <- diff(mv.kde_ES$y[1:2])
  sz_ES <- sort(mv.kde_ES$z)
  c1_ES <- cumsum(sz_ES) * dx_ES * dy_ES
  
  # specify desired contour levels:
  prob <- c(.99,0.95,0.90,0.5,0)
  
  # plot:
  dimnames(mv.kde_ES$z) <- list(mv.kde_ES$x,mv.kde_ES$y)
  dc_ES <- melt(mv.kde_ES$z)
  dc_ES$prob <- approx(sz_ES,1-c1_ES,dc_ES$value)$y
  dc_ES$id <- id_estrategia
  
  dc_ES_todos <- rbind(dc_ES_todos,dc_ES)
  
  # c) gerando o mapping das "probabilidades" para o desvio padrao 
  mv.kde_des_pad <- kde2d(testando_kde$des_pad_rw,testando_kde$media_rw, n = 500,
                                  h = c(MASS::width.SJ(testando_kde$des_pad_rw, method = "dpi")*1.5,
                                        MASS::width.SJ(testando_kde$media_rw)*1.5))
  
  dx_des_pad <- diff(mv.kde_des_pad$x[1:2])  # lifted from emdbook::HPDregionplot()
  dy_des_pad <- diff(mv.kde_des_pad$y[1:2])
  sz_des_pad <- sort(mv.kde_des_pad$z)
  c1_des_pad <- cumsum(sz_des_pad) * dx_des_pad * dy_des_pad
  
  # specify desired contour levels:
  prob <- c(.99,0.95,0.90,0.5,0)
  
  # plot:
  dimnames(mv.kde_des_pad$z) <- list(mv.kde_des_pad$x,mv.kde_des_pad$y)
  dc_des_pad <- melt(mv.kde_des_pad$z)
  dc_des_pad$prob <- approx(sz_des_pad,1-c1_des_pad,dc_des_pad$value)$y
  dc_des_pad$id <- id_estrategia
  
  dc_des_pad_todos <- rbind(dc_des_pad_todos,dc_des_pad)
  
  ######
  
}

#####

# 11.4) scatter-plot retorno-desvio padrao #####

scatter_plot_des_pad <- ggplot(dc_des_pad_todos,aes(x=Var1,y=Var2))+
  geom_contour_filled(aes(z=prob),breaks=prob)+
  scale_fill_viridis_d(name = "% das observações") +
  #scale_fill_brewer(name = "Prob",palette = 'Spectral') +
  #scale_color_brewer(name = "Prob",palette = 'Spectral') +
  #geom_point(aes(x=VaR_95_rw,y=media_rw),data=testando_kde,alpha=0.1,size=1) +
  facet_wrap(~id) +
  #ggalt::geom_dumbbell(data = df,
  #                     aes(x = loss,y = ((1+media_rw)^252)-1 ,xend = VaR_95_rw ), 
  #                     size = .5, alpha = .5,shape = 4, show.legend = TRUE , colour_x = 'red',
  #                     colour_xend = 'blue') +
  theme_minimal()  +
  geom_abline(aes(slope = 1,intercept = 0,linetype = "Sharpe = 1")  ) + 
  geom_abline(aes(slope = 2,intercept = 0,linetype = "Sharpe = 2") ) + 
  scale_linetype_manual("Nível Sharpe", values = c("dashed","dotted"),
                        breaks = c("Sharpe = 1","Sharpe = 2")) +
  scale_shape_manual(values = c(17, 19)) +
  guides(colour = guide_legend(order = 1), 
         shape = guide_legend(order = 1),
         linetype = guide_legend(order = 2)) +

  labs(title="Heatmap dos retornos-risco (Média , Desvio Padrão) em Rolling Window",
       x = "Desvio Padrão Anualizado - rolling window (T = 1000)", y = "Média Movel do Retorno (T = 1000)") +
  theme( 
    panel.grid.major.x = element_line(colour = "grey50"),
    panel.grid.major.y = element_line(colour = "grey50"),
    panel.border = element_rect(linetype = 1, fill = NA) ,
    panel.background = element_rect(fill = 'white',colour = 'black'),
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #legend.title=element_blank(),
    legend.justification = "center", 
    legend.spacing = unit(0., "cm"),
    legend.spacing.y = unit(0.0, "cm"), 
    legend.margin = margin(0, 0, 0, 0), 
    legend.box.margin = margin(0, 0, 0, 0))

#####



# 11.5) scatterplot retorno-VaR 95 #####

scatter_plot_VaR <- ggplot(dc_todos,aes(x=Var1,y=Var2))+
  geom_contour_filled(aes(z=prob),breaks=prob)+
  scale_fill_viridis_d(name = "% das observações") +
  #scale_fill_brewer(name = "Prob",palette = 'Spectral') +
  #scale_color_brewer(name = "Prob",palette = 'Spectral') +
  #geom_point(aes(x=VaR_95_rw,y=media_rw),data=testando_kde,alpha=0.1,size=1) +
  facet_wrap(~id) +
  #ggalt::geom_dumbbell(data = df,
  #                     aes(x = loss,y = ((1+media_rw)^252)-1 ,xend = VaR_95_rw ), 
  #                     size = .5, alpha = .5,shape = 4, show.legend = TRUE , colour_x = 'red',
  #                     colour_xend = 'blue') +
  theme_minimal()  +
  xlim(0,.2) +
  labs(title="Heatmap dos retornos-risco (Média , VaR 95%) em Rolling Windows",
       x = "Value at Risk 95% - rolling window (T = 1000)", y = "Média Movel do Retorno (T = 1000)") +
  theme( 
    panel.grid.major.x = element_line(colour = "grey50"),
    panel.grid.major.y = element_line(colour = "grey50"),
    panel.border = element_rect(linetype = 1, fill = NA) ,
    panel.background = element_rect(fill = 'white',colour = 'black'),
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #legend.title=element_blank(),
    legend.justification = "center", 
    legend.spacing = unit(0., "cm"),
    legend.spacing.y = unit(0.0, "cm"), 
    legend.margin = margin(0, 0, 0, 0), 
    legend.box.margin = margin(0, 0, 0, 0))

#####


# 11.6) scatterplot retorno-ES 95 ######

scatter_plot_ES <- ggplot(dc_ES_todos,aes(x=Var1,y=Var2))+
  geom_contour_filled(aes(z=prob),breaks=prob)+
  scale_fill_viridis_d(name = "% das observações") +
  #scale_fill_brewer(name = "Prob",palette = 'Spectral') +
  #scale_color_brewer(name = "Prob",palette = 'Spectral') +
  #geom_point(aes(x=VaR_95_rw,y=media_rw),data=testando_kde,alpha=0.1,size=1) +
  facet_wrap(~id) +
  #ggalt::geom_dumbbell(data = df,
  #                     aes(x = loss,y = ((1+media_rw)^252)-1 ,xend = VaR_95_rw ), 
  #                     size = .5, alpha = .5,shape = 4, show.legend = TRUE , colour_x = 'red',
  #                     colour_xend = 'blue') +
  theme_minimal()  +
  xlim(0,.3) +
  labs(title="Heatmap dos retornos-risco (Média , ES 95%) em Rolling Windows",
       x = "Expected Shortfall 95% - rolling window (T = 1000)", y = "Média Movel do Retorno (T = 1000)") +
  theme( 
    panel.grid.major.x = element_line(colour = "grey50"),
    panel.grid.major.y = element_line(colour = "grey50"),
    panel.border = element_rect(linetype = 1, fill = NA) ,
    panel.background = element_rect(fill = 'white',colour = 'black'),
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #legend.title=element_blank(),
    legend.justification = "center", 
    legend.spacing = unit(0., "cm"),
    legend.spacing.y = unit(0.0, "cm"), 
    legend.margin = margin(0, 0, 0, 0), 
    legend.box.margin = margin(0, 0, 0, 0))

########

# pode demorar para abrir  (1~5 min.)
scatter_plot_des_pad 
scatter_plot_VaR
scatter_plot_ES

# 11.5) scatterplot papel_base ######
selecao <-  names(resultados_VaR_ES_para_todos_anos_e_estrategias)[c(-11,-14,-21)]
selecao <- "Papel_base"

ggplot(dc_todos[dc_todos$id == selecao,],aes(x=Var1,y=Var2))+
  geom_contour_filled(aes(z=prob,color=..level..),breaks=prob)+
  #geom_contour(aes(z=prob,color=id),breaks=prob[2])+
  #scale_fill_brewer(name = "Prob",palette = 'Spectral') +
  #scale_color_brewer(name = "Prob",palette = 'Spectral') +
  geom_point(aes(x=VaR_95_rw,y=((1+media_rw)^252)-1),
             data=mapa_de_calor_risco_retorno[which(mapa_de_calor_risco_retorno$id == selecao),],
             alpha=0.1,size=1) +
  #facet_wrap(~id) +
  ggalt::geom_dumbbell(data = df[df$id == selecao,],
                       aes(x = loss,y = ((1+media_rw)^252)-1 ,xend = VaR_95_rw ), 
                       size = .5, alpha = .5,shape = 4, show.legend = TRUE , colour_x = 'red',
                       colour_xend = 'blue') +
  theme_minimal()  +
  labs(title=paste0("Heatmap dos retornos-risco de ",selecao, "(Média , Desvio Padrão) em Rolling Window"),
       subtitle = "com as 20 maiores violações do VaR 95%.",
       caption = "x azul = VaR 95% previsto, x vermelho = loss realizado",
       x = "Value at Risk 95% - rolling window (T = 1000)", y = "Média Movel do Retorno (T = 1000)") +
  theme( 
    panel.grid.major.x = element_line(colour = "grey50"),
    panel.grid.major.y = element_line(colour = "grey50"),
    panel.border = element_rect(linetype = 1, fill = NA) ,
    panel.background = element_rect(fill = 'white',colour = 'black'),
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    legend.title=element_blank(),
    legend.justification = "center", 
    legend.spacing = unit(0., "cm"),
    legend.spacing.y = unit(0.0, "cm"), 
    legend.margin = margin(0, 0, 0, 0), 
    legend.box.margin = margin(0, 0, 0, 0))

ggplot(dc_des_pad_todos[dc_des_pad_todos$id == selecao,],aes(x=Var1,y=Var2))+
  geom_contour_filled(aes(z=prob),breaks=prob)+
  #scale_fill_brewer(name = "Prob",palette = 'Spectral') +
  #scale_color_brewer(name = "Prob",palette = 'Spectral') +
  geom_point(aes(x=des_pad_rw*sqrt(252),y=((1+media_rw)^252)-1),
             data=mapa_de_calor_risco_retorno[which(mapa_de_calor_risco_retorno$id == selecao),],
             alpha=0.1,size=1) +
  #facet_wrap(~id) +
  theme_minimal()  +
  geom_abline(aes(slope = 1,intercept = 0,linetype = "Sharpe = 1")  ) + 
  geom_abline(aes(slope = 2,intercept = 0,linetype = "Sharpe = 2") ) + 
  scale_linetype_manual("Nível Sharpe", values = c("dashed","dotted"),
                        breaks = c("Sharpe = 1","Sharpe = 2")) +
  scale_shape_manual(values = c(17, 19)) +
  guides(colour = guide_legend(order = 1), 
         shape = guide_legend(order = 1),
         linetype = guide_legend(order = 2)) +
  labs(title=paste0("Heatmap dos retornos-risco de ",selecao, "(Média , Desvio Padrão) em Rolling Window"),
       subtitle = "com as 20 maiores violações do VaR 95%.",
       #caption = "x azul = VaR 95% previsto, x vermelho = loss realizado",
       x = "Desvio Padrão Anualizado - rolling window (T = 1000)", y = "Média Movel do Retorno (T = 1000)") +
  theme( 
    panel.grid.major.x = element_line(colour = "grey50"),
    panel.grid.major.y = element_line(colour = "grey50"),
    panel.border = element_rect(linetype = 1, fill = NA) ,
    panel.background = element_rect(fill = 'white',colour = 'black'),
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    legend.title=element_blank(),
    legend.justification = "center", 
    legend.spacing = unit(0., "cm"),
    legend.spacing.y = unit(0.0, "cm"), 
    legend.margin = margin(0, 0, 0, 0), 
    legend.box.margin = margin(0, 0, 0, 0))

######
