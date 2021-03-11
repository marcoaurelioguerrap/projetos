# Índice

###### [1) Leitura dos dados disponibilizados pela B3](.//Lendo%20Arquivos%20da%20B3/)

###### [2) Formatação dos dados para uso](.//Formatando%20os%20Dados/)

###### [3) Grid search para o GARCH utilizado](.//Grid%20search%20Garch/)

###### [4) Preparação do Backtesting](.//Gerando%20Dados%20para%20o%20Backtesting/)

[4.1) Observações](.//Gerando%20Dados%20para%20o%20Backtesting#observação-sobre-os-preços-simulados);
[4.2) Modelos de precificação de opção](.//Gerando%20Dados%20para%20o%20Backtesting#modelos-para-precificação-utilizados);
[4.3) Resultados dos Modelos de precificação de opção](.//Gerando%20Dados%20para%20o%20Backtesting/README.md#resultados).

###### [5) Backtesting](.//backtesting/)

[5.1) Resultados](.//backtesting#resultados);
[5.2) Indicadores](.//backtesting#indicadores);
[5.3) Value at Risk](.//backtesting#var);
[5.4) Expected Shortfall](.//backtesting#es);
[5.5) Performance e drawdown](.//backtesting#performance-e-drawdown).

###### [6) Backtesting com mais periodos](.//backtesting%20varios%20anos)

[6.1) Resultados Janelas separadas](.//backtesting%20varios%20anos#resultados-janelas-separadas);
[6.1.1) Indicadores de Risco](.//backtesting%20varios%20anos#indicadores);
[6.1.2) Value at Risk](.//backtesting%20varios%20anos#value-at-risk);
[6.1.3) Expected Shortfall](.//backtesting%20varios%20anos#expected-shortfall);
[6.1.4) Performance e drawdown](.//backtesting%20varios%20anos#performance-e-drawdown).

[6.2) Resultados média das janelas](.//backtesting%20varios%20anos#resultados-média-das-janelas);
[6.2.1) Retorno-Risco](.//backtesting%20varios%20anos#retorno-risco);
[6.2.2) Value at Risk](.//backtesting%20varios%20anos#var);
[6.2.3) Expected Shortfall](.//backtesting%20varios%20anos#es);
[6.2.4) Performance e drawdown](.//backtesting%20varios%20anos#performance-e-drawdown-1).

# Resumo

A ídeia para esse projeto era de criar uma ferramenta onde eu pudesse testar estratégias de ações e opções na bolsa de valores. Na primeira etapa dele ( 


Para isso utilizo dados disponibilizados pela B3, um dos desafios desse trabalho foi o tratamento dos preços dos ativos, já que dividendos e desdobramentos alteram o valor de uma ação.


# Lista dos pacotes utilizados e versões
>"xts 0.11-2" ,
"tidyverse 1.2.1",
"BatchGetSymbols 2.5.2",
"rjson 0.2.20",
"ggplot2 3.3.3",
"fOptions 3042.86" ,
"rbcb 0.1.6",
"bizdays 1.0.6",
"qrmtools 0.0-10",
"rugarch 1.4-1" ,
"combinat 0.0-8",
"nvmix 0.0-3" 
"fBasics 3042.89.1" ,
"plyr 1.8.4" ,
"PerformanceAnalytics 2.0.4" ,
"knitr 1.23",
"kableExtra 1.3.4",
"gridExtra 2.3",
"grid 3.6.1",
"lattice 0.20-38",
"ggalt 0.4.0",
"ggrepel 0.9.1",
"dplyr 0.8.3",
"timeSeries 3042.102" ,
"MASS 7.3-51.4",
"reshape2 1.4.3"
