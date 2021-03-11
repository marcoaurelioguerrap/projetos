# Índice

[1) Leitura dos dados disponibilizados pela B3](.//Lendo%20Arquivos%20da%20B3/)

[2) Formatação dos dados para uso](.//Formatando%20os%20Dados/)

[3) Grid search para o GARCH utilizado](.//Grid%20search%20Garch/)

[4) Preparação do Backtesting](.//Gerando%20Dados%20para%20o%20Backtesting/)

[4.1) Observações](.//Gerando%20Dados%20para%20o%20Backtesting#observação-sobre-os-preços-simulados);
[4.2) Modelos de precificação de opção](.//Gerando%20Dados%20para%20o%20Backtesting#modelos-para-precificação-utilizados);
[4.3) Resultados dos Modelos de precificação de opção](.//Gerando%20Dados%20para%20o%20Backtesting/README.md#resultados).

[5) Backtesting](.//backtesting/)

[5.1) Resultados](.//backtesting#resultados);
[5.2) Indicadores](.//backtesting#indicadores);
[5.3) Value at Risk](.//backtesting#var);
[5.4) Expected Shortfall](.//backtesting#es);
[5.5) Performance e drawdown](.//backtesting#performance-e-drawdown).

[6) Backtesting com mais periodos](.//backtesting%20varios%20anos)

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

A ídeia para esse projeto era de criar uma ferramenta onde eu pudesse testar estratégias de ações e opções na bolsa de valores. Não considero o projeto terminado, ainda quero fazer mudanças - adcionar mais ações, melhorar os modelos de precificação de opção, criar um teste no estilo alpha lens para os sinais gerados por cada estratégia, gerar mais estratégias, melhorar o Grid Search do GARCH, usar um GARCH multivariado, e criar uma etapa de simulações de cenário. Abaixo descrevo o que foi feito em cada etapa.

[1) Leitura dos dados disponibilizados pela B3](.//Lendo%20Arquivos%20da%20B3/): A primeira etapa dele é a coleta de dados  . Os dados são baixados diretamente do site da B3, onde para cada ano tem um arquivo com todas as operações cadastradas na bolsa de valores. No código [compila_dados_arquivos_B3.R](Lendo%20Arquivos%20da%20B3/compila_dados_arquivos_B3.R) eu defino algumas empresas do índice bovespa que negociam opções para separar os dados de ações e opções negociados por dia.

[2) Formatação dos dados para uso](.//Formatando%20os%20Dados/):  Uma vez coletados os dados eu uso o arquivo ['carrega_acaoes_opcoes.R'](./Formatando%20os%20Dados/carrega_acaoes_opcoes.R) e ['le div e split.R'](./Formatando%20os%20Dados/le%20div%20e%20split.R) para formatar e calcular o preço ajustado de uma ação. Embora separei diversas ações, no presente estado do trabalho apenas utilizo os preços da ação da PETR4 e suas opções. Pretendo adcionar as outras ações.

[3) Grid search para o GARCH utilizado](.//Grid%20search%20Garch/): Nessa etapa eu faço um grid search para encontrar os melhores modelos GARCH para os preços das ações. O script ['selecionando_garch_para_cada_acao.R'](Grid%20search%20Garch/selecionando_garch_para_cada_acao.R) testa diversos modelos variando: 
> a ordem dos parametros arma(n,n) | n = {1,2,...,6} ;  a ordem dos parametros GARCH(n,n) | n = {1,2,...,6} ; distribuição do modelo , student (std), student com viés (sstd), distribuição generalizada de erros (ged), distribuição generalizada de erros com viés (sged) ; inclusão de média )

nos arquivos 'dados_modelos_acoes_\*_\*.RData da pasta [Finanças/Grid search Garch/](.//Grid%20search%20Garch/) têm os resultados encontrados para 13 ações. Embora eu utilize o GARCH como um modelo de previsão rolling windows no trabalho, o grid search foi feito sob toda a amostra.




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
