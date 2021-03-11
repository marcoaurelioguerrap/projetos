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

A ídeia para esse projeto era de criar uma ferramenta onde eu pudesse testar estratégias de ações e opções na bolsa de valores. Não considero o projeto terminado, ainda quero fazer mudanças - adcionar mais ações, melhorar os modelos de precificação de opção, criar um teste no estilo alpha lens para os sinais gerados por cada estratégia, gerar mais estratégias, melhorar o Grid Search do GARCH, usar um GARCH multivariado, e criar uma etapa de simulações de cenários. Abaixo descrevo o que foi feito em cada etapa.

[1) Leitura dos dados disponibilizados pela B3](.//Lendo%20Arquivos%20da%20B3/): A primeira etapa dele é a coleta de dados  . Os dados são baixados diretamente do site da B3, onde para cada ano tem um arquivo com todas as operações cadastradas na bolsa de valores. No código [compila_dados_arquivos_B3.R](Lendo%20Arquivos%20da%20B3/compila_dados_arquivos_B3.R) eu defino algumas empresas do índice bovespa que negociam opções para separar os dados de ações e opções negociados por dia.

[2) Formatação dos dados para uso](.//Formatando%20os%20Dados/):  Uma vez coletados os dados eu uso o arquivo ['carrega_acaoes_opcoes.R'](./Formatando%20os%20Dados/carrega_acaoes_opcoes.R) e ['le div e split.R'](./Formatando%20os%20Dados/le%20div%20e%20split.R) para formatar e calcular o preço ajustado de uma ação. Embora separei diversas ações, no presente estado do trabalho apenas utilizo os preços da ação da PETR4 e suas opções. Pretendo adcionar as outras ações.

[3) Grid search para o GARCH utilizado](.//Grid%20search%20Garch/): Nessa etapa eu faço um grid search para encontrar os melhores modelos GARCH para os preços das ações. O script ['selecionando_garch_para_cada_acao.R'](Grid%20search%20Garch/selecionando_garch_para_cada_acao.R) testa diversos modelos variando: 
> a ordem dos parametros arma(n,n) | n = {1,2,...,6} ;  a ordem dos parametros GARCH(n,n) | n = {1,2,...,6} ; distribuição do modelo , student (std), student com viés (sstd), distribuição generalizada de erros (ged), distribuição generalizada de erros com viés (sged) ; inclusão de média )

nos arquivos 'dados_modelos_acoes_\*_\*.RData da pasta [Finanças/Grid search Garch/](.//Grid%20search%20Garch/) têm os resultados encontrados para 13 ações. Embora eu utilize o GARCH como um modelo de previsão rolling windows no trabalho, o grid search foi feito sob toda a amostra. Na pasta inclui um esboço para paralelização do Grid Search, pretendo implementar isso em algum momento.

[4) Preparação do Backtesting](.//Gerando%20Dados%20para%20o%20Backtesting/): As diferentes estratégias utilizam dados gerados pelo modelo GARCH e também depende de cálculos de preço para opções sem liquidez. Para tornar o projeto modular decidi separar essa etapa do backtesting em si. Isso foi feito por que essa etapa por si só é demorada ( 6 horas para rodar de 2004 a 2020 ) se fosse combinado os cálculos feitos no próprio backtesting ( demorado também ) esse tempo ia ser ainda maior. Tendo em vista isso, no código ['salva_os_dados_das_simulacoes_para_becktesting.R'](/Gerando%20Dados%20para%20o%20Backtesting/salva_os_dados_das_simulacoes_para_becktesting.R) gero as previsões do modelo GARCH para preço, volatilidade e probabilidade de exercício de uma opção, além dos preços para opções sem liquidez ([ALL_CALLS , ALL_PUTS , Zigmas_forcasted_todos e retornos_simulados](/Gerando%20Dados%20para%20o%20Backtesting/dados%20backtesting)). Um desafio encontrado no projeto foi de ajustar os preços (e os strikes) das opções isso foi feito criando uma função que ajustava o preço das ações, opções e simulações para tudo ficar coerente.  Como o foco principal do projeto não era a precisão dos modelos de precificação, não foquei muito nessa parte. Para os métodos de precificação eu não estimo a superfíce( e/ou o *'sorriso'* ) de volatilidade da ímplicita, apenas utilizo a voltalidade prevista do modelo. Isso têm implicação direta na presição dos modelos. Pretendo melhorar isso. Na pasta [4) Preparação do Backtesting](.//Gerando%20Dados%20para%20o%20Backtesting/) é possível visualizar os resultados econtrado para cada modelo utilizado.

[5) Backtesting](.//backtesting/): Aqui faço o teste de para 100 estratégias em um periodo de 2006-2010. São 1000 dias de trade para cada estratégia, não fiz mais tempo pois esse  cálculo demora em torno de 6 horas. Disponíbilizo o resultado de cada estratégia além de algumas medidas de risco. Também testei diferentes modelos para estimação do Value at Risk e Expected Shortfall não condicional. Aproveitei os resultados encontrados aqui para separar as estratégias para o backtesting em um tempo maior. Na ['backtesting/dados testes/'](/backtesting/dados%20testes) disponibilizo a informações dos testes. Nos arquivos 'historico_final_estrategia_\*_estrategia_\*.RData' é possível ver a posição dia a dia de cada estratégia , e nos arquivos logs_das_estrategias_final_estrategia_\*_estrategia_\*.RData é salvo todas as ordens e exercícios de opções de uma estratégia

[6) Backtesting com mais periodos](.//backtesting%20varios%20anos): Nesse teste eu separo 23 estratégias de melhor performance no periodo de 2006-2010 e faço o backtesting para varias janelas diferentes entre 2008-2020. Porém é feito duas avaliações diferentes, uma onde só avalio a performance janela a janela de cada opção ( ex.: 2008-2012, 2010-2014 , etc ) e outra onde eu agrupo as janelas tirando a média dos log retornos (são 6 colunas de retorno, eu faço a soma dos retornos por dia e divido pelo número de observações no dia. obs.: se fosse retornos percentuais sem log ia dar problema). Sei que isso não é a mesma coisa que testar uma estratégia para todo o periodo, mas é como se uma estratégia tivesse um aporte a cada dois anos. O motivo de fazer isso é que eu queria testar as medidas de erro condicionais( ou dinámica ) ( Value at Risk e Expected Shortfall - sei que em algumas literaturas o expected Shortfall  é chamado de Value at Risk condicional, porém sigo as nomeclaturas do livro ***Quantitive Risk Management - McNeil, Frey e Embrechts (2005)***). Os VaR e ES são calculados por 8 modelos diferentes. Para todas as estratégias realizo testes estátiscos em cada modelo do VaR e ES para verificar se os resultados são corretos ,i.e., número experado de violações do VaR são corretos ,e/ou independentes ( para o ES testo se a média das violações do VaR são de fato o valor do ES ). Esses resultados são disponibilizados para todas as estratégias nas pastas ( [backtesting varios anos/imagens/violacoes_var/](./backtesting%20varios%20anos/imagens/violacoes_var) e [./backtesting varios anos/imagens/testes_ES/](./backtesting%20varios%20anos/imagens/testes_ES).

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
