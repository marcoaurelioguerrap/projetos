**Info**

"salva_os_dados_das_simulacoes_para_backtesting.R" - Esse código gera os dados que serão utilizados no backtesting das estratégias. O output são os objetos : ALL_CALLS , ALL_PUTS , Zigmas_forcasted_todos e retornos_simulados. Esses objetos são utilizado pelo "backtesting.R" e "backtesting_varios_anos.R". Também é salvo se o GARCH convergiu ou não.

OBS.: demora certa de 6 horas para gerar esses dados. Caso se deseja reduzir o tamanho do periodo em que se gera os dados, na linha 126 do código é possivel escolher os dias que serão gerados as informções.

-GARCH, o modelo utilizado é definido no "funcoes_para_os_testes.R" na função gerador_das_simulacoes(). O modelo utilizado é o gjr GARCH que leva em conta choques assimétricos. Os parametros selecionados para o modelo foram: arma(2,2), GARCH(3,3) , média incluída e utilizando t-student como a distribuição dos choques. A seleção do modelo foi através de um grid search realizado nos hiperparametros ( ordem armar, ordem do garch, média inclusa ou não, distribuição utilizada ). Foi considerado todo o periodo de observação dos logs retornos dos preços ( no script "selecionando_garch_para_cada_acao.R" faço isso para 13 ações , no entanto só utilizo o resultado da PETR4 pois é o papel que estou estudando no momento. Uma melhora que estou trabalhando é de utilizar o MGARCH, que é uma versão multivariavel do GARCH.

-ALL_CALLS e ALL_PUTS contém dados de todas as opções negociadas entre 2006 e 2020. Atualmente o código só gera essas informações para o ativo PETR4. A ideia é continuar aprimorando o código para gerar essas mesmas informações para outras ações ( parte do código levou isso em conta , mas em alguns momentos eu queria a solução rapida e simplesmente usava a solução não generalizavel ). Os dados salvos dessas opções são referente aos : ultimo preço observado, preço simulado por 7 metodos diferentes , dias para o vencimento, premio em relação ao strike, quantos dias que não ocorre trade com o papel, as letras gregas e o moneyness ( porcentagem de diferença do preço da ação ) da opção. Na pasta possuem 3 arquivos de ALL_CALLS e ALL_PUTS cada, isso se deu devido a um erro que ocorreu por dois dias onde existia informação sobre as opções mas não da ação. como o tempo do execução do loop inteiro é de aproximadamente 6 horas , para evitar de rodar tudo de novo apenas dividi os arquivos. 

As imagens a seguir mostram os resultados dos preços projetados por cada modelo. Vale ressaltar que embora existam 626.716 e 496.073 observações no ALL_CALLS e ALL_PUTS, respectivamentes, apenas foram levados em conta na analise abaixo opções com 120 dias para vencimento ou menos, moneyness menor que 2. Além disso, os dados ALL_CALLS e ALL_PUTS geram dados para opções sem liquidez, ou seja, uma CALL que teve trade em um dia mas não foi mais negociada tem os preços simulados pelos 7 modelos diferentes e isso fica salvo nesses objetos. 

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/scatter_plot_call.png?raw=true)


![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/scatter_plot_put.png?raw=true)


![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/misspricing_call.png?raw=true)


![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/misspricing_put.png?raw=true)


![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/misspricing_contagem_em_log_call.png?raw=true)


![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/misspricing_contagem_em_log_put.png?raw=true)

OBS.: Ele utiliza os dados gerados pelos scripts :
> "compila_dados_arquivos_B3.R", "le div e split.R" 

Esses dados estão nos salvos nos seguintes arquivos
> "lista_das_acoes.RData" , "lista_dos_CALL.RData", "lista_dos_PUT.RData" , "preco.acoes.nivel.RData"

Embora os dois primeiros RData's não estão nesse repositório os dois ultimos estão. o "compila_dados_arquivos_B3.R" demora próximo de 4 horas para gerar as "listas".

