**Info**

"salva_os_dados_das_simulacoes_para_backtesting.R" - Esse código é utilizado para gerar os dados que serão utilizado no backtesting das estratégias. Ele consiste me criar um objeto chamado de ALL_CALLS e ALL_PUTS que é utilizado pelo "backtesting.R" e "backtesting_varios_anos.R". Também é gerado os retornos e os sigmas projetados pelo GARCH por horizonte. 

( ou seja, são n dias projetados por dia ). Também é salvo se o GARCH convergiu ou não.
OBS.: demora certa de 6 horas para rodar o código todo. para reduzir o tamanho do periodo em que se gera os dados na linha 126 do código é possivel selecionar os dias que seja gerados as informções de CALL e PUT.

-GARCH, o modelo utilizado é definido no "funcoes_para_os_testes.R" na função gerador_das_simulacoes(). O modelo utilizado é gjr GARCH que leva em conta choques assimétricos com os seguintes parametros: arma(2,2), GARCH(3,3) , média incluída e utilizando t-student como a distribuição dos choques. A seleção do modelo foi através de um grid search realizado nos hiperparametros ( ordem armar, ordem do garch, média inclusa ou não, distribuição utilizada ). A amostra considerou todo o periodo de observação dos logs retornos dos preços ( no script "selecionando_garch_para_cada_acao.R" faço isso para 13 ações , no entanto só utilizo o resultado da PETR4 pois é o papel que estou estudando no momento as estratégias. Uma ideia para melhora é de quando eu for considerar outras ações para o teste de estratégias irei utilizar o MGARCH que é uma versão multivariavel do GARCH.

-ALL_CALLS e ALL_PUTS contém dados de todas as opções negociadas entre 2006 e 2020. Atualmente o código só gera essas informações para o ativo PETR4. A ideia é continuar aprimorando o código para gerar essas mesmas informações para outras ações ( parte do código levou isso em conta , mas em alguns momentos eu queria a solução rapida e simplesmente usava a solução não generalizavel ). Os dados salvos dessas opções são referente aos preços iniciais, preço simulado por 7 metodos diferentes , dias para o vencimento, premio em relação ao strike, quantos dias que não ocorre trade com o papel, as letras gregas e o moneyness ( porcentagem de diferença do preço da ação ) da opção. Na pasta possuem 3 arquivos de ALL_CALLS e ALL_PUTS cada, isso se deu devido a um erro que ocorreu por dois dias onde existia informação sobre as opções mas não da ação. como o tempo do execução do loop inteiro é de aproximadamente 6 horas , para evitar de rodar tudo de novo apenas dividi os arquivos. 

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

