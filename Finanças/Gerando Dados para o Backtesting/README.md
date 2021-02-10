**Info**

"salva_os_dados_das_simulacoes_para_backtesting.R" - Esse código é utilizado para gerar os dados que serão utilizado no backtesting das estratégias. Ele consiste me criar um objeto
chamado de ALL_CALLS e ALL_PUTS que é utilizado pelo "backtesting.R" e "backtesting_varios_anos.R". Também é gerado os retornos e os sigmas projetados pelo GARCH por horizonte 
( ou seja, são n dias projetados por dia ). Também é salvo se o GARCH convergiu ou não.

-GARCH, o modelo GARCH utilizado é 

-ALL_CALLS e ALL_PUTS contém dados de todas as opções negociadas entre 2006 e 2020. Atualmente o código só gera essas informações para o ativo PETR4. A ideia é continuar aprimorando
o código para gerar essas mesmas informações para outras ações ( parte do código levou isso em conta , em alguns momentos eu queria a solução rapida e simplesmente usava a solução não generalizavel )
Os dados salvos dessas opções são referente aos preços inicial, preço simulado por 7 metodos diferentes , dias para o vencimento, premio em relação ao strike, quantos dias que não 
ocorre trade com o papel, as letras gregas e o moneyness ( porcentagem de diferença do preço da ação ) da opção.

OBS.: Ele utiliza os dados gerados pelos scripts :
> "compila_dados_arquivos_B3.R", "le div e split.R" 

Esses dados estão nos salvos nos seguintes arquivos
> "lista_das_acoes.RData" , "lista_dos_CALL.RData", "lista_dos_PUT.RData" , "preco.acoes.nivel.RData"

Embora os dois primeiros RData's não estão nesse repositório os dois ultimos estão. o "compila_dados_arquivos_B3.R" demora próximo de 4 horas para gerar as "listas".

