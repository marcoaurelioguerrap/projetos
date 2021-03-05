# Formatando os Dados

**Info**

['carrega_acoes_opcoes.R'](carrega_acoes_opcoes.R) - Usado para ler os arquivos gerados pelo ['compila_dados_arquivos_B3.R'](./Lendo%20Arquivos%20da%20B3/compila_dados_arquivos_B3.R)  e formatar os dados : 
>"lista_das_acoes.RData" , "lista_dos_CALL.RData", "lista_dos_PUT.RData"  

['le div e split.R'](le div e split.R) - Pega os dados ['dados_split_div.txt'](.//dados%20de%20desdobramentos%20e%20dividendos/dados_split_div.txt) que contém as informações de desdobramentos e dividendos de algumas ações selecionadas ( por volume )
OBS.: Esses dados são retirados do yahoo finance e existe alguns erros neles. por isso é preciso corrigir alguns desdobramentos de acordo com o caso. No codigo é comentado quais são 
esses casos. A forma atual do código só executa a correção para o preço ajustado ( sem mudança no código ) para PETR4. Pois o meu primeiro estudo só foi feito utilizando uma ação. Em algum momento no futuro vou 
generalizar o código para fazer o ajuste de maneira automática para todas as ações da lista de ações estudadas.

['//dados de desdobramentos e dividendos/'](//dados%20de%20desdobramentos%20e%20dividendos/) - Pasta que contém os dados (['dados_split_div.txt'](.//dados%20de%20desdobramentos%20e%20dividendos/dados_split_div.txt)) e o código que gera os dados de desdobramentos ("pega div e split.py"). Além da lista 
com as ações estudadas ( ['acoes.estudadas.csv'](.//dados%20de%20desdobramentos%20e%20dividendos/acoes.estudadas.csv))
