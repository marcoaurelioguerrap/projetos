Os codigos dessa pasta são usados para ler o site de dados aberto da câmara dos deputados ( https://dadosabertos.camara.leg.br/swagger/api.html )
e calcular o indice de apoio ao governo nas votações da casa (#Votos seguindo a orientação do governo / #Votos totais.

"lista_as_votacoes_camara.py" - cria uma lista com todas as votações na camara no periodo de 2001 a 2020.
e em seguida gera um arquivo ("proposicoes.csv"). A lista possui duplicatas mas isso não é um erro, o próprio
site da câmara que indica isso.

"scrap camara.py" - Usa os ID das votações guardadas no arquivo "proposicoes.csv" para buscar os votos por seção
para calcular os indice de apoio ao governo, salvo no arquivo "governismo.csv". Além disso, são salvos as votações
em um txt diferente ( ex.: "foi_MPV_11_2001.txt" , [foi usado ou não no calculo]_[tipo da votação]_[id do seção]_[ano].txt). 

"Grafico Governismo.py" - Cria os graficos com a média mensal e a média movél de 60 votações.

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/DataScraping/Governismo%20Camara/media_rolling.png?raw=true)

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/DataScraping/Governismo%20Camara/media_mensal.png?raw=true)
