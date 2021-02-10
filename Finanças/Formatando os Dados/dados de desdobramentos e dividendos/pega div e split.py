import json
from pandas import DataFrame, read_csv
import matplotlib.pyplot as plt
import pandas as pd 
import os
import csv
import yfinance as yf
import datetime

# Salva os dados de Dividendos e desdobramentos para as ações estudadas 

with open("acoes.estudadas.csv", newline = "") as csvfile:
    spamreader = csv.reader(csvfile, delimiter=',', quotechar='"')
    lista_acoes = list(spamreader)
#    for row in spamreader:
#        print(', '.join(row))
#       lista_acoes = lista_acoes.append(row)

lista_acoes = lista_acoes[1:]
dados = {}
# loop pra pegar os dividendos
for acao in lista_acoes:
    print(acao)
    ticker = yf.Ticker(acao[1] + ".SA")

    # escrevendo um dicionario para transportar os dados
    t = dict( {acao[1] : {}})
    t[acao[1]].update(json.loads(DataFrame(ticker.dividends).to_json(date_format = 'iso')))
    t[acao[1]].update(json.loads(DataFrame(ticker.splits).to_json(date_format = 'iso')))

    dados.update( t )
    
    #print(ticker.dividends)


# salvando a lista com os dados
with open('dados_split_div.txt', 'w') as outfile:
    json.dump(dados, outfile)


