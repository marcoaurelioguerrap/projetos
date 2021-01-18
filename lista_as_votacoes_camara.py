from requests import get
from bs4 import BeautifulSoup
import csv

# Lista todas a votações da camara

dados = [['codProp;nomeProp;dataVotacao']]
anos = range(2001,2020)

url =  'http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarProposicoesVotadasEmPlenario?ano='
for ano in anos:
    
    response = get(url+str(ano)+'&tipo=')
    xml_soup = BeautifulSoup(response.text, 'xml')
    proposicoes = xml_soup.find('proposicoes').find_all('proposicao')
    
    for i in range(len(proposicoes)):

        dados.append([xml_soup.find('proposicoes').find_all('proposicao')[i].text.replace("\n",";")[1:-1]])
        

with open('proposicoes.csv', "w", encoding="utf-8") as output:
        writer = csv.writer(output, lineterminator='\n')
        for dado in dados:
            writer.writerow(dado)
