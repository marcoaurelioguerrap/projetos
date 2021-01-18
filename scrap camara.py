from requests import get
from bs4 import BeautifulSoup
import csv
from datetime import datetime
import time
import re

# Lista todas a votações da camara

dados_governismo = [['Data','votos_a_favor_gov','id']]
dados_prop = []
errors = ['n','id','tipo_do_erro']
# le os dados da planilha com todas as votacoes da camara

with open('proposicoes.csv', newline='') as csvfile:
    reader = csv.DictReader(csvfile, delimiter = ';')
    for row in reader:
        dados_prop.append(row)

        
start_time = time.time()
for n in range(len(dados_prop)):
    print(n)
    start_time= time.time()
    # separa a coluna nomeProp em 'tipo', 'numero da votacao' e 'ano da votacao'.
    # Parametros necessários para pegar os dados do site.
    
    tipo_votacao = dados_prop[n]['nomeProp'].split()[0]
    num_votacao = dados_prop[n]['nomeProp'].split()[1].split('/')[0]
    ano_votacao = dados_prop[n]['nomeProp'].split()[1].split('/')[1]

    url = 'http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterVotacaoProposicao?'
    print('0',time.time()-start_time)
    # 1 input dos parametros para pegar no site
    response = None
    while response is None:
        try:
            # connect
            print(url+'tipo='+tipo_votacao+'&numero='+str(num_votacao)+'&ano='+str(ano_votacao))
            response = get(url+'tipo='+tipo_votacao+'&numero='+str(num_votacao)+'&ano='+str(ano_votacao))
        except:
            print('beyblade')
            pass
    # o while a cima nao existia, testando para ver se ele server pra quando da erro quando tenta ir pro site
    # response = get(url+'tipo='+tipo_votacao+'&numero='+str(num_votacao)+'&ano='+str(ano_votacao))
    xml_soup = BeautifulSoup(response.text, 'xml')
    
    print('1',time.time()-start_time)

    # 2 separa as quantidades de votos para cada categoria e a data da votacao
    
    votos_data = None
    
    try:
        votos_data = xml_soup.proposicao.Votacoes.Votacao.get('Data')
    except:
        errors.append([n,dados_prop[n]['nomeProp'],'Não obteve a data da votacao'])
        print('Não obteve a data da votacao')
        f = open('zuado_'+tipo_votacao+'_'+num_votacao+'_'+ano_votacao+'.txt','w')
        f.write(xml_soup.prettify())
        f.close
        continue
    
    votos_sim=0
    votos_nao=0
    votos_abs=0
    votos_obs=0
    
    for i in range(len(xml_soup.proposicao.Votacoes.Votacao.votos.find_all('Deputado'))):
        if xml_soup.proposicao.Votacoes.Votacao.votos.find_all('Deputado')[i].get('Voto')[0:3] =='Sim':
            votos_sim = votos_sim+1
        elif xml_soup.proposicao.Votacoes.Votacao.votos.find_all('Deputado')[i].get('Voto')[0:3] =='Não':
            votos_nao = votos_nao+1
        elif xml_soup.proposicao.Votacoes.Votacao.votos.find_all('Deputado')[i].get('Voto')[0:3] =='Abs':
            votos_abs = votos_abs+1
        elif xml_soup.proposicao.Votacoes.Votacao.votos.find_all('Deputado')[i].get('Voto')[0:3] =='Obs':
            votos_obs = votos_obs+1
    #print(time.clock()-st)
    print('2',time.time()-start_time)

    votos_total = votos_sim + votos_nao + votos_abs + votos_obs

    try:
        #teste = BeautifulSoup(str(xml_soup.proposicao.Votacoes.Votacao.orientacaoBancada.find_all(Sigla = re.compile(governo)))[1:-1], 'xml')
        teste = BeautifulSoup(str(xml_soup.proposicao.Votacoes.Votacao.orientacaoBancada.find_all(Sigla = re.compile("GOV.")))[1:-1], 'xml')
        
        orientacao_gov = teste.bancada.get('orientacao')[0:3]
    except:
        errors.append([n,dados_prop[n]['nomeProp'],'Não obteve a orientação do gov'])
        print('Não obteve a orientação do gov')
        f = open('sem_orientacao_'+tipo_votacao+'_'+num_votacao+'_'+ano_votacao+'.txt','w')
        f.write(xml_soup.prettify())
        f.close
        continue
    # 4 governismo?! mudar o nome?!
    try:
        if orientacao_gov =='Sim':
            grau_governismo = votos_sim / votos_total
        elif orientacao_gov =='Não':
            grau_governismo = votos_nao / votos_total
        elif orientacao_gov =='Abs':
            grau_governismo = votos_abs / votos_total
        elif orientacao_gov =='Obs':
            grau_governismo = votos_obs / votos_total
    except:
        errors.append([n,dados_prop[n]['nomeProp'],'erro na contagem dos votos'])
        print('erro na contagem dos votos')
        f = open('sem_orientacao_'+tipo_votacao+'_'+num_votacao+'_'+ano_votacao+'.txt','w')
        f.write(xml_soup.prettify())
        f.close
        continue
    if grau_governismo > 1:
        break
    dados_governismo.append([votos_data,grau_governismo,dados_prop[n]['nomeProp']])
    print('3',time.time()-start_time)

    # salva os dados baixados de cada link
    f = open('foi_'+tipo_votacao+'_'+num_votacao+'_'+ano_votacao+'.txt','w')
    f.write(xml_soup.prettify())
    f.close

    
with open('governismo.csv', "w", encoding="utf-8") as output:
        writer = csv.writer(output, lineterminator='\n')
        for row in dados_governismo:
            writer.writerow(row)


