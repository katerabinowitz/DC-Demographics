import pandas as pd
import numpy as np
from bs4 import BeautifulSoup
import requests 


cluster_id=range(1,40)

popCh10=[]
kidCh10=[]
bl00=[]
bl10=[]
wh00=[]
wh10=[]
hs00=[]
hs10=[]
as00=[]
as10=[]

poverty00=[]
poverty08_12=[]
unemployment00=[]
unemployment08_12=[]
employed00=[]
employed08_12=[]
avgFamIncome00=[]
avgFamIncome08_12=[]
violentCrime00=[]
violentCrime10=[]
propCrime00=[]
propCrime10=[]

homeOwnership00=[]
homeOwnership08_12=[]

medianHomePrice00=[]
medianHomePrice10=[]
medianHomePrice13=[]


for num in cluster_id:
    r = requests.get('http://neighborhoodinfodc.org/nclusters/Nbr_prof_clus' + str(num) + '.html')
    b = BeautifulSoup(r.text)
    popCh10.append(b('table')[2].find_all('tr')[9].find_all('td')[1].text)  
    kidCh10.append(b('table')[2].find_all('tr')[17].find_all('td')[1].text)   
    bl00.append(b('table')[2].find_all('tr')[28].find_all('td')[1].text)
    bl10.append(b('table')[2].find_all('tr')[29].find_all('td')[1].text)
    wh00.append(b('table')[2].find_all('tr')[31].find_all('td')[1].text)
    wh10.append(b('table')[2].find_all('tr')[32].find_all('td')[1].text)
    hs00.append(b('table')[2].find_all('tr')[34].find_all('td')[1].text)
    hs10.append(b('table')[2].find_all('tr')[35].find_all('td')[1].text)
    as00.append(b('table')[2].find_all('tr')[37].find_all('td')[1].text)
    as10.append(b('table')[2].find_all('tr')[38].find_all('td')[1].text)
    
    r = requests.get('http://neighborhoodinfodc.org/nclusters/Nbr_prof_clusb' + str(num) + '.html')
    b = BeautifulSoup(r.text)
    poverty00.append(b('table')[2].find_all('tr')[5].find_all('td')[1].text)
    poverty08_12.append(b('table')[2].find_all('tr')[6].find_all('td')[1].text)
    unemployment00.append(b('table')[2].find_all('tr')[16].find_all('td')[1].text)
    unemployment08_12.append(b('table')[2].find_all('tr')[17].find_all('td')[1].text)
    employed00.append(b('table')[2].find_all('tr')[20].find_all('td')[1].text)
    employed08_12.append(b('table')[2].find_all('tr')[21].find_all('td')[1].text)
    avgFamIncome00.append(b('table')[2].find_all('tr')[35].find_all('td')[1].text)
    avgFamIncome08_12.append(b('table')[2].find_all('tr')[36].find_all('td')[1].text)
    violentCrime00.append(b('table')[2].find_all('tr')[73].find_all('td')[1].text)
    violentCrime10.append(b('table')[2].find_all('tr')[83].find_all('td')[1].text)
    propCrime00.append(b('table')[2].find_all('tr')[89].find_all('td')[1].text)
    propCrime10.append(b('table')[2].find_all('tr')[96].find_all('td')[1].text)
    
    r = requests.get('http://neighborhoodinfodc.org/nclusters/Nbr_prof_clusc' + str(num) + '.html')
    b = BeautifulSoup(r.text)
    medianHomePrice00.append(b('table')[2].find_all('tr')[46].find_all('td')[1].text)
    medianHomePrice10.append(b('table')[2].find_all('tr')[56].find_all('td')[1].text)
    medianHomePrice13.append(b('table')[2].find_all('tr')[59].find_all('td')[1].text)
    
neighborhood_data = pd.DataFrame({id:cluster_id, 'popCh10':popCh10, 'kidCh10':kidCh10, 'bl00':bl00, 'bl10':bl10, 'wh00':wh00, 'wh10':wh10, 'hs00':hs00, 'hs10':hs10,
								'as00':as00, 'as10':as10, 'poverty00':poverty00, 'poverty0812':poverty08_12, 'unemp00':unemployment00,
								'unemp0812':unemployment08_12, 'emp00':employed00, 'emp0812':employed08_12, 'income00':avgFamIncome00,
								'income0812':avgFamIncome08_12, 'vCrime00':violentCrime00, 'vCrime10':violentCrime10, 'pCrime00':propCrime00,
								'pCrime10':propCrime10, 'home00':medianHomePrice00, 'home10':medianHomePrice10, 'home13':medianHomePrice13})

neighborhood_data.to_csv('neighborhooddata.csv')

