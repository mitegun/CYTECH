import numpy as np
import pandas as pd


#The functions we will need

#This function defines the class of a movement : incresaing, decreasing, or statying in the same level

def classifyMovements_2(tab, b):
   tab_ret = [] 
   for i in range(tab.size):
     if (tab[i]<=b):
       tab_ret.append(-1)
     else:
       tab_ret.append(1)
   return tab_ret
   
def classifyMovements_3(tab, b1, b2):
   tab_ret = [] 
   for i in range(tab.size):
     if (tab[i]<=b1):
       tab_ret.append(-1)
     elif (tab[i]>=b2):
       tab_ret.append(1)
     else:
       tab_ret.append(0)
   return tab_ret



#The following functions computes the moving average (will be applied to closing price).
def movingAverage(tab, N):
    ma_tab = []
    for t in range(N,len(tab)+1):
       ma = np.mean(tab[t-N:t])
       ma_tab.append(ma)
	   
    return ma_tab


#The following function computes the standard deviation (will be applied to closing price).
def standardDeviation(tab, N):
    sd_tab = []
    for t in range(N,len(tab)+1):
       sd = np.std(tab[t-N:t])
       sd_tab.append(sd)
	   
    return sd_tab

#The following function computes the stochastic oscillator (will be applied to closing price).
def stochasticOscillator(tab, N):
    so_tab = []
    for t in range(N,len(tab)+1):
       High = np.amax(tab[t-N:t])
       Low  = np.amin(tab[t-N:t])
       P = tab[t-1]
       so = 100*(P-Low)/(High-Low)
       so_tab.append(so)
	   
    return so_tab
	
#The following function computes the relative strength index (will be applied to closing price).

def relativeStrengthIndex(tab, N):
    rsi_tab = []
    for t in range(N,len(tab)+1):
      vals = tab[t-N:t]
      Un   = 0
      nbUn = 0
      Dn   = 0
      nbDn = 0
      for i in range(N-1):
        dif = vals[i+1]-vals[i]
        if (dif>0):
          Un +=dif
          nbUn +=1
        else:
          Dn +=abs(dif)
          nbDn +=1
      if (nbDn==0):
        rsi = 0.0
      elif (nbUn==0):
        rsi = 0.0 
      else:
        Un /=nbUn
        Dn /=nbDn
        rsi = 100*(1-1/(1+Un/Dn))
      rsi_tab.append(rsi)

    return rsi_tab


#First reading the data containing the columns Open, Close, High, Low, Volume

#df = pd.read_csv('apple.csv')
df = pd.read_csv('CAC40_old.csv')
prices = df['Close']
dates = df['Date'][:-1]

returns = np.array(prices.pct_change()[1:])
prices = np.array(prices)

b1 = np.quantile(returns, 0.3)
b2 = np.quantile(returns, 0.7)

b = np.median(returns)

N=5

#Adding column Class
#À la demande de l'utilisateur on crée 2 ou 3 classes :
#2 : baisse/hausse et 3 : baisse/hausse/stagnation

#tab_cls = classifyMovements_2(returns, b)[N-2:]
tab_cls = classifyMovements_3(returns, b1, b2)[N-2:]

lst_cls = list(tab_cls)


#Adding technical 

tab_ma = movingAverage(prices, N)
tab_sd = standardDeviation(prices, N)
tab_so = stochasticOscillator(prices, N)
tab_rsi = relativeStrengthIndex(prices, N)

#Creating the dataframe containing the data

myData = pd.DataFrame()
myData['MA'] = tab_ma
myData['SD'] = tab_sd
myData['SO'] = tab_so
myData['RSI'] = tab_rsi
myData['CLS'] = tab_cls

