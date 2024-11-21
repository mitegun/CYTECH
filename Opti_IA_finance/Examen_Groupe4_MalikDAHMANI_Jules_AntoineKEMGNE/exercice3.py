import pandas as pd
import numpy as np

# Fonction pour calculer la moyenne mobile
def movingAverage(tab, N):
    ma_tab = []
    for t in range(N, len(tab) + 1):
        ma = np.mean(tab[t - N:t])
        ma_tab.append(ma)
    return ma_tab

# Charger les données
data = pd.read_csv('goog.csv')

# Calculer deux moyennes mobiles avec des tailles de fenêtre différentes
window1 = 10
window2 = 12

ma1 = movingAverage(data['Close'].values, window1)
ma2 = movingAverage(data['Close'].values, window2)

# Ajouter les moyennes mobiles au dataframe
data['MA20'] = pd.Series(ma1, index=data.index[window1-1:])
data['MA50'] = pd.Series(ma2, index=data.index[window2-1:])

# Générer des signaux d'achat et de vente
data['Signal'] = 0
data['Signal'][window2:] = np.where(data['MA20'][window2:] > data['MA50'][window2:], 1, 0)
data['Position'] = data['Signal'].diff()

# Visualiser la stratégie
import matplotlib.pyplot as plt

plt.figure(figsize=(14, 7))
plt.plot(data['Close'], label='Prix de clôture', alpha=0.5)
plt.plot(data['MA20'], label='MA courte', alpha=0.75)
plt.plot(data['MA50'], label='MA longue', alpha=0.75)

# Tracer les signaux d'achat
plt.plot(data[data['Position'] == 1].index, 
            data['MA20'][data['Position'] == 1], 
            '^', markersize=10, color='g', lw=0, label='Signal d\'achat')

# Tracer les signaux de vente
plt.plot(data[data['Position'] == -1].index, 
            data['MA20'][data['Position'] == -1], 
            'v', markersize=10, color='r', lw=0, label='Signal de vente')

plt.title('Prix des actions et moyennes mobiles')
plt.legend()
plt.show()

# Calculer la performance de la stratégie
# Calculer les rendements quotidiens
data['Daily Return'] = data['Close'].pct_change()
data['Strategy Return'] = data['Daily Return'] * data['Position'].shift(1)

# Calculer les rendements cumulés
data['Cumulative Market Return'] = (1 + data['Daily Return']).cumprod()
data['Cumulative Strategy Return'] = (1 + data['Strategy Return']).cumprod()

# Tracer les rendements cumulés
plt.figure(figsize=(14, 7))
plt.plot(data['Cumulative Market Return'], label='Rendement du marché', alpha=0.75)
plt.plot(data['Cumulative Strategy Return'], label='Rendement de la stratégie', alpha=0.75)
plt.title('Rendements cumulés')
plt.legend()
plt.show()

