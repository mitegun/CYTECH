import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier, plot_tree, export_text
from sklearn.metrics import confusion_matrix, accuracy_score
import matplotlib.pyplot as plt

# Charger le dataset
a = pd.read_csv('4-Indicateurs techniques/nasdaq.csv', usecols=[1, 2, 3, 4, 5, 6])
a['Date'] = pd.to_datetime(a['Date'], format='%d/%m/%y')
closing_prices = a['Close'].values
N = 10

# Fonction momentum
def momentum(tab, N=10):
    mom_tab = [0] * N  # Initial momentum values are 0 for the first N elements
    for t in range(N, len(tab)):
        mom = tab[t] - tab[t-N]
        mom_tab.append(mom)
    return mom_tab

a['Momentum'] = pd.Series(momentum(closing_prices))
pos = (a['Momentum'] > 0).sum()
neg = (a['Momentum'] < 0).sum()

print(pos)
print(neg)

positive_momentum_mean = a[a['Momentum'] > 0]['Momentum'].mean()
negative_momentum_mean = a[a['Momentum'] < 0]['Momentum'].mean()

print("Moyenne des momentums positifs:", positive_momentum_mean)
print("Moyenne des momentums négatifs:", negative_momentum_mean)

# Calcul des signaux d'achat et de vente
a['Signal'] = 0  # Initialiser la colonne des signaux à 0

for i in range(1, len(a)):
    if a['Momentum'].iloc[i-1] > positive_momentum_mean / 3 and a['Momentum'].iloc[i] < negative_momentum_mean / 3:
        a.loc[i, 'Signal'] = -1  # Signal de vente
    elif a['Momentum'].iloc[i-1] < negative_momentum_mean / 3 and a['Momentum'].iloc[i] > positive_momentum_mean / 3:
        a.loc[i, 'Signal'] = 1  # Signal d'achat

# Plot
plt.figure(figsize=(14, 7))
plt.plot(a['Date'], a['Momentum'], label='Momentum')

# Ajouter les signaux d'achat et de vente
buy_signals = a[a['Signal'] == 1]
sell_signals = a[a['Signal'] == -1]

plt.scatter(buy_signals['Date'], buy_signals['Close'], marker='^', color='r', label='Sell Signal', alpha=1)
plt.scatter(sell_signals['Date'], sell_signals['Close'], marker='v', color='g', label='Buy Signal', alpha=1)
plt.plot(a['Date'], a['Close'], label='Indice', color='blue')

plt.legend()
plt.show()

# Filtrer les données pour les périodes spécifiées deux divergences
period1 = a[(a['Date'] >= '2000-05-30') & (a['Date'] <= '2000-09-11')]
period2 = a[(a['Date'] >= '2001-02-16') & (a['Date'] <= '2001-03-29')]

fig, axs = plt.subplots(2, 1, figsize=(14, 10))

# Premier subplot pour la première période
axs[0].plot(period1['Date'], period1['Momentum'], label='Momentum', color='orange')
axs[0].plot(period1['Date'], period1['Close'], label='Close', color='blue')
axs[0].set_title('Momentum et Close du 30/05/2000 au 11/09/2000')
axs[0].legend()

# Deuxième subplot pour la deuxième période
axs[1].plot(period2['Date'], period2['Momentum'], label='Momentum', color='orange')
axs[1].plot(period2['Date'], period2['Close'], label='Close', color='blue')
axs[1].set_title('Momentum et Close du 16/02/2001 au 29/03/2001')
axs[1].legend()

plt.tight_layout()
plt.show()