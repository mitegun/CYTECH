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
N = 5
a = a[(a['Date'] >= '2000-03-01') & (a['Date'] <= '2001-10-31')]

# Fonction pour calculer la moyenne mobile simple
def movingAverage(tab, N):
   ma_tab = []
   for t in range(N, len(tab) + 1):
      ma = np.mean(tab[t - N:t])
      ma_tab.append(ma)
   return ma_tab

# Fonction pour calculer la moyenne mobile exponentielle
def movingAverageExponential(tab, N):
   ema_tab = []
   alpha = 2 / (N + 1)
   ema = np.mean(tab[:N])
   ema_tab.append(ema)
   for t in range(N, len(tab)):
      ema = alpha * tab[t] + (1 - alpha) * ema
      ema_tab.append(ema)
   return ema_tab

# Fonction pour extraire les coefficients de l'EMA
def extractEMAcoefficients(tab, N):
   alpha = 2 / (N + 1)
   coefficients = [alpha * (1 - alpha) ** i for i in range(len(tab))]
   return coefficients

# Calculer la moyenne mobile simple
ma = movingAverage(closing_prices, N)
# Calculer la moyenne mobile exponentielle
ema = movingAverageExponential(closing_prices, N)
# Extraire les coefficients de l'EMA
coefficients = extractEMAcoefficients(closing_prices, N)
print("EMA Coefficients:", coefficients[:N])

# Tracer les courbes
plt.plot(closing_prices, label='Closing Prices')
plt.plot(ma, label='Simple Moving Average')
plt.plot(ema, label='Exponential Moving Average')
plt.legend()
plt.show()

coef_ema10 = extractEMAcoefficients(closing_prices, 10)
coef_ema20 = extractEMAcoefficients(closing_prices, 20)

# Tracer diagramme à barre sépare les barres qui se chevauchent
plt.figure(figsize=(14, 7))

# Définir la largeur des barres
bar_width = 0.35

# Positions des barres
r1 = np.arange(5)
r2 = [x + bar_width for x in r1]

# Tracer les barres
plt.bar(r1, coef_ema10[:5], width=bar_width, edgecolor='grey', label='EMA 10')
plt.bar(r2, coef_ema20[:5], width=bar_width, edgecolor='grey', label='EMA 20')

# Ajouter les labels et la légende
plt.xlabel('Index')
plt.ylabel('Coefficient Value')
plt.title('EMA Coefficients Comparison')
plt.xticks([r + bar_width / 2 for r in range(5)], range(5))
plt.legend()
plt.show()

# Fonction pour calculer le momentum
def momentum(tab):
   momentum_tab = [0]  # Initialiser le premier élément à 0
   for t in range(1, len(tab)):
      momentum = tab[t] - tab[t - 1]
      momentum_tab.append(momentum)
   return momentum_tab

# Calculer le momentum
momentum_values = momentum(closing_prices)
print("Momentum:", momentum_values[:5])


# Ajouter le momentum au DataFrame
a['Momentum'] = pd.Series(momentum_values, index=a.index)

# Tracer le momentum et afficher les signaux de vente et d'achat
plt.figure(figsize=(14, 7))
plt.plot(a['Date'], a['Momentum'], label='Momentum')

# Ajouter les signaux de vente et d'achat
buy_signals = a[a['Momentum'] > 0]
sell_signals = a[a['Momentum'] < 0]
plt.plot(buy_signals['Date'], buy_signals['Momentum'], 'g^', markersize=5, label='Buy Signal')
plt.plot(sell_signals['Date'], sell_signals['Momentum'], 'rv', markersize=5, label='Sell Signal')

# Ajouter les labels et la légende
plt.xlabel('Date')
plt.ylabel('Momentum Value')
plt.title('Momentum and Buy/Sell Signals')
handles, labels = plt.gca().get_legend_handles_labels()
by_label = dict(zip(labels, handles))
plt.legend(by_label.values(), by_label.keys())
plt.show()
