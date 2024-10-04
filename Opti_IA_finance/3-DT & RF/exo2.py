import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix, accuracy_score
from sklearn.tree import  plot_tree, export_text

import matplotlib.pyplot as plt

# Lecture des données
a = pd.read_csv('H:/Desktop/CYTECH/ING3/Opti_IA_finance/3-DT & RF/GOOG.csv', usecols=[1, 2, 3, 4, 5, 6])
returns = np.array(a['Close'].pct_change()[1:])

# Indicateurs techniques

# Code des 4 indicateurs

# The following functions computes the moving average (will be applied to closing price).
def movingAverage(tab, N):
    ma_tab = []
    for t in range(N, len(tab) + 1):
        ma = np.mean(tab[t - N:t])
        ma_tab.append(ma)
    return ma_tab

# The following function computes the standard deviation (will be applied to closing price).
def standardDeviation(tab, N):
    sd_tab = []
    for t in range(N, len(tab) + 1):
        sd = np.std(tab[t - N:t])
        sd_tab.append(sd)
    return sd_tab

# The following function computes the stochastic oscillator (will be applied to closing price).
def stochasticOscillator(tab, N):
    so_tab = []
    for t in range(N, len(tab) + 1):
        High = np.amax(tab[t - N:t])
        Low = np.amin(tab[t - N:t])
        P = tab[t - 1]
        so = 100 * (P - Low) / (High - Low)
        so_tab.append(so)
    return so_tab

# The following function computes the relative strength index (will be applied to closing price).
def relativeStrengthIndex(tab, N):
    rsi_tab = []
    for t in range(N, len(tab) + 1):
        vals = tab[t - N:t]
        print(vals)
        Un = 0
        nbUn = 0
        Dn = 0
        nbDn = 0
        for i in range(N):
            dif = vals[i - 1] - vals[i]
            if dif > 0:
                Un += dif
                nbUn += 1
            else:
                Dn += abs(dif)
                nbDn += 1
        if nbDn == 0:
            rsi = 0.0
        elif nbUn == 0:
            rsi = 0.0
        else:
            Un /= nbUn
            Dn /= nbDn
            rsi = 100 * (1 - 1 / (1 + Un / Dn))
        rsi_tab.append(rsi)
    return rsi_tab

# Créer le DataFrame avec ces indicateurs
closing_prices = a['Close'].values

# Paramètres pour les indicateurs
N = 5

# Calculer les indicateurs
ma = movingAverage(closing_prices, N)
sd = standardDeviation(closing_prices, N)
so = stochasticOscillator(closing_prices, N)
rsi = relativeStrengthIndex(closing_prices, N)

# Créer un nouveau DataFrame avec les indicateurs
indicators_df = pd.DataFrame({
    'Moving Average': ma,
    'Standard Deviation': sd,
    'Stochastic Oscillator': so,
    'RSI': rsi
})

# Afficher les premières lignes du DataFrame
print(indicators_df)

# Définir une classe ayant les trois valeurs (Up, Down, Equal) ou les deux valeurs (Up, Down).
class Trend:
    UP = 1
    DOWN = -1
    EQUAL = 0

# Fonction pour déterminer la tendance basée sur les indicateurs
def determine_trend(returns, b1, b2):
    trends = []
    for i in range(1, len(returns)):
        if returns[i] > b2:
            trends.append(Trend.UP)
        elif returns[i] < b1:
            trends.append(Trend.DOWN)
        else:
            trends.append(Trend.EQUAL)
    return trends

b1 = np.quantile(returns, 0.45)
b2 = np.quantile(returns, 0.55)
# Déterminer la tendance pour les indicateurs
trends = determine_trend(returns, b1, b2)[N - 2:]

# Ajouter la tendance au DataFrame des indicateurs
indicators_df['Trend'] = [Trend.EQUAL] + trends

# Afficher les premières lignes du DataFrame avec la tendance
print(indicators_df.shape)

# Diviser les données en caractéristiques et cible
X = indicators_df[['Moving Average', 'Standard Deviation', 'Stochastic Oscillator', 'RSI']]
y = indicators_df['Trend']

# Diviser les données en ensembles d'entraînement et de test
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=100)

# Créer le classificateur Random Forest avec OOB scoring
rf_classifier = RandomForestClassifier(n_estimators=100, max_depth=4, random_state=100, oob_score=True)

# Entraîner le classificateur
rf_classifier.fit(X_train, y_train)

# Faire des prédictions sur l'ensemble de test
y_pred = rf_classifier.predict(X_test)

# Calculer la matrice de confusion et la précision
conf_matrix = confusion_matrix(y_test, y_pred)
accuracy = accuracy_score(y_test, y_pred)

# Calculer l'erreur OOB
oob_error = 1 - rf_classifier.oob_score_

# Afficher la matrice de confusion, la précision et l'erreur OOB
print("Matrice de Confusion:")
print(conf_matrix)
print("\nPrécision:")
print(accuracy)
print("\nErreur OOB:")
print(oob_error)

# Afficher les importances des caractéristiques
feature_importances = rf_classifier.feature_importances_
print("\nImportances des caractéristiques:")
for feature, importance in zip(X.columns, feature_importances):
    print(f"{feature}: {importance:.4f}")


plt.figure(figsize=(10, 6))
plt.barh(X.columns, feature_importances, color='skyblue')
plt.xlabel('Importance')
plt.ylabel('Feature')
plt.title('Feature Importances in Random Forest Classifier')
plt.show()

test_error = 1 - accuracy

print("\nErreur de test:")
print(test_error)

# OOB error vs Test error
errors = {'OOB Error': oob_error, 'Test Error': test_error}
plt.figure(figsize=(8, 5))
plt.bar(errors.keys(), errors.values(), color=['blue', 'green'])
plt.xlabel('Error Type')
plt.ylabel('Error Rate')
plt.title('OOB Error vs Test Error')
plt.show()

#Afficher un arbre
tree = rf_classifier.estimators_[0]


plot_tree(tree, feature_names=X.columns, class_names=['DOWN', 'EQUAL', 'UP'], filled=True)
plt.show()

# Afficher les règles de l'arbre de décision
tree_rules = export_text(tree, feature_names=list(X.columns))
print("\nRègles de l'Arbre de Décision:")
print(tree_rules)