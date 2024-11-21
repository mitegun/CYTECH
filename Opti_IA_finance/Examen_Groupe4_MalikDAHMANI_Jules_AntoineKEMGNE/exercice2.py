import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import classification_report, confusion_matrix
import matplotlib.pyplot as plt
import tensorflow as tf
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from sklearn.preprocessing import MinMaxScaler, StandardScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
from tensorflow.keras.optimizers import Adam
import matplotlib.pyplot as plt

# Charger le fichier housing.csv
file_path = 'housing.csv'  # Remplacez par le chemin vers votre fichier
housing_data = pd.read_csv(file_path)

#1. Tracez un histogramme résumant les prix médians.

# Tracer l'histogramme pour la variable MEDV
plt.figure(figsize=(10, 6))
plt.hist(housing_data['MEDV'], bins=30, color='blue', edgecolor='black')
plt.title('Distribution des prix médians des maisons (MEDV)', fontsize=16)
plt.xlabel('Prix médian ($1000s)', fontsize=12)
plt.ylabel('Fréquence', fontsize=12)
plt.grid(axis='y', alpha=0.75)
plt.show()




#2. Ajoutez aux données une colonne Class contenant la catégorie de prix de chaque maison. Vous expliquerez comment ces catégories sont définies.

bins = [0, 15, 30, 100]
labels = ['bas', 'moyen', 'haut']
housing_data['class'] = pd.cut(housing_data['MEDV'], bins=bins, labels=labels, right=False)
print(housing_data.head())

# Séparer les caractéristiques (features) et la cible (target)
X = housing_data.drop(['MEDV', 'class'], axis=1)
y = housing_data['class']





#3. Créez et entraînez un réseau de neurones prédisant la catégorie de prix.

# Convertir les labels en one-hot encoding
y = pd.get_dummies(y)

# Diviser les données en ensembles d'entraînement et de test
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Créer et entraîner le modèle de réseau de neurones avec les données non normalisées
model = Sequential()
model.add(Dense(100, input_dim=X_train.shape[1], activation='relu'))
model.add(Dense(50, activation='relu'))
model.add(Dense(y_train.shape[1], activation='softmax'))

model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
model.fit(X_train, y_train, epochs=100, batch_size=10, verbose=1)



#4. Donnez la précision et la matrice de confusion de ce réseau de neurones.

# Évaluer le modèle sur l'ensemble de test non normalisé
loss, accuracy = model.evaluate(X_test, y_test, verbose=0)
print(f"Précision sans normalisation: {accuracy}")

# Prédire les classes sur l'ensemble de test non normalisé
y_pred = model.predict(X_test)
y_pred_classes = y_pred.argmax(axis=1)
y_test_classes = y_test.values.argmax(axis=1)


# Afficher les résultats pour les données non normalisées
print(confusion_matrix(y_test_classes, y_pred_classes))

#5. Donnez le min, le max, la moyenne et l’écart type pour chaque variable explicative. Que remarquez-vous ?
#On remarque que les variables explicatives ont des échelles différentes, ce qui peut affecter la performance du modèle.
# Pour résoudre ce problème, on peut normaliser les données.



# Afficher les statistiques descriptives des caractéristiques
descriptive_stats = X.describe()
print(descriptive_stats)


#5.b Normalisez les données, entraînez à nouveau le réseau neuronal et donnez les nouveaux résultats.
# Normaliser les caractéristiques (features)
scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)

# Créer et entraîner le modèle de réseau de neurones avec les données normalisées
model = Sequential()
model.add(Dense(100, input_dim=X_train_scaled.shape[1], activation='relu'))
model.add(Dense(50, activation='relu'))
model.add(Dense(y_train.shape[1], activation='softmax'))

model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
model.fit(X_train_scaled, y_train, epochs=100, batch_size=10, verbose=1)

# Évaluer le modèle sur l'ensemble de test normalisé
loss, accuracy = model.evaluate(X_test_scaled, y_test, verbose=0)
print(f"Précision après normalisation: {accuracy}")

# Prédire les classes sur l'ensemble de test normalisé
y_pred_scaled = model.predict(X_test_scaled)
y_pred_classes = y_pred_scaled.argmax(axis=1)
y_test_classes = y_test.values.argmax(axis=1)

# Afficher les résultats pour les données normalisées
print(confusion_matrix(y_test_classes, y_pred_classes))
