#Creating a neural network with tensorflow/keras for the iris data

import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from keras.models import Sequential
from keras.layers import Dense
import numpy as np



#Lecture des données. Affichage du nombre de variables et
#du nombre de modalités de CSpam càd
#le nombre de classes.
#De ces deux nombres nous déduisons: nbre de neurones de la couche d'entrée
#=nbVars et nb de neurones de la couche de sortie=1 car nous avons deux classes.



myData=pd.read_csv('spamDataNum.csv', sep=',')

nbColumns = len(myData.columns)
nbVars = nbColumns-1
classes = myData['CSpam'].unique().tolist()
nbClasses = len(classes)

print('Nombre de variables explicatives:',nbVars)
print('Nombre de classes:',nbClasses)

#Création des ensembles d'apprentissage et de test.
X=myData.values[:,:nbColumns-1]
Y=myData.values[:,nbColumns-1]


X_train, X_test, Y_train, Y_test = train_test_split( X, Y, test_size = 0.3, random_state = 100)

#Création du RN.

nn = Sequential()
nn.add(Dense(5, input_dim=nbVars, activation='sigmoid'))
nn.add(Dense(1, activation='sigmoid'))

#Affichage des caractéristiques du RN. Ondoit vérifier que pour une couche
#cachée ou de sortie on a:
# nb paramètres = nb de neurones de la couche*(nb de neurones de la couche précédente+1)
nn.summary()

#On complète la définition du RN. La fonction de perte=binary_crossentropy, car
#pb de classification binaire. Métrique=accuracy.
nn.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])


#Entrainement du RN. On peut faire varier le nombre d'itérations epochs
#et constater l'effet sur la performance du RN.

nn.fit(X_train, Y_train, epochs=50, batch_size=10)

#Test du RN

score = nn.evaluate(X_test, Y_test, verbose=0)
print('Test accuracy:', score[1])

#Construction de la matrice de confusion. Pour ce calcul, il faut se souvenir
#du fait que la sortie est une proba.
Y_pred = nn.predict(X_test)
l_Y_pred_1 = [(lambda x: int(x>0.5))(x) for x in list(Y_pred)]
Y_pred_1 = np.array(l_Y_pred_1)
confusion = confusion_matrix(Y_pred_1, Y_test)
print('Matrice de confusion:', confusion)


