import pandas as pn
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from keras.models import Sequential
from keras.layers import Dense
import numpy as np

#1. Préparation des données

#Dans le cas d'un problème de multiclassification à m classes, on transforme
#Les modalités de la classe en m-uplets ayant une seule valeur 1 et toutes
#les autres valeurs à 0.
def encodeClass(s_class):
    if (s_class=='Iris-setosa'):
        return [1,0,0]
    elif (s_class=='Iris-versicolor'):
        return [0,1,0]
    else:
        return [0,0,1]
		
#Lecture des données
#myData=pn.read_csv('iris.csv', sep=',')
myData=pn.read_csv('iris.csv', sep=',')

#Extraction de quelques caractéristiques de l'ensemble de données
nbColumns = len(myData.columns)
classes = myData['species'].unique().tolist()
nbClasses = len(classes)

X=myData.values[:,:nbColumns-1]
X=X.astype('float64')
Y=myData.values[:,nbColumns-1]

#Encodage des classes
encoded_Y = np.array([encodeClass(y) for y in list(Y)])

#Création des ensembles d'apprentissage et de test
X_train, X_test, Y_train, Y_test = train_test_split( X, encoded_Y, test_size = 0.3, random_state = 100)

#2. Création du RNA
#Création d'un réseau 'vierge': Utilisation de la fonction Sequential(). Cette fonction
#se trouve dans keras
nn = Sequential()

#Ajout des couches une par une. Le module keras met à notre disposition une
#fonction pour chaque type de couche. Ici, nous utilisons exclusivement
#la fonction dense correspondant aux couches du RN stanadrd.
#Pour ajouter une couche on précise son nombre de neurones et sa fonction
#d'activation, et pour la première couche cachée le nombre de ses entrées
#(càd le nombre de neurones de la couche d'entrée).

nn.add(Dense(5, input_dim=nbColumns-1, activation='sigmoid'))
nn.add(Dense(nbClasses, activation='softmax'))

#Affichage des caractéristiques du RNA, notamment le nombre de paramètres
#(poids et biais) de chaque couche.
nn.summary()

#Compléter les caractéristiques du RNA, notamment la loss function (ici 
#la categorical_crossentropy car c'est un pb de multiclassification) et la
#métriques utilisée pour mesurer sa performance (ici l'accuracy).
nn.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])

#3. Entrainement du RN. En plus de l'ensemble d'apprentissage on précise le
#nombre d'itérations (epochs) et le nombre d'exemples utilisés pour chaque
#étape de l'apprentissage.

nn.fit(X_train, Y_train, epochs=200, batch_size=10)

#4. Test du RN
#Calcul et affichage de l'accuracy

score = nn.evaluate(X_test, Y_test, verbose=0)
print('Test accuracy:', score[1])
#Pour plus d'information, on construit la matrice de confusion.
Y_pred = nn.predict(X_test)
Y_pred_1 = Y_pred.argmax(axis=1)
Y_test_1 = Y_test.argmax(axis=1)
confusion = confusion_matrix(Y_pred_1, Y_test_1)
print(confusion) 

