#Creating a neural network with tensorflow/keras for the iris data

import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from tensorflow import keras
from keras.models import Sequential
from keras.layers import Dense
import numpy


#Encoding classes

def encodeClass(s_class):
    if (s_class=='Iris-setosa'):
        return [1,0,0]
    elif (s_class=='Iris-versicolor'):
        return [0,1,0]
    else:
        return [0,0,1]

#Reading the data and creating Training and Test sets

myData=pd.read_csv('H:/Desktop/CYTECH/ING3/OptiIAfinance/6-NN/nn_Iris.py', sep=',')

nbColumns = len(myData.columns)
classes = myData['species'].unique().tolist()
nbClasses = len(classes)

X=myData.values[:,:nbColumns-1]
Y=myData.values[:,nbColumns-1]

encoded_Y = numpy.array([encodeClass(y) for y in list(Y)])

X_train, X_test, Y_train, Y_test = train_test_split( X, encoded_Y, test_size = 0.3, random_state = 100)

#Creating the neural network

nn = Sequential()
nn.add(Dense(5, input_dim=nbColumns-1, activation='sigmoid'))
nn.add(Dense(nbClasses, activation='softmax'))
nn.summary()

nn.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])

nn.fit(X_train, Y_train, epochs=200, batch_size=10)

#Training the neural network

score = nn.evaluate(X_test, Y_test, verbose=2)
print('Test accuracy:', score[1])

#For more information, we build the confusion matrix
Y_pred = nn.predict(X_test)
Y_pred_1 = Y_pred.argmax(axis=1)
Y_test_1 = Y_test.argmax(axis=1)
confusion = confusion_matrix(Y_pred_1, Y_test_1) 


