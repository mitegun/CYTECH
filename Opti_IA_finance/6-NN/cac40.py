import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from sklearn.preprocessing import MinMaxScaler, StandardScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
from tensorflow.keras.optimizers import Adam
import matplotlib.pyplot as plt
import prep_Data as prep

def encodeClass(s_class, nbClasses):
    if nbClasses == 3:
        if s_class == -1:
            return [1, 0, 0]
        elif s_class == 0:
            return [0, 1, 0]
        else:
            return [0, 0, 1]
    else:
        if s_class == -1:
            return [1, 0]
        else:
            return [0, 1]

# Reading the data and creating Training and Test sets
data = prep.myData
print(data)
nbColumns = len(data.columns)

# Splitting features and labels
X = data.values[:, :nbColumns - 1]
Y = data.values[:, nbColumns - 1]
classes = data['CLS'].unique().tolist()
nbClasses = len(classes)
for columns in data.columns:
    print(columns , np.mean(data[columns]))
    print(columns ,max(data[columns]))
    print(columns ,min(data[columns]))

# Encoding labels
encoded_Y = np.array([encodeClass(y, nbClasses) for y in list(Y)])
print(encoded_Y)

# Splitting data into training and testing sets
X_train, X_test, Y_train, Y_test = train_test_split(X, encoded_Y, test_size=0.3, random_state=100)



# Creating the neural network
nn = Sequential()
nn.add(Dense(5, input_dim=nbColumns - 1, activation='sigmoid'))
nn.add(Dense(5, input_dim=nbColumns - 1, activation='sigmoid'))
nn.add(Dense(nbClasses, activation='softmax'))
nn.summary()

# Compiling the neural network
nn.compile(loss='categorical_crossentropy', optimizer='Adam', metrics=['accuracy'])

# Training the neural network
history = nn.fit(X_train, Y_train, epochs=500, batch_size=10, validation_split=0.2)

# Evaluating the neural network
score = nn.evaluate(X_test, Y_test, verbose=2)
print('Test accuracy:', score[1])

# Building the confusion matrix
Y_pred = nn.predict(X_test)
Y_pred_1 = Y_pred.argmax(axis=1)
Y_test_1 = Y_test.argmax(axis=1)
confusion = confusion_matrix(Y_test_1, Y_pred_1)
print(confusion)



for numc in range(nbColumns-1):
    C=data.columns[numc]
    print("column", data.columns[numc]),"min",min(C),"max",max(C),"mean",np.mean(C)

#normalize data
scaler = StandardScaler()