#Pour normalisation

import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import StandardScaler

myData=pd.read_csv('iris.csv', sep=',')
nbColumns=len(myData.columns)

X=myData.values[:,:nbColumns-1]

scaler1 = MinMaxScaler()
scaler1.fit(X)
X_scaled1 = scaler1.transform(X)
for numc in range(nbColumns-1):
    C = X_scaled1[:,numc]
    print(min(C),max(C))





