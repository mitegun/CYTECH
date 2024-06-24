poids <- read.table("poids.txt", header=T, sep="\t",dec=",")
print(poids)
summary(poids)
pairs(poids,col="red", main="nuage des poins deux à deux variables")
modele <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10 ,data=poids)
print(modele)
summary(modele)
#mc et corr same same
mc <- cor(poids[,c(1,2,3,4,5,6,7,8,9,10)])
Corr <- cor(poids)

#extraire la première ligne pour avoir les corrélations entre y et xi
print(Corr[,1])

#calcul du vif

modele1<- modele
library(car)
vif(modele1)

mc <- cor(poids[,c(2,3,4,5,6,7,8,9,10)])
print(mc)

#Q4
step(modele, data=poids, direction="backward")

#q6     
modele2 <- lm(Y ~ X1+X6+X7+X8+X9+X10 ,data=poids)
print(modele2)
summary(modele2)