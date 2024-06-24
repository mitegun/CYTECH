library('readxl')
# **** Centrer et r?duire les donn?es *****
cigarettes <- read_excel("cigarettes.xlsx")
cigarettes2 <- cigarettes[,-1]
dataS = data.frame(scale(cigarettes2))
# ****  Construire un groupe entrainement et un groupe test 
dataSapp=dataS[c(1:15),]
dataStest=dataS[c(16:24),]
# ****  centrer et r?duire le groupe entrainement et le groupe test ****
dataSapp1 = data.frame(scale(dataSapp))
dataStest1 = data.frame(scale(dataStest))
# **************    Premi?res utilisations de fonction R *******
# ******* installation de quelques libraries utiles ******
library(MASS)
library(Matrix)
library(glmnet)
library(lmridge)
# ******************* avec la fonction lmridge **************
### avec lambda =0
mod.ridge1=lmridge(CO..mg. ~.,data=dataSapp1,K=0)
summary(mod.ridge1)
print.lmridge(mod.ridge1)
predtest1=predict(mod.ridge1,dataStest1)
predtestt1=predtest1*sd(cigarettes2$`CO (mg)`[16:24])+mean(cigarettes2$`CO (mg)`[16:24])
print(predtestt1)
## Question :  Comparer vos r?sultats avec ceux obtenus dans l'exercice 3 TD3 sans r?ugularisation
# *************************
### avec lambda =0.0712
mod.ridge2=lmridge(CO..mg. ~.,data=dataSapp1,K=0.0712)
summary(mod.ridge2)
print.lmridge(mod.ridge2)
predtest2=predict(mod.ridge2,dataStest1)
predtestt2=predtest2*sd(cigarettes2$`CO (mg)`[16:24])+mean(cigarettes2$`CO (mg)`[16:24])
print(predtestt2)
# *************************
## Question :  Comparer vos r?sultats avec ceux obtenus dans l'exercice 3 TD3 avec lambda=0.0712

#exercice 5

n=nrow(dataS)
l=3
taille=n%/%l
set.seed(3)
c=runif(n)
rang=rank(c)
groupeF=(rang-l)%/% taille +1
groupeF=as.factor(groupeF)
lambda_to_try=seq(0.0001, 1, 0.01)
m=length(lambda_to_try)

MFOLD= matrix(lambda_to_try,m,2)
Err_prevF=matrix(l:l,l,2)
erreurF=rep(0,l)
for (j in 1:m){
  for (i in 1:l){
    # moldèle linéaire ridge sans le groupe i  
    rid= lmridge(CO..mg.~.,dataS[groupeF!=i,], K=lambda_to_try[j])
    # prévision de la ième observation à l'aide du modèle obtenu 
    pred= predict(rid, newdata=dataS[groupeF==i,])
    # erreur de prévision 
    erreurF[i]= mean((pred - dataS[groupeF==i,]$CO)^2)
    Err_prevF[i,2]=erreurF[i]
  }
  MFOLD[j,2]=mean(Err_prevF[,2])
}



plot(MFOLD,type="l",col=2,xlab="Valeurs de lambda à tester",ylab="Moyenne Erreur de validation croisée 3-FOLD",sub="Evolution de l'erreur moyenne de la validation croisée selon les valeurs de lambda ",col.sub="blue")


Lambda_optF=MFOLD[which.min(MFOLD[,2]),1]


#on refait ridge avec lambda optimal

ridop=lmridge(CO..mg. ~.,data=dataS,K=0.030)
summary(ridop)
predtest3=predict(ridop,dataStest1)
predtestt3=predtest3*sd(cigarettes2$`CO (mg)`[16:24])+mean(cigarettes2$`CO (mg)`[16:24])