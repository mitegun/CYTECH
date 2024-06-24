poids <- read.table("poids.txt", header = TRUE, sep = "\t", dec = ",")
print(poids)
summary(poids)

# Exercice 2
n <- nrow(poids)
print(n)
TailleH <- (3 * n) %/% 4
set.seed(3)
c <- runif(n)
rang <- rank(c)
app <- rang[1:TailleH]

m1 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = poids[app,])
predict_m1 <- predict(m1, newdata = poids[-app,])

m2 <- lm(Y ~ X1 + X6 + X7 + X8 + X9 + X10, data = poids[app,])
predict_m2 <- predict(m2, newdata = poids[-app,])

m3 <- lm(Y ~ X1 + X6 + X7 + X8 + X10, data = poids[app,])
predict_m3 <- predict(m3, newdata = poids[-app,])

summary(m1)
summary(m2)
summary(m3)
predict_m1
predict_m2
predict_m3

residuHOM1 <- mean((predict_m1 - poids[-app,]$Y) ^ 2)
residuHOM2 <- mean((predict_m2 - poids[-app,]$Y) ^ 2)
residuHOM3 <- mean((predict_m3 - poids[-app,]$Y) ^ 2)

residuHOM1
residuHOM2
residuHOM3

#le résidu du modèle 2 est le plus petit donc ce modèle est le plus prédictif selon
#le critère Hold-Out

#EXERCICE 3

groupeL <- as.factor(rang)
erreurm1 <- rep(0, n)
erreurm2 <- rep(0, n)
erreurm3 <- rep(0, n)

for (i in 1:n) {
  LOOCVm1 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = poids[groupeL != i, ])
  LOOCVm2 <- lm(Y ~ X1 + X6 + X7 + X8 + X9 + X10, data = poids[groupeL != i, ])
  LOOCVm3 <- lm(Y ~ X1 + X6 + X7 + X8 + X10, data = poids[groupeL != i, ])
  
  predLOm1 <- predict(LOOCVm1, newdata = poids[groupeL == i, ])
  predLOm2 <- predict(LOOCVm2, newdata = poids[groupeL == i, ])
  predLOm3 <- predict(LOOCVm3, newdata = poids[groupeL == i, ])
  
  print(predLOm1)
  print(predLOm2)
  print(predLOm3)
  
  erreurm1[i] <- (predLOm1 - poids[groupeL==i,]$Y) ^ 2
  erreurm2[i] <- (predLOm2 - poids[groupeL==i,]$Y) ^ 2
  erreurm3[i] <- (predLOm3 - poids[groupeL==i,]$Y) ^ 2
}

print(mean(erreurm1))
print(mean(erreurm2))
print(mean(erreurm3))

#le meilleur modèle est toujours le numéro 2 erreur prédictive minimale 

#EXERCICE 4
# Utilisation des folds prédéfinis pour les modèles
Fold1 <- rang[1:7]
Fold2 <- rang[8:15]
Fold3 <- rang[16:22]
Fold12 <- rang[1:15]
Fold23 <- rang[8:22]
A <- c(1:7, 16:22)
Fold13 <- rang[A]

# Extraction des données correspondant aux différents ensembles de folds
poids12 <- poids[Fold12, ]
poids3 <- poids[Fold3, ]
poids2<-poids[Fold2,]
poids1<-poids[Fold1,]
poids23 <- poids[Fold23, ]
poids13 <- poids[Fold13, ]

# Entrainement des modèles sur Fold12 et prédiction sur Fold3
Fold12m1 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = poids12)
pred3m1 <- predict(Fold12m1, newdata = poids3)
print(pred3m1)

# Entrainement des modèles sur Fold1 et Fold3 et prédiction sur Fold2
Fold13m1 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = poids13)
pred2m1 <- predict(Fold13m1, newdata = poids2)
print(pred2m1)

# Entrainement des modèles sur Fold2 et Fold3 et prédiction sur Fold1
Fold23m1 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = poids23)
pred1m1 <- predict(Fold23m1, newdata = poids1)
print(pred1m1)


# Entraînement des modèles sur Fold12 et prédiction sur Fold3
Fold12m2 <- lm(Y ~ X1 + X6 + X7 + X8 + X9 + X10, data = poids12)
pred3m2 <- predict(Fold12m2, newdata = poids3)
print(pred3m2)

# Entraînement des modèles sur Fold1 et Fold3 et prédiction sur Fold2
Fold13m2 <- lm(Y ~ X1 + X6 + X7 + X8 + X9 + X10, data = poids13)
pred2m2 <- predict(Fold13m2, newdata = poids2)
print(pred2m2)

# Entraînement des modèles sur Fold2 et Fold3 et prédiction sur Fold1
Fold23m2 <- lm(Y ~ X1 + X6 + X7 + X8 + X9 + X10, data = poids23)
pred1m2 <- predict(Fold23m2, newdata = poids1)
print(pred1m2)

# Entraînement des modèles sur Fold12 et prédiction sur Fold3 pour le modèle 3
Fold12m3 <- lm(Y ~ X1 + X6 + X7 + X8 + X10, data = poids12)
pred3m3 <- predict(Fold12m3, newdata = poids3)
print(pred3m3)

# Entraînement des modèles sur Fold1 et Fold3 et prédiction sur Fold2 pour le modèle 3
Fold13m3 <- lm(Y ~ X1 + X6 + X7 + X8 + X10, data = poids13)
pred2m3 <- predict(Fold13m3, newdata = poids2)
print(pred2m3)

# Entraînement des modèles sur Fold2 et Fold3 et prédiction sur Fold1 pour le modèle 3
Fold23m3 <- lm(Y ~ X1 + X6 + X7 + X8 + X10, data = poids23)
pred1m3 <- predict(Fold23m3, newdata = poids1)
print(pred1m3)

# Calcul du résidu théorique prédit en utilisant la moyenne pour chaque fold et chaque modèle, en prenant en compte m1

# Calcul du résidu pour le modèle 1
residu_m1 <- rep(0, 3)

# Modèle 1 - Calcul des résidus
residu_m1[1] <- mean((pred3m1 - poids3$Y) ^ 2)  # Résidu pour Fold12
residu_m1[2] <- mean((pred2m1 - poids2$Y) ^ 2)  # Résidu pour Fold13
residu_m1[3] <- mean((pred1m1 - poids1$Y) ^ 2)  # Résidu pour Fold23

# Affichage des résidus pour le modèle 1
print("Résidus pour le modèle 1:")
print(residu_m1)

# Calcul du résidu pour le modèle 2
residu_m2 <- rep(0, 3)

# Modèle 2 - Calcul des résidus
residu_m2[1] <- mean((pred3m2 - poids3$Y) ^ 2)  # Résidu pour Fold12
residu_m2[2] <- mean((pred2m2 - poids2$Y) ^ 2)  # Résidu pour Fold13
residu_m2[3] <- mean((pred1m2 - poids1$Y) ^ 2)  # Résidu pour Fold23

# Affichage des résidus pour le modèle 2
print("Résidus pour le modèle 2:")
print(residu_m2)

# Calcul du résidu pour le modèle 3
residu_m3 <- rep(0, 3)

# Modèle 3 - Calcul des résidus
residu_m3[1] <- mean((pred3m3 - poids3$Y) ^ 2)  # Résidu pour Fold12
residu_m3[2] <- mean((pred2m3 - poids2$Y) ^ 2)  # Résidu pour Fold13
residu_m3[3] <- mean((pred1m3 - poids1$Y) ^ 2)  # Résidu pour Fold23

# Affichage des résidus pour le modèle 3
print("Résidus pour le modèle 3:")
print(residu_m3)




