xi <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
yi <- c(12.89,10.12,5.00,1.59,1.55,3.96,6.42,7.00,10.71,14.32,15.91)
plot(xi,yi, xlab="xi", ylab="yi",main="nuage de")
xi_2 <- xi*xi
datapoly <- data.frame(yi,xi, xi_2)
modele <- lm(yi~xi)
print(modele)
sm <- summary(modele)
print(sm)
modele2<- lm(yi~xi + xi_2)
print(modele2)
sm2 <- summary(modele2)
print(sm2)
# Calcul de la p-value du test de significativité globale
R2 <- 0.9
p <- 2
n <- 11

# Calcul de la statistique F
F_stat <- (R2 / p) / ((1 - R2) / (n - p - 1))
print(F_stat)

# Degrés de liberté
num_df <- p
denom_df <- n - p - 1

# Calcul de la p-value
p_value <- 1 - pf(F_stat, df1 = num_df, df2 = denom_df)

cat("Statistique de Fisher:", F_stat, "\n")
cat("Degrés de liberté numérateur/dénominateur:", num_df, denom_df, "\n")
cat("P-valeur:", p_value)

confint(modele2, "xi_2")


#7. Déterminer la p-valeur du test de nullité de a2

p<-2 #nombre de prédicteur
n<-11 #taille de l'échantillon

# Estimation de a chapeau
a2_hat <- 0.4663
ecartype_a2_hat <- 0.0603

# Calcul de la statistique de Student
t_student <- a2_hat / ecartype_a2_hat

# Degrés de liberté
num_df <- 1
denom_df <- n - p - 1

# Calcul de la p-value
p_value2 <- 2 * pt(-abs(t_student), df = denom_df)

cat("Statistique de Student:", t_student, "\n")
cat("Degrés de liberté numérateur/dénominateur:", num_df, denom_df, "\n")
cat("P-valeur:", p_value2)
