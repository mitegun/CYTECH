# Charger les bibliothèques nécessaires
library(forecast) 
library(ggplot2)  
library(tseries)



# ---------------------- Partie 1 ----------------------




# 1. Simulation d'une série ARIMA(2, 1, 1)
set.seed(123) # Pour reproductibilité
simulated_data <- arima.sim(n = 100, model = list(order = c(2, 1, 1), ar = c(0.7, -0.3), ma = c(0.5)))

# Visualisation de la série simulée
plot(simulated_data, type = "l", main = "Série simulée ARIMA(2,1,1)")

# 2. Fonction de prévision personnalisée
Forecast_Per <- function(series, phi, h) {
  n <- length(series)
  p <- length(phi)
  forecasts <- numeric(h)
  
  for (t in 1:h) {
    forecasts[t] <- sum(phi * rev(series[(n - p + 1):(n)]))
    series <- c(series, forecasts[t]) # Ajouter la prévision à la série
  }
  
  return(forecasts)
}

# Prévoir h = 2 avec les coefficients AR connus
phi <- c(0.7, -0.3) # Coefficients AR pour le modèle ARIMA(2,1,1)
forecast_h2 <- Forecast_Per(series = tail(simulated_data, 10), phi = phi, h = 2)
print("Prévisions personnalisées :")
print(forecast_h2)

# 3. Ajustement du modèle ARIMA avec forecast
model <- Arima(simulated_data, order = c(2, 1, 1))
forecast_model <- forecast(model, h = 2)

# Comparaison graphique
plot(simulated_data, type = "l", main = "Comparaison des prévisions", xlim = c(90, 105))
lines(101:102, forecast_h2, col = "blue", type = "o", pch = 19, lty = 2) # Prévisions personnalisées
lines(101:102, forecast_model$mean, col = "red", type = "o", pch = 19, lty = 1) # Prévisions avec forecast
legend("topright", legend = c("Personnalisé", "Forecast"), col = c("blue", "red"), lty = c(2, 1), pch = 19)

# 4. Simuler des vraies valeurs supplémentaires pour la comparaison
set.seed(123) # Reproductibilité
actual <- arima.sim(n = 2, model = list(order = c(2, 1, 1), ar = c(0.7, -0.3), ma = c(0.5)))
print("Vraies valeurs simulées :")
print(actual)

# 5. Calcul des métriques d'évaluation
# Comparer seulement les dernières prévisions (h = 2)
# Ici, on va utiliser seulement les deux dernières valeurs de la série
actual_values <- as.numeric(actual[2:3])  # Prendre les 2 dernières valeurs simulées
forecast_values <- as.numeric(forecast_model$mean)  # Prévisions du modèle ARIMA

# Calcul des métriques
mse <- mean((actual_values - forecast_values)^2, na.rm = TRUE)
mae <- mean(abs(actual_values - forecast_values), na.rm = TRUE)
rmse <- sqrt(mse)

# Résultats des métriques
cat("MSE:", mse, "\nMAE:", mae, "\nRMSE:", rmse, "\n")






# ---------------------- PARTIE 2 ----------------------



# Lire les données
data <- read.csv("data.csv")

# Vérifier les premières lignes du fichier pour identifier d'éventuels problèmes
head(data)

# Vérifier s'il y a des valeurs manquantes dans Piccadilly, Year et Month
cat("Valeurs manquantes dans Piccadilly :", sum(is.na(data$Piccadilly)), "\n")
cat("Valeurs manquantes dans Year :", sum(is.na(data$Year)), "\n")
cat("Valeurs manquantes dans Month :", sum(is.na(data$Month)), "\n")

# Si des valeurs manquantes existent, les supprimer
data_clean <- na.omit(data)

# Vérification après nettoyage
cat("Valeurs manquantes après nettoyage :", sum(is.na(data_clean$Piccadilly)), "\n")

# Convertir le mois en format numérique 
if (class(data_clean$Month) == "character") {
  data_clean$Month <- match(data_clean$Month, month.name)
}

# Créer une colonne de dates avec `Year` et `Month`
data_clean$DATE <- as.Date(paste(data_clean$Year, data_clean$Month, "01", sep = "-"), format = "%Y-%m-%d")

# Vérifier que les dates ont été créées correctement
head(data_clean$DATE)

# Créer une série temporelle pour la colonne 'Piccadilly'
ts_data <- ts(data_clean$Piccadilly, start = c(min(data_clean$Year), min(data_clean$Month)), frequency = 12)


# Aperçu des données
summary(data_clean$Piccadilly)
plot(ts_data, main = "Série temporelle Piccadilly", ylab = "Piccadilly", xlab = "Temps")

decomposition <- decompose(ts_data, type = "multiplicative")
plot(decomposition)
acf(ts_data, main = "ACF - Série Piccadilly",lag.max = 100)
pacf(ts_data, main = "PACF - Série Piccadilly",lag.max = 100)

adf_test <- adf.test(ts_data)
cat("p-value du test ADF :", adf_test$p.value, "\n")



# ---------------------- ARIMA ----------------------


# Diviser les données en train et test (80% train, 20% test)
train_length <- floor(0.8 * length(ts_data))
train_data <- window(ts_data, end = c(time(ts_data)[train_length]))
test_data <- window(ts_data, start = c(time(ts_data)[train_length + 1]))

# Liste des modèles ARIMA à tester
arima_models <- list(
  c(4,0,3), c(4, 0, 2), c(4, 0, 4), 
  c(3, 0, 3), c(3, 0, 2), c(3, 0, 4),
  c(5,0,3), c(5, 0, 2), c(5, 0, 4)
)

# Initialisation pour stocker les résultats
results <- list()

# Ajustement des modèles ARIMA sur les données d'entraînement
for (model in arima_models) {
  order <- model
  tryCatch({
    fit <- Arima(train_data, order = order)
    aic_value <- AIC(fit)
    
    # Prévisions sur l'ensemble de test
    forecast_result <- forecast(fit, h = length(test_data))
    
    # Calcul du RMSE
    rmse <- sqrt(mean((forecast_result$mean - test_data)^2))
    
    # Enregistrer les résultats
    results[[paste(order, collapse = ",")]] <- list(
      model = fit, 
      AIC = aic_value,
      RMSE = rmse,
      forecast = forecast_result
    )
    
    # Afficher les résultats
    cat("Modèle ARIMA(", paste(order, collapse = ","), "): AIC =", aic_value, ", RMSE =", rmse, "\n")
    
  }, error = function(e) {
    cat("Erreur avec le modèle ARIMA(", paste(order, collapse = ","), "):", e$message, "\n")
  })
}

# Trouver le meilleur modèle ARIMA selon le RMSE
best_arima_rmse <- names(which.min(sapply(results, function(x) x$RMSE)))
cat("\nMeilleur modèle ARIMA selon RMSE :", best_arima_rmse, "\n")
best_arima_fit <- results[[best_arima_rmse]]$model

# Vérification des résidus du modèle choisi
checkresiduals(best_arima_fit)

# Fonction pour afficher l'équation d'un modèle ARIMA
display_arima_equation <- function(model) {
  coef <- coef(model)
  
  # Termes AR (p)
  ar_terms <- paste0("AR(", 1:length(grep("^ar", names(coef))), "): ", 
                     coef[grep("^ar", names(coef))], collapse = ", ")
  
  # Termes MA (q)
  ma_terms <- paste0("MA(", 1:length(grep("^ma", names(coef))), "): ", 
                     coef[grep("^ma", names(coef))], collapse = ", ")
  
  # Terme constant
  intercept <- if ("intercept" %in% names(coef)) paste("C: ", coef["intercept"]) else "C: 0"
  
  # Afficher l'équation
  cat("Équation du modèle ARIMA :\n")
  cat(ar_terms, "\n")
  cat(ma_terms, "\n")
  cat(intercept, "\n")
}

# Extraire les modèles
best_arima_fit <- results[[best_arima_rmse]]$model

# Afficher l'équation du meilleur ARIMA
cat("\n=== Meilleur Modèle ARIMA ===\n")
display_arima_equation(best_arima_fit)

# 1. Prédictions sur l'ensemble du dataset
forecast_result_full <- forecast(best_arima_fit, h = 60) 

# 2. Visualisation des prévisions sur toutes les données
plot(forecast_result_full, main = "Prévisions sur l'ensemble du dataset", xlab = "Temps", ylab = "Valeur")
lines(ts_data, col = "red", lty = 2)  
legend("topleft", legend = c("Prévisions", "Données réelles"), col = c("blue", "red"), lty = c(1, 2))

# Calcul des erreurs de prévision
forecast_errors <- forecast_result_full$mean - ts_data
mean_error <- mean(forecast_errors)
rmse <- sqrt(mean(forecast_errors^2))
mae <- mean(abs(forecast_errors))  

cat("\nAnalyse a posteriori de la prévision :\n")
cat("Erreur moyenne de prévision (Mean Error):", mean_error, "\n")
cat("RMSE (Root Mean Squared Error):", rmse, "\n")
cat("Erreur absolue moyenne (MAE):", mae, "\n")





# ---------------------- autoARIMA ----------------------


# Diviser les données en train et test (80% train, 20% test)
train_length <- floor(0.8 * length(ts_data))
train_data <- window(ts_data, end = c(time(ts_data)[train_length]))
test_data <- window(ts_data, start = c(time(ts_data)[train_length + 1]))

# Initialisation pour stocker les résultats
results <- list()

# Ajustement du modèle ARIMA automatique sur les données d'entraînement
tryCatch({
  fit_auto <- auto.arima(train_data, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  
  # Affichage du modèle ARIMA sélectionné
  cat("Modèle ARIMA choisi par auto.arima :\n")
  print(fit_auto)
  
  # Prévisions sur l'ensemble de test
  forecast_result <- forecast(fit_auto, h = length(test_data))
  
  # Calcul du RMSE
  rmse <- sqrt(mean((forecast_result$mean - test_data)^2))
  
  # Enregistrer les résultats
  results <- list(
    model = fit_auto, 
    RMSE = rmse,
    forecast = forecast_result
  )
  
  # Afficher les résultats
  cat("RMSE pour le modèle sélectionné :", rmse, "\n")
  
}, error = function(e) {
  cat("Erreur avec auto.arima :", e$message, "\n")
})

# Vérification des résidus du modèle choisi
checkresiduals(fit_auto)

# Fonction pour afficher l'équation du modèle ARIMA
display_arima_equation <- function(model) {
  coef <- coef(model)
  
  # Termes AR (p)
  ar_terms <- paste0("AR(", 1:length(grep("^ar", names(coef))), "): ", 
                     coef[grep("^ar", names(coef))], collapse = ", ")
  
  # Termes MA (q)
  ma_terms <- paste0("MA(", 1:length(grep("^ma", names(coef))), "): ", 
                     coef[grep("^ma", names(coef))], collapse = ", ")
  
  # Terme constant
  intercept <- if ("intercept" %in% names(coef)) paste("C: ", coef["intercept"]) else "C: 0"
  
  # Afficher l'équation
  cat("Équation du modèle ARIMA :\n")
  cat(ar_terms, "\n")
  cat(ma_terms, "\n")
  cat(intercept, "\n")
}

# Afficher l'équation du modèle ARIMA choisi
cat("\n=== Modèle ARIMA choisi par auto.arima ===\n")
display_arima_equation(fit_auto)

# 1. Prédictions sur l'ensemble du dataset
forecast_result_full <- forecast(fit_auto, h = 60)  

# 2. Visualisation des prévisions sur toutes les données
plot(forecast_result_full, main = "Prévisions sur l'ensemble du dataset", xlab = "Temps", ylab = "Valeur")
lines(ts_data, col = "red", lty = 2)  # Ajouter les données réelles en rouge
legend("topleft", legend = c("Prévisions", "Données réelles"), col = c("blue", "red"), lty = c(1, 2))

# Calcul des erreurs de prévision
forecast_errors <- forecast_result_full$mean - ts_data
mean_error <- mean(forecast_errors)
rmse <- sqrt(mean(forecast_errors^2))
mae <- mean(abs(forecast_errors))  # Erreur absolue moyenne (MAE)

cat("\nAnalyse a posteriori de la prévision :\n")
cat("Erreur moyenne de prévision (Mean Error):", mean_error, "\n")
cat("RMSE (Root Mean Squared Error):", rmse, "\n")
cat("Erreur absolue moyenne (MAE):", mae, "\n")







# ---------------------- SARIMA ----------------------


# Diviser les données en train et test (80% train, 20% test)
train_length <- floor(0.8 * length(ts_data))
train_data <- window(ts_data, end = c(time(ts_data)[train_length]))
test_data <- window(ts_data, start = c(time(ts_data)[train_length + 1]))

# Définir les modèles SARIMA à tester
sarima_models <- list(
  list(order = c(3, 0, 3), seasonal = c(0, 1, 1)),
  list(order = c(3, 0, 3), seasonal = c(0, 1, 2)),
  list(order = c(3, 0, 3), seasonal = c(0, 1, 3))

)

# Initialisation pour stocker les résultats SARIMA
sarima_results <- list()

# Ajustement des modèles SARIMA sur les données d'entraînement
for (model in sarima_models) {
  tryCatch({
    fit <- Arima(train_data, 
                 order = model$order, 
                 seasonal = list(order = model$seasonal, period = 12))
    aic_value <- AIC(fit)
    
    # Prévisions sur l'ensemble de test
    forecast_result <- forecast(fit, h = length(test_data))
    
    # Calcul du RMSE
    rmse <- sqrt(mean((forecast_result$mean - test_data)^2))
    
    # Clé pour identifier le modèle
    key <- paste0("SARIMA(", paste(model$order, collapse = ","), ")(", 
                  paste(model$seasonal, collapse = ","), ")[12]")
    
    # Enregistrer les résultats
    sarima_results[[key]] <- list(
      model = fit, 
      AIC = aic_value,
      RMSE = rmse,
      forecast = forecast_result
    )
    
    # Afficher les résultats
    cat(key, ": AIC =", aic_value, ", RMSE =", rmse, "\n")
    
  }, error = function(e) {
    cat("Erreur avec le modèle", key, ":", e$message, "\n")
  })
}

# Trouver le meilleur modèle SARIMA selon l'AIC
best_sarima_aic <- names(which.min(sapply(sarima_results, function(x) x$AIC)))
cat("\nMeilleur modèle SARIMA selon AIC :", best_sarima_aic, "\n")
# Extraire le meilleur modèle SARIMA
best_sarima_fit <- sarima_results[[best_sarima_aic]]$model

# Vérification des résidus du modèle choisi
checkresiduals(best_sarima_fit)

# Fonction pour afficher l'équation d'un modèle SARIMA
display_sarima_equation <- function(model) {
  coef <- coef(model)
  
  # Termes AR (p) et MA (q)
  ar_terms <- paste0("AR(", 1:length(grep("^ar", names(coef))), "): ", 
                     coef[grep("^ar", names(coef))], collapse = ", ")
  ma_terms <- paste0("MA(", 1:length(grep("^ma", names(coef))), "): ", 
                     coef[grep("^ma", names(coef))], collapse = ", ")
  
  # Termes saisonniers AR (P) et MA (Q)
  sar_terms <- paste0("SAR(", 1:length(grep("^sar", names(coef))), "): ", 
                      coef[grep("^sar", names(coef))], collapse = ", ")
  sma_terms <- paste0("SMA(", 1:length(grep("^sma", names(coef))), "): ", 
                      coef[grep("^sma", names(coef))], collapse = ", ")
  
  # Terme constant
  intercept <- if ("intercept" %in% names(coef)) paste("C: ", coef["intercept"]) else "C: 0"
  
  # Afficher l'équation
  cat("Équation du modèle SARIMA :\n")
  cat(ar_terms, "\n")
  cat(ma_terms, "\n")
  cat(sar_terms, "\n")
  cat(sma_terms, "\n")
  cat(intercept, "\n")
}

# Afficher l'équation du meilleur modèle SARIMA
cat("\n=== Meilleur Modèle SARIMA ===\n")
display_sarima_equation(best_sarima_fit)

# 1. Prédictions sur l'ensemble du dataset
forecast_result_full_sarima <- forecast(best_sarima_fit, h = 60 ) 

# 2. Visualisation des prévisions sur toutes les données
plot(forecast_result_full_sarima, main = "Prévisions SARIMA sur l'ensemble du dataset", xlab = "Temps", ylab = "Valeur")
lines(ts_data, col = "red", lty = 2)  
legend("topleft", legend = c("Prévisions", "Données réelles"), col = c("blue", "red"), lty = c(1, 2))

# 3. Analyse a posteriori de la prévision
# Calcul des erreurs de prévision
forecast_errors_sarima <- forecast_result_full_sarima$mean - ts_data
mean_error_sarima <- mean(forecast_errors_sarima)
rmse_sarima <- sqrt(mean(forecast_errors_sarima^2))
mae_sarima <- mean(abs(forecast_errors_sarima)) 

cat("\nAnalyse a posteriori de la prévision (SARIMA) :\n")
cat("Erreur moyenne de prévision (Mean Error):", mean_error_sarima, "\n")
cat("RMSE (Root Mean Squared Error):", rmse_sarima, "\n")
cat("Erreur absolue moyenne (MAE):", mae_sarima, "\n")






# ---------------------- autoSARIMA ----------------------


# Diviser les données en train et test (80% train, 20% test)
train_length <- floor(0.8 * length(ts_data))
train_data <- window(ts_data, end = c(time(ts_data)[train_length]))
test_data <- window(ts_data, start = c(time(ts_data)[train_length + 1]))

# Initialisation pour stocker les résultats
results <- list()

# Ajustement du modèle ARIMA automatique sur les données d'entraînement
tryCatch({
  fit_auto <- auto.arima(train_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  
  # Affichage du modèle ARIMA sélectionné
  cat("Modèle ARIMA choisi par auto.arima :\n")
  print(fit_auto)
  
  # Prévisions sur l'ensemble de test
  forecast_result <- forecast(fit_auto, h = length(test_data))
  
  # Calcul du RMSE
  rmse <- sqrt(mean((forecast_result$mean - test_data)^2))
  
  # Enregistrer les résultats
  results <- list(
    model = fit_auto, 
    RMSE = rmse,
    forecast = forecast_result
  )
  
  # Afficher les résultats
  cat("RMSE pour le modèle sélectionné :", rmse, "\n")
  
}, error = function(e) {
  cat("Erreur avec auto.arima :", e$message, "\n")
})

# Vérification des résidus du modèle choisi
checkresiduals(fit_auto)

# Fonction pour afficher l'équation du modèle ARIMA
display_arima_equation <- function(model) {
  coef <- coef(model)
  
  # Termes AR (p)
  ar_terms <- paste0("AR(", 1:length(grep("^ar", names(coef))), "): ", 
                     coef[grep("^ar", names(coef))], collapse = ", ")
  
  # Termes MA (q)
  ma_terms <- paste0("MA(", 1:length(grep("^ma", names(coef))), "): ", 
                     coef[grep("^ma", names(coef))], collapse = ", ")
  
  # Terme constant
  intercept <- if ("intercept" %in% names(coef)) paste("C: ", coef["intercept"]) else "C: 0"
  
  # Afficher l'équation
  cat("Équation du modèle ARIMA :\n")
  cat(ar_terms, "\n")
  cat(ma_terms, "\n")
  cat(intercept, "\n")
}

# Afficher l'équation du modèle ARIMA choisi
cat("\n=== Modèle ARIMA choisi par auto.arima ===\n")
display_arima_equation(fit_auto)

# 1. Prédictions sur l'ensemble du dataset
forecast_result_full <- forecast(fit_auto, h = 60)  

# 2. Visualisation des prévisions sur toutes les données
plot(forecast_result_full, main = "Prévisions sur l'ensemble du dataset", xlab = "Temps", ylab = "Valeur")
lines(ts_data, col = "red", lty = 2)  
legend("topleft", legend = c("Prévisions", "Données réelles"), col = c("blue", "red"), lty = c(1, 2))


# Calcul des erreurs de prévision
forecast_errors <- forecast_result_full$mean - ts_data
mean_error <- mean(forecast_errors)
rmse <- sqrt(mean(forecast_errors^2))
mae <- mean(abs(forecast_errors)) 

cat("\nAnalyse a posteriori de la prévision :\n")
cat("Erreur moyenne de prévision (Mean Error):", mean_error, "\n")
cat("RMSE (Root Mean Squared Error):", rmse, "\n")
cat("Erreur absolue moyenne (MAE):", mae, "\n")













# ---------------------- Random Search ARIMA ----------------------


# Définir les limites de recherche pour Random Search
max_p <- 10
max_d <- 1  
max_q <- 10


# Initialiser les variables pour stocker le meilleur modèle et son AIC, RMSE
best_aic <- Inf
best_rmse <- Inf
best_model <- NULL
best_params <- list()

# Diviser les données en train et test (80% train, 20% test)
train_length <- floor(0.8 * length(ts_data))
train_data <- window(ts_data, end = c(time(ts_data)[train_length]))
test_data <- window(ts_data, start = c(time(ts_data)[train_length + 1]))

# Boucle Random Search
for (p in 0:max_p) {
  for (d in 0:max_d) {
    for (q in 0:max_q) {

            # Essayer de créer un modèle avec les paramètres actuels
            tryCatch({
              cat("Essai des paramètres : p =", p, "d =", d, "q =", q "\n")
              model <- Arima(train_data, order = c(p, d, q))
              
              # Calculer l'AIC
              current_aic <- AIC(model)
              
              # Prévisions sur l'ensemble de test
              forecast_result <- forecast(model, h = length(test_data))
              
              # Calculer le RMSE
              rmse <- sqrt(mean((forecast_result$mean - test_data)^2))
              
              # Afficher AIC et RMSE pour ce modèle
              cat("AIC =", current_aic, ", RMSE =", rmse, "\n")
              
              # Vérifier si c'est le meilleur modèle selon l'AIC et RMSE
              if (current_aic < best_aic ) {
                best_aic <- current_aic
                best_rmse <- rmse
                best_model <- model
                best_params <- list(p = p, d = d, q = q)
              }
            }, error = function(e) { 
              # Ignorer les erreurs dues à des modèles invalides
            })
          }
        }
      }


# Afficher les résultats finaux
cat("\nMeilleur modèle selon AIC et RMSE :\n")
cat("AIC =", best_aic, ", RMSE =", best_rmse, "\n")
cat("Paramètres optimaux : p =", best_params$p, "d =", best_params$d, "q =", best_params$q, 
    "P =", best_params$P, "D =", best_params$D, "Q =", best_params$Q, "\n")

# Résumé du meilleur modèle
cat("\nRésumé du meilleur modèle :\n")
summary(best_model)

# Définir le modèle SARIMA choisi
sarima_model <- list(order = c(9, 0, 4))

# Ajustement du modèle SARIMA sur les données d'entraînement
fit <- Arima(train_data, 
             order = sarima_model$order)

# Affichage de l'AIC du modèle ajusté
aic_value <- AIC(fit)
cat("AIC du modèle SARIMA :", aic_value, "\n")

# Prévisions sur l'ensemble de test
forecast_result <- forecast(fit, h = length(test_data))

# Calcul du RMSE
rmse <- sqrt(mean((forecast_result$mean - test_data)^2))
cat("RMSE sur les données de test :", rmse, "\n")

# Vérification des résidus
checkresiduals(fit)

# Visualisation des prévisions sur l'ensemble des données
forecast_result_full <- forecast(fit, h = 60)  
plot(forecast_result_full, main = "Prévisions ARIMA Random Search", xlab = "Temps", ylab = "Valeur")
lines(ts_data, col = "red", lty = 2)  
legend("topleft", legend = c("Prévisions", "Données réelles"), col = c("blue", "red"), lty = c(1, 2))

# Analyse des erreurs de prévision
forecast_errors <- forecast_result_full$mean - ts_data
mean_error <- mean(forecast_errors, na.rm = TRUE)
rmse_full <- sqrt(mean(forecast_errors^2, na.rm = TRUE))
mae <- mean(abs(forecast_errors), na.rm = TRUE)

cat("\nAnalyse a posteriori de la prévision :\n")
cat("Erreur moyenne de prévision (Mean Error):", mean_error, "\n")
cat("RMSE (Root Mean Squared Error):", rmse_full, "\n")
cat("Erreur absolue moyenne (MAE):", mae, "\n")




# ---------------------- Random Search sARIMA ----------------------


# Définir les limites de recherche pour Random Search
max_p <- 3
max_d <- 1  
max_q <- 3
max_P <- 3
max_D <-  1 
max_Q <- 3
seasonality <- 12 

# Initialiser les variables pour stocker le meilleur modèle et son AIC, RMSE
best_aic <- Inf
best_rmse <- Inf
best_model <- NULL
best_params <- list()

# Diviser les données en train et test (80% train, 20% test)
train_length <- floor(0.8 * length(ts_data))
train_data <- window(ts_data, end = c(time(ts_data)[train_length]))
test_data <- window(ts_data, start = c(time(ts_data)[train_length + 1]))

# Boucle Random Search
for (p in 0:max_p) {
  for (d in 0:max_d) {
    for (q in 0:max_q) {
      for (P in 0:max_P) {
        for (D in 0:max_D) {
          for (Q in 0:max_Q) {
            # Essayer de créer un modèle avec les paramètres actuels
            tryCatch({
              cat("Essai des paramètres : p =", p, "d =", d, "q =", q, 
                  "P =", P, "D =", D, "Q =", Q, "\n")
              model <- Arima(train_data, order = c(p, d, q), 
                             seasonal = list(order = c(P, D, Q), period = seasonality))
              
              # Calculer l'AIC
              current_aic <- AIC(model)
              
              # Prévisions sur l'ensemble de test
              forecast_result <- forecast(model, h = length(test_data))
              
              # Calculer le RMSE
              rmse <- sqrt(mean((forecast_result$mean - test_data)^2))
              
              # Afficher AIC et RMSE pour ce modèle
              cat("AIC =", current_aic, ", RMSE =", rmse, "\n")
              
              # Vérifier si c'est le meilleur modèle selon l'AIC et RMSE
              if (current_aic < best_aic ) {
                best_aic <- current_aic
                best_rmse <- rmse
                best_model <- model
                best_params <- list(p = p, d = d, q = q, P = P, D = D, Q = Q)
              }
            }, error = function(e) { 
              # Ignorer les erreurs dues à des modèles invalides
            })
          }
        }
      }
    }
  }
}

# Afficher les résultats finaux
cat("\nMeilleur modèle selon AIC et RMSE :\n")
cat("AIC =", best_aic, ", RMSE =", best_rmse, "\n")
cat("Paramètres optimaux : p =", best_params$p, "d =", best_params$d, "q =", best_params$q, 
    "P =", best_params$P, "D =", best_params$D, "Q =", best_params$Q, "\n")

# Résumé du meilleur modèle
cat("\nRésumé du meilleur modèle :\n")
summary(best_model)

# Définir le modèle SARIMA choisi
sarima_model <- list(order = c(0, 0, 3), seasonal = c(0, 1, 1))

# Ajustement du modèle SARIMA sur les données d'entraînement
fit <- Arima(train_data, 
             order = sarima_model$order, 
             seasonal = list(order = sarima_model$seasonal, period = 12))

# Affichage de l'AIC du modèle ajusté
aic_value <- AIC(fit)
cat("AIC du modèle SARIMA :", aic_value, "\n")

# Prévisions sur l'ensemble de test
forecast_result <- forecast(fit, h = length(test_data))

# Calcul du RMSE
rmse <- sqrt(mean((forecast_result$mean - test_data)^2))
cat("RMSE sur les données de test :", rmse, "\n")

# Vérification des résidus
checkresiduals(fit)

# Visualisation des prévisions sur l'ensemble des données
forecast_result_full <- forecast(fit, h = 60) 
plot(forecast_result_full, main = "Prévisions SARIMA", xlab = "Temps", ylab = "Valeur")
lines(ts_data, col = "red", lty = 2) 
legend("topleft", legend = c("Prévisions", "Données réelles"), col = c("blue", "red"), lty = c(1, 2))

# Analyse des erreurs de prévision
forecast_errors <- forecast_result_full$mean - ts_data
mean_error <- mean(forecast_errors, na.rm = TRUE)
rmse_full <- sqrt(mean(forecast_errors^2, na.rm = TRUE))
mae <- mean(abs(forecast_errors), na.rm = TRUE)

cat("\nAnalyse a posteriori de la prévision :\n")
cat("Erreur moyenne de prévision (Mean Error):", mean_error, "\n")
cat("RMSE (Root Mean Squared Error):", rmse_full, "\n")
cat("Erreur absolue moyenne (MAE):", mae, "\n")

  
