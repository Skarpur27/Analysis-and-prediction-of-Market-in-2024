# Nettoie l'envirronement
rm(list = ls())
# Supprimez tous les graphiques en répétant dev.off()
while (dev.cur() > 1) dev.off()


# Chargement des bibliothèques nécessaires
library(readxl)   # Pour lire les fichiers Excel
library(ggplot2)  # Pour les graphiques
library(tseries)  # Pour les tests de séries temporelles
#library(depmixS4) # Pour les modèles Markov Switching
#install.packages("MSwM")
library(MSwM)
library(forecast)
library(stats)


# Définir le chemin vers le fichier de données
file_path <- "C:/Users/bossd/Downloads/Evolution PIB.xlsx"


# Chargement des données
#-------------------------------------------------------------------------------

# Fonction pour charger et préparer les données
prepare_data <- function(sheet_name) {
  data <- read_excel(file_path, sheet = sheet_name)
  data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
  data$GDP_Growth <- as.numeric(data$GDP_Growth)
  return(data)
}

# Charger les données pour les États-Unis et la France
us_data <- prepare_data("US")
fr_data <- prepare_data("FR")

# Convertissez la colonne 'date' en objets de type Date
# us_data$Date <- as.Date(us_data$Date)

# Utilisez la fonction format pour extraire l'année et comparez-la à 1990
us_data_subset <- subset(us_data, as.integer(format(us_data$Date, "%Y")) < 2024)
# Utilisez la fonction format pour extraire l'année et comparez-la à 1990
fr_data_subset <- subset(fr_data, as.integer(format(fr_data$Date, "%Y")) < 2024)


# Graphiques
#-------------------------------------------------------------------------------

# Fonction pour créer un graphique de la croissance du PIB
plot_gdp_growth <- function(data, country) {
  ggplot(data, aes(x = Date, y = GDP_Growth)) +
    geom_line() +
    labs(title = paste("Taux de croissance du PIB de", country),
         x = "Date",
         y = "Taux de croissance du PIB (%)")
}

# Créer des graphiques pour les États-Unis et la France
plot_gdp_growth(us_data_subset, "États-Unis")
plot_gdp_growth(fr_data_subset, "France")


# Test de stationarité ADF
#-------------------------------------------------------------------------------

# Fonction pour effectuer le test de Dickey-Fuller augmenté
perform_adf_test <- function(data) {
  adf_test <- adf.test(data$GDP_Growth)
  return(adf_test$p.value)
}

# Effectuer le test de Dickey-Fuller pour les États-Unis et la France
p_value_us <- perform_adf_test(us_data_subset)
p_value_fr <- perform_adf_test(fr_data_subset)

print(paste("P-value pour les États-Unis:", p_value_us))
print(paste("P-value pour la France:", p_value_fr))


# Affichage PACF
#-------------------------------------------------------------------------------

# Fonction pour calculer et afficher la PACF
calculate_pacf <- function(data) {
  pacf(data$GDP_Growth, lag.max = 10)
}

# Calculer la PACF pour les États-Unis et la France
calculate_pacf(us_data_subset)
calculate_pacf(fr_data_subset)


# Choix retard pour AR (afficage AIC BIC)
#-------------------------------------------------------------------------------

# Créez une liste vide pour stocker les modèles AR et leurs critères AIC et BIC
ar_models <- list()

# Définissez une plage d'ordres possibles (par exemple, de 1 à 10)
order_range <- 1:10

# Boucle pour ajuster les modèles AR et calculer AIC et BIC
for (p in order_range) {
  model <- arima(us_data_subset$GDP_Growth, order = c(p, 0, 0))  # Ajustez un modèle AR(p)
  aic <- AIC(model)  # Calculez AIC
  bic <- BIC(model)  # Calculez BIC
  
  # Stockez le modèle AR, AIC et BIC dans la liste
  ar_models[[p]] <- list(order = p, model = model, AIC = aic, BIC = bic)
}

# Affichez la liste des modèles avec leurs AIC et BIC
for (p in order_range) {
  cat(paste("Order p =", p, ", AIC =", ar_models[[p]]$AIC, ", BIC =", ar_models[[p]]$BIC, "\n"))
}


# MS-AR
#-------------------------------------------------------------------------------

fit_ms_ar1 <- function(data) {
  # Ajuster un modèle MS-AR(1) avec msmFit
  # La formule indique que GDP_Growth dépend de son propre retard d'ordre 1
  model <- msmFit(GDP_Growth ~ 1, k = 2, sw = rep(TRUE,7), p = 5, data = data, control = list(maxiter = 1000))
  return(model)
}

# Ajustement du modèle pour les États-Unis
us_model <- fit_ms_ar1(us_data_subset)

# Ajustement du modèle pour la France
fr_model <- fit_ms_ar1(fr_data_subset)

# Afficher le résumé des modèles ajustés
summary(us_model)
summary(fr_model)

us_states_probaliss <- us_model@Fit@smoProb
print(us_states_probaliss)

print(formatC(us_states_probaliss, format = "f", digits = 3))

#Affiche les probabilité
plotProb(us_model)
plotProb(fr_model)

# Coefficients de l'équation de régression
coeff_MSAR1_us <- us_model@Coef[["GDP_Growth_1"]]
coeff_MSAR1_fr <- fr_model@Coef[["GDP_Growth_1"]]

cst_MSAR1_us <- us_model@Coef[["(Intercept)"]]
cst_MSAR1_fr <- fr_model@Coef[["(Intercept)"]]


# Récupération du coefficient d'expension
coeff_exp_us = coeff_MSAR1_us[1]
coeff_exp_fr = coeff_MSAR1_fr[1]

# Récupération de la constante d'expension
cst_exp_us = cst_MSAR1_us[1]
cst_exp_fr = cst_MSAR1_fr[1]

# Obtenez la dernière valeur de la série temporelle de croissance du PIB pour les États-Unis et la France
last_value_us <- tail(us_data_subset$GDP_Growth, 1)
last_value_fr <- tail(fr_data_subset$GDP_Growth, 1)

# Prévoir les 4 prochaines valeurs pour les États-Unis
prev_us <- numeric(4)
prev_us[1] <- last_value_us * coeff_exp_us + cst_exp_us
for (i in 2:4) {
  prev_us[i] <- prev_us[i - 1] * coeff_exp_us + cst_exp_us
}

# Prévoir les 4 prochaines valeurs pour la France
prev_fr <- numeric(4)
prev_fr[1] <- last_value_fr * coeff_exp_fr + cst_exp_fr
for (i in 2:4) {
  prev_fr[i] <- prev_fr[i - 1] * coeff_exp_fr + cst_exp_fr
}


