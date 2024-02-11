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
library(stargazer)
#install.packages("stargazer")

# Définir le chemin vers le fichier de données
file_path <- "C:/Users/bossd/Downloads/NvData.xlsx"


# Chargement des données
#-------------------------------------------------------------------------------

data <- read_xlsx(file_path)
#data <- read_xlsx(file_path, sheet = Donnee2)
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
data$CPI_YOY <- as.numeric(data$CPI_YOY)
data$FRCPIYOY <- as.numeric(data$FRCPIYOY)

# Valeurs sans NA
data_CPI_YOY <- data[,1:2]
data_CPI_YOY <- na.omit(data_CPI_YOY)

data_FRCPIYOY <- data[,c(1,5)]
data_FRCPIYOY <- na.omit(data_FRCPIYOY)

# Utilisez la fonction format pour extraire l'année et comparez-la à 1990
#us_data_subset <- subset(us_data, as.integer(format(us_data$Date, "%Y")) < 2024)


# Graphiques
#-------------------------------------------------------------------------------

# Fonction pour créer un graphique de la croissance du PIB

ggplot(data_CPI_YOY, aes(x = Date, y = CPI_YOY)) + geom_line() + labs(title = paste("Taux de croissance de l'inflation (CPI US)"),x = "Date",
       y = "Taux de croissance du PIB (%)")


ggplot(data_FRCPIYOY, aes(x = Date, y = FRCPIYOY)) + geom_line() + labs(title = paste("Taux de croissance de l'inflation (CPI FR)"),x = "Date",
                                                                      y = "Taux de croissance du PIB (%)")


# Test de stationarité ADF
#-------------------------------------------------------------------------------

# Fonction pour effectuer le test de Dickey-Fuller augmenté
adf_test <- adf.test(data_CPI_YOY$CPI_YOY)

# Effectuer le test de Dickey-Fuller pour les États-Unis et la France
print(paste("P-value pour les États-Unis:", adf_test$p.value))

# Fonction pour effectuer le test de Dickey-Fuller augmenté
adf_test <- adf.test(data_FRCPIYOY$FRCPIYOY)

# Effectuer le test de Dickey-Fuller pour les États-Unis et la France
print(paste("P-value pour les États-Unis:", adf_test$p.value))


# Affichage PACF
#-------------------------------------------------------------------------------
pacf(data_CPI_YOY$CPI_YOY, lag.max = 10)

pacf(data_FRCPIYOY$FRCPIYOY, lag.max = 10)


# Choix retard pour AR (afficage AIC BIC)
#-------------------------------------------------------------------------------

# Créez une liste vide pour stocker les modèles AR et leurs critères AIC et BIC
ar_models <- list()

# Définissez une plage d'ordres possibles (par exemple, de 1 à 10)
order_range <- 1:10

# Boucle pour ajuster les modèles AR et calculer AIC et BIC
for (p in order_range) {
  model <- arima(data_CPI_YOY$CPI_YOY, order = c(p, 0, 0))  # Ajustez un modèle AR(p)
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

# La formule indique que GDP_Growth dépend de son propre retard d'ordre 1

model_us <- msmFit(CPI_YOY ~ 1, k = 2, sw = rep(TRUE,4), p = 2, data = data_CPI_YOY, control = list(maxiter = 1000))

model_fr <- msmFit(FRCPIYOY ~ 1, k = 2, sw = rep(TRUE,4), p = 2, data = data_FRCPIYOY, control = list(maxiter = 1000))


# Afficher le résumé des modèles ajustés
summary(model_us)
summary(model_fr)

us_states_probaliss <- model_us@Fit@smoProb
print(formatC(us_states_probaliss, format = "f", digits = 3))

fr_states_probaliss <- model_fr@Fit@smoProb
print(formatC(fr_states_probaliss, format = "f", digits = 3))

#Affiche les probabilité
plotProb(model_us)
plotProb(model_fr)

# Coefficients de l'équation de régression
#/!\/!\/!\/!\/!\/!\/!\/!\
#   /|\
#  / | \
# /  |  \
#/   o   \
#/!\/!\/!\/!\/!\/!\/!\/!\
# ATTENTION LES ETATS CHANGENT A CHAQUE LANCEMENT DE CODE

# Coefficients 1 (Etat 1 : "calme" pour US - "volatile" pour FR)
coeff1_MSAR_us <- model_us@Coef[["CPI_YOY_1"]]
coeff1_MSAR_fr <- model_fr@Coef[["FRCPIYOY_1"]]


# Coefficient 2 (Etat 2 : "volatile" pour US - "calme" pour FR)
coeff2_MSAR_us <- model_us@Coef[["CPI_YOY_2"]]
coeff2_MSAR_fr <- model_fr@Coef[["FRCPIYOY_2"]]


# Obtenez la dernière valeur de la série temporelle de croissance du PIB pour les États-Unis et la France
last_value_us <- head(data_CPI_YOY$CPI_YOY, 2)
last_value_fr <- head(data_FRCPIYOY$FRCPIYOY, 2)

# Prévoir les 4 prochaines valeurs pour les États-Unis
prev_us <- numeric(4)
prev_us[1] <- last_value_us[1] * coeff1_MSAR_us[1] + last_value_us[2] * coeff2_MSAR_us[1]
prev_us[2] <- prev_us[1] * coeff1_MSAR_us[1] + last_value_us[1] * coeff2_MSAR_us[1]
prev_us[3] <- prev_us[2] * coeff1_MSAR_us[1] + prev_us[1] * coeff2_MSAR_us[1]
prev_us[4] <- prev_us[3] * coeff1_MSAR_us[1] + prev_us[2] * coeff2_MSAR_us[1]

# Prévoir les 4 prochaines valeurs pour la France
prev_fr <- numeric(4)
prev_fr[1] <- last_value_fr[1] * coeff1_MSAR_fr[2] + last_value_fr[2] * coeff2_MSAR_fr[2]
prev_fr[2] <- prev_fr[1] * coeff1_MSAR_fr[2] + last_value_fr[1] * coeff2_MSAR_fr[2]
prev_fr[3] <- prev_fr[2] * coeff1_MSAR_fr[2] + prev_fr[1] * coeff2_MSAR_fr[2]
prev_fr[4] <- prev_fr[3] * coeff1_MSAR_fr[2] + prev_fr[2] * coeff2_MSAR_fr[2]




