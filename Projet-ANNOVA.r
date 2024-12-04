# -------------------------------------------------------------------------
# 1. CHARGEMENT DES BIBLIOTHÈQUES NÉCESSAIRES
# -------------------------------------------------------------------------
# Installer et charger les packages nécessaires
if (!require(readr)) install.packages("readr")         # Lecture des fichiers CSV
if (!require(ggplot2)) install.packages("ggplot2")     # Visualisation des données
if (!require(dplyr)) install.packages("dplyr")         # Manipulation des données
if (!require(caret)) install.packages("caret")         # Encodage One-Hot
if (!require(corrplot)) install.packages("corrplot")   # Visualisation des corrélations
if (!require(car)) install.packages("car")             # Tests statistiques
if (!require(stats)) install.packages("stats")         # Fonctions statistiques de base

# Charger les bibliothèques après installation
library(readr)
library(ggplot2)
library(dplyr)
library(caret)
library(corrplot)
library(car)
library(stats)

# -------------------------------------------------------------------------
# 2. CHARGEMENT DES JEUX DE DONNÉES
# -------------------------------------------------------------------------
# Charger les fichiers des données de la turbine à gaz (2011 à 2015)
turbine_2011 <- read_csv("/Users/zahra/Desktop/4DS/sem1/Stat/StatsProjet/gas\ turbine\ co\ and\ nox\ emission\ data\ set/gt_2011.csv")
turbine_2012 <- read_csv("/Users/zahra/Desktop/4DS/sem1/Stat/StatsProjet/gas\ turbine\ co\ and\ nox\ emission\ data\ set/gt_2012.csv")
turbine_2013 <- read_csv("/Users/zahra/Desktop/4DS/sem1/Stat/StatsProjet/gas\ turbine\ co\ and\ nox\ emission\ data\ set/gt_2013.csv")
turbine_2014 <- read_csv("/Users/zahra/Desktop/4DS/sem1/Stat/StatsProjet/gas\ turbine\ co\ and\ nox\ emission\ data\ set/gt_2014.csv")
turbine_2015 <- read_csv("/Users/zahra/Desktop/4DS/sem1/Stat/StatsProjet/gas\ turbine\ co\ and\ nox\ emission\ data\ set/gt_2015.csv")

# Fusionner les données des années 2011 à 2015 en un seul tableau
turbine_data <- bind_rows(turbine_2011, turbine_2012, turbine_2013, turbine_2014, turbine_2015)

# Charger le fichier des données de maintenance prédictive
maintenance_data <- read_csv("/Users/zahra/Desktop/4DS/sem1/Stat/StatsProjet/ai4i2020.csv")

# -------------------------------------------------------------------------
# 3. PRÉPARATION DES DONNÉES : TRAITEMENT DES VALEURS MANQUANTES
# -------------------------------------------------------------------------
# Vérification des valeurs manquantes
sum(is.na(turbine_data))  # Nombre de valeurs manquantes dans turbine_data
sum(is.na(maintenance_data))  # Nombre de valeurs manquantes dans maintenance_data

# Imputation des valeurs manquantes pour les variables continues
turbine_data$AT[is.na(turbine_data$AT)] <- mean(turbine_data$AT, na.rm = TRUE)
turbine_data$AP[is.na(turbine_data$AP)] <- mean(turbine_data$AP, na.rm = TRUE)

# Suppression des lignes contenant des valeurs manquantes pour les autres variables
#turbine_data <- na.omit(turbine_data)
#maintenance_data <- na.omit(maintenance_data)

# -------------------------------------------------------------------------
# 4. DÉTECTION ET TRAITEMENT DES OUTLIERS (Turbine Data)
# -------------------------------------------------------------------------
# Calcul des bornes pour les outliers (méthode IQR)
Q1 <- quantile(turbine_data$CO, 0.25)  # 1er quartile
Q3 <- quantile(turbine_data$CO, 0.75)  # 3e quartile
IQR_CO <- Q3 - Q1  # Plage interquartile

# Calcul des bornes inférieure et supérieure pour les outliers
lower_bound <- Q1 - 1.5 * IQR_CO
upper_bound <- Q3 + 1.5 * IQR_CO

# Identifier les outliers avant le capping
outliers_before <- turbine_data$CO[turbine_data$CO < lower_bound | turbine_data$CO > upper_bound]
cat("Nombre d'outliers avant capping : ", length(outliers_before), "\n")

# Boxplot avant capping
boxplot(turbine_data$CO, 
        main = "Avant Capping : Emissions de CO", 
        horizontal = TRUE,                # Boxplot horizontal
        col = "lightblue",                 # Couleur de la boîte (boxplot)
        border = "black",                  # Couleur du bord de la boîte
        xlab = "Émissions de CO",          # Label pour l'axe X
        outlier.col = "red",               # Couleur des outliers en rouge
        outlier.size = 3)                  # Taille des outliers

# Appliquer le capping : limiter les outliers par les bornes calculées
turbine_data$CO[turbine_data$CO > upper_bound] <- upper_bound
turbine_data$CO[turbine_data$CO < lower_bound] <- lower_bound


# Identifier les outliers après le capping (qui ne devraient plus exister après le capping)
outliers_after <- turbine_data$CO[turbine_data$CO < lower_bound | turbine_data$CO > upper_bound]

# Afficher le nombre d'outliers après le capping
cat("Nombre d'outliers après capping : ", length(outliers_after), "\n")

# Boxplot après capping
boxplot(turbine_data$CO, 
        main = "Après Capping : Emissions de CO", 
        horizontal = TRUE,                # Boxplot horizontal
        col = "lightblue",                 # Couleur de la boîte (boxplot)
        border = "black",                  # Couleur du bord de la boîte
        xlab = "Émissions de CO",          # Label pour l'axe X
        outlier.col = "red",               # Couleur des outliers en rouge
        outlier.size = 3)                  # Taille des outliers



# ---------------------------------------------------------
# DÉTECTION  DES OUTLIERS (maintenance_data)
# ---------------------------------------------------------

# Choisissez la variable sur laquelle appliquer l'IQR (par exemple, "rotational_speed")
Q1 <- quantile(maintenance_data$rotational_speed, 0.25)  # 1er quartile
Q3 <- quantile(maintenance_data$rotational_speed, 0.75)  # 3e quartile
IQR_rotational_speed <- Q3 - Q1  # Plage interquartile

# Calcul des bornes inférieure et supérieure pour les outliers
lower_bound <- Q1 - 1.5 * IQR_rotational_speed
upper_bound <- Q3 + 1.5 * IQR_rotational_speed

# Identifier les outliers avant le capping
outliers_before <- maintenance_data$rotational_speed[maintenance_data$rotational_speed < lower_bound | maintenance_data$rotational_speed > upper_bound]
cat("Nombre d'outliers : ", length(outliers_before), "\n")



# Vérification des statistiques descriptives pour turbine_data
summary(turbine_data$CO)
summary(turbine_data$AT)
summary(turbine_data$AP)
summary(turbine_data$AH)

# Vérification des statistiques descriptives pour maintenance_data
summary(maintenance_data$rotational_speed)
summary(maintenance_data$tool_wear)
summary(maintenance_data$temperature)
summary(maintenance_data$pressure)

head(maintenance_data)
str(maintenance_data)
maintenance_data$rotational_speed <- as.numeric(maintenance_data$rotational_speed)

# Supprimer les lignes avec des NA dans `rotational_speed`
maintenance_data <- maintenance_data[!is.na(maintenance_data$rotational_speed), ]
# Vérifier la colonne `rotational_speed` après nettoyage
head(maintenance_data$rotational_speed)

# -------------------------------------------------------------------------
# 5. NORMALISATION DES VARIABLES CONTINUES (Turbine Data)
# -------------------------------------------------------------------------
# Normalisation des variables continues en utilisant la standardisation (Z-score)
turbine_data$CO <- scale(turbine_data$CO)
turbine_data$AT <- scale(turbine_data$AT)
turbine_data$AP <- scale(turbine_data$AP)
turbine_data$AH <- scale(turbine_data$AH)

# Vérification des données normalisées
summary(turbine_data$CO)
summary(turbine_data$AT)
summary(turbine_data$AP)
summary(turbine_data$AH)

# Normalisation des variables continues en utilisant la standardisation (Z-score)
maintenance_data$rotational_speed <- scale(maintenance_data$rotational_speed)
maintenance_data$tool_wear <- scale(maintenance_data$tool_wear)
maintenance_data$temperature <- scale(maintenance_data$temperature)
maintenance_data$pressure <- scale(maintenance_data$pressure)

# Vérification des données normalisées
summary(maintenance_data$rotational_speed)
summary(maintenance_data$tool_wear)
summary(maintenance_data$temperature)
summary(maintenance_data$pressure)

# -------------------------------------------------------------------------
# 6. ENCODAGE ONE-HOT DES VARIABLES CATÉGORIELLES (Maintenance Data)
# -------------------------------------------------------------------------

# Encodage One-Hot pour les variables catégorielles dans maintenance_data
dummies_maintenance <- dummyVars(" ~ product_quality + machine_failure + failure_type", data = maintenance_data)
maintenance_data_encoded <- data.frame(predict(dummies_maintenance, newdata = maintenance_data))

# Vérification des variables One-Hot encodées
head(maintenance_data_encoded)  # Voir les premières lignes des données encodées

# Ajout des variables numériques restantes dans maintenance_data
maintenance_data <- cbind(maintenance_data_encoded, maintenance_data %>% select(rotational_speed, air_temperature, process_temperature, torque, tool_wear))

# Vérification du dataset après l'ajout des variables numériques
head(maintenance_data)  # Voir les premières lignes du dataset combiné


# -------------------------------------------------------------------------
# 6. ENCODAGE ONE-HOT DES VARIABLES CATÉGORIELLES (Turbine Data)
# -------------------------------------------------------------------------

# Encodage One-Hot pour les variables catégorielles dans turbine_data
dummies_turbine <- dummyVars(" ~ failure_type", data = turbine_data)
turbine_data_encoded <- data.frame(predict(dummies_turbine, newdata = turbine_data))

# Vérification des variables One-Hot encodées
head(turbine_data_encoded)  # Voir les premières lignes des données encodées

# Ajout des variables numériques restantes dans turbine_data
turbine_data <- cbind(turbine_data_encoded, turbine_data %>% select(CO, AT, AP, AH))

# Vérification du dataset après l'ajout des variables numériques
head(turbine_data)  # Voir les premières lignes du dataset combiné


# -------------------------------------------------------------------------
# 7. MATRICE DE CORRÉLATION ET VISUALISATION (Turbine Data)
# -------------------------------------------------------------------------
correlation_matrix <- cor(turbine_data %>% select(AT, AP, AH, TIT, CO), method = "pearson")
print("Matrice de Corrélation (Pearson) :")
print(correlation_matrix)

# Visualisation de la matrice de corrélation
corrplot(correlation_matrix, method = "color", 
         title = "Matrice de Corrélation (Pearson)", 
         mar = c(0, 0, 1, 0), 
         col = colorRampPalette(c("blue", "white", "red"))(200))

# Installer et charger le package corrplot si ce n'est pas déjà fait
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)




# 8. MODÈLE DE RÉGRESSION LINÉAIRE (Turbine Data)
# -------------------------------------------------------------------------
reg_model <- lm(CO ~ AT + AP + AH + TIT, data = turbine_data)
summary(reg_model)

# Test de normalité des résidus
shapiro.test(reg_model$residuals)

# Visualisation des résidus
plot(reg_model$fitted.values, reg_model$residuals,
     main = "Homoscédasticité des Résidus",
     xlab = "Valeurs Prédites",
     ylab = "Résidus")
abline(h = 0, col = "red")

# -------------------------------------------------------------------------
# 9. TEST DU CHI-2 (Maintenance Data)
# -------------------------------------------------------------------------
table_qualite_echec <- table(maintenance_data_encoded$machine_failure.1, maintenance_data_encoded$product_quality.low)
chi2_result <- chisq.test(table_qualite_echec)
print("Résultat du Test du Chi-2 :")
print(chi2_result)

# -------------------------------------------------------------------------
# 10. TEST T POUR COMPARER LES VITESSES DE ROTATION (Maintenance Data)
# -------------------------------------------------------------------------
t_test_result <- t.test(maintenance_data$rotational_speed ~ maintenance_data_encoded$machine_failure.1)
print("Résultat du Test T :")
print(t_test_result)

# -------------------------------------------------------------------------
# Conclusion : Ce script est une analyse complète incluant la préparation, la visualisation et l'analyse des données
# -------------------------------------------------------------------------
