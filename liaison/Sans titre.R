# Charger les datasets
ai_data <- read.csv("/Users/zahra/Desktop/4DS/sem1/Stat/StatsProjet/DataSets/ai4i2020.csv")
gt_data <- read.csv("/Users/zahra/Desktop/4DS/sem1/Stat/StatsProjet/DataSets/gas\ turbine\ co\ and\ nox\ emission\ data\ set/gt_2011.csv")

# Charger les bibliothèques nécessaires
library(ggplot2)
library(corrplot)
# Ajouter un identifiant unique pour fusionner les datasets
ai_data$id <- 1:nrow(ai_data)
gt_data$id <- 1:nrow(gt_data)

# Fusionner les datasets par l'identifiant
merged_data <- merge(ai_data, gt_data, by = "id", all = TRUE)

# Vérifier les colonnes et les premières lignes
print("Colonnes du dataset fusionné :")
print(colnames(merged_data))
print("Premières lignes du dataset fusionné :")
print(head(merged_data))

# Analyse des corrélations
# Corrélation pour AI4I
cor_ai <- cor(ai_data$Air.temperature..K., ai_data$Machine.failure, use = "complete.obs")
print(paste("Corrélation entre Température (AI4I) et Machine Failure:", cor_ai))

# Corrélation pour GT
cor_gt <- cor(gt_data$AT, gt_data$CO, use = "complete.obs")
print(paste("Corrélation entre Température (GT) et CO:", cor_gt))

# Corrélation croisée (température AI4I avec CO de GT)
cor_cross <- cor(merged_data$Air.temperature..K., merged_data$CO, use = "complete.obs")
print(paste("Corrélation croisée entre Température (AI4I) et CO (GT):", cor_cross))

# Régressions linéaires
# Régression pour AI4I
model_ai <- lm(Machine.failure ~ Air.temperature..K., data = ai_data)
print("Résumé de la régression pour AI4I :")
print(summary(model_ai))

# Régression pour GT
model_gt <- lm(CO ~ AT, data = gt_data)
print("Résumé de la régression pour GT :")
print(summary(model_gt))

# Régression croisée
model_cross <- lm(CO ~ Air.temperature..K., data = merged_data)
print("Résumé de la régression croisée :")
print(summary(model_cross))

# Visualisation
# Température (AI4I) vs Machine Failure
ggplot(ai_data, aes(x = Air.temperature..K., y = Machine.failure)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Température de l'air (AI4I) vs Machine Failure",
       x = "Température de l'air (K)",
       y = "Échec de la machine")

# Température (GT) vs CO
ggplot(gt_data, aes(x = AT, y = CO)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", color = "orange") +
  labs(title = "Température ambiante (GT) vs CO",
       x = "Température ambiante (°C)",
       y = "Émissions de CO")

# Température (AI4I) vs CO (GT) croisée
ggplot(merged_data, aes(x = Air.temperature..K., y = CO)) +
  geom_point(color = "purple", alpha = 0.5) +
  geom_smooth(method = "lm", color = "pink") +
  labs(title = "Température de l'air (AI4I) vs CO (GT)",
       x = "Température de l'air (K)",
       y = "Émissions de CO")
# Matrice de corrélation pour le dataset AI4I
ai_data_numeric <- ai_data[, sapply(ai_data, is.numeric)]  # Sélectionner les colonnes numériques
cor_ai <- cor(ai_data_numeric, use = "complete.obs")
print("Matrice de corrélation pour AI4I :")
print(cor_ai)

# Matrice de corrélation pour le dataset GT
gt_data_numeric <- gt_data[, sapply(gt_data, is.numeric)]  # Sélectionner les colonnes numériques
cor_gt <- cor(gt_data_numeric, use = "complete.obs")
print("Matrice de corrélation pour GT :")
print(cor_gt)

# Matrice de corrélation pour le dataset fusionné
merged_data_numeric <- merged_data[, sapply(merged_data, is.numeric)]  # Sélectionner les colonnes numériques
cor_merged <- cor(merged_data_numeric, use = "complete.obs")
print("Matrice de corrélation pour le dataset fusionné :")
print(cor_merged)

# Visualisation des matrices de corrélation avec corrplot
# Pour AI4I
corrplot(cor_ai, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, title = "Matrice de Corrélation AI4I")

# Pour GT
corrplot(cor_gt, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, title = "Matrice de Corrélation GT")

# Pour le dataset fusionné
corrplot(cor_merged, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, title = "Matrice de Corrélation Fusionnée")
