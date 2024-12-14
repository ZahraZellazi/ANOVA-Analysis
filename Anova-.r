# Installation des librairies nécessaires:
install.packages(c("tidyverse", "ggplot2", "dplyr", "caret", "MASS", "reshape2", 
                   "GGally", "robustbase", "ggcorrplot", "car", "lmtest", "nortest", "moments", "boot"))
install.packages("conflicted")
library(conflicted)
install.packages("ggplot2")

conflict_prefer("filter", "dplyr")  # Précisez que vous préférez dplyr::filter
conflict_prefer("select", "dplyr")  # Précisez que vous préférez dplyr::select
update.packages(ask = FALSE)
detach("package:MASS", unload = TRUE)
# Charger les bibliothèques:
library(tidyverse)
library(robustbase)
library(ggplot2)
library(dplyr)
library(caret)
library(MASS)
library(reshape2)
library(GGally)
library(ggcorrplot)
library(car)
library(lmtest)
library(nortest)
library(moments)
library(boot)

# Charger les jeux de données:
gt_2011 <- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".", na.strings = "")
gt_2012 <- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".", na.strings = "")
gt_2013 <- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".", na.strings = "")
gt_2014 <- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".", na.strings = "")
gt_2015 <- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".", na.strings = "")
ai4i2020 <- read.table(file = file.choose(), header = TRUE, sep = ",", dec = ".", na.strings = "")

# Combiner les jeux de données du Gas Turbine Dataset en un seul:
gt_combined <- rbind(gt_2011, gt_2012, gt_2013, gt_2014, gt_2015)

# Creation d'un fichier pour stocker les graphes:
plot_path <- "/Users/zahra/Desktop/4DS/sem1/Stat/StatsProjet/plots"
if (!dir.exists(plot_path)) {
  dir.create(plot_path)
}

# Exploration initiale des données:
# Gas Turbine Dataset
summary(gt_combined)
str(gt_combined)

# AI4I Dataset
summary(ai4i2020)
str(ai4i2020)
# Visualisation des histogrammes
# Gas Turbine Dataset
for (column in colnames(gt_combined)) {
  if (is.numeric(gt_combined[[column]])) {
    # Calculate a dynamic binwidth based on the data range
    range_values <- range(gt_combined[[column]], na.rm = TRUE)
    binwidth <- (range_values[2] - range_values[1]) / (1 + log2(nrow(gt_combined))) # Sturges' formula
    
    # Generate the histogram
    p <- ggplot(gt_combined, aes(x = .data[[column]])) +
      geom_histogram(binwidth = binwidth, fill = "lightgreen", color = "black", alpha = 0.7) +
      labs(title = paste("Histogram of", column), x = column, y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) # Center the title
    
    # Save the histogram
    ggsave(filename = paste0(plot_path, "/histogram_gt_", column, ".jpg"), plot = p, width = 7, height = 5)
  }
}



# AI4I Dataset
for (column in colnames(ai4i2020)) {
  if (is.numeric(ai4i2020[[column]])) {
    # Dynamic binwidth: Sturges' formula for automatic bin determination
    range_values <- range(ai4i2020[[column]], na.rm = TRUE)
    binwidth <- (range_values[2] - range_values[1]) / (1 + log2(nrow(ai4i2020)))
    
    # Create histogram
    p <- ggplot(ai4i2020, aes(x = .data[[column]])) +
      geom_histogram(binwidth = binwidth, fill = "pink", color = "black", alpha = 0.7) +
      labs(title = paste("Histogram of", column), x = column, y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) # Center the title
    
    # Save the plot
    ggsave(filename = paste0(plot_path, "/histogram_ai4i_", column, ".jpg"), plot = p, width = 7, height = 5)
  }
}


#------------------------Préparation des données-------------------------------------

# 1. Vérifier les valeurs manquantes dans chaque colonne :
sapply(gt_combined, function(x) sum(is.na(x))) # Pas de valeurs manquantes pour le Gas Turbine Dataset
sapply(ai4i2020, function(x) sum(is.na(x)))   # Vérification des valeurs manquantes pour le AI4I Dataset

# 2. Traitement des valeurs aberrantes (Capping des outliers) :

# Identification des colonnes numériques pour le Gas Turbine Dataset
numeric_columns_gt <- names(gt_combined)[vapply(gt_combined, is.numeric, logical(1))]

# Identification des colonnes numériques pour le AI4I Dataset
numeric_columns_ai4i <- names(ai4i2020)[vapply(ai4i2020, is.numeric, logical(1))]

# Afficher les colonnes numériques pour chaque dataset
print("Colonnes numériques pour le Gas Turbine Dataset :")
print(numeric_columns_gt)

print("Colonnes numériques pour le AI4I Dataset :")
print(numeric_columns_ai4i)

# Boxplot pour chaque colonne numérique avant le traitement des valeurs aberrantes
# Pour le Gas Turbine Dataset
for (col in numeric_columns_gt) {
  png(filename = paste0(plot_path, "/Boxplot_Before_GT_", col, ".jpg"))
  boxplot(
    gt_combined[[col]], 
    main = paste("Boxplot for", col, "Before Handling Outliers (Capping)"), 
    ylab = col, 
    col = "grey", 
    outcol = "red"
  )
  dev.off()
}

# Pour le AI4I Dataset
for (col in numeric_columns_ai4i) {
  png(filename = paste0(plot_path, "/Boxplot_Before_AI4I_", col, ".jpg"))
  boxplot(
    ai4i2020[[col]], 
    main = paste("Boxplot for", col, "Before Handling Outliers (Capping)"), 
    ylab = col, 
    col = "grey", 
    outcol = "orange"
  )
  dev.off()
}


# Fonction pour capper les valeurs aberrantes basées sur l'IQR
cap_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data[[column]] <- ifelse(data[[column]] < lower_bound, lower_bound,
                           ifelse(data[[column]] > upper_bound, upper_bound, data[[column]]))
  return(data)
}

# Appliquer le capping aux colonnes numériques du Gas Turbine Dataset
for (col in numeric_columns_gt) {
  gt_combined <- cap_outliers(gt_combined, col)
}

# Appliquer le capping aux colonnes numériques du AI4I Dataset
for (col in numeric_columns_ai4i) {
  ai4i2020 <- cap_outliers(ai4i2020, col)
}

# Boxplot pour chaque colonne numérique après le capping (Gas Turbine Dataset)
for (col in numeric_columns_gt) {
  png(filename = paste0(plot_path, "/Boxplot_After_GT_", col, ".jpg"))
  boxplot(
    gt_combined[[col]], 
    main = paste("Boxplot for", col, "(Gas Turbine Dataset)", "After Handling Outliers (Capping)"), 
    ylab = col, 
    col = "lightgreen", 
    outcol = "red"
  )
  dev.off()
}

# Boxplot pour chaque colonne numérique après le capping (AI4I Dataset)
for (col in numeric_columns_ai4i) {
  png(filename = paste0(plot_path, "/Boxplot_After_AI4I_", col, ".jpg"))
  boxplot(
    ai4i2020[[col]], 
    main = paste("Boxplot for", col, "(AI4I Dataset)", "After Handling Outliers (Capping)"), 
    ylab = col, 
    col = "pink", 
    outcol = "orange"
  )
  dev.off()
}
#heatmap pour AI data set 
library(reshape2)

# Vérifiez la structure des données pour voir les colonnes
str(ai4i2020)

# Si 'numeric_columns_ai4i' n'est pas défini ou contient des erreurs, créez-le :
# Sélectionner les colonnes numériques de manière dynamique
numeric_columns_ai4i <- sapply(ai4i2020, is.numeric)
numeric_columns_ai4i <- names(ai4i2020)[numeric_columns_ai4i]

# Calculer la matrice de corrélation pour l'AI4I Dataset avec les bonnes colonnes numériques
cor_matrix_ai4i <- cor(ai4i2020[, numeric_columns_ai4i], use = "complete.obs")

# Transformer la matrice de corrélation en un format long pour ggplot
cor_matrix_melt <- melt(cor_matrix_ai4i)

# Créer et sauvegarder la heatmap comme avant
cor_plot <- ggplot(data = cor_matrix_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Matrice de Corrélation (AI4I Dataset)", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sauvegarder le graphique dans un fichier JPG
cor_plot_file <- paste(plot_path, "correlation_matrix_ai4i.jpg", sep = "/")
ggsave(cor_plot_file, plot = cor_plot, width = 8, height = 6)
#-------------------GTcombined---------------------------
# Charger les bibliothèques nécessaires
library(tidyverse)
library(reshape2)

# Sélectionner les colonnes numériques
numeric_columns_gt <- sapply(gt_combined, is.numeric)
numeric_columns_gt <- names(gt_combined)[numeric_columns_gt]

# Calculer la matrice de corrélation
cor_matrix_gt <- cor(gt_combined[, numeric_columns_gt], use = "complete.obs")

# Transformer la matrice de corrélation en format long
cor_matrix_melt_gt <- melt(cor_matrix_gt)

# Créer et sauvegarder la heatmap
cor_plot_gt <- ggplot(data = cor_matrix_melt_gt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Matrice de Corrélation (Gas Turbine Dataset)", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sauvegarder la heatmap
cor_plot_file <- paste(plot_path, "correlation_matrix_gt.jpg", sep = "/")
ggsave(cor_plot_file, plot = cor_plot_gt, width = 8, height = 6)

#------------------------Exploration des données-------------------------------------

# Visualisation des tendances pour le Gas Turbine Dataset
#for (col in numeric_columns_gt) {
# png(filename = paste0(plot_path, "/Hist_GT_", col, ".png"))
#  hist(
#    gt_combined[[col]], 
#    main = paste("Histogram for", col, "(Gas Turbine Dataset)"), 
#    xlab = col, 
#    col = "lightgreen"
#  )
#  dev.off()
#}

# Visualisation des tendances pour le AI4I Dataset
#for (col in numeric_columns_ai4i) {
#  png(filename = paste0(plot_path, "/Hist_AI4I_", col, ".png"))
#  hist(
#    ai4i2020[[col]], 
#    main = paste("Histogram for", col, "(AI4I Dataset)"), 
#    xlab = col, 
#    col = "lightgreen"
#  )
#  dev.off()
#}

#---------------------------------------------------------------------------------------------------------------------------
#GT DATA SET
# Charger les bibliothèques nécessaires
library(tidyverse)
library(caret)
library(ggcorrplot)

# Sélectionner les colonnes numériques
numeric_columns_gt <- sapply(gt_combined, is.numeric)

# Récupérer les noms des colonnes numériques
numeric_columns_gt_names <- names(gt_combined)[numeric_columns_gt]

# Fonction pour supprimer les colonnes fortement corrélées
remove_highly_correlated <- function(data, threshold) {
  cor_matrix <- cor(data, use = "everything")
  highly_correlated <- findCorrelation(cor_matrix, cutoff = threshold)
  if (length(highly_correlated) > 0) {
    data <- data[, -highly_correlated]
    return(remove_highly_correlated(data, threshold))
  } else {
    return(data)
  }
}

# Supprimer les colonnes fortement corrélées
gt_combined <- remove_highly_correlated(gt_combined[, numeric_columns_gt_names], threshold = 0.8)

# Calculer la matrice de corrélation après suppression
cor_matrix_reduced <- cor(gt_combined, use = "everything")

# Créer la heatmap
cor_plot <- ggcorrplot(cor_matrix_reduced, hc.order = TRUE, type = "upper", lab = TRUE, 
                       title = "Correlation Matrix After")

# Sauvegarder le graphique dans un fichier JPG
plot_path <- "/Users/zahra/Desktop/4DS/sem1/Stat/StatsProjet/plots"
cor_plot_file <- paste(plot_path, "correlation_matrix_gt_after.jpg", sep = "/")
ggsave(cor_plot_file, plot = cor_plot, width = 8, height = 6)



#---------------------------------------------------------------------------


# 4. Transformation de la variable cible (CO)
# Appliquer une transformation logarithmique pour résoudre les problèmes de non-normalité
if ("CO" %in% names(gt_combined)) {
  gt_combined$CO <- log1p(gt_combined$CO)  # Transformation log1p pour éviter des valeurs infinies pour 0
}

# 5. Test de normalité avec Kolmogorov-Smirnov
ks_test <- ks.test(gt_combined$CO, "pnorm", mean = mean(gt_combined$CO), sd = sd(gt_combined$CO))
print(ks_test)

# Si p-value < 0.05, la distribution n'est pas normale et nécessite une transformation.
if (ks_test$p.value < 0.05) {
  # Appliquer une transformation Box-Cox pour rendre les données plus normales
  # S'assurer que CO est positif avant la transformation Box-Cox
  if (any(gt_combined$CO <= 0)) {
    gt_combined$CO <- gt_combined$CO + abs(min(gt_combined$CO)) + 1
  }
  
  # Appliquez la transformation Box-Cox
  boxcox_trans <- boxcox(lm(CO ~ ., data = gt_combined))
  lambda <- boxcox_trans$x[which.max(boxcox_trans$y)]  # Trouver la valeur lambda optimale
  
  # Appliquer la transformation Box-Cox en fonction du lambda calculé
  gt_combined$CO <- if (lambda == 0) {
    log(gt_combined$CO)
  } else {
    (gt_combined$CO^lambda - 1) / lambda
  }
}



# 6. Ajouter des termes quadratiques ou interactions
gt_combined$TIT_squared <- gt_combined$TIT^2
gt_combined$AT_TIT <- gt_combined$AT * gt_combined$TIT

# 7. Division des données en ensembles d'entraînement et de test pour la régression linéaire
set.seed(123)
train_indices <- sample(1:nrow(gt_combined), 0.8 * nrow(gt_combined))
train_data <- gt_combined[train_indices, ]
test_data <- gt_combined[-train_indices, ]

# 8. Standardiser le jeu de données en utilisant les statistiques des données d'entraînement
numeric_columns <- names(gt_combined)[sapply(gt_combined, is.numeric)]

# Calculer la moyenne et l'écart type à partir des données d'entraînement
means <- sapply(train_data[, numeric_columns], mean)
sds <- sapply(train_data[, numeric_columns], sd)

# Appliquer la standardisation aux ensembles d'entraînement et de test
train_data[, numeric_columns] <- scale(train_data[, numeric_columns], center = means, scale = sds)
test_data[, numeric_columns] <- scale(test_data[, numeric_columns], center = means, scale = sds)

# Vérifier les données standardisées
summary(train_data)

# ----------------- Analyse statistique exploratoire et tests -----------------------------------
# 1. Analyse de corrélation:
# 1.1 Corrélation de Pearson:
correlation_matrix <- cor(train_data[, numeric_columns], use = "complete.obs")
print("Matrice de corrélation de Pearson :")
print(correlation_matrix)
png(filename = paste0(plot_path, "pearson_correlation.png"))
heatmap(correlation_matrix, 
        main = "Matrice de corrélation de Pearson", 
        col = colorRampPalette(c("white", "lightblue"))(100),
        scale = "none",  # Pas de mise à l'échelle des valeurs
        cexRow = 0.8, cexCol = 0.8)
dev.off()

# 1.2 Corrélation de Spearman:
spearman_correlation <- cor(train_data[, numeric_columns], method = "spearman")
print("Matrice de corrélation de Spearman :")
print(spearman_correlation)
png(filename = paste0(plot_path, "spearman_correlation.png"))
heatmap(spearman_correlation, 
        main = "Matrice de corrélation de Spearman", 
        col = colorRampPalette(c("white", "lightblue"))(100),
        scale = "none",  # Pas de mise à l'échelle des valeurs
        cexRow = 0.8, cexCol = 0.8)
dev.off()

# 1.3 Nuages de points pour CO par rapport aux autres variables :
for (var in numeric_columns) {
  if (var != "CO") {
    # Créez le graphique pour chaque variable
    plot <- ggplot(gt_combined, aes(x = .data[[var]], y = CO)) +
      geom_point() +
      geom_smooth(method = "lm", col = "red") +
      ggtitle(paste("Scatterplot of CO vs", var)) +
      xlab(var) +
      ylab("CO") +
      theme_minimal()
    
    # Enregistrer le graphique dans un fichier PNG avec un fond blanc
    ggsave(filename = paste0("scatterplot_CO_vs_", var, ".png"),
           plot = plot,
           path = plot_path,
           width = 8, height = 6,
           bg = "white")  # Définit un fond blanc
  }
}
# ----------------- Tests statistiques -----------------------------------
# 2.1 T-Tests
# Test t indépendant pour CO entre deux groupes
gt_combined$FailureCategory <- ifelse(gt_combined$CO > mean(gt_combined$CO), "High", "Low")
t_test_result <- t.test(CO ~ FailureCategory, data = gt_combined)
print(t_test_result)

# Graphique pour le test t (Boxplot pour CO par catégorie de défaillance)
ggplot(gt_combined, aes(x = FailureCategory, y = CO)) + 
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Boxplot de CO par catégorie de défaillance") +
  xlab("Catégorie de défaillance") +
  ylab("CO")

# 2.2 Test du Chi-Carré
contingency_table <- table(gt_combined$FailureCategory, gt_combined$NOX > median(gt_combined$NOX))
chi_sq_test <- chisq.test(contingency_table)
print(chi_sq_test)

# Graphique pour le test du Chi-Carré (Barplot pour Catégorie de défaillance et NOX > médiane)
ggplot(as.data.frame(contingency_table), aes(x = Var1, fill = Var2)) +
  geom_bar(position = "dodge") +
  ggtitle("Test du Chi-Carré : Catégorie de défaillance vs NOX > médiane") +
  xlab("Catégorie de défaillance") +
  ylab("Fréquence") +
  scale_fill_manual(values = c("lightblue", "lightgreen"))

# 2.3 Test de Kruskal-Wallis
kruskal_test <- kruskal.test(CO ~ FailureCategory, data = gt_combined)
print(kruskal_test)

# Graphique pour le test de Kruskal-Wallis (Boxplot pour CO par catégorie de défaillance)
ggplot(gt_combined, aes(x = FailureCategory, y = CO)) + 
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Boxplot de CO par catégorie de défaillance (Test de Kruskal-Wallis)") +
  xlab("Catégorie de défaillance") +
  ylab("CO")


#--------------------------Regression Linéaire---------------------------------
# Modèle de régression par étapes.
stepwise_model <- step(
  lm(CO ~ GTEP + AT + AP + AH + AFDP + TAT + TEY + NOX, data = train_data), 
  direction = "both",
  trace = FALSE  # Suppress detailed output
)
summary(stepwise_model)

# Diagnostics des résidus pour le modèle par étapes
Residuals_stepwise <- resid(stepwise_model)

# Tracer les résidus par rapport aux valeurs ajustées avec couleurs
png(filename = paste0(plot_path, "Stepwise_Fitted_vs_Residuals.png"))
plot(stepwise_model$fitted.values, Residuals_stepwise, 
     main = "Residuals vs Fitted Values for Stepwise Model", 
     xlab = "Fitted Values", ylab = "Residuals",
     col = "lightblue", pch = 16)  # Points colorés en bleu clair
abline(h = 0, col = "red")  # Ligne de référence en rouge
dev.off()

# Tracer le graphique QQ pour vérifier la normalité des résidus avec couleurs
png(filename = paste0(plot_path, "Stepwise_QQ_Plot.png"))
qqnorm(Residuals_stepwise, col = "lightblue")  # Points colorés en bleu clair
qqline(Residuals_stepwise, col = "red")  # Ligne QQ en rouge
dev.off()

# Test de Breusch-Pagan pour l'hétéroscédasticité
#bptest_stepwise <- bptest(stepwise_model)
#print(bptest_stepwise)

#---------------------------------- ANOVA Analysis--------------------------------------

# 1. Analyse ANOVA pour le modèle à sélection pas à pas (Stepwise Model)
anova_results <- anova(stepwise_model)
print(anova_results)  # Les résultats montrent que toutes les variables sélectionnées ont un effet significatif sur CO.

# Graphique de l'ANOVA pour le modèle pas à pas
png(filename = paste0(plot_path, "Stepwise_ANOVA.png"))
boxplot(CO ~ fitted(stepwise_model), data = train_data, 
        main = "ANOVA: Modèle à sélection pas à pas", 
        xlab = "Valeurs ajustées", ylab = "CO", 
        col = "lightblue")
dev.off()

# 4. Comparer plusieurs modèles à l'aide de l'ANOVA
anova_results_multiple <- anova(anova_model_1, anova_model_2, anova_model_3, anova_model_4, anova_model_5)
print(anova_results_multiple)  # Display ANOVA results for multiple models.

# Graphique de l'ANOVA pour chaque modèle comparé
png(filename = paste0(plot_path, "Multiple_Model_ANOVA.png"))
par(mfrow = c(1, 5))  # Plusieurs graphiques sur une ligne
models <- list(anova_model_1, anova_model_2, anova_model_3, anova_model_4, anova_model_5)
for (i in 1:5) {
  boxplot(CO ~ fitted(models[[i]]), data = train_data,
          main = paste("Modèle", i), xlab = "Valeurs ajustées", ylab = "CO",
          col = "lightblue")
}
dev.off()

# 5. Analyse ANOVA pour chaque variable prédictive
variables <- c("AP", "AH", "TAT", "AFDP")
for (var in variables) {
  # Check if variable can be used in Bartlett's test
  if (length(unique(gt_combined[[var]])) > 10) {
    print(paste("Variable", var, "has too many unique values. Binning it for Bartlett's test."))
    gt_combined[[var]] <- cut(gt_combined[[var]], breaks = 10)  # Bin the variable
  }
  
  # Check for sufficient group sizes
  if (all(table(gt_combined[[var]]) >= 2)) {
    # Bartlett test
    bartlett_test <- bartlett.test(as.formula(paste("CO ~ as.factor(", var, ")")), data = gt_combined)
    print(paste("Bartlett test for", var, ":"))
    print(bartlett_test)
    
    # Graphique pour chaque test de Bartlett
    png(filename = paste0(plot_path, var, "_Bartlett_Test.png"))
    boxplot(CO ~ as.factor(gt_combined[[var]]), data = gt_combined, 
            main = paste("Bartlett Test for", var), 
            xlab = var, ylab = "CO", col = "lightblue")
    dev.off()
  } else {
    print(paste("Skipped Bartlett test for", var, "due to insufficient group sizes."))
  }
}

