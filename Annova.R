#**********Etape 1 : Analyse des donn�es*************
#chargement des bases de donn�es 

# Charger le fichier AI4I 2020
ai4i2020 = read.table(file = file.choose(), header = TRUE, sep = ",", stringsAsFactors = TRUE)
str(ai4i2020)
# on a 14 variables  et 10000 observations
#Interpr�tation des variables
#UDI (Unique Data Identifier) : int
#Product.ID : Factor avec 10 000 niveaux , Type : Facteur (chaque niveau repr�sente un produit unique).
#Type : Factor avec 3 niveaux ("H", "L", "M"),Type : Facteur (cat�gorie),"H" : Haute qualit�,"M" : Moyenne qualit�,"L" : Basse qualit�
#Air.temperature..K. : num ,Utilisation : Analyser l'influence de la temp�rature ambiante sur les performances ou d�faillances
#Process.temperature..K. : num , Utilisation : Variable ind�pendante pour �tudier son effet sur d'autres mesures.
#Rotational.speed..rpm. : int , Description : Vitesse de rotation de la machine en tours par minute,Utilisation : Indicateur cl� de la machine
#Torque..Nm. : num , Description : Couple (force de rotation) mesur� en Newton-m�tres,Utilisation : Comprendre comment le couple influence la d�faillance
#Tool.wear..min. : int,Description : Temps d'usure de l'outil, mesur� en minutes,Utilisation : Indicateur de la dur�e de vie de l'outil
#Machine.failure : int (binaire),Description : Indique si une machine a connu une d�faillance,0 : Pas de d�faillance/1 : D�faillance ,Utilisation : Variable cible pour la pr�diction des d�faillances
#TWF, HDF, PWF, OSF, RNF : int (binaires)
#Description : Indicateurs sp�cifiques de types de d�faillances :
#TWF : Usure de l'outil.
#HDF : Dissipation thermique.
#PWF : Probl�me d'alimentation.
#OSF : Surcharge m�canique.
#RNF : D�faillance al�atoire
#Utilisation : Identifier les causes sp�cifiques des �checs machines
#R�sum� des Variables :
#Quantitatives : Temp�ratures, vitesse de rotation, couple, usure de l'outil
#Qualitatives : Type de produit et indicateurs de d�faillance

# Charger le fichier GT 2011
gt_2011 = read.table(file = file.choose(), header = TRUE, sep = ",", stringsAsFactors = TRUE)
str(gt_2011)
# on a 11 variabes et 7411 observations avec toutes les variables sont numeriques





#**************Etape 2 :Nettoyae des donn�es****************

#V�rifier les valeurs manquantes
#pour la bd AI4I 2020
colSums(is.na(ai4i2020))
# il n'y a pas des valeurs manquantes pour cette bd 

#pour la bd GT 2011
colSums(is.na(gt_2011))
# il n'y a pas des valeurs manquantes pour cette bd 

#V�rifier les doublons
sum(duplicated(ai4i2020)) 
sum(duplicated(gt_2011)) 

#on ne d�t�cte aucune valeur dupliqu�e
#Corriger les erreurs dans les donn�es
#Identifier les valeurs aberrantes (outliers)
# Utiliser un boxplot pour identifier les valeurs aberrantes
attach(ai4i2020)
boxplot(ai4i2020)
attach(gt_2011)
boxplot(gt_2011)
#il existe des valeurs abberantes dans les deux bases de donn�es
#on va supprimer ces valeurs abberantes
# Appliquer l'IQR pour supprimer les outliers sur plusieurs colonnes
#pour la bd ai4i2020

# Cr�er une copie propre des donn�es
# Cr�er une copie des donn�es pour nettoyage
ai4i2020_cleaned = ai4i2020
gt_2011_cleaned = gt_2011

#Fonction pour traiter les outliers

library(ggplot2)
library(gridExtra)
create_boxplot_with_outlier_handling_2 <- function(data, var) {
  x <- data[[var]]
  
  prev_mean <- mean(x, na.rm = TRUE)
  
  for (i in 1:7) {  # Allow up to 20 iterations
    # Calculate Q1, Q3, IQR, and bounds
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Identify outliers
    outliers <- which(x < lower_bound | x > upper_bound)
    
    if (length(outliers) == 0) break  # Exit if no outliers are left
    
    # Replace outliers with the mean of non-outliers
    mean_value <- mean(x[x >= lower_bound & x <= upper_bound], na.rm = TRUE)
    x[outliers] <- mean_value
    
    # Check if the change in the mean is too small
    current_mean <- mean(x, na.rm = TRUE)
    if (abs(current_mean - prev_mean) < 0.01) {  # Threshold for change in mean
      cat(sprintf("Iteration %d: Mean stabilized at %.2f\n", i, current_mean))
      break
    }
    prev_mean <- current_mean
    cat(sprintf("Iteration %d: Mean of %s = %.2f\n", i, var, current_mean))
  }
  
  # Replace the data column with the cleaned data
  data[[var]] <- x
  
  # Final summary
  cat(sprintf("\nSummary of %s after outlier handling:\n", var))
  print(summary(data[[var]]))
  cat("\n")
  
  # Create the final boxplot without red outliers
  p <- ggplot(data, aes_string(y = var)) +
    geom_boxplot(fill = "lightblue", color = "black", outlier.shape = NA) +  # Hide outliers
    labs(title = paste("Boxplot of", var, "(Outliers Removed)"), y = var) +
    theme_minimal()
  
  return(list(plot = p, data = data))
}


#-------------- Outliers to gt_2011-----------------
plots <- list()
# Define the variables with outliers to process
data_with_outliers <- c("AP", "AH", "AFDP", "TIT", "TAT", "TEY", "CO", "NOX")
# Loop through selected variables, updating the data and creating boxplots
for (var in data_with_outliers) {
  result <- create_boxplot_with_outlier_handling_2(gt_2011, var)
  gt_2011_cleaned <- result$data  # Store the cleaned data
  plots[[var]] <- result$plot   # Add the generated plot to the plots list
}

# Display the plots in a grid layout (adjust the number of columns as needed)
do.call(grid.arrange, c(plots, ncol = 2))  # Display two plots per row

dim(gt_2011_cleaned);


#-------------- Outliers to ai4i2020-----------------
plots_ai4i2020 <- list()
# Define the variables with outliers to process
ai4i2020_with_outliers <- c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.", "Machine.failure", "TWF", "HDF" , "PWF", "OSF" , "RNF" )
# Loop through selected variables, updating the data and creating boxplots
for (var in ai4i2020_with_outliers) {
  result <- create_boxplot_with_outlier_handling_2(ai4i2020, var)
  ai4i2020_cleaned <- result$data  # Store the cleaned data
  plots_ai4i2020[[var]] <- result$plot   # Add the generated plot to the plots list
}

# Display the plots in a grid layout (adjust the number of columns as needed)
do.call(grid.arrange, c(plots_ai4i2020, ncol = 2))  # Display two plots per row

dim(ai4i2020_cleaned);



#**********Etape 3 : Analyse de correlation*************
library(corrplot)
library(caret)


#-------------- Correlation to gt_2011-----------------
# Compute correlation matrix
cor_matrix <- cor(gt_2011_cleaned, method = "pearson", use = "complete.obs")
# Identify highly correlated features
high_corr <- findCorrelation(cor_matrix, cutoff = 0.8)
# Remove highly correlated features from the dataset
selected_data <- gt_2011_cleaned[, -high_corr]

# Calculate the correlation matrix
cor_matrix <- cor(selected_data, use = "complete.obs", method = "pearson")
windows();

# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.7, tl.col = "black", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         title = "Correlation Matrix for Data after Feature Selection", 
         mar = c(0, 0, 1, 0))

# Nouvelle data apres features selection
gt_2011_cleand_2 <- selected_data
head(gt_2011_cleand_2)
dim(gt_2011_cleand_2)

#-------------- Correlation to ai4i2020-----------------

# Charger les packages nécessaires
library(caret)
library(corrplot)

# Étape 1 : Sélectionner uniquement les colonnes numériques
ai4i2020_numeric <- ai4i2020_cleaned[, sapply(ai4i2020_cleaned, is.numeric)]

# Vérifier qu'il reste des colonnes numériques
if (ncol(ai4i2020_numeric) == 0) {
  stop("Aucune colonne numérique trouvée dans le dataset.")
}

# Étape 2 : Supprimer les colonnes constantes
ai4i2020_numeric <- ai4i2020_numeric[, sapply(ai4i2020_numeric, function(x) length(unique(x)) > 1)]

# Étape 3 : Calculer la matrice de corrélation
cor_matrix_ai4i <- cor(ai4i2020_numeric, method = "pearson", use = "complete.obs")

# Étape 4 : Identifier les colonnes fortement corrélées (corrélation > 0.8 ou < -0.8)
high_corr_ai4i <- findCorrelation(cor_matrix_ai4i, cutoff = 0.8)

# Étape 5 : Supprimer les colonnes fortement corrélées
ai4i2020_reduced <- ai4i2020_numeric[, -high_corr_ai4i]

# Étape 6 : Recalculer la matrice de corrélation après nettoyage
cor_matrix_ai4i_reduced <- cor(ai4i2020_reduced, method = "pearson", use = "complete.obs")

# Étape 7 : Visualiser la matrice de corrélation
# Ouvrir une nouvelle fenêtre graphique pour Windows (si applicable)
if (.Platform$OS.type == "windows") windows()

# Tracer la matrice de corrélation
corrplot(cor_matrix_ai4i_reduced, 
         method = "color", 
         type = "upper", 
         tl.cex = 0.7, 
         tl.col = "black", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         title = "Correlation Matrix for AI4I 2020 Data after Feature Selection", 
         mar = c(0, 0, 1, 0))




# Aperçu des premières lignes et dimensions
head(ai4i2020_reduced)
dim(ai4i2020_reduced)


#-------------- standardization to gt_2011-----------------

gt_2011_standardized <- scale(gt_2011_cleand_2)
head(gt_2011_standardized)
gt_2011_standardized_2 <- as.data.frame(gt_2011_standardized)

attach(gt_2011_standardized_2)
str(gt_2011_standardized_2)

#-------------- standardization to ai4i2020-----------------
# Réintégrer la variable `Type` avant la standardisation
ai4i2020_reduced$Type <- ai4i2020_cleaned$Type

# Sélectionner les variables numériques pour la standardisation
ai4i2020_numeric <- ai4i2020_reduced[, sapply(ai4i2020_reduced, is.numeric)]

# Récupérer les noms des colonnes avant la standardisation
column_names <- colnames(ai4i2020_numeric)

# Standardisation des variables numériques uniquement
ai4i2020_standardized <- scale(ai4i2020_numeric)

# Convertir le résultat de `scale()` en data frame pour conserver les noms de colonnes
ai4i2020_standardized <- as.data.frame(ai4i2020_standardized)

# Réaffecter les noms des colonnes après la conversion en data frame
colnames(ai4i2020_standardized) <- column_names

# Réintégrer la colonne `Type` dans le dataset standardisé
ai4i2020_standardized$Type <- ai4i2020_reduced$Type

# Vérifier les données après la standardisation
head(ai4i2020_standardized)
str(ai4i2020_standardized)


#**********Etape 4 : Régression lineaire*************

regression_model_CO = lm(CO ~.-NOX, data = gt_2011_standardized_2)
summary(regression_model_CO)
AIC(regression_model_CO)
Residus_CO=resid(regression_model_CO)
plot(Residus_CO)
qqnorm(Residus_CO);qqline(Residus_CO)
#On peut confirmer la normalité

regression_model_NOX = lm(NOX ~.-CO, data = gt_2011_standardized_2)
summary(regression_model_NOX)
AIC(regression_model_NOX)
# R^2=0.6909: 69% de l'emission de NOX est expliquée en fonction des autres variables linéairement
# un bon modèle de régression linéaire
# Retirer les variables non significatives
# on commence par éliminer les variables avec pvalue>0.5
# On va eliminer la variable AP
regression_model_NOX_2 = lm(NOX ~.-CO-AP, data = gt_2011_standardized_2)
summary(regression_model_NOX_2)
# R^2 n'a pas changé => AP est non significative

# Critère AIC: Crière pour comparer entre deux
# modèles/ Le modèle avec AIC  le plus faible
# est le meilleur modèle
AIC(regression_model_NOX) 
# AIC=12344.98
AIC(regression_model_NOX_2)
# AIC=12343.05
# AIC(regression_model_NOX_2)<AIC(regression_model_NOX)
# Le deuxième modèle est meilleur que le premier

#Représentation graphique des residus 
Residus_NOX=resid(regression_model_NOX_2)
plot(Residus_NOX)
qqnorm(Residus_NOX);qqline(Residus_NOX)
#On peut confirmer la normalité




#**********Etape 5 : ANOVA*************

str(ai4i2020_standardized_2)
#Tester l'homogénité des variances 
bartlett.test(Air.temperature..K.~Machine.failure,ai4i2020_cleaned)
# p-value = 0.9562>0.05 => on accepte H0 => les variances sont egales
Analyse=aov(Air.temperature..K.~Machine.failure,data=ai4i2020_cleaned)
summary(Analyse)
boxplot(Air.temperature..K.~Machine.failure,data=ai4i2020_cleaned)
#Pvalue=2e-16<0.05 => on accepte H1 => il ya une differance significative 
====> Air.temperature influence la défaillance de la machine

#Tester l'homogénité des variances 
bartlett.test(Process.temperature..K.~Machine.failure,ai4i2020_cleaned)
# p-value = 0.0007342<0.05 => on accepte H1 => les variances ne sont pas
#egales
#Verifier si les moyennes sont egales 
Analyse2=aov(Process.temperature..K.~Machine.failure,data=ai4i2020_cleaned)
summary(Analyse2)
boxplot(Process.temperature..K.~Machine.failure,data=ai4i2020_cleaned)
plot(TukeyHSD(Analyse2))
#Pvalue=2e-16<0.05 => on accepte H1 => il ya une differance significative 
====> Process.temperature influence la défaillance de la machine

#Tester l'homogénité des variances 
bartlett.test(Rotational.speed..rpm.~Machine.failure,ai4i2020_cleaned)
# p-value = 1.712e-11<0.05 => on accepte H1 => les variances ne sont pas
#egales
Analyse3=aov(Rotational.speed..rpm.~Machine.failure,data=ai4i2020_cleaned)
summary(Analyse3)
boxplot(Rotational.speed..rpm.~Machine.failure,data=ai4i2020_cleaned)
#Pvalue=2e-16<0.05 => on accepte H1 => il ya une differance significative 
====> Rotational.speed influence la défaillance de la machine

#Tester l'homogénité des variances 
bartlett.test(Torque..Nm.~Machine.failure,ai4i2020_cleaned)
# p-value = 0.6119>0.05 => on accepte H0 => les variances sont egales
Analyse4=aov(Torque..Nm.~Machine.failure,data=ai4i2020_cleaned)
summary(Analyse4)
boxplot(Torque..Nm.~Machine.failure,data=ai4i2020_cleaned)
plot(TukeyHSD(Analyse4))
#Pvalue=2e-16<0.05 => on accepte H1 => il ya une differance significative 
====> Torque influence la défaillance de la machine

Tester l'homogénité des variances 
bartlett.test(Tool.wear..min.~Machine.failure,ai4i2020_cleaned)
# p-value = 0.00391<0.05 => on accepte H1 => les variances ne sont pas
#egales
Analyse5=aov(Tool.wear..min.~Machine.failure,data=ai4i2020_cleaned)
summary(Analyse5)
boxplot(Tool.wear..min.~Machine.failure,data=ai4i2020_cleaned)
plot(TukeyHSD(Analyse5))
#Pvalue=2e-16<0.05 => on accepte H1 => il ya une differance significative 
====> Tool.wear influence la défaillance de la machine










































