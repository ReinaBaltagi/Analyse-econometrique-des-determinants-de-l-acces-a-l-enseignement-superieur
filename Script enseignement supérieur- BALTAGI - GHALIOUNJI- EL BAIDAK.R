#Nettoyer l’environnement de travail
rm(list=ls())  

setwd("C:/Users/dell/Desktop/econometrie")

#Charger les packages necessaires
install.packages('brant')
install.packages('ordinal')
install.packages("pROC")
install.packages("pscl")
install.packages("marginaleffects")
library(haven)
library(MASS)    # polr()
library(broom)   # mise en forme des résultats
library(brant)   #test de proportionnalité (optionnel)
library(ordinal)
library(marginaleffects)
library(ggplot2)
library(pscl)
library(pROC)

#Importer la base de données 
df<- read_dta("3_Etudes_sup.dta")

##### exploration initial de la base de données

#Afficher le nombre de lignes
nrow(df)

#Afficher le nombre de colonnes
ncol(df)

#Afficher les noms des colonnes
names(df)

# Aperçu des premières lignes
head(df)

###### nettoyage de la base de données 

#Vérifier les valeurs manquantes par colonne
colSums(is.na(df))

#Vérifier les types des variables
sapply(df, class)

#Vérifier les valeurs uniques de chaque variable 
#pour s'assurer qu'elles ne contiennent pas de valeurs inattendues
unique(df$pared)
unique(df$public)
unique(df$apply)

#Convertir apply en facteur ordonné 
df$apply<-factor(df$apply,levels = c(0,1,2),
labels=c("unlikely","somewhat likely","very likely"),
ordered=TRUE
)

#Convertir les variables binaires en facteur simple
df$pared<-as.factor(df$pared)
df$public<-as.factor(df$public)

#Assurer que gpa est bien une valeur numérique continue
df$gpa<-as.numeric(df$gpa)

#Vérifier les types des variables aprés la conversion
sapply(df, class)

#Ces conversions sont necessaire pour une modélisation correcte.

#Sauvergarder la version propre du dataset au format rds
saveRDS(df,"df_clean.rds")

#Importer la version propre du dataset.
df_clean<-readRDS("df_clean.rds")

###############################------- Statistiques descriptives -------###############################

#On crée des tableaux synthétiques pour les variables avec modalités 
#afin de visualiser, pour chaque catégorie, le nombre d’observations (effectif) 
#et leur part dans l’échantillon (pourcentage)

#---- section apply 

table_apply<-table(df_clean$apply)
apply_prop<-round(prop.table(table_apply)*100,1)

summary_apply<-data.frame(
  Niveau=names(table_apply),
  Effectif=as.vector(table_apply),
  Pourcentage=as.vector(apply_prop)
)
print(summary_apply) #tableau synthètique

ggplot(df_clean,aes(x=apply))+
  geom_bar(fill="#BFD8B8")+
  scale_x_discrete(labels=c("unlikely"="Improbable",
                            "somewhat likely"="Peu probable",
                            "very likely"="Très probable"))+
  labs(title="Probabilité perçue d’accès aux études supérieures",
       x="Probabilité (apply)",
       y="Effectif")+
  theme_minimal() #Visualisation de la distribution de la variable apply

#---- section gpa

ggplot(df_clean,aes(x=gpa))+
  geom_histogram(binwidth = 0.5,fill="#BFD8B8",color="gray40")+
  labs(title="Distribution du GPA des étudiants",
       x="GPA",
       y="Effectif")+
  theme_minimal() #Visualisation de la distribution de la variable gpa

#---- Section pared 

table_pared<-table(df_clean$pared)
pared_prop<-round(prop.table(table_pared)*100,1)

summary_pared<-data.frame(
  Niveau=names(table_pared),
  Effectif=as.vector(table_pared),
  Pourcentage=as.vector(pared_prop)
)

print(summary_pared) #tableau synthètique


ggplot(df_clean,aes(x = pared))+
  geom_bar(fill="#BFD8B8")+
  scale_x_discrete(labels=c("0"="Pas diplomé","1"="Diplomé"))+
  labs(title = "Répartition des étudiants selon le niveau d’études des parents",
       x="Parent diplomé",
       y="Effectif")+
  theme_minimal() #Visualisation de la distribution de la variable pared


#---- Section public

table_public<-table(df_clean$public)
public_prop<-round(prop.table(table_public)*100,1)

summary_public<-data.frame(
  Niveau=names(public_prop),
  Effectif=as.vector(table_public),
  Pourcentage=as.vector(public_prop)
)

print(summary_public) #tableau synthètique

ggplot(df_clean,aes(x = public))+
  geom_bar(fill="#BFD8B8")+
  scale_x_discrete(labels=c("0"="Privé","1"="Public"))+
  labs(title = "Type d’établissement fréquenté par les étudiants",
       x="Type de l'établissement",
       y="Effectif")+
  theme_minimal() #Visualisation de la distribution de la variable public

##### Statistique croisée : 

#Visualisation du GPA selon les catégories de apply (boxplot)

ggplot(df_clean,aes(x = apply,y = gpa))+
  geom_boxplot(fill="#BFD8B8")+
  labs(title = "",
       x="Probabilité (apply)",
       y="GPA")+
  theme_minimal() 

#ici, on vérifie avec un test ANOVA si la moyenne du GPA varie selon les modalités de la variable apply
summary(aov(gpa~apply,data = df_clean))
# le résultat du test est p < 0.05 donc on rejette l’hypothèse nulle 
# les moyennes de gpa varient significativement selon les catégories de apply

#Visualisation de la répartition relative de pared selon apply

ggplot(df_clean,aes(x = apply,fill = pared))+
  geom_bar( position = "fill" )+
  scale_fill_manual(values=c("0"="#BFD8B8","1"="#C3B4D2"),
                    labels=c("0"="Pas diplomé","1"="Diplomé"))+
  scale_y_continuous(labels=scales::percent)+
  labs(title = 'Répartition des étudiants selon Apply et Pared',
       x="Probabilité (apply)",
       y="Parent diplomé (pared)")+
  theme_minimal() 


#Visualisation de la répartition relative de public selon apply

ggplot(df_clean,aes(x = apply,fill = public))+
  geom_bar( position = "fill" )+
  scale_fill_manual(values=c("0"="#BFD8B8","1"="#C3B4D2"),
                    labels=c("0"="Privé","1"="Public"))+
  scale_y_continuous(labels=scales::percent)+
  labs(title = 'Répartition des étudiants selon Apply et Public',
       x="Probabilité (apply)",
       y="Parent diplomé (public)",
       fill = "Établissement d'origine (public)")+
  theme_minimal()

###############################-------  MODELISATION  -------###############################

#####Model null 
modelnull <- clm(apply ~ 1,
              data = df_clean,
              link = "logit")
summary(modelnull)


#####Modèle 1 avec uniquement comme variable dépendante les notes 

#####Model null 
model1 <- clm(apply ~ gpa,
                 data = df_clean,
                 link = "logit")
summary(model1)
#GPA (la note obtenu par l'élève) semble significativement contribuer à l'accès à l'éducation supérieure de l'élève

#On compare entre le modèle nulle et le premier modèle et on constate déjà une amiélioration de l'AIC
anova(modelnull, model1)

###### Modèle 2: GPA +éducation des parent 
model2 <- clm(apply ~ gpa +pared,
              data = df_clean,
              link = "logit")
summary(model2)
#L'effet du GPA semble avoir été réduit avec l'introduction de la variable de l'éducation des parents 


####Modèle 3: GPA + éducation des parents + le fait d'être dans une école privée ou non
###### Modèle 2: GPA +éducation des parent 
model3 <- clm(apply ~ gpa +pared+public,
              data = df_clean,
              link = "logit")
summary(model3)
#le fait d'être dans une école privée ne semble pas être pertinent 


anova(model1, model2, model3)
#Selon le critère de l'AIC, le modèle 2 semble être le modèle le plus pertinent 

#Nous avions decider de tester l'interaction entre les deux variables clés de notre modèle 2 (pared et GPA) pour savoir si les résultats s'amélioreront 
model_interaction <- polr(apply ~ gpa * pared + public, data = df_clean, Hess = TRUE)
AIC(model2, model_interaction)
anova(model2, model_interaction)
#Selon le critère de l'AIC on reste sur notre modèle 2 qui, ayant l'AIC le plus faible parait le plus pertinent

#On prend le deuxième modèle pour en calculer les effets marginaux

#test of parallel lines: brand test on veut qu'il ne soit pas snigificatif 
#On refait le même modèle jute avec un autre package
model2_test<- polr(apply ~ gpa +pared, data=df_clean, Hess= T)
summary(model2_test)

brant(model2_test)
#Parallel assumptions line hold (hypothèses  clés des modèles logit ordonnés)

#Effet marginaux
install.packages('marginaleffects')
avg_slopes (model2)


###############################------- Diagnostics & Graphiques  -------###############################


# Recalcul du modèle choisi (logit ordinal)
model2 <- polr(apply ~ gpa + pared, data = df_clean, Hess = TRUE)

### 5.1 - Proba prédite vs GPA (inspiré de la courbe logit ajustée)
#Ce graphique nous permet de visualiser la relation entre le GPA et la probabilité d’accéder à des études supérieures 
#(plus précisément, la probabilité que la réponse soit "very likely" dans la variable apply).

# La probabilité prédite que chaque individu soit classé comme "very likely" selon le modèle
df_clean$predicted_prob <- predict(model2, type = "probs")[, "very likely"]

ggplot(df_clean, aes(x = gpa, y = predicted_prob)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Proba prédite (very likely) en fonction du GPA",
       x = "GPA", y = "Probabilité prédite")
#Le graphique montre une tendance claire entre le GPA et la probabilité d'accéder aux études supérieures.
#Les étudiants avec une moyenne plus élevée (GPA) sont plus nombreux à être classés dans la catégorie "très probable".

### 5.2 - Les odds ratios (avec IC)
#Les Odds Ratio sous forme de tableau
# Calculer les OR et IC 95%
OR_table <- exp(cbind(
  OR = coef(model2),                        # Odds Ratio estimé
  confint(model2)                           # Intervalle de confiance
))

# Afficher la table 
print(round(OR_table, 3))
#Les deux variables présentent un résultat d'OR supérieur à 1, ce qui indique que les deux augmentent la probabilité d’accéder à une catégorie plus élevée (apply)
#Un GPA plus élevé augmente la probabilité d’être classé comme "very likely".
#Avoir au moins un parent diplômé favorise aussi la probabilité d'être classé comme "very likely".

#Forest Plot des Odds Ratios (avec IC)
# Ce code permet de visualiser graphiquement les effets estimés dans notre modèle, sous forme d’odds ratios avec leurs intervalles de confiance à 95%.
#Pour convertir les résultats du modèle en tableau propre facilitant ainsi la visualisation avec ggplot (il s'agit des même résultats que OR_table, mais d'une manière plus adapté à la visualisation graphique avec ggplot)
model_tidy <- tidy(model2, conf.int = TRUE, exponentiate = TRUE)

ggplot(model_tidy, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_x_log10() +
  labs(title = "Odds Ratios (avec IC à 95%)", x = "Odds Ratio", y = "Variables")
#Ce forest plot montre graphiquement ce qu'on a déjà vu dans le tableau des Odds Ratios à savoir, que le GPA et l’éducation parentale (pared) ont un effet positif et significatif sur la probabilité d’aspirer à des études supérieures.

### 5.3 - Qualité du modèle : AIC, pseudo-R², test du modèle
AIC(modelnull, model1, model2, model3, model_interaction)
#Plus l’AIC est bas, meilleur est l’ajustement du modèle en pénalisant la complexité inutile
#Le modèle 2 présente un AIC plus faible que les autres, ce qui indique une meilleure qualité d'ajustement.

pseudoR2 <- pR2(model2)
print(pseudoR2)

#Le calcul du Pseudo-R2 donne plusieurs indicateurs de qualité du modèle (McFadden, r2ML, r2CU) et permet d’évaluer la proportion de variance expliquée par le modèle
# Le pseudo-R² de McFadden est de 0.033, montrant une amélioration modeste mais non négligeable par rapport au modèle nul.
#Les autres indices (r2ML = 0.0586, r2CU = 0.0695) confirment que le modèle capte une part limitée mais significative de l'information. 

#Test du rapport de vraisemblance (Likelihood Ratio Test) (comparaison avec le modèle nul car c'est un test de validité)
#Ce test (basé sur une loi du Khi²) indique si l’ajout des variables explicatives améliore significativement le modèle
model_null <- polr(apply ~ 1, data = df_clean, Hess = TRUE)
anova(model_null, model2)
#Le test de vraisemblance est très significatif (p < 0.001), donc les variables gpa et pared améliorent clairement le modèle.
# Le test de vraisemblance montre que le modèle avec gpa et pared améliore significativement l’ajustement par rapport au modèle nul.

### 5.4 - Matrice de confusion et taux d’erreur
#La performance du modèle peut être évaluée via une matrice de confusion, qui permet de comparer les classes prédites par le modèle avec les classes réelles de la variable apply.
predicted_class <- predict(model2, type = "class")
conf_matrix <- table(Réel = df_clean$apply, Prédit = predicted_class)
print(conf_matrix)
#le modèle classe très bien les individus dans la catégorie "unlikely" (201 cas correctement prédits sur 220), mais échoue totalement à prédire correctement les cas "very likely" (0 prédiction correcte).
# Cela revient au fait que la matrice de confusion est pas très bien adaptée à un modèle ordinal car elle ne prends en compte que la classe avec la proba maximale 

taux_erreur <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Taux d’erreur de prédiction :", round(taux_erreur, 3), "\n")
#Cette classification a lieu avec un taux d'erreur de 42.2% 

#C'est pour cela on utilise le score de brier, particulièrement la version cumulative car elle est mieux adapté au modèle ordinal 
#On transforme les classes en format cumulatif (ex: "très probable" = 1 pour toutes les coupures)
true_cum <- t(apply(true_classes, 1, cumsum))

#Probabilités cumulés prédites
probs_cum <- t(apply(probs, 1, cumsum))

# Brier score cumulatif
brier_score_cum <- mean(rowSums((probs_cum - true_cum)^2))
print(brier_score_cum)
#Un score de 0.3217 de brier indique que le modèle prédit correctement les probabilités en tenant compte de l’ordre des catégories.

### 5.5 - Courbe ROC (option simplifiée : very likely vs autres)
#Il s'agit d'évaluer la capacité du modèle à prédire la classe "very likely" vs les autres

# Création d’une version binaire de la variable cible :
# 1 si "very likely", 0 sinon
df_clean$apply_binary <- ifelse(df_clean$apply == "very likely", 1, 0)

# Calcul de la courbe ROC, en  utilisant la probabilité prédite d'être "very likely"
roc_curve <- roc(df_clean$apply_binary, df_clean$predicted_prob)

#Affichage de la croube ROc
plot(roc_curve, main = "Courbe ROC – Very likely vs autres")
#Calcul et affichage de l'AUC (aire sous la courbe)
cat("AUC =", auc(roc_curve), "\n")
#L’AUC mesure la capacité du modèle à discriminer les "very likely" des autres.
#AUC = 0.623, il s'agit d'une capacité de discrimination modeste, mais meilleure que le hasard (0.5)
#Dans 62,3% des cas, le modèle attribue une probabilité plus élevée à un individu réellement "very likely" qu’à un autre qui ne l’est pas.
#Comme le ROC et l’AUC présentent aussi des limites pour un modèle ordinal, 
#nous avons décidé de présenter des boxplots qui comparent les probabilités prédites pour chaque modalité en fonction de la classe réelle
#afin d’évaluer visuellement la cohérence avec l’ordre des classes.

df_clean$prob_unlikely <- predicted_probs[, "unlikely"]
df_clean$prob_somewhat <- predicted_probs[, "somewhat likely"]
df_clean$prob_very <- predicted_probs[, "very likely"]

# Boxplot pour la probabilité prédite d'être "very likely"
ggplot(df_clean, aes(x = apply, y = prob_very)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Probabilité prédite d’être 'very likely' selon la classe réelle",
    x = "Classe réelle (apply)",
    y = "Probabilité prédite ('very likely')"
  ) +
  theme_minimal()

# Boxplot pour la probabilité prédite d'être "somewhat likely"
ggplot(df_clean, aes(x = apply, y = prob_somewhat)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Probabilité prédite d’être 'somewhat likely' selon la classe réelle",
    x = "Classe réelle (apply)",
    y = "Probabilité prédite ('somewhat likely')"
  ) +
  theme_minimal()

# Boxplot pour la probabilité prédite d'être "unlikely"
ggplot(df_clean, aes(x = apply, y = prob_unlikely)) +
  geom_boxplot(fill = "salmon") +
  labs(
    title = "Probabilité prédite d’être 'unlikely' selon la classe réelle",
    x = "Classe réelle (apply)",
    y = "Probabilité prédite ('unlikely')"
  ) +
  theme_minimal()
### 5.6 - Effets marginaux moyens
marg_effects <- avg_slopes(model2)
summary(marg_effects)
#plot(marg_effects) Ne marche pas à cause de la nouvelle version de R
#Visualisation pour GPA 

# Visualiser effet marginal de GPA
plot_slopes(
  model2,
  variables = "gpa",
  by = "pared"
) +
  labs(
    title = "Effet marginal de GPA selon le niveau d’éducation parentale",
    x = "GPA",
    y = "Effet marginal"
  )


#Visualisation pour Pared
plot_slopes(
  model2,
  variables = "pared",
  by = "pared"
) +
  labs(
    title = "Effet marginal de Pared (0 vs 1)",
    x = "Pared (0 = non diplômé, 1 = diplômé)",
    y = "Effet marginal estimé"
  )


### 5.7 - Test de l'hypothèse de ligne parallèle (Brant Test)
brant_test <- brant(model2)
print(brant_test)
# p > 0.05 = H0 non rejetée, l’hypothèse de lignes parallèles est respectée.
#le modèle est approprié pour expliquer la probabilité d’appartenance aux modalités ordonnées de la variable apply, et nous pouvons conserver donc ce modèle pour l’analyse.


