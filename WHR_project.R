# ########################################
#                                                              
#        WORLD HAPPINESS REPORT
#    "L'ARGENT FAIT-IL LE BONHEUR ?"
#           ----------------
#  Projet présenté par Ophélie ENGASSER  
#       IA School - Janvier 2023
#                                                              
# ########################################

# Ici nous avons à faire à des données qui rassemblent plusieurs études, afin d'en faire une méta-analyse :
# c'est-à-dire analyser les données statistiquement pour répondre à une problématique.
# Ici, notre méthode sera supervisée en grande partie, dans le sens où nous savons à l'avance sur quelles variables nous allons nous focaliser.
# Cela nous permettra de choisir nos modèles en conséquence.
# Nous allons également passer en revue certains modèles non supervisés.

# #######################################
#                                                              
# 0. LECTURE ET PREPARATION DES DONNEES
#                                                              
# #######################################

# =================================================
# Affichage et définition du répertoire de travail
# =================================================

getwd()
setwd("C:/Users/Engasser Ophélie/Desktop/WHR_Project")

# ======================
# Affichage des données 
# ======================

# lecture du dataset à partir de la fonction read.csv() et stockage dans une variable
raw_data = read.csv("worldHappinessReport.csv", header = TRUE)
View(raw_data)

# ================================================================
# Vérification de la structure du dataset et des types de données 
# ================================================================

dim(raw_data) 
# n = 147 avec 13 variables (la 1re est inutile et ne nous apporte pas d'info supplémentaire car nous avons 
# déjà formaté notre dataset avec un header : nous allons donc la supprimer dans la partie suivante)
class(raw_data) #data.frame
str(raw_data) # données de type numeric et character
colnames(raw_data)

class(raw_data$Happiness) # num
typeof(raw_data$Happiness) # la majorité des données sont numériques de type double (float) (idem pour les autres var. num)
typeof(raw_data$Country) # 2 variables sont des character, elles nous serviront d'indicateur de comparaison entre pays pour les différents scores (idem Region)

# bien que les données soient à l'origine discrètes (basées sur des scores d'individus à des échelles ou des questions binaires 0/1), 
# les scores ont été aggrégés en moyennes par pays, donc des valeurs décimales comprises entre 0 et 1 (sauf pour la variable Generosity qui est un résidu d'une 
# régression et comprend donc des valeurs négatives)
# Toutes les données numériques sont donc continues, ce qui va impacter le choix de nos analyses statistiques et modèles

# =====================
# Formatage du dataset
# =====================

# suppression de la colonne 1 et stockage dans une nouvelle variable qui constituera notre dataset de travail
df = raw_data[-1]
dim(df)
View(df)

# inspection plus fine des données (échelles, valeurs manquantes ou aberrantes)

# Happiness : théoriquement cette variable peut prendre les valeurs 0 à 10 (selon les résultats au questionnaire)
# dans l'étude les scores sont des moyennes et sont des réels (échelle continue)
str(Happiness) # type num
summary(Happiness) # pas de NA
boxplot(Happiness)$out # pas d'outlier

# GDP : déjà décrite avant
str(GDP) # type num
summary(GDP) # 4 NA
boxplot(GDP)$out # pas d'outlier

# SocialSupport : théoriquement cette variable peut être comprise entre 0 et 1 (selon les résultat à une question binaire NON=0 ou OUI=1) 
# échelle continue
str(SocialSupport) # type num
summary(SocialSupport) # 1 NA
boxplot(SocialSupport)$out # 2 outlier mais qui ne semblent pas aberrants puisqu'ils se situent dans les scores attendus et ne sont pas éloignés du Q3

# Health : il s'agit de l'espérance de vie donc les valeurs sont des âges sur une échelle numérique continue
str(Health) # type num
summary(Health) # 2 NA
boxplot(Health)$out # pas d'outlier

# Freedom : à nouveau théoriquement les scores peuvent se situer entre 0 et 1 ; ce sont des moyennes sur une échelle continue
str(Freedom) # type num
summary(Freedom) # 1 NA
boxplot(Freedom)$out # 3 outlier qui ne sont pas aberrants et restent dans les scores attendus théoriquement

# Generosity : scores pouvant être situés entre -1 et 1 car ils résultent de la moyenne des résidus d'une régression
str(Generosity) # type num
summary(Generosity) # 5 NA
boxplot(Generosity)$out # 2 outlier dont le max qui est beaucoup éloigné du Q3

# Corruption : scores initiaux entre 0 et 1, moyennés sur une échelle continue
str(Corruption) # type num
summary(Corruption) # 11 NA
boxplot(Corruption)$out # plusieurs outlier
length(boxplot(Corruption)$out) # 12 outlier qui se situent tous en dessous de Q1

# PositiveAffect : scores initiaux entre 0 et 1, moyennés sur une échelle continue
str(PositiveAffect) # type num
summary(PositiveAffect) # 1 NA
boxplot(PositiveAffect)$out # pas d'outlier

# NegativeAffect : scores initiaux entre 0 et 1, moyennés sur une échelle continue
str(NegativeAffect) # type num
summary(NegativeAffect) # 1 NA
boxplot(NegativeAffect)$out # 3 outlier mais non aberrants

# ConfidenceInGovernment : scores initiaux entre 0 et 1, moyennés sur une échelle continue
str(ConfidenceInGovernment) # type num
summary(ConfidenceInGovernment) # 13 NA
boxplot(ConfidenceInGovernment)$out # pas d'outlier

# nous avons pris la décision de ne pas supprimer les outliers car nous estimons qu'ils ne sont pas aberrants et restent dans les limites des scores théoriques
# peut-être qu'ils reflètent simplement les disparités entre les pays (le plus flagrant est sur la variable Corruption)
# pour le moment nous gardons les NA et prendrons une décision par la suite en fonction de chaque analyse

# ##########################
#                                                              
# A. REPORTING (Question 1)
#                                                              
# ##########################

install.packages("DataExplorer", dependancies = TRUE)
library(DataExplorer)
create_report(df)

# Ce reporting nous précise qu'il y a seulement 2.2% de données manquantes (le plus grand nb dans les variables Corruption et ConfidenceInGovernment)
# Il serait intéressant d'aller voir dans quels pays nous constatons le plus de NA dans ces variables-là : nous voyons qu'en Chine il y en a beaucoup
# ce qui est cohérent avec la réalité car la Chine est un pays ayant un régime dictatorial ne permettant pas aux individus de donner leur opinion
# La Corée du Nord, quant à elle, ne figure même pas dans le dataset !
# Du fait du petit pourcentage de NA, nous ne réalisons pas de traitement particulier (sauf nécessité).

# Les QQ-plot (quantiles théoriques vs. quantiles observés) nous renseignent, de même que les histogrammes, sur l'asymétrie de certaines distributions (notamment Corruption)

# La heat-map (matrice de corrélations) nous montre des corrélations positives entre plusieurs variables : citons celles qui sont supérieures à 0.6 :
# GDP-Health / GDP-Happiness / GDP-SocialSupport / Happiness-SocialSupport / Happiness-Health / SocialSupport-Health / Freedom-PositiveAffect

# à partir de ces premiers résultats, nous pourrions nous poser un axe de travail et répondre à une question : 
# "l'argent fait-il le bonheur ?" : il s'agit d'une question philosophique, mais à travers les données dont nous disposons, est-il possible de mesurer cela statistiquement ?
# Le "bien-être" est une notion complexe, elle fait généralement intervenir les concepts de prospérité, de santé et de bonheur.
# Le bien-être n'est pas chiffrable avec précision, et il n'est pas aisé de trouver l'indicateur idéal pour le mesurer, car cela est très subjectif.
# Dans cette étude, plusieurs indicateurs de "qualité de vie" (liberté, bonheur, sécurité, soutien...) vont nous permettre d'étudier cette problématique.
# Une variable est particulièrement intéressante à cerner, c'est le sentiment de bonheur perçu (Happiness). Elle peut constituer une variable dépendante (VD) pour nos analyses.
# Le GDP est un point intéressant également, qui peut être étudié en tant que variable indépendante (VI), dans la mesure où elle pourrait expliquer certains scores.
# Nous verrons en fonction de l'avancement de l'analyse quelles autres variables explicatives peuvent être retenues.

# ##########################
#                                                              
# B. STATISTIQUE UNIVARIEE
#                                                              
# ##########################

attach(df)

# =============
# Variable GDP
# =============

# signification (Question 2)

# il s'agit du Produit Intérieur Brut (PIB) à parité de pouvoir d'achat (PPA) par habitant : il s'agit d'un indicateur économique qui permet de comparer les niveaux de 
# richesse de différents pays, c'est donc un indicateur de développement. Mais ici il est nuancé par le PPA car une correction est réalisée qui tient compte du coût de la vie 
# qui est différent selon les pays. Il reflète donc davantage la réalité du niveau de vie de chaque pays et ne renvoie pas de valeurs biaisées comme le ferait le seul PIB.
# Son calcul est un ratio : PIB / nb moyen d'habitants d'un pays. Nous supposons que l'unité de mesure de cette variable est le Dollar Américain (en K$).
# Cet indicateur est intéressant pour comparer des pays (utile donc dans notre étude) mais ne dit rien des inégalités entre populations à l'intérieur d'un même pays.

# résumé statistique et calcul des coefficients de skewness et kurtosis (Question 3)

# paramètres de position et de dispersion 
summary(GDP)
# la moyenne du GDP des pays est de 9.28 environ
# 50% des pays ont une valeur de GDP > 9.49 (médiane)
# le min est de 6.49 et le max de 11.45
# 50% des pays ont un GDP entre 8.46 et 10.28 (intervalle inter-quartiles)
# 4 données sont manquantes

sd_GDP = sd(GDP, na.rm = TRUE)
coef_variation = sd_GDP/mean(GDP, na.rm = TRUE)*100
coef_variation # 12.9% ce qui reflète une homogénéité des données de cette variable

# nous pouvons représenter ce résumé statistique à l'aide d'un boxplot
boxplot(GDP)

# paramètres de forme
install.packages("moments")
library(moments) # pour les fonctions skewness() et kurtosis()

# visualisation de la forme de la distribution
hist(GDP, prob = TRUE)
lines(density(GDP, na.rm = TRUE), col = "red", lwd=3)
plot(density(GDP, na.rm = TRUE), lwd=3)

# asymétrie : skewness
# on observe une courbe où la densité est inégalement répartie autour de la moyenne (la majorité des données est à droite) : vérifions cela à l'aide du skewness
skewness(GDP, na.rm = TRUE)
# skew < 0 : dataset skewed à gauche, ce qui valide notre observation : cela s'interprète par le fait qu'il y a davantage de pays qui se trouvent au-dessus du GDP moyen

# nous pouvons le vérifier en filtrant les données
library(dplyr) # pour la manipulation des données
df%>%
  filter(GDP>9.28)
# 82 pays (56% env.) se situent au-dessus du GDP moyen (cela reste une faible majorité)

# aplatissement : kurtosis
kurtosis(GDP, na.rm = TRUE)
# kurt < 3 : la forme de la distribution est plus aplatie que la courbe de Gauss, moins concentrées autour de la moyenne 

# existe-il des données aberrantes ? (Question 4)
boxplot(GDP)$out
# il n'y a pas d'outlier dans cette variable, c'est-à-dire de données extrêmes ou anormales (en-dessous ou au-dessus de la distribution)
# cela signifie que dans cette étude, aucun pays ne dépasse les limites < ou > (min ou max)

# cette variable suit-elle la loi normale ? (Question 5)
library(stats) # pour la fonction shapiro.test()
shapiro.test(GDP)
# la statistique de test (W) est significative, donc on rejette H0, la variable n'est donc pas distribuée normalement, et nous pouvons affirmer cela avec
# un risque très faible (<.0005) de nous tromper
# cela signifie que d'autres facteurs que le hasard, interviennent dans la détermination du GDP dans chaque pays, ce qui est plutôt cohérent puisque 
# ce sont des facteurs économiques, sociaux, politiques, etc. qui influent sur le GDP d'un pays, et c'est justement ce que nous souhaitons étudier

# intervalle de confiance à 90% de la moyenne de GDP (Question 6)
t.test(GDP, conf.level=0.90) 
# cela signifie qu'il y a 90% de chances que la vraie moyenne de la variable GDP (la moyenne de la population) soit comprise entre 9.13 et 9.46
# il serait intéressant d'affiner notre analyse en calculant un IC pour les pays à haut GDP et pour les pays à faible GDP, afin de vérifier s'ils 
# se chevauchent ou non ; s'ils ne se chevauchent pas, la différence entre les moyennes des 2 groupes sera significative

# 10 pays ayant le GDP le plus élevé (Question 7)
# pour manipuler le dataset, convertissons-le en tibble car c'est la structure de donnée qui est propre à la librairie dplyr
df_tibble = as_tibble(df)
df_tibble%>%
  select(Country, GDP)%>%
  arrange(desc(GDP))%>%
  head(n=10)
# le Luxembourg arrive en tête avec un GDP de 11.45, suivi de Singapore à 11.36

# moyenne des GDP par régions (Question 8) : 2 méthodes :
# 1. méthode avec dplyr :
df_tibble%>%
  select(Region, GDP)%>%
  group_by(Region)%>%
  summarise(moyenne = mean(GDP, na.rm=TRUE))

# la commande ci-dessus a opéré un groupement des données par régions et de ce fait, une fonction d'agrégation est attendue :
# ici nous avons fait la moyenne

# 2. méthode "classique" :
moyennes_GDP_region = tapply(GDP, Region, mean, na.rm = TRUE)
moyennes_GDP_region

# graphique à barres :
barplot(moyennes_GDP_region, ylim = c(0, 15), main = "GDP moyen selon la région")

# le GDP moyen le plus élevé est : en Amérique du Nord (GDP moyen = 10.8), en Europe de l'Ouest (10.7) et en Australie-Nouvelle-Zélande (10.6), etc

# ================
# Variable Region
# ================

# étudier cette variable (Question 9)

# effectifs : nb d'observations (de pays) par régions : pour cela il suffit de compter le nombre de lignes dans chaque modalité, puisqu'il y a une ligne par pays
table(Region)

# fréquences : effectif / total d'observation (n)
table(Region)/length(Region)

# proportions : 3 méthodes qui renvoient les mêmes résultats : 
# 1. fréquence * 100 
(table(Region)/length(Region)) * 100

# 2. fonction freq()
install.packages("questionr")
library(questionr) # pour la fonction freq()
freq(Region)

# 3. fonction prop.table()
prop.table(table(Region)) * 100

# visualisation
pays_par_regions = prop.table(table(Region))
barplot(pays_par_regions)
pie(pays_par_regions)

# le mode de cette variable, c'est-à-dire la modalité où il y a le plus d'effectifs est l'Afrique Sub-Saharienne (35 pays y sont représentés, soit 23.8% des pays)
# il est intéressant de constater que ce sont dans les régions où il y a le moins de pays (et donc par déduction, le moins d'habitants), que 
# les GDP moyens observés ci-dessus sont les plus élevés : cela reflète le phénomène d'inégalité dans la répartition des richesses

# #########################################
#                                                              
# C. STATISTIQUE GENERALE ET INFERENTIELLE
#                                                              
# #########################################

# =======================================
# Question 10. 2 variables catégorielles
# =======================================

# nous avons d'abord réalisé un test d'indépendance entre les 2 uniques variables catégorielles du dataset, à savoir Country et Region, mais d'une part le test n'était pas significatif,
# et d'autre part cette comparaison nous a paru peu pertinente
# par conséquent nous avons manipulé le dataset pour transformer une variable continue en variable catégorielle
# nous nous sommes intéressée au sentiment de confiance dans le gouvernement (ConfidenceInGovernment) et à la Region

# transformation de la variable ConfidenceInGov en variable catégorielle (score <= 0.5 : 0 ; score > 0.5 : 1)
sub_1 = subset(df, ConfidenceInGovernment > 0.5) 
df_1 = sub_1%>%
  mutate(ConfidenceInGovernment = "1")
View(df_1)
nrow(df_1) # n = 65

sub_2 = subset(df, ConfidenceInGovernment <= 0.5)
df_2 = sub_2%>%
  mutate(ConfidenceInGovernment = "0")
View(df_2)
nrow(df_2) # n = 69
# la répartition entre les 2 catégories est idéale

df_chi2 = rbind(df_1, df_2)
View(df_chi2)

# visualisation
boxplot(ConfidenceInGovernment ~ Region)
# l'Asie du Sud-Est semble avoir un score supérieur aux autre régions, vérifions si cela se vérifie statistiquement

# pour vérifier l'indépendance des 2 variables, le test adéquat est le Chi2
df$ConfidenceInGovernment = as.factor(df$ConfidenceInGovernment)
df$Region = as.factor(df$Region)
str(df)
table_chi2 = table(df$ConfidenceInGovernment, df$Region)
table_chi2
chisq.test(table_chi2)
# la statistique de test X² est très élevée (1206), elle est dans la zone d'indépendance ; la p value est > .05
# nous pouvons en conclure que le test n'est pas significatif, on accepte H0 : les variables sont indépendantes 

# =======================================
# Question 11. 2 variables quantitatives
# =======================================

# pour les 2 questions suivantes, nous n'avons pas testé l'hypothèse de normalité des variables continues, car nous avons considéré que la taille de l'échantillon était suffisamment
# importante pour que les distributions approximent une loi normale. De ce fait nous avons privilégié des tests paramétriques (qui sont plus sensibles)

# pour analyser le lien entre 2 variables quantitatives, le test adéquat est une corrélation de Pearson
# la corrélation permet d'analyser la force de la relation entre 2 variables (le coefficient de corrélation varie entre 0 = aucune corrélation et 1 = corrélation parfaite)
# la matrice de corrélation du reporting a montré que plusieurs variables étaient fortement corrélées : étudions les variables Freedom et SocialSupport

# dans un 1er temps, visualisons ces 2 variables sur un graphique en nuage de points
plot(Freedom, SocialSupport)
reg = lm(SocialSupport ~ Freedom, data = df)
abline(reg$coef, col = "skyblue", lwd=2)
# il semble exister une corrélation positive, vérifions si elle est statistiquement significative

# calculons le coefficient de corrélation de Pearson
cor.test(Freedom, SocialSupport)
# le coefficient r de Pearson est de 0.39, ces 2 variables sont corrélées à 39%, avec un risque très faible de se tromper (p < 6.637e-07)
# la corrélation est positive, c'est-à-dire que plus le sentiment de liberté augmente, plus les gens se sentent soutenus socialement

# ========================================================
# Question 12. 1 var. quantitative et 1 var. catégorielle
# ========================================================

# nous souhaitons nous intéresser au lien entre le score de Happiness et la Region (Country n'est pas utilisable car il n'y a qu'une observation par pays)
# le test adéquat est une ANOVA (Region a 10 modalités)

# représentons ces 2 variables sur un graphique : le score Happiness en fonction de la Région
boxplot(Happiness ~ Region)
moyennes_happiness_region = tapply(Happiness, Region, mean)
barplot(moyennes_happiness_region)
# sur les graphiques on observe des différences selon les pays : l'Amérique du Nord, l'Australie-Nouvelle-Zélande et l'Europe de l'Ouest ont des scores de Happiness
# supérieurs aux autres régions

# testons si les différences sont significatives, en d'autres termes si les variables sont dépendantes (si le type de région influe sur le score Happiness)
summary(aov(Happiness ~ Region)) 
# la statistique F obtenue est de 21.05 à 9 ddl (plus F est élevée, plus les moyennes sont éloignées les unes des autres)
# la valeur de p est < <2e-16 donc le test est très significatif, nous pouvons rejeter H0 (indépendance)
# les variables sont dépendantes et nous pouvons dire que selon la région où l'on vit, on n'a pas le même sentiment de bonheur
# pour le moment nous pouvons conclure uniquement sur la présence de differences entre les pays, mais comme la variable Region a 10 modalités, 
# il faut réaliser un test a posteriori (post-hoc) pour réaliser des comparaisons multiples 2 à 2 : elles indiqueront quelles régions se différencient significativement

# nous allons utiliser un test t de Student pour comparaisons multiples
pairwise.t.test(Happiness, Region)
# par ex. nous observons des différences significatives entre : Afrique Sub-Saharienne et Australie-NZ, Europe de l'Est et Europe de l'Ouest,
# Europe de l'Ouest et Afrique Sub-Saharienne, etc.
# la tendance observée plus haut concernant de meilleurs scores de bien-être dans les pays occidentaux qui possèdent également un bon GDP moyen se trouve ici confirmée statistiquement

# #########################
#                                                              
# D. MODELE DE REGRESSION
#                                                              
# #########################

# la régression linéaire est une méthode statistique permettant d'étudier les liens entre une variable dépendante (VD ou variable à expliquer) et des variables indépendantes (VI ou variables explicatives)
# le modèle sous-jacent est une fonction affine ou encore une droite d'équation y = ax + b (a = coefficient directeur ; b = ordonnée à l'origine)
# une régression multiple est une généralisation de la régression linéaire simple, le modèle reste linéaire puisqu'on ajoute des vecteurs dans la combinaison linéaire : y = ax1 + bx2 + cx3 + ... + cxn (pour n dimensions)
# la méthode de calcul est la méthode des moindres carrés qui aboutit à la statistique R² qui détermine la qualité de la relation (de 0 : aucune relation, à 1 : corrélation parfaite)

# ici notre VD est la variable Happiness, et nous allons rechercher dans quelle mesure elle est corrélée aux autres variables, avant de choisir celles qui vont le mieux alimenter notre modèle

# Question 13

# le lien entre Happiness et Region a été étudié à la Question 12 avec l'ANOVA

# étudions la manière dont Happiness est corrélée aux autres variables quantitatives
cor.test(Happiness, GDP) # r = 0.76 p < 2.2e-16 (significatif = s***) -> + le GDP augmente, + les gens éprouvent un sentiment de bonheur
cor.test(Happiness, SocialSupport) # r = 0.76 p < 2.2e-16 (s***) -> + les gens ont des proches sur qui ils peuvent compter, + ils sont heureux
cor.test(Happiness, Health) # r = 0.76 p < 2.2e-16 (s***) -> + l'espérance de vie augmente, + les gens éprouvent un sentiment de bonheur
cor.test(Happiness, Freedom) # r = 0.53 p < 3.55e-12 (s***) -> + les gens ont l'impression d'être libres dans leurs choix, + ils sont heureux
cor.test(Happiness, Generosity) # r = 0.18 p < .05 (s*) -> + les gens sont généreux, + ils sont heureux
cor.test(Happiness, Corruption) # r = -0.42 p < 3.278e-07 (s***) -> + les gens ont l'impression de vivre dans un pays corrompu, moins ils sont heureux
cor.test(Happiness, PositiveAffect) # r = 0.52 p < 1.614e-11 (s***) -> + les gens éprouvent des émotions positives, + ils sont heureux (ce qui est cohérent)
cor.test(Happiness, NegativeAffect) # r = -0.58 p < 2.076e-14 (s***) -> + les gens éprouvent des émotions négatives, moins ils sont heureux (cohérent également)
cor.test(Happiness, ConfidenceInGovernment) # r = -0.15 (ns)
# une tendance se dessine : les individus se décrivent généralement comme plus heureux lorsqu'ils vivent dans une région libre et à fort développement
# cela rajoute une donnée supplémentaire à notre problématique de départ : non seulement le bonheur est favorisé par la richesse, mais il faut également que 
# les gens éprouvent une forme de liberté dans leur vie et leurs choix (ce sont toutes ces variables que nous ferons entrer dans notre modèle)

# ===========================
# Régression linéaire simple
# ===========================

# pour la régression, nous avons étudié le site suivant : https://delladata.fr/la-regression-lineaire-simple-avec-le-logiciel-r/

# régression simple pour prédire Happiness en fonction du GDP (Question 15)

# étape 1. évaluation visuelle de la linéarité 

# méthode d'affichage du nuage de points
plot(Happiness ~ GDP)

# méthode d'affichage de la courbe de régression de type lowess et son intervalle de confiance
library(car) # pour la fonction scatterplot()
scatterplot(Happiness ~ GDP, data = df)
# la droite de régression est comprise dans l'IC de la courbe lowess, l'hypothèse de linéarité est donc acceptable, nous pouvons donc initier le modèle

# étape 2. modèle de régression linéaire simple et visualisation
reg_simple = lm(Happiness ~ GDP, data = df) 
plot(Happiness ~ GDP)
abline(reg_simple$coef, col = "skyblue", lwd=2)

# étape 3. évaluation des hypothèses de validité des résultats :
# Le test d’évaluation de la significativité du lien linéaire entre les deux variables est valide, si les résidus (c'est-à-dire les différences entre
# les valeurs observées et les valeurs estimées) :
# sont indépendants
# sont distribués selon une loi normale de moyenne 0
# sont distribués de façon homogène, c’est-à-dire, avec une variance constante

# réalisons ces 3 évaluations : 

# 1. hypothèse d'indépendance des résidus
# réalisons le test de Durbin-Watson : il s'agit de vérifier qu'il n'existe pas d'auto-corrélation des résidus
durbinWatsonTest(reg_simple)
# la p-value est supérieure à .05 donc l'hypothèse d'indépendance des résidus (H0) est acceptée

# 2. hypothèse de normalité des résidus

# méthode d'évaluation graphique à l'aide d'un QQplot (diagramme quantile-quantile)
# si les résidus sont bien distribués le long de la droite, l'hypothèse de normalité est acceptée
plot(reg_simple, 2)
# les points sont bien alignés sur la droite, donc nous pouvons accepter l'hypothèse de normalité

# méthode d'évaluation statistique avec le test de Shapiro-Wilk
shapiro.test(residuals(reg_simple))
# la p-value est supérieure à .05 donc l'hypothèse de normalité (H0) est acceptée

# 3. évaluation de l'hypothèse d'homogénéité des résidus

# méthode d'évaluation graphique à l'aide d'un "residuals vs fitted plot" qui utilise la racine carrée des résidus standardisés
plot(reg_simple, 3)
# les "fitted values" sont les valeurs de Happiness prédites par le modèle pour les valeurs de GDP présentes dans les données
# même si la courbe de régression locale n'est pas plate, le graphique montre que les résidus ont tendance à être répartis de façon
# homogène le long du gradient des valeurs de prestige prédites
# on peut donc graphiquement accepter l'hypothèse d'homogénéité des résidus
# mais il faut le vérifier statistiquement à l'aide du test de Breush-Pagan

# méthode d'évaluation statistique avec le test de Breush-Pagan
ncvTest(reg_simple)
# la p-value est supérieure à .05 donc l'hypothèse d'homogénéité (HO) est acceptée

# étape 4. évaluation a posteriori de l'hypothèse de linéarité
plot(reg_simple, 1)
# on observe que même lorsque les réponses prédites par le modèle (fitted values) augmentent ou diminuent, les résidus restent uniformément 
# distribués de part et d'autre de 0 : la droite de régression est donc bien adaptée aux données
# l'hypothèse de linéarité est acceptable

# étape 5. interprétation des résultats
# comme les hypothèses de linéarité, normalité, homogénéité et indépendance des résidus sont validées, 
# alors les résultats de la régression sont valides et nous pouvons les interpréter
summary(reg_simple) 
# dans la partie "Residuals" nous pouvons vérifier la normalité des résidus : en effet la médiane est 
# proche de 0 et les valeurs absolues de Q1 et Q3 sont proches
# dans la partie "Coefficients", la première ligne concerne l'ordonnée à l'origine de la droite 
# et la deuxième ligne sa pente (avec pour chaque ligne la valeur, l'erreur standard, la statistique t et la p-value)
# nous voyons que la p-value est <.05 donc le lien linéaire entre les 2 variables est significatif
# enfin, les valeurs de R² et R² ajusté sont très proches (0.59), ce qui signifie que la variation de Happiness est expliquée à 59% par la variation de GDP.
# il n'y a pas vraiment de seuil concernant la valeur de R² mais 59% peut être considéré comme satisfaisant

# équation de la droite : y = 0.71 * x - 1.14
# interprétation de l'équation : la pente est positive donc plus le GDP augmente plus le score Happiness augmente
# la droite de régression coupe l'axe des ordonnées à la valeur -1.14

# intervalle de confiance à 95% des coefficients
confint(reg_simple)
# il y a 95% de chance que la vraie pente (celle de la population) soit comprise entre 0.61 et 0.81

# étape 6. prédictions
# pour tester notre modèle, créons un dataframe contenant des valeurs de GDP fictives (pour 3 individus)
data_test = data.frame(GDP = c(7.31, 9.84, 10.99))
predict(reg_simple, newdata=data_test, interval="confidence")
# résultats : 
# pour un GDP = 7.31, on peut prédire un score de Happiness de 4.07 (IC = [3.84 ; 4.30])
# pour un GDP = 9.84, on peut prédire un score de Happiness de 5.88 (IC = [5.75 ; 6.01])
# pour un GDP = 10.99, on peut prédire un score de Happiness de 6.70 (IC = [6.50 ; 6.91])
# nos résultats sont cohérents 
# nous avons fait les calculs également à partir de l'équation de la droite : 
# y1 = 0.71 * 7.31 - 1.14 = 4.05
# y2 = 0.71 * 9.84 - 1.14 = 5.85
# y3 = 0.71 * 10.99 - 1.14 = 6.66
# scores identiques à 10^-2 près

# étape 7. représentation finale de la régression
library(ggplot2) # pour une visualisation optimale
ggplot(df, aes(y=Happiness, x=GDP), ymax=20)+
ylim(2.5, 9)+
geom_point()+
geom_smooth(colour="red", method="lm", fill="red")+
ylab("Happiness")+
xlab("GDP")+
theme_classic()+
annotate("text", x=9, y=80, label="Happiness=0.71*GDP-1.14\n (p<.001)")

# =============================
# Régression linéaire multiple
# =============================

# régression multiple entre Happiness et d'autres variables (Question 16)

# étape 1. étude des corrélations linéaires entre les VI numériques prises 2 à 2
# cette vérification est importante car 2 VI fortement corrélées entre elles peuvent rendre le modèle instable
# en effet, c'est le phénomène de multicolinéarité : lorsque 2 vecteurs sont colinéaires, il y en a un des 2 qui apporte moins d'infos et est donc superflu, et de plus biaise le modèle
install.packages("GGally")
library(GGally) # pour la fonction ggpairs() qui permet de représenter, à l'aide d'une matrice de plots, les distributions de chaque variable ainsi que leurs liens

# suppression des variables qualitatives du dataset
df_vi_num = df%>%
  select(Happiness, 
         GDP, 
         SocialSupport, 
         Health, 
         Freedom, 
         Generosity,
         Corruption, 
         PositiveAffect, 
         NegativeAffect,
         ConfidenceInGovernment)
View(df_vi_num)

ggpairs(df_vi_num)
# le but de cette fonction est d'éliminer les corrélations les plus significatives entre les VI (nous nous fixons un seuil de 85%)
# la seule corrélation > 85% est entre les variables GDP et Health, elle est de 85.1%, comme la variable GDP est importante et que Health nous semble importante également,
# nous décidons de les garder toutes les 2 pour l'instant (et d'y revenir plus tard si nécessaire)

# étape 2. évaluation visuelle de la linéarité entre la VD (Happiness) et les VI
# cela a déjà été réalisé avec des tests de corrélations plus haut, mais ici nous allons utiliser une visualisation à l'aide de la fonction scatterplotMatrix() du package "car"
library(car) # pour la fonction scatterplotMatrix()
scatterplotMatrix(df_vi_num)
# les graphiques qui nous intéressent sont situés sur la première ligne : ils représentent les liens entre Happiness et toutes les autres variables
# pour le moment nous gardons toutes les variables

# étape 3. modème de régression linéaire multiple
reg_multiple = lm(Happiness ~., data = df_vi_num) # ~. permet d'inclure toutes les variables
summary(reg_multiple)

# étape 4. évaluation des multicolinéarités par les VIF (Variance Inflation Factor)
# le VIF évalue l'augmentation de la variance d'un coefficient de régression par rapport à une situation d'indépendance
# un VIF élevé signifie une multicolinéarité (seuil de 3, 5 ou 10 généralement, nous nous fixons un seuil de 5)
install.packages("performance")
library(performance) # pour les fonctions check_collinearity() et check_model()
check_collinearity(reg_multiple)
# le VIF de la variable GDP est de 5.46, comme il est légèrement au-dessus du seuil nous décidons de garder cette variable car elle est importante
# concernant les autres variables, tous les VIF sont inférieurs à 5, donc nous les gardons

# étape 5. évaluation des hypothèses de normalité et d'homoscédasticité des résidus
# la fonction check_model évalue les hypothèses de linéarité, d'homoscédasticité et de normalité des résidus, ainsi que les multi-colinéarités
install.packages("see")
install.packages("patchwork")
library(see)
library(patchwork)
check_model(reg_multiple)

# hypothèse d'indépendance des résidus
durbinWatsonTest(reg_multiple)
# la p-value est supérieure à .05 donc l'hypothèse d'indépendance des résidus (H0) est acceptée

# hypothèse de normalité des résidus (2 méthodes)
shapiro.test(residuals(reg_multiple))
check_normality(reg_multiple)
# dans les 2 tests, la p-value est supérieure à .05 donc l'hypothèse de normalité (H0) est acceptée

# hypothèse d'homogénéité des résidus (2 méthodes)
ncvTest(reg_multiple)
check_heteroscedasticity(reg_multiple)
# la p-value est inférieure à .05 donc l'hypothèse d'homogénéité (HO) est rejetée
# la question de pose de savoir s'il faut améliorer l'homoscédasticité des résidus
# poursuivons notre démarche pour le moment

# étape 6. interprétation des résultats : modèle complet
summary(reg_multiple)

# étape 7. sélection des variables pertinentes : modèle parcimonieux
# retirons de manière itérative les variables les moins significativement liées à la réponse
reg_multiple_parcimonieux_1 = update(reg_multiple, .~.-NegativeAffect)
Anova(reg_multiple_parcimonieux_1)

reg_multiple_parcimonieux_2 = update(reg_multiple_parcimonieux_1, .~.-Health)
Anova(reg_multiple_parcimonieux_2)

reg_multiple_parcimonieux_3 = update(reg_multiple_parcimonieux_2, .~.-PositiveAffect)
Anova(reg_multiple_parcimonieux_3)

reg_multiple_parcimonieux_4 = update(reg_multiple_parcimonieux_3, .~.-Generosity)
Anova(reg_multiple_parcimonieux_4)
# toutes les variables restantes sont fortement liées à Happiness

# vérifions à nouveau les hypothèses sur les résidus

# hypothèse d'indépendance des résidus
durbinWatsonTest(reg_multiple_parcimonieux_4)
# la p-value est supérieure à .05 donc l'hypothèse d'indépendance des résidus (H0) est acceptée

# hypothèse de normalité des résidus (2 méthodes)
shapiro.test(residuals(reg_multiple_parcimonieux_4))
check_normality(reg_multiple)
# dans les 2 tests, la p-value est supérieure à .05 donc l'hypothèse de normalité (H0) est acceptée

# hypothèse d'homogénéité des résidus (2 méthodes)
ncvTest(reg_multiple)
check_heteroscedasticity(reg_multiple_parcimonieux_4)
# la p-value est inférieure à .05 donc l'hypothèse d'homogénéité (HO) est rejetée
# ce critère n'est toujours pas satisfaisant

# étape 8. recherche des outliers
# recherchons des valeurs aberrantes susceptibles d'avoir influencé les résultats
check_outliers(reg_multiple_parcimonieux_4) # aucun outlier

# étape 9. interprétation du modèle parcimonieux
summary(reg_multiple_parcimonieux_4)
# 5 variables sont fortement corrélées à Happiness (5 dimensions)
# p-value < .05
# R² : 0.76, R² ajusté : 0.75 -> ce qui signifie que 75% de la variabilité de Happiness est expliquée par ces variables, 
# cela connote une bonne performance du modèle
# équation obtenue : y = 0.26*x1 + 2.86*x2 + 2.76*x3 - 1.43*x4 - 1.56*x5

# étape 10. prédictions
# prenons des valeurs fictives d'un individu pour les variables et stockons-les dans un dataframe
data_test_2 = data.frame(GDP = 8.21,
                         SocialSupport = 0.64,
                         Freedom = 0.82,
                         Corruption = 0.61,
                         ConfidenceInGovernment = 0.53)
predict(reg_multiple_parcimonieux_4, 
        newdata = data_test_2, 
        se.fit = TRUE,
        interval = "prediction",
        level = 0.95)
# pour ces valeurs, nous pouvons prédire un score de Happiness de 4.97 pour notre individu, ce qui est cohérent, mais notre individu est moyennement heureux

# =============================
# Standardisation des données
# =============================

# (Question 17)

# nos variables ne sont pas toutes à la même échelle : la plupart varient entre 0 et 1 (moyennes de scores entre 0 et 1), sauf :
# GDP va de 0 à 12 environ
# Health de 0 à 100 
# Generosity de -1 à 1
# procédons à la standardisation de ces données : c'est-à-dire que la fonction va les centrer autour de la moyenne et les réduire en des divisant par l'écart-type
# elles seront ainsi comparables (moyenne = 0 et écart-type = 1)
df_scaled = data.frame(scale(df_vi_num))
View(df_scaled)
# nous avons testé ces nouvelles données sur le modèle de régression multiple mais cela n'améliore pas l'homoscédasticité des résidus

# ########
#                                                              
# E. PCA 
#                                                              
# ########

# pour ce modèle, nous avons consulté le site : http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/#:~:text=Standardisation%20des%20donn%C3%A9es,-Dans%20l'analyse&text=Ceci%20est%20particuli%C3%A8rement%20recommand%C3%A9%20lorsque,de%20rendre%20les%20variables%20comparables

# l'ACP est une méthode non supervisée permettant de réduire la dimensionnalité d'un jeu de données en faisant ressortir les variables 
# qui apportent le plus d'information pour focaliser notre modèle sur ces variables (utile lorsqu'il y a un nombre important de variables 
# que l'on ne peut étudier à la main) : le cercle de corrélation renvoie en sortie des vecteurs regroupés en familles.
# Le principe est de ne conserver que les variables qui sont linéairement indépendantes, c'est-à-dire qui ne sont pas corrélées entre elles : ce sont celles qui expliquent une grande partie de la variation des données

# installation des librairies 
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR) # pour la PCA
library(factoextra) # pour l'interprétation

# suppression des variables qualitatives
df_acp = df%>%
  select(-Country,
         -Region)
View(df_acp)
# bien que nous ayons dans la question précédente créé un dataset standardisé, il n'est pas nécessaire de standardiser les données 
# au préalable dans l'ACP car la fonction ACP() possède une option qui permet de normaliser les données
pca = PCA(df_acp, scale.unit = TRUE, ncp = 5, graph = TRUE)
pca

# sortie du modèle : visualisation et interprétation

# extraction des valeurs propres (eigenvalues) et variances des composantes principales
# les valeurs propres mesurent la quantité de variance (information) expliquée par chaque axe principal
get_eigenvalue(pca)
# la proportion de variance expliquée par chaque valeur propre se trouve dans la 2e colonne
# on voit que 46.4% de la variation est expliquée par la 1re valeur propre, et 68.12% de la variance totale est déjà expliquée par les 2 premières valeurs propres
# il est généralement admis de conserver les valeurs propres > 1 : ici, les 3 premières composantes expliquent 76.8% de la variation, ce qui est très acceptable

# visualisation des valeurs propres
fviz_eig(pca)

# partie 1. résultats pour les variables
var = get_pca_var(pca)

# cercle de corrélation pour représenter les variables
head(var$coord) # coordonnées sur le cercle
fviz_pca_var(pca, col.var = "darkblue")
# les variables positivement corrélées sont regroupées et forment une famille de vecteurs liés (colinéaires)
# les variables négativement corrélées sont positionnées de manière opposée
# la longueur des vecteurs mesure la qualité de représentation des variables
# il nous semble sur ce cercle que les variables les plus représentées sont : ConfidenceInGovernment, Freedom, Happiness, SocialSupport, Health, GDP
# ces 4 dernières forment une famille liée (cela est cohérent avec les résultats statistiques observés précédemment)
# Freedom et Corruption sont opposés, ce qui coïncide également avec nos résultats précédents (et qui a du sens dans notre étude)

# qualité de représentation (cos2)
head(var$cos2) # données chiffrées
# un cos2 élevé signifie que la variable est proche de la circonférence du cercle
# un cos2 faible indique que la variable est proche du centre

# visualisation des cos2 sur une matrice
library(corrplot)
corrplot(var$cos2, is.corr = FALSE)
# nous voyons clairement la qualité de la représentation sur les 2 dimensions : Happiness en tête pour Dim.1 et ConfidenceInGov pour Dim.2

# visualisation des cos2 sur un graphique à barres
fviz_cos2(pca, choice = "var", axes = 1:2)

# coloration des variables selon un gradient sur cos2
fviz_pca_var(pca, 
             col.var = "cos2",
             gradient.cols = c("yellow", "orange", "red"),
             repel = TRUE)

# visualisation de la contribution (en %) des variables
var$contrib # données chiffrées
corrplot(var$contrib, is.corr = FALSE) # matrice
fviz_contrib(pca, choice = "var", axes = 1, top = 10) # PC1 : Happiness > SocialSupport > Health > GDP > NegativeAffect
fviz_contrib(pca, choice = "var", axes = 2, top = 10) # PC2 : ConfidenceInGov > Generosity > Freedom > Corruption
fviz_contrib(pca, choice = "var", axes = 1:2, top = 10) # PC 1 et 2
# une variable au-dessus du seuil de 10% (trait rouge, contribution moyenne théorique) est considérée comme importante

# description des variables corrélées aux dimensions
dimdesc(pca, axes = c(1,2), proba = 0.05)
# Dim.1 : Happiness (89%), SocialSupport (84%), etc
# Dim.2 : ConfidenceInGov (88%), Generosity (63%), etc

# partie 2. résultats pour les individus (pays)
ind = get_pca_ind(pca)

# cercle de corrélation
head(ind$coord) # coordonnées
fviz_pca_ind(pca,
             col.ind = "cos2",
             gradient.cols = c("yellow", "orange", "red"),
             repel = TRUE)
# les pays similaires sont regroupés par coloration

# biplot des variables et des pays
fviz_pca_biplot(pca,
                repel = TRUE,
                col.var = "darkblue",
                col.ind = "darkgray")
# un individu qui se trouve du même côté d’une variable représente fortement cette variable (par ex. pays 93 (Myanmar) pour ConfidenceInGov)
# un individu qui se trouve sur le côté opposé d’une variable donnée a une faible valeur pour cette variable (par ex. pays 116 (Sierra Leone) pour SocialSupport, Health, GDP)

# ##################################
#                                                              
# F. CLASSIFICATION : DECISION TREE
#                                                              
# ##################################

# modification de la colonne Happiness (Question 18)

# nous avons pris la liberté de remplacer les valeurs numériques 0 et 1 par NON et OUI pour une meilleure interprétation de l'arbre
sub1 = subset(df, df$Happiness >= 5) 
df1 = sub1%>%
  mutate(Happiness = "OUI")
View(df1)
nrow(df1)

sub2 = subset(df, df$Happiness < 5)
df2 = sub2%>%
  mutate(Happiness = "NON")
View(df2)
nrow(df2)

df_decision_tree = rbind(df1, df2)
df_decision_tree$Happiness = as.factor(df_decision_tree$Happiness)
df_decision_tree$Country = as.factor(df_decision_tree$Country)
df_decision_tree$Region = as.factor(df_decision_tree$Region)
str(df_decision_tree)

# peut-on créer un modèle de classification (decision tree) pour prédire cette nouvelle variable ? (Question 19)

# un modèle decision tree (arbre décisionnel) est une autre méthode supervisée qui cherche à prédire une valeur (prédiction) ou une catégorie (classement) d'une variable en fonction d'autres variables
# dans cet algorithme, chaque branche correspond à une décision par rapport à la variable d'intérêt
# par exemple ici notre variable d'intérêt est Hapiness, et nous l'avons transformée en variable binaire (1 = heureux ; 0 = pas heureux)
# ainsi, l'idée est de prédire en sortie, après des décisions successives, si notre individu sera heureux ou non
# les variables peuvent être qualitatives ou quantitatives (ici = modèle de classification)

# pour ce modèle, nous avons étudié le site : https://www.lovelyanalytics.com/2016/08/18/un-arbre-de-decision-avec-r/

# séparation des données en un échantillon apprentissage et un échantillon test/validation
set.seed(3033)
training_size = floor((nrow(df_decision_tree)*0.75)) # échantillon d'apprentissage : 75% du dataset (n = 110)
indices = sample(seq_len(nrow(df_decision_tree)), size = training_size)
train = df_decision_tree[indices,]
test = df_decision_tree[-indices,]

# comme nous l'avons vu en cours, la fonction predict() sur l'arbre que nous avons généré avec la librairie "rpart" nous renvoyait en sortie 110 lignes (c'est-à-dire le nb de lignes de notre échantillon d'entrainement).
# par conséquent, comme notre échantillon de test fait 37 lignes, nous ne parvenions pas à construire notre matrice de confusion
# nous avons donc utilisé une autre librairie (party), et cela a pu fonctionner
install.packages("party")
library(party)

# construction et visualisation du modèle
modele_tree = ctree(Happiness ~., data = train)
plot(modele_tree, type = "extended")
text(modele_tree, pretty=0) # avec cette fonction cette ligne n'apporte pas d'info supplémentaire

# interprétation
print(modele_tree)
summary(modele_tree)
# lecture de la sortie du modèle :
# à chaque noeud, le modèle va tenter de départager les observations à partir de la variable qu'il va juger la plus pertinente
# pour cela, il teste chaque variable sur laquelle il applique une métrique pour faire son choix, et ce à chaque noeud
# nb de noeuds : 5
# la première variable discriminante est SocialSupport (et nous pouvons observer que quelle que soit la fonction c'est toujours cette
# variable qui ressort) : 
# - Pour des valeurs SocialSupport >0.824 c'est la variable Health qui sépare le mieux les pays (>64.3 c'est OUI, sinon NON)
# - Pour des valeurs SocialSupport >0.824 c'est la variable GDP : pour GDP >8.213 c'est NegativeAffect (>0.33 c'est NON sinon c'est OUI), sinon c'est NON

# étape 2. phase de test
prediction_tree = predict(modele_tree, newdata = test)
matrice_confusion = confusionMatrix(prediction_tree, test$Happiness)
# le taux de "bien classé" est de 84% ce qui est un bon résultat

# avis personnel sur cet exercice (Question 20)
# j'ai beaucoup apprécié cet exercice car ce fut pour moi la première fois que je me lançais dans un exercice entièrement fait à travers ma réflexion et non en suivant les
# conseils d'un tutoriel (même si Udemy m'a permis d'apprendre beaucoup de choses !)
# cela m'a permis de mieux différencier statistiques inférentielles, modèles supervisés et non supervisés
# j'ai ressenti que cet exercice était fortement "addictif" : il fut difficile de quitter mon écran durant ces 2 dernières semaines :)
# cela me conforte dans cette voie de la data science

# ###############
#                                                              
# G. CLUSTERING
#                                                              
# ###############

# ==============================================
# Modèle de clustering non hiérarchique k-means
# ==============================================

# k-means est un algorithme de clustering non supervisé et non hiérarchique qui va regrouper en K clusters des données qui se ressemblent
# chaque cluster est une famille de vecteurs possédant les mêmes caractéristiques
# l'algorithme définit les clusters en calculant des distances entre les vecteurs
# le principe est de déterminer le bon nb de clusteurs de manière à minimiser la distance inter-classe
# ce qui signifie regrouper des vecteurs qui ont un minimum de distance entre eux (en référence au point dit "centroïd", le centre du cluster)
# par itération il va minimiser la somme des distances entre chaque individu et le centroïd

# algorithme de clustering k-means avec un nb de cluster de 5 (Question 1)
install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(cluster)
library(factoextra)

# sélection des variables quantitatives et suppression des NA
df_kmeans = df%>%
  select(Happiness, 
         GDP, 
         SocialSupport, 
         Health, 
         Freedom, 
         Generosity,
         Corruption, 
         PositiveAffect, 
         NegativeAffect,
         ConfidenceInGovernment)%>%
  drop_na()
View(df_kmeans)
rownames(df_kmeans)=na.omit(df)$Country

# clustering
km = kmeans(df_kmeans, center=5)
df_kmeans$cluster = km$cluster
print(km)

# visualisation
clusplot(df_kmeans, df_kmeans$cluster) # visu sans les pays
fviz_cluster(km, data = df_kmeans) # visu avec les pays
# pour aller plus loin, il serait intéressant de rajouter les clusters dans le dataset et faire une classification (arbre de décision)

# déterminer le nb optimal de clusters et dessiner un graph (Question 2)

# méthode 1 : essais avec 4, 3 puis 2 clusters
km = kmeans(df_kmeans, center=2)
df_kmeans$cluster = km$cluster
print(km)
fviz_cluster(km, data = df_kmeans) # visu avec les pays
# intuitivement, 2 clusters nous paraît un nombre optimal car les ensembles se recoupent à peine, alors qu'avec davantage de 
# clusters, nos catégories se chevauchaient beaucoup

# méthode 2 : essais avec fonctions de R
install.packages("NbClust")
library(NbClust)

# méthode elbow
fviz_nbclust(df_kmeans, kmeans, method = "wss")+geom_vline(xintercept = 4, linetype = 2)
# cette méthode retient 4 clusters

# méthode silhouette
fviz_nbclust(df_kmeans, kmeans, method = "silhouette")
# cette méthode retient 2 clusters, ce qui correspond à notre intuition initiale

# ces 2 clusters retenus, expliquent 100% de la variation observée dans les données

# ===============================================
# Modèle de clustering hiérarchique (Question 3)
# ===============================================

# Modèle CAH (Classification Ascendante Hiérarchique)

# Il s'agit également d'un modèle de clustering comme k-means, à la différence qu'il ne demande pas de choisir le nb de clusters
# Il est hiérarchique dans le sens où il réalise son traitement étape par étape (itération) : du niveau où l'individu est seul dans son groupe
# jusqu'au niveau où toutes les données sont dans le même groupe : c'est alors que l'on choisit le nb de cluster pertinent

# pour ce modèle, nous avons consulté le site : http://perso.ens-lyon.fr/lise.vaudor/classification-ascendante-hierarchique/
# ainsi que le site : https://larmarange.github.io/analyse-R/classification-ascendante-hierarchique.html

# sélection des variables quantitatives et suppression des NA
df_cah = df%>%
  select(Happiness, 
         GDP, 
         SocialSupport, 
         Health, 
         Freedom, 
         Generosity,
         Corruption, 
         PositiveAffect, 
         NegativeAffect,
         ConfidenceInGovernment)%>%
  drop_na()
View(df_cah)

# représentation des distances entre les individus
distances = dist(df_cah)
distances

# construction de l'arbre de classification (dendogramme)
dendogramme = hclust(distances, method = "ward.D2")
dendogramme
plot(dendogramme, labels = FALSE)
# nous voyons que 2 branches paraissent bien distinctes sur notre arbre

# découpage du dendogramme

# représentation des sauts d'inertie du dendogramme selon le nb de classes
inertie = sort(dendogramme$height, decreasing = TRUE)
plot(inertie[1:12], type = "s")
points(c(2, 3, 6), inertie[c(2, 3, 6)], col = c("red", "green", "blue"), cex = 2, lwd = 2)

# partition du dendogramme en 2, 3 ou 6 classes
plot(dendogramme, labels = FALSE)
rect.hclust(dendogramme, 2, border = "red")
rect.hclust(dendogramme, 3, border = "green")
rect.hclust(dendogramme, 6, border = "blue")

# recherche de la meilleure partition
library(FactoMineR)
library(devtools)
install_github("larmarange/JLutils")
library(JLutils)
source(url("https://raw.githubusercontent.com/larmarange/JLutils/master/R/clustering.R"))
best.cutree(dendogramme) # 3
typo = cutree(dendogramme, 3)
freq(typo)
# classe 1 : n = 41 (31.8%)
# classe 2 : n = 60 (46.5%)
# classe 3 : n = 28 (21.7%)

# typologie des classes
install.packages("gtsummary")
library(gtsummary)
df_cah$typo = cutree(dendogramme, 3)
df_cah%>%
  tbl_summary(by = typo)
View(df_cah)
# le dataset comporte la colonne typo avec (1,2,3) selon la classe

# résidus du Chi²
library(GGally)
df_cah$typo = factor(df_cah$typo)
ggtable(df_cah,
        columnsX = "typo",
        columnsY = names(df_cah),
        cells = "col.prop",
        fill = "std.resid")+
  labs(fill = "Résidus standardisés du Chi²")+
  theme(legend.position = "bottom")

# représentation des clusters précédents sur un plan défini par les 2 premiers composants (Question 4)

# utilisation de la fonction HCPC de la librairie FactoMineR pour réaliser une CAH à partir de la PCA
hcpc = HCPC(pca, graph = FALSE)
plot(hcpc, choice ="map") # 2D
plot(hcpc, choice = "3D.map") # 3D
# les 3 clusters sont représentés sur un graphique (2D ou 3D) selon les 2 composantes principales

# informations de sortie du modèle 
hcpc$data.clust # renvoie le dataset avec les clusters
hcpc$desc.va # description des variables (moyenne, sd, etc.) dans chaque cluster
hcpc$desc.axes # description par axe (Dim.1 et Dim.2)
hcpc$desc.ind # description par individu
# ce modèle comporte encore de nombreuses informations intéressantes que nous n'avons pas eu le temps d'analyser

# #########################
#                                                              
# H. DETECTION D'ANOMALIE
#                                                              
# #########################

# détecter les 5% des pays qui pourraient être considérés comme une anomalie (Question 5)

# D'après nos recherches, nous pourrions utiliser un modèle comme le Naive Bayes
# c'est un modèle de classification qui pourra déterminer notre variable Happiness, et révéler des pays qui pourraient être considérés comme des anomalies (faux positifs ou faux négatifs)

# pour ce modèle nous avons consulté le site : https://www.r-bloggers.com/2013/08/classifieur-naif-bayesien/

# considérons le dataset créé pour l'arbre de décision
set.seed(3033)
training_size = floor((nrow(df_decision_tree)*0.75)) # échantillon d'apprentissage : 75% du dataset (n = 110)
indices = sample(seq_len(nrow(df_decision_tree)), size = training_size)
nb_train = df_decision_tree[indices,]
nb_test = df_decision_tree[-indices,]

# phase d'apprentissage
library(e1071)
nb = naiveBayes(nb_train$Happiness ~., data = nb_train)
nb
# nous voyons que la probabilité de Happiness = NON est de 0.37 et de Happiness = OUI est de 0.63
# dans ce jeu de données, le modèle a appris qu'il était plus probable d'être heureux !

# phase de test
nb_predict = predict(object = nb, newdata = nb_test)

# matrice de confusion
confusion = cbind(nb_test, nb_predict)
resu_nb = table(confusion$Happiness, confusion$nb_predict)
round(prop.table(resu_nb), 2)
# le taux de "bien classé" est de 0.92 soit une très bonne performance

# anomalies
anomalies = confusion[(confusion$Happiness == "OUI") & (confusion$nb_predict == "NON"),]
View(anomalies)
# les 3 pays "anomalies" sont l'Algérie, le Ghana et le Nigéria : ils ont été prédits par le modèle comme n'étant pas heureux alors qu'ils le sont
# ce sont des faux négatifs
# il n'y a pas de faux positifs dans le résultat du test

# carte du monde avec la variable Happiness (Question 6)

# l'équivalent du package folium de Python est la librairie leaflet de R qui fait appel à la structure du langage JavaScript
install.packages("leaflet")
library(leaflet)
devtools::install_github("rstudio/leaflet")
install.packages("rnaturalearth")
library(rnaturalearth)
library(sf)
# malheureusement nous avons manqué de temps, à poursuivre...

# ###################
#                                                              
# I. AUTOMATISATION
#                                                              
# ###################

# automatisation de l'ensemble de ce travail (Question 7)