rm(list=objects())

library(ggplot2)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(gtable)
library(grid)
library("reshape2")
library("ggplot2")
library(Metrics)
library(glmnet)
library(FactoMineR)
library(factoextra)
library(matrixStats)
library(MASS)
library(pls)
library(ROCR)

setwd("C:/Users/lambe/OneDrive/Bureau/projet203")

graphics.off()

#2.1. ------------------------------------
#Lecture des donnees :
load("cookie.app.Rdata") #donnees d'apprentissage
load("cookie.val.RData") #donnees de validation

#Matrices des variables explicatives :
xtrain <- subset(cookie.app, select = -c(sucres)) #pour l'apprentissage
xtest  <- subset(cookie.val, select = -c(sucres)) #pour la validation

#Vecteurs reponse :
ytrain <- subset(cookie.app, select = c(sucres)) #pour l'apprentissage
ytest  <- subset(cookie.val, select = c(sucres)) #pour la validation

#Valeurs extremes du jeu de donnees xtrain :
min(xtrain)
max(xtrain)

#Boxplots des variables explicatives du jeu de donnees d'apprentissage :
boxplot(xtrain,
        names   = 1098 + 2*1:700,
        outline = FALSE,
        staplecol = 'lightblue',
        whiskcol = 'lightblue',
        boxcol = 'lightblue',
        col = 'lightblue',
        medcol = 'lightblue',
        outcol = 'blue',
        xlab    = 'Longueur d\'onde (en nm)',
        ylab    = 'Intensite observee',
        main   = 'Boxplots des intensites mesurees par rapport aux longueurs d\'onde')

#Courbes des spectres pour chaque observation du jeu d'apprentissage :
matplot(x    = 1098 + 2*1:length(xtrain[1,]),
        y    = t(xtrain),
        type = 'l',
        #axes = F,
        xlab = 'Longueur d\'onde (en nm)',
        ylab = 'Intensite observee',
        main   = 'Courbes des spectres')

#2.2. --------------------

#Effectuons une ACP a l'aide du package FactoMineR.
#On pose ncp = 6 pour calculer les coordonnees dans les 6 premiers axes principaux.
acp <- PCA(xtrain, graph = FALSE, ncp = 39)

#On recupere les valeurs propres :
valeursPropres <- acp$eig[,1]

#Nombre de valeurs propres :
length(valeursPropres)

#Calcul des valeurs propres de la matrice de correlation de
#xtrain puis on les trie par ordre decroissant. On affiche
#les 45 premieres :
sort(eigen(cor(xtrain))$values,decreasing = T)[1:45]

#Observons les valeurs propres :
barplot(valeursPropres,
        names = F,
        main = "Graphique des 39 valeurs propres",
        xlab = "valeurs propres")

#Coordonnees des variables dans les 6 premiers axes principaux :
var_coord <- get_pca_var(acp)$coord

#Valeurs extremales des composantes principales des variables
#(afin de representer ces composantes)
x_min <- min(var_coord[,1:6])
x_max <- max(var_coord[,1:6])

#Representation de chaque composante principale
p1 <- qplot(x = var_coord[,1], y = rep(x = 0,700), colour = I('red'),xlim=c(x_min,x_max),ylab="",xlab="1er axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
p2 <- qplot(x = var_coord[,2], y = rep(x = 0,700), colour = I('blue'),xlim=c(x_min,x_max),ylab="",xlab="2eme axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
p3 <- qplot(x = var_coord[,3], y = rep(x = 0,700), colour = I('green'),xlim=c(x_min,x_max),ylab="",xlab="3eme axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
p4 <- qplot(x = var_coord[,4], y = rep(x = 0,700), colour = I('black'),xlim=c(x_min,x_max),ylab="",xlab="4eme axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
p5 <- qplot(x = var_coord[,5], y = rep(x = 0,700), colour = I('yellow'),xlim=c(x_min,x_max),ylab="",xlab="5eme axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
p6 <- qplot(x = var_coord[,6], y = rep(x = 0,700), colour = I('purple'),xlim=c(x_min,x_max),ylab="",xlab="6eme axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())

grid.arrange(
  p1,p2,p3,p4,p5,p6,
  ncol = 1,
  top = "Representation du nuage des variables dans les 6 premiers axes principaux",
  bottom = textGrob(
    "",
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

#Coordonnees des individus dans les 6 premiers axes principaux :
ind_coord <- get_pca_ind(acp)$coord

#Valeurs extremales des composantes principales des individus
#(afin de representer ces composantes)
x_min <- min(ind_coord[,1:6])
x_max <- max(ind_coord[,1:6])

#Representation de chaque composante principale
p1 <- qplot(x = ind_coord[,1], y = rep(x = 0,40), colour = I('red'),xlim=c(x_min,x_max),ylab="",xlab="1er axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
p2 <- qplot(x = ind_coord[,2], y = rep(x = 0,40), colour = I('blue'),xlim=c(x_min,x_max),ylab="",xlab="2eme axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
p3 <- qplot(x = ind_coord[,3], y = rep(x = 0,40), colour = I('green'),xlim=c(x_min,x_max),ylab="",xlab="3eme axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
p4 <- qplot(x = ind_coord[,4], y = rep(x = 0,40), colour = I('black'),xlim=c(x_min,x_max),ylab="",xlab="4eme axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
p5 <- qplot(x = ind_coord[,5], y = rep(x = 0,40), colour = I('yellow'),xlim=c(x_min,x_max),ylab="",xlab="5eme axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())
p6 <- qplot(x = ind_coord[,6], y = rep(x = 0,40), colour = I('purple'),xlim=c(x_min,x_max),ylab="",xlab="6eme axe principal") + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())

grid.arrange(
  p1,p2,p3,p4,p5,p6,
  ncol = 1,
  top = "Representation du nuage des individus dans les 6 premiers axes principaux",
  bottom = textGrob(
    "",
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

#2.3. ------------------------------------------------------

#Fonction de reconstruction du nuage
reconstruct <- function(res,nr,Xm,Xsd)
{
  #Les nr premiers axes principaux des individus :
  u_s <- t(t(res$var$coord[,1:nr])/sqrt(res$eig[,1])[1:nr])
  
  #Reconstruction du nuage centre reduit par multiplication 
  #matricielle des nr premieres composantes principales des 
  #individus par la transposee des nr premiers axes principaux
  #des individus
  rep <- res$ind$coord[,1:nr]%*%t(u_s)
  
  #On multiplie chaque colonne de la matrice centree reduite
  #reconstruite par l'ecart-type des variables explicatives.
  #Autrement dit, on annule la reduction des donnees :
  rep <- sweep(rep,2,res$call$ecart.type,FUN="*")
  
  #On ajoute a chaque colonne de la matrice centree
  #reconstruite la moyenne des variables explicatives.
  #Autrement dit, on annule le centrage des donnees :
  rep <- sweep(rep,2,res$call$centre,FUN="+")
  
  #Finalement, on renvoie la matrice reconstruite
  return(rep)
}

#-------------------------------------------------------
#Verification du code de la fonction reconstruct.
#On va reconstruire le nuage avec le plus de d'axes possibles (39)
#puis on va observer differentes erreurs.

#On procede a la transformation du nuage (centrage puis reduction)
xtrainscaled <- scale(xtrain, center = T, scale = T)

#On recupere les vecteurs Xsd et Xm des ecarts-types et des 
#moyennes des colonnes.
Xsd = attr(xtrainscaled,"scaled:scale")
Xm = attr(xtrainscaled,"scaled:center")

#On reconstruit le nuage :
rec <- reconstruct(acp,39,Xm,Xsd)

#On calcule l'ordre maximal de l'erreur :
max(abs(rec-xtrain))

#On calcule les erreurs RMSE et MAE :
rmse(as.matrix(xtrain),as.matrix(rec))
mae(as.matrix(xtrain),as.matrix(rec))

#----------------------------------------------------------
#Reconstruction du nuage pour nr = 1,...,5,39 :
pdf("6recnuage.pdf")

par(mfrow=c(3,2),cex.axis=0.7, cex.main = 0.9, mai=c(0.3,0.3,0.3,0.3), oma = c(0, 0, 3, 0))

for(nr in c(1,2,3,4,5,39))
{
  #Reconstruction ou on ajuste la valeur de nr
  rec <- reconstruct(acp,nr,Xm,Xsd)
  
  #Courbes des spectres pour chaque observation du jeu d'apprentissage :
  matplot(x    = 1098 + 2*1:length(xtrain[1,]),
          y    = t(rec),
          type = 'l', 
          xlab = 'Longueur d\'onde (en nm)',
          ylab = 'Intensite observee',
          main   = paste('nr = ',nr," , RMSE = ",formatC(rmse(as.matrix(xtrain),as.matrix(rec)),format='e',digits=3),", MAE = ",formatC(mae(as.matrix(xtrain),as.matrix(rec)),format='e',digits=3)))
}

#Titre
mtext("Reconstruction du nuage pour nr = 1,...,5,39", outer = TRUE, cex = 1.5)

dev.off()

#-----------------------------------------------------------
#Reconstruction de la variable X24 pour nr = 1,...,5,39 :
pdf("recX24.pdf")

par(mfrow=c(3,2),cex.axis=0.7, cex.main = 0.9, mai=c(0.3,0.3,0.3,0.3), oma = c(0, 0, 3, 0))

for(nr in c(1,2,3,4,5,39))
{
  #Reconstruction ou on ajuste la valeur de nr
  rec <- reconstruct(acp,nr,Xm,Xsd)
  
  #Courbes des spectres pour la variable X24
  plot(x    = 1:40,
       y    = rec[,24],
       type = 'l',
       xlab = 'Longueur d\'onde (en nm)',
       ylab = 'Intensite observee',
       col = 'red',
       main   = paste('nr = ',nr," , RMSE = ",formatC(rmse(as.matrix(xtrain[,24]),as.matrix(rec[,24])),format='e',digits=3),", MAE = ",formatC(mae(as.matrix(xtrain[,24]),as.matrix(rec[,24])),format='e',digits=3)))
}

#Titre
mtext("Reconstruction de la variable X24 pour nr = 1,...,5,39", outer = TRUE, cex = 1.5)

dev.off()

#####     QUESTION 3.1     #####

nobs   <- nrow(xtrain)
grid   <- 10^seq(6,-10,length=100)

# Modele "original" 
reg.original <- glmnet(xtrain,
                       as.matrix(ytrain),
                       family = 'gaussian',
                       alpha = 0,
                       lambda = grid,
                       thresh = 2*10^(-11))

# Verifions l'intercept en fonction de lambda.
plot(reg.original$a0, type='l',
     main = 'Intercept vs. \n parametre de regularisation',
     xlab = 'Parametre de regularisation',ylab = 'Intercept', xaxt = 'n')
axis(1, at = 1:100, labels=signif(grid, digits = 2))

# Comparaison de l'intercept avec mean(Y) quand tau >> 1
print(reg.original$a0[1])
print(mean(as.matrix(ytrain)))

# Comparaison de l'intercept avec l'intercept de la reg.lin
# quand tau << 1
reg.lin <- lm(sucres ~.,
              data = cbind(as.matrix(ytrain), xtrain))

print(tail(reg.original$a0, n = 1))
reg.lin$coefficients[1]

# Verifions les formules de centrage et reduction.

#####     Quantites necessaires     #####

X_bar <- matrix(1, nrow = nrow(xtrain), ncol = ncol(xtrain)) %*% diag(colMeans(xtrain))
Y_bar <- matrix(colMeans(ytrain), nrow = nrow(ytrain), ncol = ncol(ytrain))

X_centered <- as.matrix(xtrain - X_bar)
Y_centered <- as.matrix(ytrain - Y_bar)

sigma_hat_X <- colSds(X_centered) * (nobs)/(nobs - 1)
sigma_hat_Y <- colSds(Y_centered) * (nobs)/(nobs - 1)

X_red <- X_centered %*% diag(1/sigma_hat_X)
Y_red <- Y_centered / sigma_hat_Y

X_test_red <- scale(xtest) * (nobs)/(nobs - 1)
Y_test_red <- scale(ytest) * (nobs)/(nobs - 1)

#####     Cas 2. X centree.     #####

reg.X.centre <- glmnet(X_centered,
                       as.matrix(ytrain),
                       family = 'gaussian',
                       alpha = 0,
                       lambda = grid,
                       thresh = 2*10^(-11))

# Fonction qui calcule l'intercept pour un lambda donne

intercept.X.centre <- function(nLambda) {
  reg.X.centre.coeffs <- reg.X.centre$beta@x[(1 + (nLambda-1)*700):(nLambda*700)]
  intercept <- reg.X.centre$a0[nLambda] - (X_bar %*% reg.X.centre.coeffs)[1]
  return(intercept)
}

intercepts.X.centre <- sapply(c(1:100), FUN = intercept.X.centre)

indices.non.na <- which(!is.na(intercepts.X.centre), arr.ind = TRUE)
plot(intercepts.X.centre[indices.non.na], type = 'l',
     main = 'Intercept vs. \n parametre de regularisation \n (X centree)',
     xlab = 'Parametre de regularisation',ylab = 'Intercept', xaxt = 'n') 
axis(1, at = 1:100, labels=signif(grid, digits = 2))

# C'est exactement le meme graphe, on obtient le meme resultat
# en utilisant la formule de la partie 1 que les intercepts
# du modele original. Cela montre que la formule est bonne.

# Graphe dans l'echelle centre
plot(reg.X.centre$a0, type = 'l',
     main = 'Intercept (X centree) \n (echelle centree)',
     xlab = 'Parametre de regularisation',ylab = 'Intercept', xaxt = 'n') 
axis(1, at = 1:100, labels=signif(grid, digits = 2))

#####     Cas 2. Y centree.     #####

reg.Y.centre <- glmnet(xtrain,
                       as.matrix(Y_centered),
                       family = 'gaussian',
                       alpha = 0,
                       lambda = grid,
                       thresh = 2*10^(-11))

# Fonction qui calcule l'intercept pour un lambda donne

intercept.Y.centre <- function(nLambda) {
  intercept <- reg.Y.centre$a0[nLambda] + Y_bar[1]
  return(intercept)
}

intercepts.Y.centre <- sapply(c(1:100), FUN = intercept.Y.centre)

indices.non.na <- which(!is.na(intercepts.Y.centre), arr.ind = TRUE)
plot(intercepts.Y.centre[indices.non.na], type = 'l',
     main = 'Intercept vs. \n parametre de regularisation \n (Y centree)',
     xlab = 'Parametre de regularisation',ylab = 'Intercept', xaxt = 'n') 
axis(1, at = 1:100, labels=signif(grid, digits = 2))

# C'est exactement le meme graphe

# Graphe dans l'echelle centre
plot(reg.Y.centre$a0, type = 'l',
     main = 'Intercept (Y centree) \n (echelle centree)',
     xlab = 'Parametre de regularisation',ylab = 'Intercept', xaxt = 'n') 
axis(1, at = 1:100, labels=signif(grid, digits = 2))

#####     Cas 3. X et Y centrees.     #####

reg.XY.centre <- glmnet(X_centered,
                        as.matrix(Y_centered),
                        family = 'gaussian',
                        alpha = 0,
                        lambda = grid,
                        thresh = 2*10^(-11))

# Fonction qui calcule l'intercept pour un lambda donne

intercept.XY.centre <- function(nLambda) {
  reg.XY.centre.coeffs <- reg.XY.centre$beta@x[(1 + (nLambda-1)*700):(nLambda*700)]
  intercept <- reg.XY.centre$a0[nLambda] + (Y_bar[1] - (X_bar %*% reg.XY.centre.coeffs)[1])
  return(intercept)
}

intercepts.XY.centre <- sapply(c(1:100), FUN = intercept.XY.centre)

indices.non.na <- which(!is.na(intercepts.XY.centre), arr.ind = TRUE)
plot(intercepts.XY.centre[indices.non.na], type = 'l',
     main = 'Intercept vs. \n parametre de regularisation \n (X et Y centrees)',
     xlab = 'Parametre de regularisation',ylab = 'Intercept', xaxt = 'n') 
axis(1, at = 1:100, labels=signif(grid, digits = 2))

# Graphe dans l'echelle centre
plot(reg.XY.centre$a0, type = 'l',
     main = 'Intercept (X et Y centrees) \n (echelle centree)',
     xlab = 'Parametre de regularisation',ylab = 'Intercept', xaxt = 'n') 
axis(1, at = 1:100, labels=signif(grid, digits = 2))

# C'est exactement le meme graphe

# On a alors verifie tous les trois cas.

########## Theta dans la limite tau -> 0 ##########

#####     Estimation avec glmnet      #####

# Premier essai en utilisant les mêmes paramètres précédents

reg.glmnet     <- glmnet(X_red,
                         Y_red,
                         family = 'gaussian',
                         alpha = 0,
                         lambda = grid,
                         thresh = 2*10^(-11), 
                         standardize = FALSE,
                         intercept = FALSE)

coeffs.glmnet <- as.matrix(predict(reg.glmnet, s = (min(reg.glmnet$lambda) / (nobs)^2),
                                   type = "coefficients",
                                   exact = TRUE, x = X_red, y = Y_red)[-1,])

View(coeffs.glmnet)

#####   Estimation a la main avec thm. 1.4  #####
ridge.manuel <- solve(crossprod(X_red,X_red) + diag(min(reg.glmnet$lambda) / nobs, ncol(xtrain)),
                      crossprod(X_red,Y_red))

# On voit que glmnet ne converge pas, donc on change le paramètre
# thresh de manière qu'il converge avec des lambdas plus petits 

reg.glmnet     <- glmnet(X_red,
                         Y_red,
                         family = 'gaussian',
                         alpha = 0,
                         lambda = grid,
                         thresh = 8*10^(-10), 
                         standardize = FALSE,
                         intercept = FALSE)

coeffs.glmnet <- as.matrix(predict(reg.glmnet, s = (min(reg.glmnet$lambda) / (nobs)^2),
                                   type = "coefficients",
                                   exact = TRUE, x = X_red, y = Y_red)[-1,])

View(coeffs.glmnet)

# Maintenant on a des meilleurs coefficients

#####     Estimation avec lm.ridge     #####
#####         (QUESTION 3.2)           #####
# Ne penalise pas l'intercept automatiquement

reg.lm.ridge <- lm.ridge(sucres ~.,
                         data = as.data.frame(cbind(Y_red, X_red)),
                         lambda = grid)

coeffs.lm.ridge <- tail(t(reg.lm.ridge$coef), n = 1)

#####     Estimation avec formules de 1.3.     #####

# On doit trouver les v_i, qui sont vec. prop. de X'X

# On travaillera seulement avec les 39 premiers 
rank.Xt.X <- rankMatrix(t(X_red) %*% X_red)[1]

V <- eigen(t(X_red) %*% X_red)$vectors[,1:rank.Xt.X]

sigmas.2 <- eigen(t(X_red) %*% X_red)$values[1:rank.Xt.X]

# Voici une fonction qui calcule une composante de la somme
# donnee dans la matrice A
composante.i <- function(i){(1/sigmas.2[i]) * drop(V[,i] %o% t(V[,i]))}

# On cree maintenant toutes les composantes de la somme.
composantes <- lapply(c(1:rank.Xt.X), composante.i)

# On realise la somme effectivement
somme <- Reduce(`+`, composantes)

# COMPARAISON ENTRE RIDGE MANUEL ET COEFFS.LIMITE :
matrice.inverse <- solve(crossprod(X_red,X_red) + diag(nobs * min(reg.glmnet$lambda), ncol(xtrain)), diag(ncol(xtrain)))
dim(matrice.inverse)
dim(somme)
View(somme - matrice.inverse)

coeffs.limite <- somme %*% (t(X_red) %*% Y_red)

#####     Comparaisons     #####

View(coeffs.glmnet) 
View(ridge.manuel)
View(coeffs.lm.ridge) 
View(coeffs.limite)   

#On regarde le max de l'ecart absolu entre les differents theta pour
#les comparer :
# par rapport à coeffs.limite :
print(max(abs(coeffs.limite - t(coeffs.lm.ridge))))
print(max(abs(coeffs.limite - coeffs.glmnet)))
print(max(abs(coeffs.limite - ridge.manuel)))

# par rapport à ridge.manuel :
print(max(abs(ridge.manuel - t(coeffs.lm.ridge))))
print(max(abs(ridge.manuel - coeffs.glmnet)))
print(max(abs(ridge.manuel - coeffs.limite)))

# par rapport à coeffs.lm.ridge :
print(max(abs(t(coeffs.lm.ridge) - ridge.manuel)))
print(max(abs(t(coeffs.lm.ridge) - coeffs.glmnet)))
print(max(abs(t(coeffs.lm.ridge) - coeffs.limite)))

# par rapport à coeffs.glmnet :
print(max(abs(coeffs.glmnet - t(coeffs.lm.ridge))))
print(max(abs(coeffs.glmnet - ridge.manuel)))
print(max(abs(coeffs.glmnet - coeffs.limite)))

##########   PARTIE 3 QUESTION 3   ##########

# Affiner l'etendue de la grille
grid <- 10^seq(-6,7,length=500)

# Choix de lambda par validation croisee
set.seed(1)
nplis <- 4
cv.segments <- cvsegments(N = nobs,
                          k = nplis)

# 3. Calculer l'erreur sur chaque pli et pour chaque
# valeur du parametre

cv.erreurs  <- c()

for(pli in 1:nplis){ 
  indices.pli <- unlist(cv.segments[[pli]])
  
  X.hors.pli  <- X_red[-c(indices.pli),]
  Y.hors.pli  <- Y_red[-c(indices.pli),]
  
  X.pli <- X_red[c(indices.pli),]
  Y.pli <- Y_red[c(indices.pli),] 
  
  reg.glmnet.cv  <- glmnet(X.hors.pli,
                           Y.hors.pli,
                           family = 'gaussian',
                           alpha = 0,
                           lambda = (1/nrow(X.hors.pli)) * grid,
                           standardize = FALSE,
                           intercept = FALSE)
  
  cv.preds <- predict(reg.glmnet.cv,
                      newx = X.pli,
                      type = "response",
                      s = (1/nrow(X.hors.pli)) * grid)
  
  # Chaque colonne donne la prediction pour un lambda
  
  erreur.pli <- colMeans((cv.preds - Y.pli)^2)
  cv.erreurs <- cbind(cv.erreurs, erreur.pli)
  # Colonne : pli
  # Ligne : lambda
  # Valeur : erreur du pli pour ce lambda.
}

# Le resultat souhaite est stocke dans cv.erreurs.

# Representer pour chaque valeur du parametre l'erreur
# moyenne et un intervalle de confiance de cette erreur
erreur.moyenne    <- rowMeans(cv.erreurs)
ecart.type.erreur <- rowSds(cv.erreurs)
weightserreur <- cv.erreurs * (1/(rowSums(cv.erreurs)))
cvsd <- sqrt(rowSums((cv.erreurs - erreur.moyenne)^2 * weightserreur) / (nrow(cv.erreurs) - 1))

plot(x = log((1/nobs) * grid),
     y = erreur.moyenne,
     type = 'l', col = 'blue',
     ylim = c(0.2, 1.1), xlim = c(-13, 13),
     main = 'evolution de l\'erreur moyenne',
     xlab = expression(paste('log(', tau,')')),
     ylab = 'Erreur moyenne')

lines(x = log((1/nobs) * grid), y = (erreur.moyenne + cvsd), type = 'l', lty = 2, col = 'red')
lines(x = log((1/nobs) * grid), y = (erreur.moyenne - cvsd), type = 'l', lty = 2, col = 'red')

tau.opti <- grid[which.min(erreur.moyenne)] / nobs
print(tau.opti)
print(min(erreur.moyenne))

# Comparer avec la representation des resultats de la
# fonction cv.glmnet.
cv.par.glmnet <- cv.glmnet(x = X_red,
                           y = Y_red,
                           lambda = (1/nobs)*grid,
                           type.measure = "mse",
                           nfolds = 4)

plot(cv.par.glmnet)

# Choisir le parametre que vous semble optimum.

# Par cv.glmnet
tau.cv.glmnet <- cv.par.glmnet$lambda.1se
min(cv.par.glmnet$cvm)

# Reajuster sur la totalite du jeu d'apprentissage puis
# calculer l'erreur de generalisation
reg.glmnet.final  <- glmnet(X_red,
                            Y_red,
                            family = 'gaussian',
                            alpha = 0,
                            lambda = grid,
                            thresh = 8*10^(-10),
                            standardize = FALSE,
                            intercept = FALSE)

pred.final <- predict(reg.glmnet.final,
                      newx = X_test_red,
                      type = "response",
                      s = tau.cv.glmnet / nobs)

# Chaque colonne donne la prediction pour un lambda

erreur.gen <- mean((pred.final - Y_test_red)^2)
print(erreur.gen) # 0.1592101 pour cv.glmnet 
# 0.1703468 pour choix manuel

# DANS L'eCHELLE ORIGINALE
reg.glmnet.final.original  <- glmnet(xtrain,
                                     as.matrix(ytrain),
                                     family = 'gaussian',
                                     alpha = 0,
                                     lambda = grid,
                                     thresh = 8*10^(-10),
                                     standardize = FALSE,
                                     intercept = FALSE)

pred.final.original <- predict(reg.glmnet.final.original,
                               newx = as.matrix(xtest),
                               type = "response",
                               s = tau.cv.glmnet / nobs)

erreur.gen.original <- mean((pred.final.original - as.matrix(ytest))^2)
print(erreur.gen.original)

# 4.1 -----------------------------------------------

#On s'interese au seuil de 18 pour la teneur en sucre. 
#On cree donc 2 variables z et ztest qui contiennent les valeurs
#de l'indicatrice de depassement de seuil pour les jeux de donnees
#ytrain et ytest.
z <- ifelse(ytrain<18,0,1)
ztest <- ifelse(ytest<18,0,1)

#Tables de confusion :
table(z)
table(ztest)

# 4.2 -----------------------------------------

#Grille de lambda :
grid=10^seq(3,-6,length=100)

#Cas ridge :
cv.ridge <- cv.glmnet(x = as.matrix(xtrain), y = z, alpha = 0, family = "binomial",
                      nfolds = 5,lambda=grid,type.measure="deviance")

#Cas lasso :
cv.lasso <- cv.glmnet(x = as.matrix(xtrain), y = z, alpha = 1, family = "binomial",
                      nfolds = 5,lambda=grid,type.measure="deviance")

#On recupere les lambdas optimaux :
lambda.ridge <- formatC(cv.ridge$lambda.min,format='e',digits=2)
lambda.lasso <- formatC(cv.lasso$lambda.min,format='e',digits=2)

#On construit les modeles finaux avec les valeurs optimales de lambda :
model.ridge <- glmnet(x = as.matrix(xtrain), y = z, alpha = 0, family = "binomial",
                      lambda = lambda.ridge)
model.lasso <- glmnet(x = as.matrix(xtrain), y = z, alpha = 1, family = "binomial",
                      lambda = lambda.lasso)

#On represente les coefficients en fonction de leur norme L^1 :
plot(cv.ridge$glmnet.fit,"norm",xlim=c(0,2500))
plot(cv.lasso$glmnet.fit,"norm",xlim=c(0,2500))

#Nombre de coefficients non nuls :
cv.ridge$nzero
cv.lasso$nzero

# 4.3 -------------------------------------------
#On trace les courbes ROc pour les donnees d'apprentissage :

#On calcule les predictions sur le jeu de test xtrain :
prob.ridge.appr <- predict(model.ridge, type="response", newx = as.matrix(xtrain))
prob.lasso.appr <- predict(model.lasso, type="response", newx = as.matrix(xtrain))

pred.ridge.appr <- prediction(prob.ridge.appr,z) 
pred.lasso.appr <- prediction(prob.lasso.appr,z) 

#On trace les courbes ROC
par(mfrow=c(1,2))

#On trace la courbe de la regle parfaite
plot(x = c(0,0,1), y = c(0,1,1),type='l',lty=3,col=1,main='Courbe ROC \n(ridge)'
     ,xlab = "False positive rate",ylab="Sensitivity")

plot(performance(pred.ridge.appr,"sens","fpr"),col=2,add='TRUE')

#On trace la courbe de la regle aleatoire
lines(c(0,1),c(0,1),col=1,lty=2) 

#On trace la courbe de la regle parfaite
plot(x = c(0,0,1), y = c(0,1,1),type='l',lty=3,col=1,main='Courbe ROC \n(lasso)'
     ,xlab = "False positive rate",ylab="Sensitivity")

plot(performance(pred.lasso.appr,"sens","fpr"),col=2,add='TRUE')

#On trace la courbe de la regle aleatoire
lines(c(0,1),c(0,1),col=1,lty=2)  

#On calcule les aires sous les courbes ROC
performance(pred.ridge.appr,'auc')@y.values[[1]]
performance(pred.lasso.appr,'auc')@y.values[[1]]

#On trace les courbes ROc pour les donnees de test :
prob.ridge.test <- predict(model.ridge, type="response", newx = as.matrix(xtest))
prob.lasso.test <- predict(model.lasso, type="response", newx = as.matrix(xtest))

pred.ridge.test = prediction(prob.ridge.test,ztest) 
pred.lasso.test = prediction(prob.lasso.test,ztest) 

#On trace les courbes ROC superposees
par(mfrow=c(1,1))

#On trace la courbe de la regle parfaite
plot(x = c(0,0,1), y = c(0,1,1),type='l',lty=3,col=1,main='Courbes ROC'
     ,xlab = "False positive rate",ylab="Sensitivity")

plot(performance(pred.ridge.test,"sens","fpr"),col=2,add='TRUE')
plot(performance(pred.lasso.test,"sens","fpr"),col=3,add='TRUE')
legend("bottomright",legend=c("ridge", "lasso"),
       col=c("red", "green"), pt.cex=0.5,cex=0.7, lty=c(1,1),bty='n')

#On trace la courbe de la regle aleatoire
lines(c(0,1),c(0,1),col=1,lty=2)          

#On calcule les aires sous les courbes ROC
performance(pred.ridge.test,'auc')@y.values[[1]]
performance(pred.lasso.test,'auc')@y.values[[1]]

