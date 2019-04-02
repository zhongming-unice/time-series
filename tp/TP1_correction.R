##############
# Exercice 3 #
##############

# Clear
rm(list=ls(all=TRUE))
# Ecart-type
sigma = 0.5
# Nombre d'échantillons
nt = 10
# Taille des échantillons
nx = 1000
# Génération d'un bruit blanc gaussien de variance sigma^2 et de longueur nx
Z=matrix(nrow=nt,ncol=nx)
for (i in (1:nt))
{
Z[i,1:nx] = rnorm(nx)*sigma
}
# Vérification de la moyenne et de la variance
moyenne = 0
variance = 0
for (i in (1:nt))
{
moyenne[i] = mean(Z[i,])
variance[i] = mean((Z[i,]-moyenne[i])^2)
}
# Paramètre
theta = 0.8
# Processus X
X = matrix(nrow=(nt-2),ncol=nx)
for (i in (3:nt))
{
X[i-2,] = Z[i,]+theta*Z[(i-2),]
}
# Auto-covariance
h = 2
autocov = 0
for (i in (1:(nt-2-h)))
{
autocov[i] = cov(X[(i+h),],X[i,])
}
# Auto-corrélation (nécessite l'auto-covariance h=0)
autocov0 = 0
for (i in (1:(nt-2)))
{
autocov0[i] = cov(X[i,],X[i,])
}
autocor = 0
for (i in (1:(nt-2-h)))
{
autocor[i] = autocov[i]/autocov0[i]
}
# Variance de la moyenne des Xi
Y = (X[1,]+X[2,]+X[3,]+X[4,])/4
varY = var(Y)

##############
# Exercice 4 #
##############

# Clear
rm(list=ls(all=TRUE))
# Nombre d'échantillons
nt = 10
# Taille des échantillons
nx = 1000
# Génération d'un bruit blanc gaussien de variance 1 et de longueur nx
Z=matrix(nrow=nt,ncol=nx)
for (i in (1:nt))
{
Z[i,1:nx] = rnorm(nx)
}
# Processus X
X = matrix(nrow=(nt-1),ncol=nx)
for (i in (2:nt))
{
if ((i %% 2) == 0)
{
X[i-1,] = Z[i,]
}
else
{
X[i-1,] = (Z[i-1,]^2-1)/(sqrt(2))
}
}
# Dessin de X
plot(density(X[1,]))
for (i in (2:(nt-1)))
{
lines(density(X[i,]))
}
# Moyennes et variances
moyenne = 0
variance = 0
for (i in (1:(nt-1)))
{
moyenne[i] = mean(X[i,])
variance[i] = var(X[i,])
}


