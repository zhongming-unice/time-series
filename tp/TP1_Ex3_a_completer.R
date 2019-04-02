##############
# Exercice 3 #
##############

# Clear
rm(list=ls(all=TRUE))
# Ecart-type
sigma = ??
# Nombre d'échantillons
nt = ??
# Taille des échantillons
nx = ??
# Génération d'un bruit blanc gaussien de variance sigma^2 et de longueur nx
Z=matrix(nrow=nt,ncol=nx)
for (i in (1:nt))
{
Z[i,1:nx] = ??
}
# Vérification "manuelle" de la moyenne et de la variance
moyenne = 0
variance = 0
for (i in (1:nt))
{
moyenne[i] = ??
variance[i] = ??
}
# Paramètre
theta = ??
# Processus X
X = matrix(nrow=(nt-2),ncol=nx)
for (i in (3:nt))
{
X[i-2,] = ??
}
# Auto-covariance
h = ??
autocov = 0
for (i in (1:(nt-2-h)))
{
autocov[i] = ??
}
# Auto-corrélation (nécessite l'auto-covariance h=0)
autocov0 = 0
for (i in (1:(nt-2)))
{
autocov0[i] = ??
}
autocor = 0
for (i in (1:(nt-2-h)))
{
autocor[i] = ??
}
# Variance de la moyenne des Xi
Y = ??
varY = ??

