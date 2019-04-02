##############
# Exercice 4 #
##############

# Clear
rm(list=ls(all=TRUE))
# Nombre d'échantillons
nt = ??
# Taille des échantillons
nx = ??
# Génération d'un bruit blanc gaussien de variance 1 et de longueur nx
Z=matrix(nrow=nt,ncol=nx)
for (i in (1:nt))
{
Z[i,1:nx] = ??
}
# Processus X
X = matrix(nrow=(nt-1),ncol=nx)
for (i in (2:nt))
{
if ((i %% 2) == 0)
{
X[i-1,] = ??
}
else
{
X[i-1,] = ??
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
moyenne[i] = ??
variance[i] = ??
}


