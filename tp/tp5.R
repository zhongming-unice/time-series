mu = 0
phi = 0.6
x_tile = 0.271
sigma = sqrt(2)
nt = 25
nx = 10000

z = matrix(nrow = nt, ncol = nx)
for (i in 1:nt){
  z[i,1:nx]<-rnorm(nx,mean = 0,sd = sigma)
}

x = matrix(nrow = nt, ncol = nx)

x[1,] <- arima.sim(list(ar = c(phi)),n = nx,innov = z[1,])
for (i in 2:nt){
  x[i,] <- mu + phi*(x[i-1,]-mu)+z[i,]
}


