################################## TP 2 ####################################################################
############################################################################################################

############################## TD5 EXERCICE 3 ##############################################################

# Clear
rm(list=ls(all=TRUE))
# Standard deviation
sigma=1
# number of samples
nt = 25
# sample size
nx = 10000
# mean of Xt
mu=0
# MA(1) parameter
theta=-0.6

############# Generating a Gaussian white noise with variance sigma^2 and number of observations nx ########

Z=matrix(nrow=nt,ncol=nx)
for (i in (1:nt))
{
  Z[i,1:nx] <-rnorm(nx, mean = 0, sd = sigma)
}

######################## Generating MA(1) process X ##########################################################

X = matrix(nrow=nt-1,ncol=nx) 
for (i in (2:nt))
{
  X[i-1,]<-mu+Z[i,]+theta*Z[i-1,]
}

############################# Plotting #######################################################################

par(mfrow = c(2, 1))
plot(X[3,1:100],type="b")  #Time series plot
acf(X[3,],lag.max=20) #Sample Autocorrelation Function plot


########################## Determination of the autocovariance sum v #########################################

v = rep(0,nt-1)
for (i in (1:(nt-1)))
{
  for (h in ((1-i):(nt-1-i)))
  {
    
    v[i]=v[i]+cov(X[i,],X[(i+h),])
    
  }  
} 

######################### 95% confidence interval ############################################################

Er=1.96*sqrt(v)/sqrt(nx)

#Comparison with theoretical results
v_theory=(sigma*(1+theta))**2
Er_theory=1.96*sqrt(v_theory)/sqrt(nx)
print(Er_theory)
print(Er)

IC_left = rep(0,nt-1)
IC_right = rep(0,nt-1)
for (i in (1:nt-1))
{
  IC_left[i]=mean(X[i,])-Er[i]
  IC_right[i]=mean(X[i,])+Er[i]
  
}  

######################### Test if mu is in IC ##############################################################

print(mu >= IC_left & mu <= IC_right)

############################################################################################################
#################################### TD5 EXERCICE 4.b ###################################################### 
############################################################################################################

######################### Element (1,1) and (2,2) of covariance matrix W ###################################

W11 = rep(0,nt-3)
W22 = rep(0,nt-5)
for (i in (1:(nt-3)))
{
  for (k in (1:(nt-2-i)))
  {
    W11[i]= W11[i]+((cov(X[i,],X[i+k+1,])/cov(X[i,],X[i,]))+(cov(X[i,],X[i+k-1,])/cov(X[i,],X[i,]))-2*(cov(X[i,],X[i+1,])*cov(X[i,],X[i+k,])/cov(X[i,],X[i,])**2))**2
    
  }
} 

for (i in (2:(nt-4)))
{
  for (k in (1:(nt-3-i)))
  {
    W22[i-1]= W22[i-1]+((cov(X[i,],X[i+k+2,])/cov(X[i,],X[i,]))+(cov(X[i,],X[i+k-2,])/cov(X[i,],X[i,]))-2*(cov(X[i,],X[i+2,])*cov(X[i,],X[i+k,])/cov(X[i,],X[i,])**2))**2
    
  }
}  

######################### 95% confidence interval of rho(1) and rho(2) #####################################

Er_rho1=1.96*sqrt(W11)/sqrt(nx)
Er_rho2=1.96*sqrt(W22)/sqrt(nx)

#Comparison with theoretical results
Er_rho1_theory=1.96*sqrt(1-3*(theta/(1+theta**2))**2+4*(theta/(1+theta**2))**4)/sqrt(nx)
Er_rho2_theory=1.96*sqrt(1+2*(theta/(1+theta**2))**2)/sqrt(nx)

print(Er_rho1_theory)
print(Er_rho1)
print(Er_rho2_theory)
print(Er_rho2)

IC_left_Rho1 = rep(0,nt-3)
IC_right_Rho1 = rep(0,nt-3)
for (i in (1:(nt-3)))
{
  acf1=acf(X[i,],plot=F) #Sample Autocorrelation Function
  rho_hat1=acf1$acf[2]   #Sample Autocorrelation Function for h=1
  IC_left_Rho1[i]=rho_hat1-Er_rho1[i]
  IC_right_Rho1[i]=rho_hat1+Er_rho1[i]
  
}  

IC_left_Rho2 = rep(0,nt-5)
IC_right_Rho2 = rep(0,nt-5)
for (i in (1:(nt-5)))
{
  acf2=acf(X[i+1,],plot=F) #Sample Autocorrelation Function
  rho_hat2=acf2$acf[3]     #Sample Autocorrelation Function for h=2
  IC_left_Rho2[i]=rho_hat2-Er_rho2[i]
  IC_right_Rho2[i]=rho_hat2+Er_rho2[i]
  
}  
