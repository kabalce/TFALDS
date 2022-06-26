###Data generation
mu=7
eps=0.1
n=200
altern=rbinom(n,1,eps)
muvec=3*(1-altern)+mu*altern
X=rnorm(n,muvec,1)
hist(X)


#####initialization
eps_k=rep(0,500)
mu_k=rep(0,500)
eps_k[1]=0.5
mu_k[1]=mean(X)
D=1
k=1

####EM algorithm
while (D>10^(-5) & k<500){
  ####expectation step
  
  pi=dnorm(X-mu_k[k])*eps_k[k]/(dnorm(X-mu_k[k])*eps_k[k]+dnorm(X-3)*(1-eps_k[k]));
 
  
  ####maximization step
  mu_k[k+1]=sum(pi*X)/sum(pi)
  eps_k[k+1]=sum(pi)/n;
  
  ###stop criterion
  D=(mu_k[k+1]-mu_k[k])^2+(eps_k[k+1]-eps_k[k])^2;
  k=k+1;
}


#### Analysis of results

k


plot(eps_k[eps_k>0], pch=19, col=4)
abline(h=eps)
plot(mu_k[mu_k>0], pch=19, col=2)
abline(h=mu)

