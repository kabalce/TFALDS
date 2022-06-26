n=10000
#beta=0.6
eps=0.05
#r=1
#mu=sqrt(2*r*log(n))
mu=3
c0=1
cA=1
rep=1000
FWER=c()
FDR=c()
cost1=c()
thres=(mu/2)+(log(c0/cA)+log((1-eps)/eps))/mu


for (i in 1:rep) {
altern=rbinom(n,1,eps)  #=0 for H0
X=rnorm(n)*(1-altern)+rnorm(n,mu)*altern
result=(X>=thres)       #=1, when H0 is rejected
V=result&(1-altern)     #=1, when H0 is rejected and H0 is true
T=(1-result)&(altern)   #=1, when H0 is accepted and H1 is true
FWER=c(FWER,(sum(V)>=1))
FDR=c(FDR,sum(V)/max(sum(result),1))
cost1=c(cost1, (c0*sum(V)+cA*sum(T)))
}

mean(FWER)
mean(FDR)
mean(cost1)

q=0.05
BFDR=function(x){F0=pnorm(x)
F=(1-eps)*pnorm(x)+eps*pnorm(x,mean = mu) 
return((1-eps)*(1-F0)/(1-F)-q)}

x=seq(-10,10,0.02)
plot(BFDR(x)~x)

cBFDR=uniroot(BFDR, interval = c(-10,10))$root


FWER=c()
FDR=c()
R=c()
cost2=c()

for (i in 1:rep) {
  altern=rbinom(n,1,eps)  #=0 for H0
  X=rnorm(n)*(1-altern)+rnorm(n,mu)*altern
  result=(X>=cBFDR)       #=1, when H0 is rejected
  V=result&(1-altern)     #=1, when H0 is rejected and H0 is true
  T=(1-result)&(altern)   #=1, when H0 is accepted and H1 is true
  FWER=c(FWER,(sum(V)>=1))
  FDR=c(FDR,sum(V)/max(sum(result),1))
  R=c(R, sum(result)>0)
  cost2=c(cost2, (c0*sum(V)+cA*sum(T)))
}

mean(FWER)
mean(FDR)
mean(FDR)/mean(R)
mean(cost2)

