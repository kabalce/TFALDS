n=10000
alpha=0.05
Rep=1000

#H_0
p<-c()

for (i in 1:Rep) {
y0=rnorm(n)
T=sum(y0*y0)
Z=(T-n)/sqrt(2*n)
p=c(p,abs(Z)>abs(qnorm(0.05/2)))}

P_Type1Er=sum(p)/Rep
P_Type1Er


#power

mu=runif(n,0,1)
k<-1
mu=mu/sqrt(sum(mu*mu))*sqrt(k*sqrt(2*n))


theta=sum(mu*mu)/sqrt(2*n)
theta

p<-c()


for (i in 1:Rep) {
  y1=rnorm(n)+mu
  T=sum(y1*y1)
  Z=(T-n)/sqrt(2*n)
  p=c(p,abs(Z)>abs(qnorm(0.05)))}

Power=sum(p)/Rep
Power

#comparison Bonferroni and chisquare test

n=10000
alpha=0.05
Rep=1000

#Example 1

mu1=rep(1,n)

p1<-c()
p2<-c()

for (i in 1:Rep) {
  y1=rnorm(n)+mu1
  T=sum(y1*y1)
  Z=(T-n)/sqrt(2*n)
  p1=c(p1,abs(Z)>abs(qnorm(0.05/2)))
  p2=c(p2,max(abs(y1))>abs(qnorm(alpha/(2*n))))}


Power_chi=sum(p1)/Rep
Power_chi
Power_Bonf=sum(p2)/Rep
Power_Bonf


#Example 2

sqrt(2*log(n))

mu1=c(7,rep(0,n-1))

p1<-c()
p2<-c()

for (i in 1:Rep) {
  y1=rnorm(n)+mu1
  T=sum(y1*y1)
  Z=(T-n)/sqrt(2*n)
  p1=c(p1,abs(Z)>abs(qnorm(0.05)))
  p2=c(p2,max(abs(y1))>abs(qnorm(alpha/(2*n))))}


Power_chi=sum(p1)/Rep
Power_chi
Power_Bonf=sum(p2)/Rep
Power_Bonf

