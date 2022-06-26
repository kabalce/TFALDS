a_0=0.05
A_Bonf<-function(n, alpha=a_0){alpha/n}
A_Sidak<-function(n, alpha=a_0){1-(1-alpha)^{1/n}}

n=10^(seq(1,10,0.05))

plot(A_Bonf(n)~seq(1,10,0.05), type = "l", col="red")
lines(A_Sidak(n)~seq(1,10,0.05))

A_Bonf(n)/A_Sidak(n)

plot(A_Bonf(n)/A_Sidak(n)~seq(1,10,0.05), type = "l", col="red")


1/(1+a_0/2)
