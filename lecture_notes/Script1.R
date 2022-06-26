alpha=0.05
n=100000
1-(1-alpha/n)^n

n=100
x=rnorm(n)
p=2*(1-pnorm(abs(x)))
p_sort=sort(p)
p_sort[1]<alpha/n
plot(p_sort)
abline(0,1/n, col="red")
abline(h=alpha/n, col="green")


n=100
x=c(rnorm(n-1),10)
p=2*(1-pnorm(abs(x)))
p_sort=sort(p)
p_sort[1]<alpha/n
plot(p_sort)
abline(0,1/n, col="red")
abline(h=alpha/n, col="green")

