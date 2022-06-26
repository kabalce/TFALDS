left_func<-function(t){dnorm(t)/t*(1-1/(t^2))}
middle_func<-function(t){1-pnorm(t)}
right_func<-function(t){dnorm(t)/t}


plot(t,type="n", xlim = c(0,10), ylim=c(0,0.5))
curve(left_func,from = 0, to=10, add = T, col="red")
curve(right_func,from = 0, to=10, add = T, col="orange")
curve(middle_func,from = 0, to=10, add = T)

plot(t,type="n", xlim = c(2,10), ylim=c(0,0.05))
curve(left_func,from = 0, to=10, add = T, col="red")
curve(right_func,from = 0, to=10, add = T, col="orange")
curve(middle_func,from = 0, to=10, add = T)


middle_func(2:10)-left_func(2:10)
right_func(2:10)-middle_func(2:10)
