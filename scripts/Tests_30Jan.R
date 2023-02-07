library(gplite)
library(ggplot2)

set.seed(2)
n <- 6000
n_per_cluster <- n/3
d <- 2
x <- 0.5*cbind( matrix(rnorm(n_per_cluster*d), nrow=d) + c(3,3),
                matrix(rnorm(n_per_cluster*d), nrow=d) - c(0,0),
                matrix(rnorm(n_per_cluster*d), nrow=d) + c(-3,3))
x <- t(x)
y <- c(rep(0,n_per_cluster), rep(1,n_per_cluster), rep(0,n_per_cluster))

# plot
ggplot() +
  geom_point(data=data.frame(x=x[,1],y=x[,2]), aes(x=x,y=y), color=y+2, size=1) +
  xlab('x1') + ylab('x2')

gp <- gp_init(
  lik = lik_bernoulli(),
  cfs = cf_sexp(),
  method = method_fitc(num_inducing=50)
)
gp <- gp_optim(gp,x,y)

# predict
ng <- 20
x1g <- seq(-4,4,len=ng)
x2g <- seq(-2,4,len=ng)

xnew <- cbind( rep(x1g,each=ng), rep(x2g,ng) )
pred <- gp_pred(gp, xnew, transform=T)
prob <- pred$mean

# visualize
ggplot() +
  geom_contour(data=data.frame(x=xnew[,1], y=xnew[,2], prob=prob),
               aes(x=x, y=y, z=prob, colour=..level..) ) +
  scale_colour_gradient(low = "red", high = "green", guide='none') +
  geom_point(data=data.frame(x=x[,1],y=x[,2]), aes(x=x,y=y), color=y+2, size=1) +
  xlab('x1') + ylab('x2')
