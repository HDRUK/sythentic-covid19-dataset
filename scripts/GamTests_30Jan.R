library(mgcv)
library(tidyr)

# Define log-normal function
lognormal <- function(x, mu, sigma) {
  (1/(x*sigma*sqrt(2*pi)))*exp(-((log(x)-mu)^2)/(2*sigma^2))
}


x <- seq(1,10,0.01)
df <- data.frame(x=x,y=lognormal(x,1.1,0.7))

df %>% ggplot(aes(x=x,y=y)) +
       geom_line()

model <- gam(y ~ te(x, fx = lognormal, by = list(mu = c(1, 2), sigma = c(0.5, 1))), data = df)
