library(lme4)
library(lattice)
library(ggplot2)
library(dplyr)
library(mgcv)
library(broom.mixed)

data1 <- as.data.frame(nlme::Orthodont)
data1$Subject <- factor(data1$Subject, ordered = FALSE)


mod2 <- lmer(distance ~ age + (1|Subject), data=data1, REML=F)
summary(mod2)


lognormal <- function(x,mu=log(50),sd=log(2)){
  exp(-0.5*(log(x)-mu)^2/sd^2)
}


data <- data.frame(x=seq(0,200,0.1)) %>%
        mutate(y=lognormal(x))

data %>% ggplot(aes(x=x)) +
  geom_line(aes(y=y))


time <- seq(0,400,0.1)
measurements <- sample(time,length(time)*0.5,replace = T)

data_d1 <- data.frame(time=measurements) %>%
           rowwise() %>%
           mutate(sex=sample(c(1,2), 1)) %>%
           #mutate(y = lognormal(time,mu=log(50*sex^0.5))*rnorm(1,2,0.5)) %>%
           mutate(y = rnorm(1,1,0.1)*lognormal(time,mu=log(40*sex),sd=log(2))) %>%
           mutate(dose=1)

#time <- seq(0,400,0.1)
measurements <- sample(time,length(time)*0.3,replace = T)
data_d2 <- data.frame(time=measurements) %>%
  rowwise() %>%
  mutate(sex=sample(c(1,2), 1)) %>%
  #mutate(y = rnorm(1,0.8,0.1) + lognormal(time,mu=log(50*sex^0.5),sd=log(3))*rnorm(1,3,0.5)) %>%
  mutate(y = rnorm(1,1,0.1)*lognormal(time,mu=log(50*sex),sd=log(3))) %>%
  mutate(dose=2)


data <- data_d1 %>% rbind(data_d2) %>% ungroup #%>% mutate(dose=factor(dose))
data %>% ggplot(aes(x=time,y=y,color=factor(dose))) + geom_point() +
  facet_grid(. ~ sex)


nform <- ~ exp(-0.5*(log(input)-mu)^2/sd^2)
#nform <- ~ input*mu + sd

nfun <- deriv(nform,
              namevec=c("mu","sd"),
              function.arg=c("input","mu","sd"))

startvec <- c(mu = log(50), sd=log(3))

m0 <- nls(y ~ nfun(time,mu,sd),# + sd, #nfun(time, mu, sd),
          data,
          start=startvec)
startvec <- coef(m0)
startvec

nm1 <- nlmer(y ~ nfun(time, mu, sd) ~ (mu|sex:dose) + (sd|sex:dose) ,#(scale|dose) + (mu|dose),
             data,
             start = startvec)
summary(nm1)
coef(nm1)$dose

coef(nm1)$sex

fixef(nm1)

data.frame(estimate=fixef(nm1)) %>% cbind(
      confint(nm1,parm="beta_",method="Wald")) %>%
      mutate_all(exp)

exp(coef(nm1)$sex)

nm2 <- nlmer(y ~ nfun(time, mu, sd) ~ (mu|sex) + (mu|dose) + (sd|dose)  ,#(scale|dose) + (mu|dose),
             data,
             start = startvec)


tidy(nm2,conf.int=TRUE,exponentiate=F,effects="fixed")

summary(nm2)
fixef(nm2)
confint(nm2,parm="beta_",method="Wald")
coef(nm2)$dose


confint(nm2)

coef(nm2)$dose
coef(nm2)$sex

anova(nm2,nm1)

library(sjPlot)
library(sjmisc)
library(sjlabelled)





