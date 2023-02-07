devtools::document()
library(seave)
library(eavehelpers)

# Find MLE estimates using optim
library(optimx)
df <- generate_ace(n=20000)


df_ana <- df %>% filter(!is.na(vaccine_date1) & positivity_date > vaccine_date1) %>%
  mutate(time=case_when(
    !is.na(vaccine_date3) & positivity_date > vaccine_date3 ~ positivity_date - vaccine_date3,
    !is.na(vaccine_date2) & positivity_date > vaccine_date2 ~ positivity_date - vaccine_date2,
    !is.na(vaccine_date1) & positivity_date > vaccine_date1 ~ positivity_date - vaccine_date1
  )) %>%
  mutate(dose=case_when(
    !is.na(vaccine_date3) & positivity_date > vaccine_date3 ~ 3,
    !is.na(vaccine_date2) & positivity_date > vaccine_date2 ~ 2,
    !is.na(vaccine_date1) & positivity_date > vaccine_date1 ~ 1
  )) %>%
  mutate(timeG = cut(time,breaks=seq(0,200,20),labels=seq(20,200,20))) %>%
  mutate(timeGG = as.integer(as.character(timeG))) %>%
  mutate(igg=IgG_BAU_wuhan_average) %>%
  mutate(insufficient=ifelse(igg<200,1,0)) %>%
  mutate(dose=as.factor(dose),
         sex=as.factor(sex))




#my_fun <- function(x,mean,sd) { 100*exp(-0.5*((log(x)-mean)^2/sd^2)) }
#,by=dose
#fit <- gam(igg ~ s(my_fun(time)) ,data=df_ana,family=gaussian(link="log"))
#fit <- gam(igg ~ my_fun(time,mean=1.7,sd=0.9) + dose ,data=df_ana,family=gaussian())

#fit <- gam(igg ~ s(time,by=dose) + dose ,data=df_ana,family=gaussian())

#fit <- gam(igg ~ s(time,by=dose) + dose + sex ,data=df_ana,family=gaussian())
fit <- gam(igg ~ s(time,by=dose) + dose + sex ,data=df_ana,family=gaussian(link = 'identity'))
summary(fit)

time = seq(0,200,0.1)
dose = levels(df_ana[['dose']])
sex = levels(df_ana[['sex']])

newdata <- expand.grid(list(time=time,dose=dose,sex=sex)) %>% as_tibble %>%
           mutate(dose=as.factor(dose))

spline <- newdata %>% cbind(predict.gam(fit,newdata,se.fit = TRUE) %>% as.data.frame) %>%
          as_tibble()

spline %>% ggplot(aes(x=time,y=fit,color=dose,fill=dose)) +
           geom_line() +
           geom_ribbon(aes(ymin=fit-1.96*se.fit,ymax=fit+1.96*se.fit),alpha=0.5) +
           geom_pointrange(aes(
              x=timeGG,
              y=igg,
              color=dose,
              ymax=igg+er,
              ymin=igg-er),
              data = df_ana %>%
                group_by(timeGG,dose) %>%
                summarise(er=sd(igg),igg=mean(igg))
              ) +
          labs(y='IgG',color="Dose",fill="Dose",x="Days Since Vaccine") +
          theme_classic()

eavehelpers::get_pterm_or_from_gam(fit,model=df_ana) %>%
  ggplot(aes(x=OR,y=names)) +
  geom_pointrange(aes(xmin=LCL,xmax=UCL))

library(ggeffects)
p <- ggpredict(fit, c("time","dose"))
p
plot(p)+labs(title="", x="Age",y="IgG Level")+
  scale_x_continuous(limits = c(0,500))+
  scale_y_continuous(limits=c(0,500))+
  theme(legend.position = "bottom",legend.title = element_blank()) #+
  #scale_fill_npg(labels=c("One dose Pfizer","One dose AZ","Two doses Pfizer"))+
  #scale_color_npg(labels=c("One dose Pfizer","One dose AZ","Two doses Pfizer"))





df <- generate_ace(n=50000)
df_ana <- df %>% filter(!is.na(vaccine_date1) & positivity_date > vaccine_date1) %>%
  mutate(time=case_when(
    !is.na(vaccine_date3) & positivity_date > vaccine_date3 ~ positivity_date - vaccine_date3,
    !is.na(vaccine_date2) & positivity_date > vaccine_date2 ~ positivity_date - vaccine_date2,
    !is.na(vaccine_date1) & positivity_date > vaccine_date1 ~ positivity_date - vaccine_date1
  )) %>%
  mutate(dose=case_when(
    !is.na(vaccine_date3) & positivity_date > vaccine_date3 ~ 3,
    !is.na(vaccine_date2) & positivity_date > vaccine_date2 ~ 2,
    !is.na(vaccine_date1) & positivity_date > vaccine_date1 ~ 1
  )) %>%
  mutate(timeG = cut(time,breaks=seq(0,200,20),labels=seq(20,200,20))) %>%
  mutate(timeGG = as.integer(as.character(timeG))) %>%
  mutate(igg=IgG_BAU_wuhan_average) %>%
  mutate(insufficient=ifelse(igg<20,1,0)) %>%
  mutate(sufficient=ifelse(igg>20,1,0)) %>%
  mutate(dose=as.factor(dose),
         sex=as.factor(sex)) %>%
  filter(!is.na(igg) & !is.na(time) & !is.na(dose) & !is.na(insufficient))


df2 <- df_ana %>% mutate(x=cut(time,breaks=seq(0,200,4),labels=seq(2,200,4))) %>%
  mutate(vaccine_dose=as.factor(vaccine_dose),
         x=as.numeric(as.character(x))) %>%
  filter(!is.na(x)) %>%
  group_by(x,vaccine_dose) %>%
  summarise(y=mean(IgG_BAU_wuhan_average),
            yerr=sd(IgG_BAU_wuhan_average)/sqrt(n()))


ggplot() +
  geom_pointrange(aes(x=x,y=y,ymin=y-yerr,ymax=y+yerr,
                      color=as.factor(vaccine_dose)), data=df2) +
  xlim(0,200) +
  labs(x='Days since vaccination',y='IgG',
       color='Vaccine Dose',
       fill='Vaccine Dose')



df_ana %>% select(igg,time,dose) %>% filter(time<200) %>%
  ggplot(aes(x=igg,fill=factor(dose))) + geom_histogram() +
  scale_y_log10()

lognormal <- function (x,mean,sd){
  exp(-0.5*((log(x) - mean)^2/(sd^2)))
}

normal <- function (x,mean,sd){
  exp(-0.5*( x - mean)^2/(sd^2))
}



likelihood <- function(params, data) {
  mean <- params[1]
  sigma <- params[2]
  alpha <- params[3]

  mu_t <- lognormal(data$time,mean,alpha)

  -sum(dnorm(data$igg, mean = mu_t, sd = sigma,log=TRUE))
}

mle_estimates <- optim(c(1,1,1), likelihood,
                       data = df_ana %>% filter(dose==1) %>% select(igg,time),
                       method = "L-BFGS-B",
                       lower = c(0.01,0.01,0.0), upper = c(Inf, Inf, Inf))

par <- mle_estimates$par
par


data.frame(x=rnorm(100,mean=par[1],sd=par[2])) %>%
  ggplot(aes(x=x)) + geom_histogram()






df_ana %>% select(igg,time,dose) %>% filter(dose==1 & time<100 & time>14) %>%
  ggplot(aes(x=igg)) + geom_histogram()


rnorm(10000,mean=mle_estimates[1],mle_estimates[3]) %>% as_tibble %>%
  filter(value<1000) %>%
  ggplot(aes(x=value)) +
  geom_histogram()


temp <- data.frame(x=seq(0,50,0.1)) %>%
        mutate(y = dlnorm(x,mean=mle_estimates[1],sd=mle_estimates[2])) %>%
temp %>% ggplot(aes(x=x,y=y)) + geom_line()

mean <- exp(-0.5*((log(300) - mle_estimates[1])^2/(mle_estimates[2]^2)))
mean
data.frame(igg=rnorm(1000,mean=mean,sd=mle_estimates[3])) %>%
        ggplot(aes(x=igg)) + geom_histogram()




d1 <- df_ana %>% filter(dose==1) %>% select(igg,time,dose) %>%
  mutate(x=cut(time,breaks=seq(0,100,4),labels=seq(2,100,4))) %>%
  mutate(dose=as.factor(dose),
         x=as.numeric(as.character(x))) %>%
  filter(!is.na(x)) %>%
  group_by(x) %>%#,dose
  summarise(y=mean(igg),
            yerr=sd(igg)/sqrt(n()))
d1

d1 %>% ggplot(aes(x=x,y=y)) + geom_pointrange(aes(ymin=y+yerr,ymax=y-yerr))




# Generate sample data
set.seed(123)
n <- 1000
a_true <- 1
mu_true <- 5
sigma_true <- 2
x1 <- rnorm(n, mean = mu_true, sd = sigma_true)

x2 <- rnorm(n, mean = mu_true*1.5, sd = sigma_true)

x <- data.frame(x=5*x1,sex='F') %>% rbind(data.frame(x=x2,sex='M'))
x %>% as_tibble %>% ggplot(aes(x=x,fill=factor(sex))) +
      geom_histogram()

# Define the likelihood function
gaussian_likelihood <- function(params, data) {
  #a <- params[1]
  mu <- params[1]
  sigma <- params[2]
  mu_sex <- params[3]

  dF <- data %>% filter(sex=='F')
  dM <- data %>% filter(sex=='M')

  -sum(dnorm(dF$x, mean = mu, sd = sigma, log = TRUE) +
         dnorm(dM$x, mean = mu*mu_sex, sd = sigma, log = TRUE) )
}

mle_estimates <- optim(c(0, 1,1), gaussian_likelihood, data = x, method = "L-BFGS-B",
                       lower = c(-Inf, 0, 0), upper = c(Inf, Inf, Inf))$par

mle_estimates

mu_hat <- mle_estimates[1]
sigma_hat <- mle_estimates[2]

# Print MLE estimates
cat("MLE estimate for mu:", mu_hat, "\n")
cat("MLE estimate for sigma:", sigma_hat, "\n")








