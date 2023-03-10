library(mgcv)
library(phsstyles)
#library(nlstools)
#library(propagate)
library(investr)
library(seave)


df_truth <- truth_ace(3)
df_truth %>% mutate(id=as.factor(id+1)) %>%
             ggplot(aes(x=day,y=immune,group=id,color=id)) +
             geom_line() +
             labs(x='Date [days]',y='Immune Response [arb]',color='Person') +
             theme_classic()


df_ace <- generate_ace(5000) %>% as_tibble


df_ace %>% ggplot(aes(x=IgG_BAU_wuhan_average,
                      fill=as.factor(vaccine_dose))) +
           geom_histogram() +
           labs(fill='vaccine_dose')


#df_ace %>% ggplot(aes(x=IgG_BAU_wuhan_average,fill=as.factor(age))) + geom_histogram()


df <- df_ace %>% filter(vaccine_dose>0) %>% mutate(
      time = case_when(
        positivity_date > vaccine_date3 ~ (positivity_date - vaccine_date3),
        positivity_date > vaccine_date2 ~ (positivity_date - vaccine_date2),
        positivity_date > vaccine_date1 ~ (positivity_date - vaccine_date1)
      ))


temp <- df %>% select(IgG_BAU_wuhan_average,time,vaccine_dose,age,sex) %>%
       mutate(timeG=cut(time,breaks=seq(0,200,10),labels=seq(10,200,10))) %>%
       mutate(ageG=cut(age,breaks=seq(18,26,2))) %>%
       mutate(x=as.numeric(as.character(timeG))) %>%
       group_by(x,sex,vaccine_dose) %>%
       summarise(sd=sd(IgG_BAU_wuhan_average)/sqrt(n()),
                 y=mean(IgG_BAU_wuhan_average))

temp %>%
       ggplot(aes(x=x,y=y,color=as.factor(sex))) +
       geom_pointrange((aes(ymin=y-sd,ymax=y+sd))) +
       scale_color_discrete_phs(palette='all') +
       facet_grid(. ~ vaccine_dose,scale='free',space='free') +
       labs(x='Days since Vaccination',y='IgG [BAU/ml]',color='Sex')


model <- gam(IgG_BAU_wuhan_average ~ s(time,by=vaccine_dose) + age + sex , data=df)
summary(model)

term_list <- list()
for (term in labels(model$terms)){
  new_term <- model[["var.summary"]][[term]][[1]]
  term_list <- append(term_list, list(new_term))
}
names(term_list) <-  labels(model$terms)
term_list[['time']] <- seq(0,500,1)


term_list[['vaccine_dose']] <- c(1,2,3)


new_data <- expand.grid(term_list)
pred <- predict.gam(model,new_data,se.fit = TRUE)
pred <- cbind(new_data, pred) %>% as_tibble %>% mutate(vaccine_dose=as.factor(vaccine_dose))

func <- function(a,b){
  return (a);
  return (a/b);
  return (exp(a - b));
}

ref_fit <- 100
p <- ggplot(pred %>% group_by(vaccine_dose) %>%
            mutate(y=fit/max(fit),
                   ymin=(fit-se.fit)/max(fit),
                   ymax=(fit+se.fit)/max(fit)),
  aes(x=time, y=y, color=vaccine_dose)) +
  geom_line(size = 0.5, linetype='dashed') +
  geom_ribbon(aes(x = time, ymin = ymin,
                            ymax = ymax,#(fit+se.fit)/max(fit),
                            color=vaccine_dose,
                            fill=vaccine_dose)
              , alpha = 0.3) +
  labs(y='Test',x='time',color='',fill='') +
  #scale_colour_discrete_phs('all') +
  #geom_vline(xintercept=var_ref,linetype='dotted') +
  #geom_hline(yintercept=1,linetype='dotted') +
  #scale_fill_discrete_phs(palette='all') +
  #scale_y_log10() +
  theme_classic()

p

std.error <- function(x) sd(x)/sqrt(length(x))

fit_lognormal <- function (df) {
  fit_params <- df %>% group_map(~fitdistr(.x$y,"lognormal"))

  fit_params <- fit_params[[1]]
  mean <- fit_params$estimate[['meanlog']]
  sd <- fit_params$estimate[['sdlog']]

  x <- seq(0,length(unique(df$time)))
  fit <- dlnorm(x, mean,sd)

  return (fit);

}

df2 <- df %>% mutate(time=cut(time,
                              breaks=seq(0,300,10),
                              labels=seq(10,300,10))
                     ) %>%
              mutate(time=as.numeric(as.character(time))) %>%
              group_by(vaccine_dose,time) %>%
              summarise(
                        y=mean(IgG_BAU_wuhan_average),
                        yerr=std.error(IgG_BAU_wuhan_average)
              ) %>% ungroup




d1 <- df2 %>% filter(vaccine_dose==1) %>% filter(!is.na(time))
#fit <- nls(y ~ a*dlnorm(time, meanlog=5.0, sdlog=1),
#              data=d1, start=list(a=300))




data <- NULL
fits <- list()
new_data <- data.frame(time = seq(min(df$time),max(df$time),0.1)) %>% as_tibble

for(dose in c(1,2,3)){

  #fit <- nls(IgG_BAU_wuhan_average ~  a*dlnorm(time,a2,a3) + b*(time/(time+b2)),
  #           data=df %>% filter(vaccine_dose==dose),
  #           control=list(warnOnly = TRUE),
  #           start=list(a=300.,a2=5.0,a3=1.,b=0.1,b2=0.1),
  #           lower=list(a=100.,a2=2.0,a3=0,b=0,b2=0.0),
  #           algorithm = "port")

  fit <- nls(IgG_BAU_wuhan_average ~  a*dlnorm(time,1.1,0.1),
             data=df %>% filter(vaccine_dose==dose),
             start=list(a=300.),
             control=list(warnOnly = TRUE),
             algorithm = "port")

  #fit <- nls(IgG_BAU_wuhan_average ~  a*dlnorm(time,a2,a3) ,
  #          data=df %>% filter(vaccine_dose==dose),
  #           control=list(warnOnly = TRUE),
  #           start=list(a=300.,a2=5.0,a3=1.),
  #           lower=list(a=100.,a2=2.0,a3=0),
  #           algorithm = "port")


  est <- investr::predFit(fit,
                 newdata = new_data,
                 interval = "confidence",
                 level= 0.9) %>% as_tibble

  est$x = new_data$time
  est$vaccine_dose = dose
  data <- rbind(data,est)

  fits[[dose]] <- fit
}

p <- NULL
i <- 0
for (f in fits){
  i <- i+1
  p <- rbind(p,tibble(id=i,name=labels(coef(f)),param=coef(f)) %>% cbind(confint(f) %>% as_tibble))
}

p %>% mutate_at(vars('param','2.5%','97.5%'),exp) %>% filter(name=='a2') %>% ggplot() +
      geom_pointrange(
                   aes(x=1,y=param,ymin=`2.5%`,ymax=`97.5%`,color=as.factor(id)),
                   position=position_dodge(width = 1)) +
      #coord_flip() +
      labs(x='Dose',y='Days to Peak Response') +
      ylim(0,200) +
      theme_classic() +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())

df2 <- df %>% mutate(x=cut(time,breaks=seq(0,400,50),labels=seq(25,400,50))) %>%
              mutate(vaccine_dose=as.factor(vaccine_dose),
                     x=as.numeric(as.character(x))) %>%
              filter(!is.na(x)) %>%
              group_by(x,vaccine_dose) %>%
              summarise(y=mean(IgG_BAU_wuhan_average),
                        yerr=sd(IgG_BAU_wuhan_average)/sqrt(n()))

ggplot() +
        geom_pointrange(aes(x=x,y=y,ymin=y-yerr,ymax=y+yerr,
                            color=as.factor(vaccine_dose)), data=df2) +
        #geom_ribbon(aes(x=x,y=fit,
        #                ymin=lwr,ymax=upr,
        #                color=as.factor(vaccine_dose),
        #                fill=as.factor(vaccine_dose)),
        #            alpha=0.4,
        #            data=data) +
        xlim(0,400) +
        labs(x='Days since vaccination',y='y',
             color='Vaccine Dose',
             fill='Vaccine Dose')


#est$y <- predict(fit, est, se.fit = TRUE)

temp <- predict(fit,newdata=est, se.fit = TRUE)
temp %>% as.data.frame


est %>% ggplot(aes(x=time,y=y)) + geom_line()


p2 <- df %>%
       #mutate(vaccine_dose=as.factor(vaccine_dose)) %>%
       ggplot(aes(x=time,y=IgG_BAU_wuhan_average)) +
       geom_point() +
       #geom_pointrange(aes(color=vaccine_dose,group=vaccine_dose,ymin=y-yerr,ymax=y+yerr)) +
       geom_line(aes(x=time,y=y),data=est)
p2
#+
       #geom_smooth(method = "nls", formula = y ~ a*exp(-1*(log(x) - b)^2 / c) + d*(x/(x-e)), se = F,
      #             method.args = list(start = list(a = 300, b = 0,c=0.3,d=300,e=0.1)))
       #geom_smooth(method = "nls", formula = y ~ a*exp(-(x - b) /c), se = F,
       #geom_smooth(method = "nls", formula = y ~ a*(x/(x-b)), se = F,
      #             method.args = list(start = list(a = 100, b = 0.1)))
p2


