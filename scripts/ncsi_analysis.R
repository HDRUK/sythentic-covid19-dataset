library(mgcv)
library(phsstyles)


df_ace <- generate_ace(10000) %>% as_tibble

#df_ace %>% ggplot(aes(x=IgG_BAU_wuhan_average,fill=as.factor(age))) + geom_histogram()


df <- df_ace %>% filter(vaccine_dose>0) %>% mutate(
      time = case_when(
        positivity_date > vaccine_date3 ~ (positivity_date - vaccine_date3),
        positivity_date > vaccine_date2 ~ (positivity_date - vaccine_date2),
        positivity_date > vaccine_date1 ~ (positivity_date - vaccine_date1)
      ))


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
p <- ggplot(pred, aes(x=time, y=func(fit,ref_fit), color=vaccine_dose)) +
  geom_line(size = 0.5, linetype='dashed') +
  geom_ribbon(aes(x = time, ymin = func(fit-se.fit,ref_fit), ymax = func(fit+se.fit,ref_fit), color=vaccine_dose, fill=vaccine_dose), alpha = 0.3) +
  labs(y='Test',x='time',color='',fill='') +
  #scale_colour_discrete_phs('all') +
  #geom_vline(xintercept=var_ref,linetype='dotted') +
  geom_hline(yintercept=1,linetype='dotted') +
  #scale_fill_discrete_phs(palette='all') +
  #scale_y_log10() +
  theme_classic()

p

std.error <- function(x) sd(x)/sqrt(length(x))
p2 <- df %>% mutate(time=cut(time,breaks=seq(0,300,10))) %>%
       group_by(vaccine_dose,time) %>% summarise(y=mean(IgG_BAU_wuhan_average),
                                    yerr=std.error(IgG_BAU_wuhan_average)) %>%
       mutate(vaccine_dose=as.factor(vaccine_dose)) %>%
       ggplot(aes(x=time,y=y,color=vaccine_dose,group=vaccine_dose)) +
       geom_pointrange(aes(ymin=y-yerr,ymax=y+yerr)) +
       geom_smooth(method = "nls", formula = y ~ a*exp(-1*(log(x) - b)^2 / c) + d*(x/(x-e)), se = F,
                   method.args = list(start = list(a = 300, b = 0,c=0.3,d=300,e=0.1)))
       #geom_smooth(method = "nls", formula = y ~ a*exp(-(x - b) /c), se = F,
       #geom_smooth(method = "nls", formula = y ~ a*(x/(x-b)), se = F,
      #             method.args = list(start = list(a = 100, b = 0.1)))
p2


