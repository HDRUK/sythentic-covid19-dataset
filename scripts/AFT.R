library(dplyr)
library(ggplot2)
library(knitr)
library(ciTools)
library(here)
set.seed(20180925)

library(eavehelpers)
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
  mutate(insufficient=ifelse(igg<20,1,0)) %>%
  mutate(sufficient=ifelse(igg>20,1,0)) %>%
  mutate(dose=as.factor(dose),
         sex=as.factor(sex)) %>%
  filter(!is.na(igg) & !is.na(time) & !is.na(dose) & !is.na(insufficient))


ggplot(df_ana, aes(x = igg, y = time)) +
  geom_point(aes(color = factor(sufficient)))+
  ggtitle("Censored obs. in red") +
  theme_bw()

fit <- survreg(Surv(time, sufficient) ~ igg + dose, data = df_ana,dist = "lognormal")

summary(fit)
coef(fit)
exp(coef(fit))

with_ints <- ciTools::add_ci(df_ana %>% select(igg,dose,time,sufficient),fit, names = c("lcb", "ucb")) %>%
  ciTools::add_pi(fit, names = c("lpb", "upb")) %>% as_tibble

ggplot(with_ints, aes(x = igg, y = time)) +
  geom_point(aes(color = dose)) +
  facet_wrap(~dose)+
  theme_bw() +
  ggtitle("Model fit with 95% CIs and PIs",
          "solid line = mean, dotted line = median") +
  geom_line(aes(y = mean_pred), linetype = 1) +
  geom_line(aes(y = median_pred), linetype = 2) +
  geom_ribbon(aes(ymin = lcb, ymax = ucb), alpha = 0.5) +
  geom_ribbon(aes(ymin = lpb, ymax = upb), alpha = 0.1) +
  scale_y_continuous(lim=c(0,1000))


probs <- ciTools::add_probs(df_ana %>% select(igg,dose,time,sufficient), fit, q = 500,
                            name = c("prob", "lcb", "ucb"),
                            comparison = ">")
ggplot(probs, aes(x = igg, y = prob)) +
  ggtitle("Estimated prob. of avg. spring lasting longer than 500 hrs.") +
  ylim(c(0,1)) +
  facet_wrap(~dose)+
  theme_bw() +
  geom_line(aes(y = prob)) +
  geom_ribbon(aes(ymin = lcb, ymax = ucb), alpha = 0.5)

