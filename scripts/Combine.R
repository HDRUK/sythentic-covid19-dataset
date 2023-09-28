library(ggplot2)
library(dplyr)
df1 <- readRDS('50k_10k_all_effects.rds')

df2 <- readRDS('50k_10k_dose_age.rds')

df3 <- readRDS('50k_10k_dose_only.rds')

df4 <- readRDS('10k_1k_all_effects.rds')

get_data <- function(df){
  df_ana <- df %>% mutate()  %>%
    mutate(
      outcome=as.integer(ifelse(igg<300,1,0)),
      sex=as.factor(sex),
      ageG=relevel(as.factor(cut(age,breaks=seq(0,100,20))),ref='(40,60]'),
      days_sinceG=relevel(as.factor(cut(days_since,breaks=c(0,25,50,75,100,150,Inf))),ref='(0,25]'),
      nrisks=as.factor(nrisks),
      dose=relevel(as.factor(dose),ref=2)) %>%
    filter(!is.na(outcome) & !is.na(ageG))

  cnames <- df_ana %>% select(contains('c_')) %>% colnames
  f <- paste0('outcome ~ ageG + dose + days_sinceG + ',paste(cnames,collapse=' + '))

  fit <- glm(f, family=binomial,data=df_ana,na.action=NULL)
  summary(fit)

  ORs <- data.frame(OR=exp(coef(fit))) %>%
    cbind(exp(confint(fit)))
  ORs$var <- row.names(ORs)
  ORs

  ORs %>% tail(-1) %>% filter(!is.na(`2.5 %`)) %>%
    filter(!grepl('days_since',var)) %>%
    ggplot(aes(x=OR,y=var)) +
    geom_pointrange(aes(xmin=`2.5 %`,xmax=`97.5 %`)) +
    geom_vline(xintercept = 1, linetype='dashed') +
    scale_x_log10(limits=c(0.01,1000))

  return (ORs);
}


d1 <- get_data(df1)
d2 <- get_data(df2)
d3 <- get_data(df3)
d4 <- get_data(df4)

d <- d1 %>% mutate(dataset='Dose + Age + Risks',size='(50k)') %>%
     rbind(
       d2 %>% mutate(dataset='Dose + Age',size='(50k)')
     ) %>%
     rbind(
       d3 %>% mutate(dataset='Dose',size='(50k)')
     ) %>%
    rbind(
      d4 %>% mutate(dataset='Dose + Age + Risks',size='(10k)')
    )

nmap <- c(
  "c_asthma"="Asthma",
  "c_blood_cancer"="Blood Cancer",
  "c_chd"="CHD",
  "c_copd"="COPD",
  "c_diabetes"="Diabetes",
  "c_fracture"="Fracture",
  "c_parkinsons"="Parkinsons",
  "c_respiratory_cancer"="Respiratory Cancer",
  "dose1"="Dose 1",
  "dose3"="Dose 3",
  "ageG(0,20]"="0-20",
  "ageG(20,40]"="20-40",
  "ageG(40,60]"="40-60",
  "ageG(60,80]"="60-80",
  "ageG(80,100]"="80-100"
  )

get_var <- function(x){
  case_when(
    grepl('age',x) ~ 'Age',
    grepl('dose',x) ~ 'Dose',
    grepl('c_',x) ~ 'Risk',
    TRUE ~ 'Unknown'
  )
}

d %>% filter(!is.na(`2.5 %`)) %>%
  filter(!grepl('Intercept',var)) %>%
  filter(!grepl('days_since',var)) %>%
  mutate(level=nmap[var],
         var=get_var(var)) %>%
  ggplot(aes(x=OR,y=level,color=as.factor(dataset),shape=factor(size,levels=c('(50k)','(10k)')))) +
  geom_pointrange(aes(xmin=`2.5 %`,xmax=`97.5 %`),position=position_dodge(width=1.0)) +
  geom_vline(xintercept = 1, linetype='dashed') +
  geom_hline(yintercept=c(2:100)-0.5,linetype='dotted',size=0.4,alpha=0.6) +
  scale_x_log10(limits=c(0.001,1000),labels = scales::comma) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(0.3, "lines"),
    panel.border = element_rect(fill = NA, color = "black", linetype = "solid")
  ) +
  labs(shape='Dataset Size',color='Effects',y='',x='Odds Ratios (95% CI)') +
  scale_color_brewer(palette = 'Set1') +
  facet_grid(var ~ .,space='free',scale='free',switch='both')

