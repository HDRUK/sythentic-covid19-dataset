library(survival)


demo <- read.csv('eave/demo.csv')
demo

pcr <- read.csv('eave/pcr.csv')
pcr

gp <- read.csv('eave/gp.csv')
gp

vaccines <- read.csv('eave/vaccines.csv')
vaccines

hosp <- read.csv('eave/hospitalisations.csv')
hosp


df <- demo %>% right_join(pcr %>% filter(result==1) %>% rename(test_date=date)) %>% as_tibble
df

df <- df %>%
      left_join(vaccines %>% rename(vaccine_date=date) %>%
        group_by(LINKNO) %>% mutate(dose=row_number()) %>% ungroup) %>%
      left_join(hosp%>% rename(outcome=date)) %>% as_tibble
df

df_gp <- gp %>% pivot_wider(id_cols='LINKNO',names_from='condition',
                   values_from='condition') %>%
                mutate_at(vars(-("LINKNO")),~ ifelse(is.na(.),0,1)) %>%
                mutate(nrisks = rowSums(across(!contains('LINKNO'))))

df <- df %>% left_join(df_gp) %>%
  mutate(nrisks = ifelse(is.na(nrisks),0,nrisks)) %>%
  mutate(nrisks = factor(case_when(
    nrisks < 3 ~ as.character(nrisks),
    TRUE ~ '3+'
  )))

df %>% group_by(nrisks) %>% summarise(n=n())

df_ana <- df %>% #filter(!is.na(outcome)) %>%
   mutate(days_since_vaccine = outcome-vaccine_date) %>%
   mutate(days_since_vaccine = ifelse(days_since_vaccine<0,NA,days_since_vaccine)) %>%
   group_by(LINKNO,outcome,days_since_vaccine) %>% filter(row_number()==n()) %>% ungroup %>%
   mutate(start=test_date,end=ifelse(is.na(outcome),max(df$outcome,na.rm=T),outcome),
          outcome=ifelse(is.na(outcome),0,1),
          dose=as.factor(ifelse(is.na(dose),0,dose)),
          ageG=cut(age,breaks=seq(0,100,10))) %>%
   mutate(dose=factor(dose,levels=c(1,0,2,3))) %>%
   filter(end>start)

df_ana %>% group_by(dose) %>% summarise(n=n())


model <- coxph(Surv(start,end, outcome) ~ dose + sex + ageG,
               data = df_ana)

summary(model)
est <- exp(coef(model))
err <- exp(confint(model))
err <- err %>% as_tibble %>% mutate(name=labels(err)[[1]])
res <- tibble(name=labels(est),estimate=est) %>% left_join(err)

res <- res %>% mutate(
  grp = case_when(
    grepl('sex',name) ~ 'Sex',
    grepl('ageG',name) ~ 'Age',
    grepl('dose',name) ~ 'Dose',
  ),
  name = case_when(
    name == 'sexMale' ~ 'Male',
    name == 'nrisks3+' ~ '3+',
    name == 'nrisks2+' ~ '2',
    name == 'nrisks1+' ~ '1',
    name == 'dose3' ~ '3',
    name == 'dose2' ~ '2',
    name == 'dose0' ~ '0',
    name == 'ageG(90,100]' ~ '90-100',
    name == 'ageG(80,90]' ~ '80-90',
    name == 'ageG(70,80]' ~ '70-80',
    name == 'ageG(60,70]' ~ '60-70',
    name == 'ageG(50,60]' ~ '50-60',
    name == 'ageG(40,50]' ~ '40-50',
    name == 'ageG(30,40]' ~ '30-40',
    name == 'ageG(20,30]' ~ '20-30',
    name == 'ageG(10,20]' ~ '10-20'
  )
)

res %>% ggplot(aes(x=estimate,xmin=`2.5 %`,xmax=`97.5 %`,y=name)) +
        geom_pointrange(color='darkgreen') +
        geom_vline(xintercept=1,linetype='dashed') +
        labs(x='Hazard Ratio [HR 95% CI]',y='') +
        facet_grid(grp ~ ., switch='y',space='free',scale='free') +
        theme_classic() +
        theme(
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.spacing = unit(0.5, "lines"),
          panel.border = element_rect(fill = NA, color = "black", linetype = "dashed")) +
        scale_x_log10(limits=c(0.5,3))

