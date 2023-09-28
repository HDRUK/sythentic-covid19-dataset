library(seave)
library(phsstyles)

get_igg <- function(person){
  do.call(rbind,lapply(
    seq(1,400),
    function(i) data.frame(date=i,igg=person$get_immune_response(i))
  )) %>% as_tibble
}


pandemic <- new(seave::Pandemic)
pandemic$create_default_variants()

population <- new(seave::Population)
population$set_pandemic(pandemic)

n <- 10000
cohort <- lapply(rep(1, n), function(i){population$generate()})
cohort

person <- sample(cohort,1)[[1]]
person

infection_dates <- do.call(rbind, lapply(cohort, function(p) {data.frame(d=p$infection_dates)})) %>%
                  as_tibble


outcome_dates <- do.call(rbind, lapply(cohort, function(p) {data.frame(d=p$outcome_dates)})) %>%
                 as_tibble


vaccine_dates <- do.call(rbind, lapply(cohort, function(p) {data.frame(d=p$vaccine_dates)})) %>%
  as_tibble



temp <- infection_dates %>% mutate(d = 10*cut(d, breaks=seq(0,350,10),labels=F)) %>%
        group_by(d) %>% summarise(ni=n()) %>%
        left_join(
          outcome_dates %>% mutate(d = 10*cut(d, breaks=seq(0,350,10),labels=F)) %>%
            group_by(d) %>% summarise(no=n())
        )

temp

temp %>% ggplot(aes(x=d,y=100*no/ni)) + geom_line()



x <- seq(0,350,0.1)

data.frame(x=x) %>% as_tibble %>% rowwise %>%
  mutate(y=pandemic$get_p_infection(x)) %>%
  ggplot() +
  geom_histogram(aes(x=d,y = ..count../50000),bins=50,
                 fill='red',alpha=0.4,
                 data=infection_dates) +
  geom_line(aes(x=x,y=y),color='purple',linewidth=1.) +
  labs(x='Dates [days]',y='Simulated Infection Probability') +
  scale_y_continuous(
    "Simulated Infection Probability",
    sec.axis = sec_axis(~ . * 50000, name = "Number of Infections")
  ) +
  theme_classic() +
  theme( axis.line.y.right = element_line(color = "red"),
         axis.title.y.right = element_text(color = "red"),
         axis.text.y.right = element_text(color = "red"),
         axis.ticks.y.right = element_line(color = "red"),
         axis.line.y.left = element_line(color = "purple"),
         axis.title.y.left = element_text(color = "purple"),
         axis.text.y.left = element_text(color = "purple"),
         axis.ticks.y.left = element_line(color = "purple")
         )


df_igg <- get_igg(person) %>% mutate(person='Person #1') %>%
          rbind(get_igg(sample(cohort,1)[[1]]) %>% mutate(person='Person #2')) %>%
          rbind(get_igg(sample(cohort,1)[[1]]) %>% mutate(person='Person #3'))


df_igg %>%
  ggplot(aes(x=date,y=igg,color=as.factor(person))) +
  geom_line(size=1.5) +
  scale_color_brewer(palette = "Set2") +
  labs(color='',x='Date [days]',y='Immune Response [arb]',color='Person') +
  scale_y_log10() +
  theme_classic()


labels(person)

person$vaccine_dates

person$age

person$sex

person$bmi

person$ethnicity

person$get_nrisks()

person$get_comorbidities()


extract_person <- function(p){
  df <- data.frame(age=p$age,sex=p$sex,bmi=p$bmi,
                   nrisks=p$get_nrisks())
  comorbidities <- p$get_comorbidities()
  for (c in comorbidities){
    df[paste0('c_',c)] <- 1
  }
  return (df)
}

df_people <- do.call(dplyr::bind_rows,lapply(cohort,extract_person))  %>%
             as_tibble %>%
             mutate_at(vars(contains('c_')), ~replace(., is.na(.), 0))
df_people

df_people %>% ggplot(aes(x=age,fill=as.factor(sex))) +
  geom_bar() +
  scale_x_binned(n.breaks = 20) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette = "Set2") +
  labs(color='',x='Age',y='Number of Simulated People',fill='') +
  theme_classic()


df_people %>% ggplot(aes(x=age,fill=as.factor(nrisks))) +
  geom_bar(position='dodge') +
  scale_x_binned(n.breaks = 10) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette = "Set2") +
  labs(color='',x='Age',y='Number of Simulated People',fill='Number of Clinical Risks') +
  theme_classic()

nmap <- c('c_asthma'='Asthma',
          'c_diabetes'='Diabetes',
          'c_fracture'='Hip Fracture',
          'c_blood_cancer'='Blood Cancer',
          'c_respiratory_cancer'='Respiratory Cancer',
          'c_copd'='COPD',
          'c_chd'='CHD',
          'c_parkinsons'='Parkinsons'
          )

temp <- df_people %>% summarise_at(vars(contains('c_')), sum) %>% t() %>% as.data.frame
temp$x <- rownames(temp)
temp <- temp %>% mutate(x = nmap[as.character(x)])
temp

temp %>% ggplot(aes(x=x,y=V1,fill=as.factor(x))) +
  geom_col() +
  scale_y_log10(expand=c(0,0)) +
  scale_fill_brewer(palette = "Set2") +
  labs(color='',x='',y='Number of Simulated People',fill='') +
  theme_classic() +
  guides(fill="none")


extract_serology <- function(p,dmin=0,dmax=500){
  #random day in the pandemic when the measurement was taken
  day <- sample(dmin:dmax,1)
  #simulate the resolution of the measurement
  #i.e. the measurement has some error
  resolution <- rnorm(1,1,0.2)
  igg <- resolution*p$get_immune_response(day)

  dose <- which(p$vaccine_dates < day)
  if (length(dose)>1){
    dose <- tail(dose,n=1)
  }
  if(identical(dose, integer(0))){
    dose <- 0
  }

  days_since <- NA
  dd <- NA


  if(dose>0 & !is.na(dose)){
    dd <- p$vaccine_dates[dose]
    days_since <- day - p$vaccine_dates[dose]
  }

  df <- data.frame(date=day,days_since=days_since,igg=igg,
             dose=dose,dose_date=dd,
             age=p$age,sex=p$sex,bmi=p$bmi,nrisks=p$get_nrisks())

  comorbidities <- p$get_comorbidities()
  for (c in comorbidities){
    df[paste0('c_',c)] <- 1
  }

  return (df)
}

df_serology <- do.call(dplyr::bind_rows,
                       lapply(sample(cohort,10000,replace=T),extract_serology))  %>%
  as_tibble %>% mutate_at(vars(contains('c_')), ~replace(., is.na(.), 0))
  #filter(dose>0)



df_serology %>% select(date,igg) %>%
    mutate(date=cut(date,breaks=seq(0,500,10),labels=seq(5,500,10))) %>%
    arrange(date) %>%
    group_by(date) %>%
    summarise(spos = sum(igg>10)/n(),n=n(),mean=mean(igg)) %>%
    mutate(date=as.integer(as.character(date))) %>%
    ggplot() +
    geom_histogram(aes(x=d,y=..count../3),data=vaccine_dates,fill='darkgreen',alpha=0.5) +
    scale_y_continuous(
      "Number of Vaccines",
      sec.axis = sec_axis(~ . *3, name = "Mean IgG")
    ) +
    geom_line(aes(x=date,y=mean),color='red',size=2) +
    labs(x='Date [days]',y='Number of Vaccines',color='') +
    theme_classic()

#saveRDS(df_serology,"50k_10k_dose_age.rds")


extract_outcomes<- function(p){
  dd <- p$outcome_dates
  df <- data.frame(hospitalisation_date=dd)
  return (df)
}

df_outcomes <- do.call(dplyr::bind_rows,
                       lapply(cohort,extract_outcomes))  %>%
  as_tibble
df_outcomes


nrow(df_serology)
nrow(df_serology %>% filter(c_blood_cancer==1))

df_ana <- df_serology %>% #filter(c_blood_cancer==1) %>%
          mutate(day=cut(days_since,breaks=seq(0,1000,10),labels=seq(10,1000,10))) %>%
          mutate(day=as.integer(as.character(day))) %>%
          group_by(day,dose,nrisks) %>%
          summarise(sd=ifelse(n()>1,sd(igg)/sqrt(n()),mean(igg)),
                    igg=mean(igg),
                    n=n())




df_ana %>% ggplot(aes(x=day,y=igg,ymin=igg-sd,ymax=igg+sd,color=as.factor(nrisks))) +
           geom_pointrange() +
           xlim(0,200) +
           facet_wrap(. ~ dose, scale='free',
                      labeller = labeller(dose =
                                            c("1" = "Dose 1",
                                            "2" = "Dose 2",
                                            "3" = "Dose 3")
                                          )
                      ) +
           labs(color='Number of Risk Groups',
                y='Simulated Immune Response',
                x='Days since vaccination') +
           #scale_y_log10() +
           scale_color_discrete_phs(palette='all') +
           theme_classic()


df_ana <- df_serology %>%
          mutate(day=cut(days_since,breaks=seq(0,1000,10),labels=seq(10,1000,10))) %>%
          mutate(day=as.integer(as.character(day))) %>%
          group_by(day,dose,c_blood_cancer) %>%
          filter(!is.na(igg)) %>%
          summarise(n=n(),sd=ifelse(n()>1,sd(igg)/sqrt(n()),mean(igg)),igg=mean(igg))

df_ana %>% filter(!is.na(igg) & !is.na(c_blood_cancer) & !is.na(dose)) %>%
           ggplot(aes(x=day,y=igg,ymin=igg-sd,ymax=igg+sd,
                      color=as.factor(c_blood_cancer))) +
           geom_pointrange() +
           xlim(0,200) +
           facet_wrap(. ~ dose, scale='free') +
           scale_color_discrete_phs(palette='all')


df_ana <- df_serology %>%
  mutate(day=cut(days_since,breaks=seq(0,1000,20),labels=seq(10,1000,20))) %>%
  mutate(day=as.integer(as.character(day))) %>%
  mutate(ageG=cut(age,breaks=seq(0,100,20))) %>%
  group_by(day,dose,ageG) %>%
  filter(!is.na(igg) & !is.na(ageG)) %>%
  summarise(n=n(),sd=ifelse(n()>1,sd(igg)/sqrt(n()),mean(igg)),igg=mean(igg))

df_ana %>% ggplot(aes(x=day,y=igg,ymin=igg-sd,ymax=igg+sd,
                      color=as.factor(ageG))) +
  geom_pointrange() +
  xlim(0,200) +
  facet_wrap(. ~ dose, scale='free',
             labeller = labeller(dose =
                                   c("1" = "Dose 1",
                                     "2" = "Dose 2",
                                     "3" = "Dose 3")
             )
  ) +
  labs(color='Age',
       y='Simulated Immune Response',
       x='Days since vaccination') +
  #scale_y_log10() +
  scale_color_brewer(palette = "Set2") +
  theme_classic()


df_serology %>% ggplot() +
  geom_histogram(aes(x=igg,fill=as.factor(dose))) +
  labs(fill='Vaccine Dose',
       y='Number of Samples',
       x='Simulated Immune Response') +
  scale_x_continuous(expand=c(0,0),limits=c(0,5000)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette = "Set2") +
  theme_classic()


df_serology %>% filter(igg<100) %>% group_by(dose) %>% summarise(n=n())

df_ana <- df_serology %>% mutate()  %>%
  mutate(
    outcome=as.integer(ifelse(igg<300,1,0)),
    sex=as.factor(sex),
    ageG=relevel(as.factor(cut(age,breaks=seq(0,100,20))),ref='(40,60]'),
    days_sinceG=relevel(as.factor(cut(days_since,breaks=c(0,25,50,75,100,150,Inf))),ref='(0,25]'),
    nrisks=as.factor(nrisks),
    dose=relevel(as.factor(dose),ref=2)) %>%
  filter(!is.na(outcome) & !is.na(ageG))

df_ana %>% group_by(days_sinceG) %>% summarise(n=n())
df_ana %>% group_by(outcome) %>% summarise(n=n())
df_ana %>% group_by(ageG) %>% summarise(n=n())


cnames <- df_ana %>% select(contains('c_')) %>% colnames

fit <- glm(outcome ~ ageG + sex + nrisks + dose, family=binomial,data=df_ana,na.action=NULL)
summary(fit)

ORs <- data.frame(OR=exp(coef(fit))) %>%
  cbind(exp(confint(fit)))
ORs$var <- row.names(ORs)
ORs

ORs %>% tail(-1) %>%
  ggplot(aes(x=OR,y=var)) +
  geom_pointrange(aes(xmin=`2.5 %`,xmax=`97.5 %`)) +
  geom_vline(xintercept = 1, linetype='dashed') +
  scale_x_log10(limits=c(0.01,1000))


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








