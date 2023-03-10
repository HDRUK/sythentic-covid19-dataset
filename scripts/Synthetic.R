

population <- new(seave::Population)
population

n <- 10000
n <- 10
cohort <- lapply(rep(1, n), function(i){population$generate()})
cohort

person <- sample(cohort,1)[[1]]
person

df_igg <-  do.call(rbind,lapply(
  seq(1,1000),
  function(i) data.frame(date=i,igg=person$get_immune_response(i))
)) %>% as_tibble

df_igg %>%
  ggplot(aes(x=date,y=igg)) +
  geom_line(color='red') +
  labs(x='Date [days]',y='Immune Response [arb]',color='Person') +
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
  data.frame(age=p$age,sex=p$sex,bmi=p$bmi,nrisks=p$get_nrisks())
}

df_people <- do.call(rbind,lapply(cohort,extract_person))  %>%
             as_tibble
df_people

df_people %>% ggplot(aes(x=age,fill=as.factor(sex))) +
  geom_bar() +
  scale_x_binned(n.breaks = 20)


df_people %>% ggplot(aes(x=age,fill=as.factor(nrisks))) +
  geom_bar(position='dodge') +
  scale_x_binned(n.breaks = 10)





extract_serology <- function(p){
  #random day in the pandemic when the measurement was taken
  day <- sample(150:500,1)
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

  data.frame(date=day,days_since=days_since,igg=igg,
             dose=dose,dose_date=dd,
             age=p$age,sex=p$sex,bmi=p$bmi,nrisks=p$get_nrisks())
}

df_serology <- do.call(rbind,lapply(sample(cohort,10000,replace=T),extract_serology))  %>%
  as_tibble %>% filter(dose>0)

df_ana <- df_serology %>%
          mutate(day=cut(days_since,breaks=seq(0,1000,10),labels=seq(10,1000,10))) %>%
          mutate(day=as.integer(as.character(day))) %>%
          group_by(day,dose,sex) %>%
          summarise(sd=ifelse(n()>1,sd(igg)/sqrt(n()),mean(igg)),igg=mean(igg))


df_ana %>% ggplot(aes(x=day,y=igg,ymin=igg-sd,ymax=igg+sd,color=as.factor(sex))) +
           geom_pointrange() +
           xlim(0,200) +
           facet_wrap(. ~ dose, scale='free')



