#' @useDynLib seave, .registration = TRUE
#' @import methods Rcpp
"_PACKAGE"

library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(dplyr)
library(forcats)


test2_serology <- function(df){#n=5000){
  #df <- generate_serology(n) %>% as_tibble
  df <- df %>% mutate(ndose = case_when(
         sample_date>v4_date ~ 4,
         sample_date>v3_date ~ 3,
         sample_date>v2_date ~ 2,
         sample_date>v1_date ~ 1,
         TRUE ~ 0))

  df %>% ggplot(aes(x=quant_result,fill=as.factor(ndose))) +
         geom_histogram() %>% #+ scale_y_log10() %>%
         return
}


test_hosp_analysis <- function(data,n=0){

  temp <- data$infections
  if(n>0){
    temp <- temp %>% sample_n(n)
  }

  temp <- temp %>% left_join(data$hosp %>% rename(hdate=date)) %>%
    mutate(days=hdate-date) %>%
    filter((days>0 & days<28) | is.na(days)) %>%
    mutate(outcome=ifelse(is.na(days),0,1))

  demo <- data$demographics %>% left_join(
    data$gp %>% group_by(id) %>% summarise(nrisks=n())
  ) %>% mutate(nrisks=as.factor(ifelse(is.na(nrisks),0,nrisks))) %>%
    left_join(
      data$gp %>% pivot_wider(names_from = condition,
                              values_from = condition,
                              names_prefix='c_',
                              values_fill = NA) %>%
                  mutate_at(vars(-("id")),~ifelse(is.na(.),0,1))
    ) %>%
    mutate_at(vars(starts_with('c_')),~ifelse(is.na(.),0,.))

  demo <- demo  %>%
          mutate(age = relevel(cut(age,breaks=seq(-1,100,25)),ref='(49,74]'),
                 bmi = relevel(cut(bmi,breaks=c(0,20,25,30,100)),ref='(20,25]'))

  temp <- temp %>% left_join(demo)

  temp <- temp %>% left_join(data$vaccine %>% rename(vdate=date)) %>%
    group_by(id,date) %>% mutate(ndose=row_number(),days_since = date-vdate) %>%
    arrange(id,date,days_since) %>%
    mutate(ndose=ifelse(days_since<0 | is.na(ndose),0,ndose)) %>%
    filter(all(days_since<0) | days_since>0) %>%
    ungroup %>% group_by(id,date) %>%
    filter(row_number()==1) %>%
    ungroup %>%
    mutate(ndose=as.factor(ndose))

  return(temp %>% as_tibble);

}

plot_hosp_analysis <- function(temp){

  #print (temp %>% group_by(ndose) %>% summarise(n=n()))
  #print (temp %>% group_by(ndose,outcome) %>% summarise(n=n()))
  print (temp %>% group_by(nrisks,outcome) %>% summarise(n=n()))

  #ns(date)
  conditions <- temp %>% select(contains('c_')) %>% colnames
  f <- paste0('outcome ~ age + bmi  + ndose + ',
              paste0(conditions,collapse=' + '))
  #print (f)
  temp <- temp %>% mutate_at(vars(starts_with('c_')),as.factor)
  fit <- glm(as.formula(f) ,data=temp)

  #fit <- glm(outcome ~ age + bmi + nrisks + ndose ,data=temp)
  #fit <- glm(outcome ~ age + bmi + nrisks ,data=temp)
  tab <- exp(coef(fit))

  tabci <- exp(confint(fit))


  tab <- tab %>% as.data.frame  %>% rownames_to_column('name') %>%
    as_tibble %>% rename(or=".")

  tabci <- tabci %>% as.data.frame  %>% rownames_to_column('name') %>%
    rename(LCL=`2.5 %`,UCL=`97.5 %`)

  tab <- tab %>% left_join(tabci) %>% tail(-1)


  tab %>% ggplot() +
    geom_pointrange(aes(x=name,y=or,ymin=LCL,ymax=UCL,fill='purple'),
                    width=.2,alpha=1,shape=21,
                    position=position_dodge(.9)) +
    coord_flip() %>% return


}


test_serology_analysis <- function(data,n=0){

  temp <- data$serology
  if(n>0){
    temp <- temp %>% sample_n(n)
  }
  temp <- temp %>% left_join(data$demographics)
  temp <- temp %>% left_join(data$vaccine %>% rename(vdate=date)) %>%
    group_by(id,date) %>% mutate(ndose=row_number(),days_since = date-vdate) %>%
    arrange(id,date,days_since) %>%
    mutate(ndose=ifelse(days_since<0 | is.na(ndose),0,ndose)) %>%
    filter(all(days_since<0) | days_since>0) %>%
    ungroup %>% group_by(id,date) %>%
    filter(row_number()==1) %>%
    ungroup %>%
    mutate(ndose=as.factor(ndose))
  #temp %>% select(id,date,vdate,ndose,days_since)

  #print (temp %>% group_by(ndose) %>% summarise(n=n()))

  #return (temp %>% ggplot(aes(x=date)) + geom_histogram());


  temp <- temp %>%
    mutate(age = relevel(cut(age,breaks=seq(-1,100,25)),ref='(49,74]'),
           bmi = relevel(cut(bmi,breaks=c(0,20,25,30,100)),ref='(20,25]'))


  temp <- temp %>% left_join(data$gp) %>% group_by(id) %>%
    #mutate(nrisks = relevel(as.factor(as.character(sum(!is.na(condition)))),ref='0')) %>%
    mutate(nrisks = factor(sum(!is.na(condition)),levels=c(0,1,2,3,4,5:10))) %>%
    filter(row_number()==1) %>% ungroup %>%
    mutate(outcome = ifelse(igg<20,1,0))

  print (temp %>% group_by(ndose) %>% summarise(n=n()))


  fit <- glm(outcome ~ age + bmi + nrisks + ndose,data=temp)
  tab <- exp(coef(fit))

  tabci <- exp(confint(fit))


  tab <- tab %>% as.data.frame  %>% rownames_to_column('name') %>%
      as_tibble %>% rename(or=".")

  tabci <- tabci %>% as.data.frame  %>% rownames_to_column('name') %>%
           rename(LCL=`2.5 %`,UCL=`97.5 %`)

  tab <- tab %>% left_join(tabci) %>% tail(-1)


  tab %>% ggplot() +
          geom_pointrange(aes(x=name,y=or,ymin=LCL,ymax=UCL,fill='purple'),
                          width=.2,alpha=1,shape=21,
                          position=position_dodge(.9)) +
          coord_flip(ylim=c(0.33,3)) +
          scale_y_log10() %>% return

}


test_risk_effect <- function(data){
  temp <- data$serology %>% left_join(data$demographics) %>%
    mutate(ageG = cut(age,breaks=seq(-1,100,25)),
           dateG = cut(date,breaks=seq(0,365*2,50)))

  temp <- temp %>% left_join(data$gp) %>% group_by(id) %>%
    mutate(nrisks = as.factor(sum(!is.na(condition)))) %>%
    filter(row_number()==1) %>% ungroup

  temp %>% filter(!is.na(dateG) & !is.na(ageG)) %>%
    #group_by(ageG) %>% mutate(nG=n()) %>%
    group_by(dateG,nrisks) %>%
    summarize(igg = mean(igg)) %>% ungroup %>%
    ggplot(aes(x=dateG,y=igg,fill=nrisks)) +
    geom_bar(stat='identity',position="dodge")


}

test_age_effect <- function(data){
  temp <- data$serology %>% left_join(data$demographics) %>%
    mutate(ageG = cut(age,breaks=seq(-1,100,25)),
           dateG = cut(date,breaks=seq(0,365*2,50)))


  temp %>% filter(!is.na(dateG) & !is.na(ageG)) %>%
    #group_by(ageG) %>% mutate(nG=n()) %>%
    group_by(dateG,ageG) %>%
    summarize(igg = mean(igg)) %>% ungroup %>%
    ggplot(aes(x=dateG,y=igg,fill=ageG)) +
    geom_bar(stat='identity',position="dodge")
  #stat_summary(fun=mean, geom="step")#, position = "stack")
}

test_response <- function(n=700){
  p <- generate_people(1)[[1]]
  sapply(1:n, function(x) p$get_immune_response(x)) %>% return
}

test_get_dataframe <- function(n=10000){
  data <- generate_people(n)
  data <- do.call(rbind, lapply(data,function(x){
    return(c(age=x$age,nrisks=x$get_nrisks()))
    })
    ) %>% as.data.frame()
  return (data);
}

test_infections <- function(data){
  data$infections %>%
    ggplot(aes(x=date)) +
    geom_histogram() %>% return
}

test_vaccines <- function(data){
  data$vaccine %>% filter(date>0) %>%
    ggplot(aes(x=date)) +
    geom_histogram() %>% return
}

test_hosp <- function(data){

  h <- data$hospitalisation %>%
        group_by(date) %>%
        summarise(nh=n())

  i <- data$infections %>%
         group_by(date) %>%
         summarise(ni=n())

  i <- i %>% full_join(h) %>%
              mutate(nh=ifelse(is.na(nh),0,nh)) %>%
              mutate(ni=ifelse(is.na(ni),0,ni)) %>%
              mutate(p=nh/ni)

  #return (i);

  maxi <- max(i$ni)
  maxh <- max(i$nh)

  print(maxi)
  print(maxh)

  i %>% ggplot(aes(x=date)) +
    geom_line(aes(y=ni/maxi)) +
    geom_line(aes(y=nh/maxh), color='red') %>%
    return


}


test_serology <- function(data){
  data$serology %>%
    mutate(date = cut(date,breaks=seq(1,365*2,20))) %>%
    group_by(date) %>% summarise(igg=mean(igg)) %>%
    ggplot(aes(x=date,y=igg,group=1)) +
    geom_line() %>% return
}

test_run_pandemic <- function(){
  run_pandemic(300) %>% as_tibble %>%
    mutate(x=1:300) %>%
    ggplot(aes(x=x,y=value)) + geom_line() %>% return
}

test_outcomes <- function(n=10000){
  people <- generate_people(n)

  get_demo <- function(i){
    x = people[[i]]
    return(c(
      id=paste0("SEAVE",i),
      age=x$age,
      sex=x$sex,
      bmi=round(x$bmi,2),
      nrisks=x$get_nrisks()))
  }
  df_demo <- do.call(rbind, lapply(1:length(people),get_demo)) %>%
             as.data.frame()

  get_infections <- function(i){
    x = people[[i]]
    return(c(
      id=paste0("SEAVE",i),
      date=x$infection_dates))
  }
  df_infect <- do.call(rbind, lapply(1:length(people),get_infections)) %>%
    as.data.frame()

  return (df_infect);



  #for (p in people) {
  #  outcomes <- p$outcome_dates
  #  if(length(outcomes) > 0) {
  #    print (outcomes)
  #  }
  #}
}
