#include <Rcpp.h>
#include "variant.h"
#include "pandemic.h"
#include "population.h"
#include <iostream>
#include <fstream>
#include <random>


class Eave : public Population {

protected:
  std::uniform_real_distribution<> dis;
  std::normal_distribution<> vaccine_dis;

  //3 years;
  int ndays = 365*3;
  int istart = 0;

public:
  Eave() : dis(0,1),vaccine_dis(150,30),
  Population() {
    Pandemic *pandemic = new Pandemic();
    pandemic->create_default_variants();
    this->set_pandemic(pandemic);

  };

  double get_vaccine_product(double score, double dose){
     if(dose < 3){
       if(score >= 0.75){
         return 1;
       }
       else if(score>=0.5){
         return 2;
       }
       else{
         return 3;
       }
     }
     else if(dose == 3){
       if(score >= 0.75){
         return 2;
       }
       else{
         return 3;
       }
     }
     else{
       return 3;
     }
   }
  double get_vaccine_priority_score(Person* p){
      int age = p->get_age();
      int nrisks = p->get_nrisks();
      double bmi = p->get_bmi();

      double s;
      if(age>80 || nrisks>3){
        s = 1;
      }
      else if(age> 60 || nrisks>2){
        s = 0.75;
      }
      else if(age> 40 || nrisks>0){
        s = 0.5;
      }
      else if(age> 20){
        s = 0.25;
      }
      else if(age> 6){
        s = 0.1;
      }
      else{
        s = 0.001;
      }
      return s;
  }

  Person* simulate(){

    std::normal_distribution<> vaccine_dis(90,20);
    Person* p = this->generate();

    int days = 170;
    double pscore = get_vaccine_priority_score(p);

    days *= pow(pscore,0.4);

    int nvaccines = 3;
    for(int i=0; i<nvaccines; i++){
      //random not vaccinated with this dose
      if(dis(this->gen) < 0.1*nvaccines) break;
      days += int(vaccine_dis(this->gen));
      if(i>1){
        days += int(vaccine_dis(this->gen));
      }

      int product = get_vaccine_product(pscore,i+1);

      p->vaccine_dates.push_back(days);
      p->vaccine_products.push_back(product);
    }


    int i = 0;
    while(i<this->ndays){
      double pi = this->pandemic->get_p_infection(i);
      if(dis(gen) < pi){
        p->infection_dates.push_back(i);
        i+=100;//dont get infected for at least another 100 days
      }
      i++;
    }


    p->create_immune_response();
    p->create_infection_response();

    //simulate severe outcomes, on infection dates + 10 days
    for(auto date: p->infection_dates){
      for(int i = date;i<date+10;i++){
        double h = p->get_hazard(i);
        double rand = dis(this->gen);
        if (rand < h){
          p->outcome_dates.push_back(i);
          break;
        }
      }
    }
    return p;
  }

};

// [[Rcpp::export]]
Rcpp::List generate_eave(double n=30000,int id_start=1){

  int ndays = 365*3;

  Eave *eave = new Eave();


  std::uniform_int_distribution<int> dist(1,ndays);
  std::random_device rd;
  std::mt19937_64 gen(rd());


  //resolution of measurement - 10%
  std::normal_distribution<double> res(1,0.1);
  //std::uniform_real_distribution<> dis(0,100);
  std::uniform_real_distribution<> dis(0,1);
  std::uniform_real_distribution<> dis_idate(0,14);
  std::uniform_real_distribution<> get_day(0,ndays);
  std::uniform_int_distribution<> get_person(0,n-1);


  int i = 0;
  std::vector<Person*> cohort;
  int f = n/10;
  while(i<n){
    cohort.push_back(eave->simulate());
    if(i%f == 0 ){
      std::cout << "Done " << i << " / " << n << " (" << 100*i/n << " % )" << std::endl;
    }
    i++;
  }

  Rcpp::IntegerVector demo_person_id(n);
  Rcpp::IntegerVector  demo_age(n);
  Rcpp::CharacterVector  demo_sex(n);
  Rcpp::CharacterVector  demo_ethnicity(n);
  Rcpp::IntegerVector  demo_imd(n);
  Rcpp::NumericVector  demo_bmi(n);

  for(int i=0; i<n; i++){
    int id = i+id_start;
    cohort[i]->id = id;
    demo_person_id[i] = cohort[i]->id;
    demo_age[i] = cohort[i]->get_age();
    demo_sex[i] = cohort[i]->get_sex();
    demo_ethnicity[i] = cohort[i]->get_ethnicity();
    demo_imd[i] = cohort[i]->get_imd();
    demo_bmi[i] = cohort[i]->get_bmi();
  }

  Rcpp::DataFrame df_demo =  Rcpp::DataFrame::create(
    Rcpp::Named("LINKNO") = demo_person_id,
    Rcpp::Named("age") = demo_age,
    Rcpp::Named("sex") = demo_sex,
    Rcpp::Named("ethnicity") = demo_ethnicity,
    Rcpp::Named("bmi") = demo_bmi
  );

  int ncond = 0;
  int nvaccines = 0;
  int nhosp = 0;
  int ninfections = 0;
  for(int i=0; i<n; i++){
    ncond+=cohort[i]->get_comorbidities().size();
    nvaccines+=cohort[i]->vaccine_dates.size();
    nhosp+=cohort[i]->outcome_dates.size();
    ninfections+=cohort[i]->infection_dates.size();
  }

  Rcpp::IntegerVector gp_person_id(ncond);
  Rcpp::CharacterVector  gp_condition(ncond);

  int j=0;
  for(int i=0; i<n; i++){
    for(int ii=0;ii<cohort[i]->get_comorbidities().size();ii++){
      std::string cond = cohort[i]->get_comorbidities()[ii];
      gp_condition[j] = cond;
      gp_person_id[j] = cohort[i]->id;
      j++;
    }
  }

  Rcpp::DataFrame df_gp =  Rcpp::DataFrame::create(
    Rcpp::Named("LINKNO") = gp_person_id,
    Rcpp::Named("condition") = gp_condition
  );


  Rcpp::IntegerVector vaccine_person_id(nvaccines);
  Rcpp::IntegerVector  vaccine_date(nvaccines);
  Rcpp::IntegerVector  vaccine_product(nvaccines);

  j=0;
  for(int i=0; i<n; i++){
    for(int ii=0;ii<cohort[i]->vaccine_dates.size();ii++){
      vaccine_person_id[j] = cohort[i]->id;
      vaccine_date[j] = cohort[i]->vaccine_dates[ii];
      vaccine_product[j] = cohort[i]->vaccine_products[ii];
      j++;
    }
  }

  Rcpp::DataFrame df_vaccine =  Rcpp::DataFrame::create(
    Rcpp::Named("LINKNO") = vaccine_person_id,
    Rcpp::Named("date") = vaccine_date,
    Rcpp::Named("product") = vaccine_product
  );



  Rcpp::IntegerVector hosp_person_id(nhosp);
  Rcpp::IntegerVector  hosp_date(nhosp);

  j=0;
  for(int i=0; i<n; i++){
    for(int ii=0;ii<cohort[i]->outcome_dates.size();ii++){
      hosp_person_id[j] = cohort[i]->id;
      hosp_date[j] = cohort[i]->outcome_dates[ii];
      j++;
    }
  }

  Rcpp::DataFrame df_hosp =  Rcpp::DataFrame::create(
    Rcpp::Named("LINKNO") = hosp_person_id,
    Rcpp::Named("date") = hosp_date
  );

  //include an extra 20% of testing data
  ninfections *= 1.2;
  Rcpp::IntegerVector pcr_person_id(ninfections);
  Rcpp::IntegerVector  pcr_date(ninfections);
  Rcpp::IntegerVector  pcr_result(ninfections);

  double pcr_thres = 0.005;
  j=0;
  for(int i=0; i<n; i++){
    for(int ii=0;ii<cohort[i]->infection_dates.size();ii++){

      if(dis(gen)<0.3){
        //skip 30% of infections
        continue;
      }

      pcr_person_id[j] = cohort[i]->id;
      int date = int(dis_idate(gen)) + cohort[i]->infection_dates[ii];
      double level = cohort[i]->get_infection_level(date);
      int result = 0;
      if(level>pcr_thres){
        result = 1;
      }
      pcr_date[j] = date;
      pcr_result[j] = result;
      j++;
    }
  }


  while(j<ninfections){
    int ii = get_person(gen);
    int date = get_day(gen);


    double level = cohort[ii]->get_infection_level(date);
    int result = 0;
    if(level>pcr_thres){
      result = 1;
    }

    pcr_person_id[j] = cohort[ii]->id;
    pcr_date[j] = date;
    pcr_result[j] = result;
    j++;
  }

  Rcpp::DataFrame df_pcr =  Rcpp::DataFrame::create(
    Rcpp::Named("LINKNO") = pcr_person_id,
    Rcpp::Named("date") = pcr_date,
    Rcpp::Named("result") = pcr_result
  );

  int nserology_pc = 0.1*n;

  Rcpp::IntegerVector pc_igg_person_id(nserology_pc);
  Rcpp::IntegerVector  pc_igg_date(nserology_pc);
  Rcpp::NumericVector  pc_igg_quant_result(nserology_pc);
  Rcpp::IntegerVector  pc_igg_qual_result(nserology_pc);

  j=0;
  while(j<nserology_pc){
    int ii = get_person(gen);
    int date = get_day(gen);

    if(dis(gen) < (0.1 + cohort[ii]->get_nrisks()/6.)){
      continue;
    }

    pc_igg_person_id[j] = cohort[ii]->id;
    pc_igg_date[j] = date;
    pc_igg_quant_result[j] = cohort[ii]->get_immune_response(date);
    pc_igg_qual_result[j] = pc_igg_quant_result[j] > 10 ? 1 : 0;

    j++;

  }

  Rcpp::DataFrame df_serology_pc =  Rcpp::DataFrame::create(
    Rcpp::Named("LINKNO") = pc_igg_person_id,
    Rcpp::Named("sample_date") = pc_igg_date,
    Rcpp::Named("quant_result") = pc_igg_quant_result,
    Rcpp::Named("qual_result") = pc_igg_qual_result
  );


  int nserology_bd = 0.05*n;

  Rcpp::IntegerVector bd_igg_person_id(nserology_bd);
  Rcpp::IntegerVector  bd_igg_date(nserology_bd);
  Rcpp::NumericVector  bd_igg_quant_result(nserology_bd);
  Rcpp::IntegerVector  bd_igg_qual_result(nserology_bd);

  j=0;
  while(j<nserology_bd){
    int ii = get_person(gen);
    int date = get_day(gen);

    if(dis(gen) < (1 - cohort[ii]->get_nrisks()/6.)){
      continue;
    }

    bd_igg_person_id[j] = cohort[ii]->id;
    bd_igg_date[j] = date;
    bd_igg_quant_result[j] = cohort[ii]->get_immune_response(date);
    bd_igg_qual_result[j] = bd_igg_quant_result[j] > 10 ? 1 : 0;

    j++;

  }

  Rcpp::DataFrame df_serology_bd =  Rcpp::DataFrame::create(
    Rcpp::Named("LINKNO") = bd_igg_person_id,
    Rcpp::Named("sample_date") = bd_igg_date,
    Rcpp::Named("quant_result") = bd_igg_quant_result,
    Rcpp::Named("qual_result") = bd_igg_qual_result
  );



  Rcpp::List retval = Rcpp::List::create(
    Rcpp::Named("demo") = df_demo,
    Rcpp::Named("gp") = df_gp,
    Rcpp::Named("vaccines") = df_vaccine,
    Rcpp::Named("hospitalisations") = df_hosp,
    Rcpp::Named("pcr") = df_pcr,
    Rcpp::Named("serology_pc") = df_serology_pc,
    Rcpp::Named("serology_bd") = df_serology_bd
  );

  return retval;
}


