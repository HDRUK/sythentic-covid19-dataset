#include <Rcpp.h>
#include "variant.h"
#include "pandemic.h"
#include "population.h"
#include <iostream>
#include <fstream>
#include <random>



class MyCohort : public Population {
protected:
  std::uniform_real_distribution<> dis;
  std::normal_distribution<> vaccine_dis;
  //std::normal_distribution<> vdismean;
  //std::normal_distribution<> vdiswidth;
  //std::normal_distribution<> vdisscale;

  //2 years;
  int ndays = 365*2;
  int istart = 0;

  double normal(double x, double mu, double sigma){
    return 1.0 / (sigma * sqrt(2.0 * M_PI)) * exp(-(pow((x - mu)/sigma, 2)/2.0));
  }

public:
  MyCohort() : dis(0,1),
               vaccine_dis(80, 20),
               Population() {
    Pandemic *pandemic = new Pandemic();
    std::map<std::string,Variant*> variants;
    variants["A"] = new Variant(1400,50,20);
    variants["B"] = new Variant(1300,200,30);
    variants["C"] = new Variant(1300,450,20);

    pandemic->set_variants(variants);
    this->set_pandemic(pandemic);
  };

  Person* simulate(){
    Person* person = this->generate();

    //simulate infections
    for(int i=this->istart; i<this->ndays+this->istart; i++){
      double p = this->pandemic->get_p_infection(i);
      if(this->dis(this->gen) < p){
        person->infection_dates.push_back(i);
        i+=50;//dont get infected for another 50 days
      }
    }

    // max 3 vaccines
    //start 100 days into the pandemic
    /*
    int nrisks = person->get_nrisks();
    double p_vaccine = pow((person->age+1)/100.,-0.3);

    if(nrisks>0){
      p_vaccine *= pow(nrisks/5.,-0.3);
    }*/

    int days = int(0);//*p_vaccine);
    //days = 100;
    int nvaccines = 1;
    for(int i=0; i<nvaccines; i++){
      //random not vaccinated with this dose
      //if(dis(this->gen) < 0.1) break;

      days += int(vaccine_dis(this->gen));
      person->vaccine_dates.push_back(days);
    }

    person->create_immune_response();
    person->create_infection_response();


    //simulate severe outcomes, on infection dates + 10 days
    for(auto date: person->infection_dates){
      for(int i = date;i<date+10;i++){
          double h = 50.*person->get_hazard(i);
          double rand = dis(this->gen);
             if (rand < h){
            person->outcome_dates.push_back(i);
          }
          break;
      }
    }
    //if(i==date && vdate<date && vdate>0){
    //   std::cout << " "<< p << " " << v << " " << pbad << std::endl;
    //}

    //double rand = dis(this->gen);
    //if (rand < pbad){
    //  person->outcome_dates.push_back(i);
    //  //hospitalised, so dont have it recorded again...
    //    break;
    //}


    return person;
  }

};


// [[Rcpp::export]]
Rcpp::DataFrame truth_generate(){
  int ndays = 365*2;
  MyCohort *cohort = new MyCohort();
  Person *p = cohort->simulate();

  Rcpp::IntegerVector day(ndays);
  Rcpp::NumericVector immune(ndays);
  Rcpp::NumericVector virus(ndays);
  Rcpp::NumericVector hazard(ndays);
  for(int i=0; i<ndays;i++){
    day[i] = i;
    immune[i] = p->get_immune_response(i);
    virus[i] = p->get_infection_level(i);
    hazard[i] = p->get_hazard(i);
  }

  Rcpp::DataFrame df =  Rcpp::DataFrame::create(
    Rcpp::Named("day")=day,
    Rcpp::Named("immune")=immune,
    Rcpp::Named("virus")=virus,
    Rcpp::Named("hazard")=hazard
  );
  return df;

}


// [[Rcpp::export]]
Rcpp::DataFrame my_pandemic_generate(double n=10000){

  int ndays = 365*2;

  MyCohort *cohort = new MyCohort();

  std::uniform_int_distribution<int> dist(1,ndays);
  std::random_device rd;
  std::mt19937_64 gen(rd());

  Rcpp::IntegerVector id(n);
  Rcpp::IntegerVector age(n);
  Rcpp::CharacterVector sex(n);
  Rcpp::NumericVector bmi(n);
  Rcpp::IntegerVector nrisks(n);
  Rcpp::IntegerVector mdate(n);
  Rcpp::IntegerVector igg(n);
  Rcpp::IntegerVector date(n);
  Rcpp::IntegerVector vdate1(n);

  std::map<std::string,Rcpp::IntegerVector> comorbids;

  for(auto const& c: cohort->comorbidities){
    Rcpp::IntegerVector col(n);
    comorbids[c.first] = col;
  }

  //resolution of measurement
  std::normal_distribution<double> res(1,0.1);
  std::uniform_real_distribution<> dis(0,100);
  int i=0;
  while(i<n){
    Person *p = cohort->simulate();

    id[i] = i+1;
    age[i] = p->get_age();
    sex[i] = p->get_sex();
    bmi[i] = p->get_bmi();
    nrisks[i] = p->comorbidities.size();
    //vdate1[i] = NA_REAL;
    //mdate[i] = NA_REAL;
    igg[i] = NA_REAL;
    date[i] = (p->outcome_dates.size() > 0) ? p->outcome_dates.at(0) : NA_REAL;

    if(p->vaccine_dates.size() > 0){
      vdate1[i] = p->vaccine_dates.at(0);
    }
    int rday = dist(gen);
    mdate[i] = rday;
    igg[i] = p->get_immune_response(rday);


    for(auto const& c: cohort->comorbidities){
      comorbids[c.first][i] = 0;
    }

    for(auto const& c: p->comorbidities){
      comorbids[c.first][i] = 1;
    }

    if(i%1000 == 0 ){
      std::cout << "done " << i << " / " << n << " (" << int(100*i/n) << " %)" << std::endl;
    }

    i++;
  }

  Rcpp::DataFrame df =  Rcpp::DataFrame::create(
    Rcpp::Named("id")=id,
    Rcpp::Named("age")=age,
    Rcpp::Named("sex")=sex,
    Rcpp::Named("bmi")=bmi,
    Rcpp::Named("nrisks")=nrisks,
    Rcpp::Named("meas_date")=mdate,
    Rcpp::Named("igg")=igg,
    Rcpp::Named("hosp_date")=date,
    Rcpp::Named("v1_date")=vdate1
  );

  for(auto const& c: cohort->comorbidities){
    df.push_back(comorbids[c.first],c.first);
  }

  return df;
}



