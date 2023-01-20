#include <Rcpp.h>
#include "variant.h"
#include "pandemic.h"
#include "population.h"
#include <iostream>
#include <fstream>
#include <random>


class Ace : public Population {
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
  Ace() : dis(0,1),vaccine_dis(80,20),
  Population() {
    //Pandemic *pandemic = new Pandemic();
    /*std::map<std::string,Variant*> variants;
    variants["A"] = new Variant(1400,50,20);
    variants["B"] = new Variant(1300,200,30);
    variants["C"] = new Variant(1300,450,20);

    pandemic->set_variants(variants);
    this->set_pandemic(pandemic);*/
  };

  Person* simulate(){
   Person* person = this->generate();
   while (person->age>25 || person->age<18){//} || person->comorbidities.size()>1){
     person = this->generate();
   }

    int days = int(0);//*p_vaccine);
    //days = 100;
    int nvaccines = 3;
    for(int i=0; i<nvaccines; i++){
      //random not vaccinated with this dose
      if(dis(this->gen) < 0.1) break;

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

    return person;
  }

};

// [[Rcpp::export]]
Rcpp::DataFrame generate_ace(double n=30000){

  int ndays = 365*2;

  Ace *ace = new Ace();

  std::uniform_int_distribution<int> dist(1,ndays);
  std::random_device rd;
  std::mt19937_64 gen(rd());

  Rcpp::IntegerVector participant_id(n);
  Rcpp::CharacterVector location(n);
  Rcpp::IntegerVector  visit_no(n);
  Rcpp::IntegerVector  visit_date(n);
  Rcpp::IntegerVector  age(n);
  Rcpp::CharacterVector  sex(n);
  Rcpp::CharacterVector  ethnicity(n);
  Rcpp::IntegerVector  positivity_status(n);
  Rcpp::IntegerVector  positivity_date(n);
  Rcpp::CharacterVector  positivity_determination(n);
  Rcpp::IntegerVector vaccination_status(n);
  Rcpp::CharacterVector  vaccine_type(n);
  Rcpp::IntegerVector  vaccine_dose(n);
  Rcpp::IntegerVector  vaccine_date1(n);
  Rcpp::IntegerVector  vaccine_date2(n);
  Rcpp::IntegerVector  vaccine_date3(n);
  Rcpp::NumericVector  IgG_BAU_wuhan_average(n);
  Rcpp::NumericVector  IgG_BAU_nucleocapsid_average(n);
  Rcpp::CharacterVector  cohort(n);


  //resolution of measurement - 10%
  std::normal_distribution<double> res(1,0.1);
  //std::uniform_real_distribution<> dis(0,100);
  std::uniform_real_distribution<> dis(0,1);
  std::uniform_real_distribution<> get_day(0,ndays);


  int i=0;
  int id=0;

  while(i<n){
    Person *p = ace->simulate();
    id++;
    int visit = 1;
    int mday = 0;
    do {
      //get the measurement day
      mday += get_day(gen);
      if (mday>ndays) break;

      double igg = res(gen)*p->get_immune_response(mday);

      participant_id[i] = id;
      location[i] = p->get_imd();
      age[i] = p->age;
      sex[i] = p->sex;
      ethnicity[i] = p->ethnicity;
      visit_no[i] = visit;
      visit_date[i] = mday;

      positivity_status[i] = igg > 20;
      positivity_date[i] = mday;
      positivity_determination[i] = "test";

      int vdate1 = (p->vaccine_dates.size()>0) ? p->vaccine_dates.at(0) : NA_INTEGER;
      int vdate2 = (p->vaccine_dates.size()>1) ? p->vaccine_dates.at(1) : NA_INTEGER;
      int vdate3 = (p->vaccine_dates.size()>2) ? p->vaccine_dates.at(2) : NA_INTEGER;

      vaccination_status[i] = (vdate1 != NA_INTEGER ) ? 1 : 0;
      vaccine_dose[i] = NA_INTEGER;
      if(vdate3 != NA_INTEGER && mday>vdate3){
        vaccine_dose[i] = 3;
      }
      else if(vdate2 != NA_INTEGER && mday>vdate2){
        vaccine_dose[i] = 2;
      }
      else if(vdate1 != NA_INTEGER && mday>vdate1){
        vaccine_dose[i] = 1;
      }
      else{
        vaccine_dose[i] = 0;
      }

      vaccine_date1[i] = vdate1;
      vaccine_date2[i] = vdate2;
      vaccine_date3[i] = vdate3;

      IgG_BAU_nucleocapsid_average[i] = igg;
      IgG_BAU_wuhan_average[i] = igg;

      cohort[i] = "ACE";

      visit++;
      i++;

      if(i%1000 == 0 ){
        std::cout << "done " << i << " / " << n << " (" << int(100*i/n) << " %)" << std::endl;
      }

    } while (dis(gen)<0.5 && i<n); //do some random multiple measurements



  }

  Rcpp::DataFrame df =  Rcpp::DataFrame::create(
    Rcpp::Named("participant_id") = participant_id,
    Rcpp::Named("location") = location,
    Rcpp::Named("visit_no") = visit_no,
    Rcpp::Named("visit_date") = visit_date,
    Rcpp::Named("age") = age,
    Rcpp::Named("sex") = sex,
    Rcpp::Named("ethnicity") = ethnicity,
    Rcpp::Named("positivity_status") = positivity_status,
    Rcpp::Named("positivity_date") = positivity_date,
    Rcpp::Named("positivity_determination") = positivity_determination,
    Rcpp::Named("vaccination_status") = vaccination_status,
    Rcpp::Named("vaccine_type") = vaccine_type,
    Rcpp::Named("vaccine_dose") = vaccine_dose,
    Rcpp::Named("vaccine_date1") = vaccine_date1,
    Rcpp::Named("vaccine_date2") = vaccine_date2,
    Rcpp::Named("vaccine_date3") = vaccine_date3,
    Rcpp::Named("IgG_BAU_wuhan_average") = IgG_BAU_wuhan_average,
    Rcpp::Named("IgG_BAU_nucleocapsid_average") = IgG_BAU_nucleocapsid_average,
    Rcpp::Named("Cohort – ACE Immunity") = cohort
  );

  return df;
}



