#include <Rcpp.h>
#include "variant.h"
#include "pandemic.h"
#include "population.h"
#include <iostream>
#include <fstream>
#include <random>
#include <cctype>

RCPP_MODULE(RcppSeaveEx){
  Rcpp::class_<Variant>("Variant")
    .constructor<double,double,double>()
    .method("get_abundance", &Variant::get_abundance)
    .method("test", &Variant::get_abundance2);
}

RCPP_MODULE(RcppSeavePandemic){
  Rcpp::class_<Pandemic>("Pandemic")
   .constructor()
   .method("run",&Pandemic::run)
   .method("get_p_infection",&Pandemic::get_p_infection)
   .method("create_default_variants",&Pandemic::create_default_variants);
}

RCPP_MODULE(RcppSeavePerson){
  Rcpp::class_<Person>("Person")
  .constructor()
  .field("age", &Person::age)
  .field("sex", &Person::sex)
  .field("imd", &Person::imd)
  .field("bmi", &Person::bmi)
  .field("ethnicity", &Person::ethnicity)
  .field("vaccine_dates", &Person::vaccine_dates)
  .field("infection_dates", &Person::infection_dates)
  .field("outcome_dates", &Person::outcome_dates)
  .method("get_nrisks", &Person::get_nrisks)
  .method("get_immune_response", &Person::get_immune_response)
  .method("get_comorbidities", &Person::get_comorbidities);

}

RCPP_EXPOSED_CLASS(Person);

RCPP_EXPOSED_CLASS(Population);

RCPP_EXPOSED_CLASS(Pandemic);

RCPP_MODULE(RcppSeavePopulation){
  Rcpp::class_<Population>("Population")
  .constructor()
  .method("set_pandemic",&Population::set_pandemic)
  .method("generate", &Population::test);
}

// [[Rcpp::export]]
Rcpp::NumericVector generate_ages(double n=10000){
  Rcpp::NumericVector retval;
  Population *pop = new Population();
  for(int i=0; i<n; i++){
    Person* p = pop->generate();
    retval.push_back(p->age);
    //delete p;
  }
  return retval;
}

// [[Rcpp::export]]
Rcpp::NumericVector generate_bmi(double n=10000){
  Rcpp::NumericVector retval;
  Population *pop = new Population();
  for(int i=0; i<n; i++){
    Person *p = pop->generate();
    retval.push_back(p->get_bmi());
    //delete p;
  }
  return retval;
}

// [[Rcpp::export]]
Rcpp::NumericVector generate_risks(double n=10000){
  Rcpp::NumericVector retval;
  Population *pop = new Population();
  for(int i=0; i<n; i++){
    Person *p = pop->generate();
    retval.push_back(p->get_nrisks());
    //delete p;
  }
  return retval;
}

/*
// [[Rcpp::export]]
Rcpp::List generate_people(double n=10000){
  Rcpp::List retval;
  Population *pop = new Population();
  for(int i=0; i<n; i++){
    Person *p = pop->generate();
    retval.push_back(&p);
  }
  return retval;
}
 */


// [[Rcpp::export]]
Rcpp::List generate_dataframe(double n=100){

  Population *pop = new Population();

  Rcpp::IntegerVector days(365*2);
  Rcpp::NumericVector abundance(365*2);
  Pandemic* pandemic = pop->pandemic;
  for(int i=0; i<365*2; i++){
    days[i] = i;
    abundance[i] = pandemic->get_p_infection(i);
  }

  Rcpp::DataFrame df_truth_pandemic =  Rcpp::DataFrame::create(
    Rcpp::Named("days")=days,
    Rcpp::Named("abundance")=abundance
  );

  /*
  std::ofstream demo;
  demo.open("data/demo.csv"); // Create file
  demo << "id,age,sex,bmi" << std::endl;

  std::ofstream gp;
  gp.open("data/gp.csv"); // Create file
  gp << "id,condition" << std::endl;

  std::ofstream infect;
  infect.open("data/infect.csv"); // Create file
  infect << "id,date" << std::endl;

  std::ofstream vaccine;
  vaccine.open("data/vaccine.csv"); // Create file
  vaccine << "id,date" << std::endl;

  std::ofstream hosp;
  hosp.open("data/hosp.csv"); // Create file
  hosp << "id,date" << std::endl;
  */

  std::cout << "start loop" <<std::endl;
  std::vector<Person*> people(n);
  std::cout << "generating people" << std::endl;
  int n_infections = 0;
  int n_vaccines = 0;
  int n_hosp = 0;
  int n_gp = 0;
  for(int i=0; i<n; i++){
    Person *p = pop->generate();
    people[i] = p;
    n_infections+=p->infection_dates.size();
    n_gp+=p->comorbidities.size();
    n_vaccines+=p->vaccine_dates.size();
    n_hosp+=p->outcome_dates.size();

    if(i%1000 == 0 ){
      std::cout << "done " << i << " / " << n << " (" << int(100*i/n) << " %)" << std::endl;
    }


  }
  std::cout << "ninfections = " << n_infections << std::endl;
  std::cout << "ngp = " << n_gp << std::endl;
  std::cout << "nvaccines = " << n_vaccines << std::endl;
  std::cout << "nhosp = " << n_hosp << std::endl;

  Rcpp::IntegerVector id(n);
  Rcpp::IntegerVector age(n);
  Rcpp::CharacterVector sex(n);
  Rcpp::NumericVector bmi(n);

  Rcpp::IntegerVector igg_id(n);
  Rcpp::IntegerVector igg_date(n);
  Rcpp::NumericVector igg_value(n);

  Rcpp::IntegerVector infect_id(n_infections);
  Rcpp::IntegerVector infect_date(n_infections);

  Rcpp::IntegerVector vaccine_id(n_vaccines);
  Rcpp::IntegerVector vaccine_date(n_vaccines);

  Rcpp::IntegerVector hosp_id(n_hosp);
  Rcpp::IntegerVector hosp_date(n_hosp);

  Rcpp::IntegerVector gp_id(n_gp);
  Rcpp::CharacterVector gp_condition(n_gp);


  int i = 0;
  int i_infections = 0;
  int i_vaccines = 0;
  int i_hosp = 0;
  int i_gp = 0;

  std::uniform_int_distribution<int> dist(1,365*2);
  std::random_device rd;
  std::mt19937_64 gen(rd());

  for(auto p: people){
    int _id = i+1;
    id[i] = _id;
    age[i] = p->age;
    sex[i] = p->sex;
    bmi[i] = p->bmi;

    int rday = dist(gen);
    double igg = p->get_immune_response(rday);
    igg_id[i] = _id;
    igg_date[i] = rday;
    igg_value[i] = igg;

    i++;

    for(auto c: p->comorbidities){
      //gp << id << "," << name << std::endl;
      gp_id[i_gp] = _id;
      gp_condition[i_gp] = c.first;
      i_gp++;
    }

    for(auto date: p->infection_dates){
      //infect << id << "," << date << std::endl;
      infect_id[i_infections] = _id;
      infect_date[i_infections] = date;
      i_infections++;
    }

    for(auto date: p->vaccine_dates){
      //vaccine << id << "," << date << std::endl;
      vaccine_id[i_vaccines] = _id;
      vaccine_date[i_vaccines] = date;
      i_vaccines++;
    }


    for(auto date: p->outcome_dates){
      //hosp << id << "," << date << std::endl;
      hosp_id[i_hosp] = _id;
      hosp_date[i_hosp] = date;
      i_hosp++;
    }

    delete p;

  }

  // create a new data frame
  Rcpp::DataFrame df_demo =  Rcpp::DataFrame::create(
    Rcpp::Named("id")=id,
    Rcpp::Named("age")=age,
    Rcpp::Named("bmi")=bmi,
    Rcpp::Named("sex")=sex
  );

  Rcpp::DataFrame df_igg =  Rcpp::DataFrame::create(
    Rcpp::Named("id")=igg_id,
    Rcpp::Named("date")=igg_date,
    Rcpp::Named("igg")=igg_value
  );


  Rcpp::DataFrame df_infect =  Rcpp::DataFrame::create(
    Rcpp::Named("id")=infect_id,
    Rcpp::Named("date")=infect_date
  );

  Rcpp::DataFrame df_vaccine =  Rcpp::DataFrame::create(
    Rcpp::Named("id")=vaccine_id,
    Rcpp::Named("date")=vaccine_date
  );

  Rcpp::DataFrame df_hosp =  Rcpp::DataFrame::create(
    Rcpp::Named("id")=hosp_id,
    Rcpp::Named("date")=hosp_date
  );

  Rcpp::DataFrame df_gp =  Rcpp::DataFrame::create(
    Rcpp::Named("id")=gp_id,
    Rcpp::Named("condition")=gp_condition
  );

  Rcpp::List retval = Rcpp::List::create(
    Rcpp::Named("demographics")=df_demo,
    Rcpp::Named("serology")=df_igg,
    Rcpp::Named("infections")=df_infect,
    Rcpp::Named("vaccine")=df_vaccine,
    Rcpp::Named("hospitalisation")=df_hosp,
    Rcpp::Named("gp")=df_gp,
    Rcpp::Named("truth.pandemic")=df_truth_pandemic
  );

  return retval;

}

// [[Rcpp::export]]
Rcpp::List get_true_response(){

  Population *pop = new Population();

  int n = 365*2;
  Rcpp::IntegerVector date(n);
  Rcpp::NumericVector level(n);

  Person *p = pop->generate();
  for (int i=0;i<n;i++){
    double igg = p->get_immune_response(i);
    date[i] = i;
    level[i] = igg;
  }

  Rcpp::List retval  = Rcpp::List::create(
    Rcpp::Named("data") = Rcpp::DataFrame::create(
                            Rcpp::Named("date")=date,
                            Rcpp::Named("level")=level
    ),
    Rcpp::Named("dates") = p->vaccine_dates
  );

  return retval;
}




// [[Rcpp::export]]
Rcpp::DataFrame generate_serology(double n=100){

  Population *pop = new Population();

  std::uniform_int_distribution<int> dist(1,365*2);
  std::random_device rd;
  std::mt19937_64 gen(rd());

  Rcpp::IntegerVector id(n);
  Rcpp::IntegerVector age(n);
  Rcpp::CharacterVector sex(n);
  Rcpp::NumericVector bmi(n);
  Rcpp::NumericVector quant(n);
  Rcpp::CharacterVector qual(n);
  Rcpp::IntegerVector date(n);
  Rcpp::IntegerVector vdate1(n);
  Rcpp::IntegerVector vdate2(n);
  Rcpp::IntegerVector vdate3(n);
  Rcpp::IntegerVector vdate4(n);

  //resolution of measurement
  std::normal_distribution<double> res(1,0.1);

  int i=0;
  int thres = 1000;
  while(i<n){
    Person *p = pop->generate();

    //if(p->get_age()<50 && p->comorbidities.size()<2)
    //  continue;

    if(p->get_age()>50)
      continue;

    if(p->vaccine_dates.size()==0)
      continue;

    int rday=0;
    int v1 = p->vaccine_dates[0];
    int v3 = (p->vaccine_dates.size()>2) ? p->vaccine_dates[2] : 1000;

    while (rday<v1 || rday>v3){
        rday = dist(gen);
    }



    double igg = p->get_immune_response(rday)*res(gen);
    igg = (igg>thres) ? thres : igg;
    id[i] = i+1;
    age[i] = p->get_age();
    sex[i] = p->get_sex();
    bmi[i] = p->get_bmi();
    quant[i] = igg;
    qual[i] = (igg<20) ? "Negative" : "Positive";
    date[i] = rday;
    vdate1[i] = NA_REAL;
    vdate2[i] = NA_REAL;
    vdate3[i] = NA_REAL;
    vdate4[i] = NA_REAL;
    for (int iv=0; iv < p->vaccine_dates.size(); iv++){
      if(iv==0){
        vdate1[i] = p->vaccine_dates[iv];
      }
      else if(iv==1){
        vdate2[i] = p->vaccine_dates[iv];
      }
      else if(iv==2){
        vdate3[i] = p->vaccine_dates[iv];
      }
      else if(iv==3){
        vdate4[i] = p->vaccine_dates[iv];
      }
    }

    i++;
  }


  Rcpp::DataFrame df =  Rcpp::DataFrame::create(
    Rcpp::Named("id")=id,
    Rcpp::Named("age")=age,
    Rcpp::Named("sex")=sex,
    Rcpp::Named("bmi")=bmi,
    Rcpp::Named("quant_result")=quant,
    Rcpp::Named("qual_result")=qual,
    Rcpp::Named("sample_date")=date,
    Rcpp::Named("v1_date")=vdate1,
    Rcpp::Named("v2_date")=vdate2,
    Rcpp::Named("v3_date")=vdate3,
    Rcpp::Named("v4_date")=vdate4
  );

  return df;
}



// [[Rcpp::export]]
std::vector<double> run_pandemic(int n=300){
  std::vector<double> retval;
  Pandemic *p = new Pandemic();
  std::vector<double> ivec(n);
  std::iota(ivec.begin(), ivec.end(), 0);
  retval = p->run(ivec);
  return retval;
}


class SeaveCohort : public Population {
protected:
  std::uniform_real_distribution<> dis;
  std::normal_distribution<> vaccine_dis;

  //2 years;
  int ndays = 365*2;
  int istart = 0;

  double normal(double x, double mu, double sigma){
    return 1.0 / (sigma * sqrt(2.0 * M_PI)) * exp(-(pow((x - mu)/sigma, 2)/2.0));
  }

public:
  SeaveCohort() : dis(0,1),vaccine_dis(80,20),
  Population() {};
  Person* simulate(){
    Person* person = this->generate();

    while (person->age<18 || person->age>65 || (person->sex == "Male" && this->dis(this->gen)<0.5)) {
      person = this->generate();
    }


    //simulate infections
    for(int i=this->istart; i<this->ndays+this->istart; i++){
      double p = this->pandemic->get_p_infection(i);
      if(this->dis(this->gen) < p){
        person->infection_dates.push_back(i);
        i+=50;//dont get infected for another 50 days
      }
    }


    int days = int(0);//*p_vaccine);
    //days = 100;
    int nvaccines = 1;
    for(int i=0; i<nvaccines; i++){
      //random not vaccinated with this dose
      if(dis(this->gen) < 0.5) break;

      days += int(vaccine_dis(this->gen));
      person->vaccine_dates.push_back(days);
    }

    person->create_immune_response();
    person->create_infection_response();


    //simulate severe outcomes, on infection dates + 10 days
    for(auto date: person->infection_dates){
      for(int i = date;i<date+10;i++){
        double h = 50.*person->get_hazard(i);
        //std::cout << h << " " << std::endl;
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
Rcpp::DataFrame generate_simple_vaccine_effectiveness(double n=10000){

  SeaveCohort *pop = new SeaveCohort();
  Pandemic *pandemic = new Pandemic();

  std::map<std::string,Variant*> variants;
  variants["A"] = new Variant(0.4,50,20);

  pandemic->set_variants(variants);
  pop->set_pandemic(pandemic);


  std::uniform_int_distribution<int> dist(1,365*2);
  std::random_device rd;
  std::mt19937_64 gen(rd());

  Rcpp::IntegerVector id(n);
  Rcpp::IntegerVector age(n);
  Rcpp::CharacterVector sex(n);
  Rcpp::NumericVector bmi(n);
  Rcpp::IntegerVector nrisks(n);
  Rcpp::IntegerVector date(n);
  Rcpp::IntegerVector vdate1(n);

  //resolution of measurement
  std::normal_distribution<double> res(1,0.1);

  int i=0;
  while(i<n){
    Person *p = pop->simulate();

    id[i] = i+1;
    age[i] = p->get_age();
    sex[i] = p->get_sex();
    bmi[i] = p->get_bmi();
    nrisks[i] = p->comorbidities.size();
    vdate1[i] = (p->vaccine_dates.size() > 0) ? p->vaccine_dates.at(0) : NA_REAL;
    date[i] = (p->outcome_dates.size() > 0) ? p->outcome_dates.at(0) : NA_REAL;

    i++;
  }


  Rcpp::DataFrame df =  Rcpp::DataFrame::create(
    Rcpp::Named("id")=id,
    Rcpp::Named("age")=age,
    Rcpp::Named("sex")=sex,
    Rcpp::Named("bmi")=bmi,
    Rcpp::Named("nrisks")=nrisks,
    Rcpp::Named("hosp_date")=date,
    Rcpp::Named("v1_date")=vdate1
  );

  return df;
}







