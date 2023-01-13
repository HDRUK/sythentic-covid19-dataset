#ifndef POPULATION_H
#define POPULATION_H

#include <iostream>
#include <random>
#include <string>
#include <map>
#include "comorbidity.h"
#include "pandemic.h"

class Person{
public:
  int age;
  std::string sex;
  int imd;
  double bmi;
  std::map<std::string,Comorbidity*> comorbidities;
  Person() {};
  ~Person() {
    //comorbidities.clear();
    // vaccine_dates.clear();
    //infection_dates.clear();
    //outcome_dates.clear();
  }
  std::vector<int> vaccine_dates;
  std::vector<int> infection_dates;
  std::vector<int> outcome_dates;
  int get_age() { return this->age;};
  double get_bmi() { return this->bmi;};
  std::string get_sex() { return this->sex;};
  int get_imd() { return this->imd;};
  int get_nrisks() {return this->comorbidities.size();};
  void set_immune_response();
  double get_immune_response(int);
private:
  std::vector<std::function<double(int)>> _immune_response;
  //int get_nrisks() {return this->comorbidities.size();};
};


class Population {
protected:
  int age_turn = 50;
  int age_end = 105;
  std::mt19937_64 gen;
  std::map<std::string,Comorbidity*> comorbidities;
public:
  Pandemic* pandemic;
  explicit Population();
  int get_age();
  int get_imd();
  double get_bmi();
  std::string get_sex();
  Person* generate();
  Person test();
  ~Population(){};
};

#endif /* POPULATION_H */
