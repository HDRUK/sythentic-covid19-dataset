#include "pandemic.h"
#include "variant.h"
#include <map>

Pandemic::Pandemic(){
  this->variants["A"] = new Variant(0.1,50,5*2);
  this->variants["B"] = new Variant(0.5,80,3*2);
  this->variants["B2"] = new Variant(0.2,130,20);
  this->variants["C"] = new Variant(0.6,200,30);
  this->variants["D"] = new Variant(0.1,300,12*2);
  this->variants["E"] = new Variant(0.4,400,12*2);

};

std::map<std::string,Variant*> Pandemic::get_variants(){
  return this->variants;
};

double Pandemic::get_p_infection(int day){
  double value=0;
  for (auto &var: this->variants){
    value += var.second->get_abundance(day);
  }
  return value;
}

std::vector<double> Pandemic::run(std::vector<double> _x){
  std::vector<double> retval;
  for(auto x : _x){
    double value = 0;
    for (auto &var: this->variants){
      value += var.second->get_abundance(x);
    }
    retval.push_back(value);
  }
  return retval;
};

