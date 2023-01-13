#ifndef COMORBIDITY_H
#define COMORBIDITY_H

#include <iostream>
#include <random>
#include <string>
#include <map>
#include <functional>
#include "population.h"

//forward declare
class Person;

class Comorbidity{
public:
  explicit Comorbidity(std::function<double(Person*)> lambda_prev,
                       std::function<double(Person*)> lambda_immune){
    this->lambda_prev = lambda_prev;
    this->lambda_immune = lambda_immune;
  };
  double get_prevelance(Person* person){
    return this->lambda_prev(person);
  };
  double get_immune_influence(Person* person){
    return this->lambda_immune(person);
  };
private:
  std::function<double(Person*)> lambda_prev;
  std::function<double(Person*)> lambda_immune;
};

#endif /* COMORBIDITY_H */
