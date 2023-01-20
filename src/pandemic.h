#ifndef PANDEMIC_H
#define PANDEMIC_H

#include "variant.h"
#include <map>

class Pandemic{
private:
  std::map<std::string,Variant*> variants;
public:
  Pandemic();
  void set_variants(std::map<std::string, Variant*> variants);
  std::map<std::string,Variant*> get_variants();
  std::vector<double> run(std::vector<double>);
  double get_p_infection(int day);
};

#endif
