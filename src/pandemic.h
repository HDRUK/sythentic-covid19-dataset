#ifndef PANDEMIC_H
#define PANDEMIC_H

#include "variant.h"
#include <map>

class Pandemic{
private:
  std::map<std::string,Variant*> variants;
public:
  Pandemic();
  std::map<std::string,Variant*> get_variants();
  std::vector<double> run(std::vector<double>);
  double get_p_infection(int day);
  /*
   for(int i=0; i<360; i++) {
   double a = 0;
   for(auto &v: variants){
   a+= v->get_abundance(i);
   }
   std::cout << a << std::endl;
   //std::cout << std::setw(2) << std::setfill('0') << i << ' '
   //		<< std::string(v,'*') << '\n';
   }
   }*/
};

#endif
